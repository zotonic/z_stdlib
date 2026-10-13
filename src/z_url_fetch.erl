% @author Marc Worrell
%% @copyright 2014-2026 Marc Worrell
%% @doc Fetch (part of) the data of an Url, including its headers. Also
%% support decoding 'data:' URLs and streaming fetched data to a device.
%% Uses the httpc OTP library for the actual fetching.
%%
%% The fetch functions are able to unzip partially fetched data.
%%
%% Set {autoredirect, false} for fixed protocol endpoints: redirect responses are
%% returned as HTTP errors without contacting the Location URL or forwarding credentials.
%%
%% Explicit authorization only follows HTTPS redirects within the same origin.
%% Other redirects return {error, authorization_redirect} without sending a request.
%%
%% {authorization, none} suppresses authorization on every request, including redirects.
%%
%% {options_fun, Fun} resolves request options per destination. Fun(Method, Url, Options)
%% receives the method, normalized binary URL and original options (without options_fun).
%% Use it for destination-specific credentials instead of forwarding a fixed token.
%%
%% Returned options apply only to that request. The original anonymous option always
%% takes precedence. Transfer options (device and max_length) are set by the caller.
%% @end

%% Copyright 2014-2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_url_fetch).

-author("Marc Worrell <marc@worrell.nl>").

%% Maximum nmber of bytes fetched for metadata extraction
-define(HTTPC_LENGTH, 64*1024).
-define(HTTPC_MAX_LENGTH, 1024*1024*100).  % Max 100MB

%% Number of redirects followed before giving up
-define(HTTPC_REDIRECT_COUNT, 10).

%% Total request timeout
-define(HTTPC_TIMEOUT, 20000).

%% Connect timeout, server has to respond before this
-define(HTTPC_TIMEOUT_CONNECT, 10000).

%% Some url shorteners return HTML+Javascript, except for simple text-only browsers
-define(CURL_UA, "curl/7.21.4 (universal-apple-darwin11.0) libcurl/7.21.4 OpenSSL/0.9.8r zlib/1.2.5").

%% Use our own user agent string. Sites sometimes handle well known user agents like 
%% twitterbot or apple messages badly. Using our own user agent string works better.
%% Picky websites to test this on: asos.com, hm.com.
-define(HTTPC_UA, "ZStdLib/1.0").

% Default Accept header
-define(HTTP_ACCEPT, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8").

-export([
    fetch/2,
    fetch/4,
    fetch_partial/1,
    fetch_partial/2,
    fetch_partial/4,

    profile/1,
    ensure_profiles/0,
    periodic_cleanup/0
    ]).

-type options() :: list(option()).

-type option() :: {device, pid()}
                | {timeout, pos_integer()}
                | {max_length, pos_integer()}
                | {use_range, boolean()}
                | {autoredirect, boolean()}
                | {insecure, boolean()}
                | {authorization, binary() | string() | none | undefined}
                | {options_fun, options_fun()}
                | {accept, binary() | string()}
                | {user_agent, binary() | string()}
                | {language, atom()}
                | {content_type, binary() | string()}
                | {headers, [ {binary()|string(), binary()|string()} ]}
                | insecure.

-type options_fun() :: fun((get | post | delete | put | patch, binary(), options()) -> options()).

-type fetch_result() :: {ok, {
            FinalUrl :: string(),
            RespHeaders :: list({string(), string()}),
            ContentLength :: non_neg_integer(),
            Content :: binary()
        }}
        | {error, Reason :: term()}.

-export_type([
    options/0,
    option/0,
    options_fun/0,
    fetch_result/0
]).

-define(is_method(M), (M =:= get orelse M =:= post orelse M =:= delete orelse M =:= put orelse M =:= patch)).

-include_lib("kernel/include/logger.hrl").


%% @doc Fetch the data and headers from an url
-spec fetch(Url, Options) -> fetch_result() when
    Url :: string() | binary(),
    Options :: options().
fetch(Url, Options) ->
    fetch_partial(get, Url, <<>>, Options).

%% @doc Fetch the data and headers from an url
-spec fetch(Method, Url, Payload, Options) -> fetch_result() when
    Method :: get | post | put | delete | patch,
    Url :: string()|binary(),
    Payload :: binary(),
    Options :: options().
fetch(Method, Url, Payload, Options) when is_binary(Payload), ?is_method(Method) ->
    fetch_partial(Method, Url, Payload, Options).


%% @doc Fetch the first 64 kilobytes of data and headers from an url
-spec fetch_partial(Url) -> fetch_result() when
    Url :: string() | binary().
fetch_partial(Url) ->
    fetch_partial(get, Url, <<>>, [{max_length, ?HTTPC_LENGTH}]).

%% @doc Fetch the first N bytes of data and headers from an url, optionally save to the file device
-spec fetch_partial(Url, Options) -> fetch_result() when
    Url :: string() | binary(),
    Options :: options().
fetch_partial("data:" ++ _ = DataUrl, Options) ->
    fetch_data_url(DataUrl, Options);
fetch_partial(<<"data:", _/binary>> = DataUrl, Options) ->
    fetch_data_url(DataUrl, Options);
fetch_partial(Url, Options) ->
    fetch_partial(get, Url, <<>>, Options).

% Track the fetch state, some requests use redirects, some need
% multiple range fetches.
-record(fstate, {
    method :: atom(),
    url :: string(),
    payload :: binary(),
    code :: undefined | integer(),
    headers :: list({string(), string()}),
    redirects = 0 :: non_neg_integer(),
    max = undefined :: non_neg_integer() | undefined,
    length = 0 :: non_neg_integer(),
    data = <<>> :: binary(),
    device = undefined :: pid() | undefined,
    options :: options(),
    request_options = [] :: options()
}).

%% @doc Fetch the first N bytes of data and headers from an url, optionally save to the file device
-spec fetch_partial(Method, Url, Payload, Options) -> fetch_result() when
    Method :: get | post | delete | put | patch,
    Url :: string()|binary(),
    Payload :: binary(),
    Options :: options().
fetch_partial(Method, Url, Payload, Options) when is_binary(Payload), ?is_method(Method) ->
    OutDevice = proplists:get_value(device, Options),
    MaxLength = proplists:get_value(max_length, Options, ?HTTPC_MAX_LENGTH),
    FState = #fstate{
        code = undefined,
        method = Method,
        url = z_convert:to_list(Url),
        headers = [],
        redirects = 0,
        payload = Payload,
        length = 0,
        max = MaxLength,
        data = <<>>,
        device = OutDevice,
        options = Options
    },
    case fetch_partial_loop(FState) of
        {ok, #fstate{ code = Code } = FState1} ->
            FState2 = maybe_handle_content_encoding(FState1),
            if
                Code >= 200 andalso Code < 300 ->
                    {ok, {
                        FState2#fstate.url,
                        FState2#fstate.headers,
                        FState2#fstate.length,
                        FState2#fstate.data
                    }};
                true ->
                    {error, {
                        Code,
                        FState2#fstate.url,
                        FState2#fstate.headers,
                        FState2#fstate.length,
                        FState2#fstate.data
                    }}
            end;
        {error, _} = Error ->
            Error
    end.

-spec ensure_profiles() -> ok.
ensure_profiles() ->
    case inets:start(httpc, [{profile, z_url_fetch}]) of
        {ok, _} ->
            ok = httpc:set_options([
                {max_sessions, 10},
                {max_keep_alive_length, 10},
                {keep_alive_timeout, 20000},
                {cookies, enabled}
            ], z_url_fetch),
            periodic_cleanup(),
            ok;
        {error, {already_started, _}} -> ok
    end.

-spec periodic_cleanup() -> ok.
periodic_cleanup() ->
    httpc:reset_cookies(z_url_fetch),
    {ok, _} = timer:apply_after(3600*1000, ?MODULE, periodic_cleanup, []),
    ok.

-spec profile(string()|binary()) -> atom().
profile(_Url) ->
    ensure_profiles(),
    z_url_fetch.

%% -------------------------------------- Fetch first part of a HTTP location -----------------------------------------

fetch_data_url(DataUrl, Options) when is_list(DataUrl) ->
    fetch_data_url(iolist_to_binary(DataUrl), Options);
fetch_data_url(DataUrl, Options) when is_binary(DataUrl) ->
    case z_url:decode_data_url(DataUrl) of
        {ok, Mime, _Charset, Bytes} ->
            % TODO: charset
            Headers = [
                {"content-type", z_convert:to_list(Mime)},
                {"content-length", z_convert:to_list(size(Bytes))}
            ],
            case proplists:get_value(device, Options) of
                undefined ->
                    {ok, {"data://", Headers, size(Bytes), Bytes}};
                Dev ->
                    file:write(Dev, Bytes),
                    {ok, {"data://", Headers, size(Bytes), <<>>}}
            end;
        {error, _} = Error ->
            Error
    end.

fetch_partial_loop(#fstate{ url = Url, redirects = Redirects }) when Redirects >= ?HTTPC_REDIRECT_COUNT ->
    ?LOG_ERROR(#{
        in => zotonic_stdlib,
        text => <<"Error fetching URL, too many redirects">>,
        result => error,
        reason => too_many_redirects,
        redirects => Redirects,
        url => unicode:characters_to_binary(Url, utf8)
    }),
    {error, too_many_redirects};
fetch_partial_loop(#fstate{ url = Url0, payload = Payload } = FState) when is_binary(Payload) ->
    httpc_flush(),
    case normalize_url(Url0) of
        {ok, {Host, UrlBin}} ->
            #fstate{
                method = Method,
                max = Max,
                length = Length,
                options = Opts
            } = FState,
            Url = to_list(UrlBin),
            RequestOptions = request_options(Method, UrlBin, Opts),
            {Headers, ContentType} = headers_ct(Method, Url, Length, Max, RequestOptions),
            Request = case Method of
                get -> {Url, Headers};
                delete when Payload =:= <<>> -> {Url, Headers};
                delete -> {Url, Headers, ContentType, Payload};
                post -> {Url, Headers, ContentType, Payload};
                put -> {Url, Headers, ContentType, Payload};
                patch -> {Url, Headers, ContentType, Payload}
            end,
            FState1 = FState#fstate{
                url = Url,
                request_options = RequestOptions
            },
            case fetch_stream(start_stream(Host, Method, Url, Request, RequestOptions), FState1) of
                {ok, Result} ->
                    case maybe_redirect(Result) of
                        {redirect, NewMethod, NewUrl, NewOpts} ->
                            FState2 = FState1#fstate{
                                method = NewMethod,
                                payload = case Result#fstate.code of 303 -> <<>>; _ -> Payload end,
                                url = NewUrl,
                                headers = [],
                                redirects = FState1#fstate.redirects + 1,
                                length = 0,
                                data = <<>>,
                                options = NewOpts
                            },
                            fetch_partial_loop(FState2);
                        {ok, #fstate{ method = ResMethod, code = Code, max = ResMax, length = ResLength } = FState2} when
                                Code >= 200, Code < 300,
                                is_integer(Max), ResLength < ResMax,
                                ResMethod =:= get ->
                            case is_fetch_complete(FState2#fstate.headers, ResMax) of
                                true ->
                                    {ok, FState2};
                                false ->
                                    fetch_partial_loop(FState2)
                            end;
                        {ok, FState2} ->
                            {ok, FState2};
                        {error, _} = Error ->
                            Error
                    end;
                {error, Reason} = Error ->
                    ?LOG_WARNING(#{
                        in => zotonic_stdlib,
                        text => <<"Error fetching URL">>,
                        result => error,
                        reason => Reason,
                        url => unicode:characters_to_binary(Url)
                    }),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

is_fetch_complete(_Hs, undefined) ->
    true;
is_fetch_complete(Hs, Max) ->
    % {"content-length","1000000"},
    % {"content-range","bytes 0-999999/4645135"},
    case proplists:get_value("content-range", Hs) of
        undefined ->
            true;
        Range ->
            case cow_http_hd:parse_content_range(list_to_binary(Range)) of
                {bytes, '*', _} ->
                    true;
                {_, _} ->
                    true;
                {bytes, _First, Last, '*'} ->
                    Last >= Max + 1;
                {bytes, _First, Last, Complete} when Last + 1 == Complete ->
                    true;
                {bytes, _First, Last, _Complete} when Last >= Max + 1 ->
                    true;
                {bytes, _First, _Last, _Complete} ->
                    false
            end
    end.

headers_ct(Method, Url, TotalSz, Max, Opts) ->
    Language = z_convert:to_list(proplists:get_value(language, Opts, en)),
    Accept = z_convert:to_list(proplists:get_value(accept, Opts, ?HTTP_ACCEPT)),
    UserAgent = z_convert:to_list(proplists:get_value(user_agent, Opts, httpc_ua(Url))),
    UseRange = z_convert:to_bool(proplists:get_value(use_range, Opts, true)),
    ContentType = case proplists:get_value(content_type, Opts) of
        undefined -> "application/octet-stream";
        CT -> to_list(CT)
    end,
    Headers0 = [
        {"Accept", Accept},
        {"Accept-Encoding", "identity"},
        {"Accept-Charset", "UTF-8;q=1.0, ISO-8859-1;q=0.5, *;q=0"},
        {"Accept-Language", Language ++ ",*;q=0"},
        {"User-Agent", UserAgent}
    ],
    Headers1 = if
        is_integer(Max), Method =:= get, UseRange ->
            Range = "bytes="
                ++ integer_to_list(TotalSz)
                ++ "-"
                ++ integer_to_list(Max-1),
            [ {"Range", Range} | Headers0 ];
        true ->
            Headers0
    end,
    Headers2 = case proplists:get_value(authorization, Opts) of
        undefined ->
            Headers1;
        Auth ->
            [ {"Authorization", to_list(Auth)} | Headers1 ]
    end,
    FinalHeaders = case proplists:get_value(headers, Opts) of
        undefined ->
            Headers2;
        [] ->
            Headers2;
        Hs ->
            Hs1 = lists:map(
                fun({K,V}) ->
                    {z_convert:to_list(K), z_convert:to_list(V)}
                end,
                Hs),
            Headers2 ++ Hs1
    end,
    {FinalHeaders, ContentType}.

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

-spec normalize_url(string() | binary()) -> {ok, {binary(), binary()}} | {error, url}.
normalize_url(Url) ->
    case uri_string:parse(z_convert:to_binary(Url)) of
        #{
            host := Host,
            path := Path
        } = Parts ->
            Scheme = maps:get(scheme, Parts, <<"http">>),
            Port = case maps:get(port, Parts, undefined) of
                undefined -> <<>>;
                P -> <<$:,(integer_to_binary(P))/binary>>
            end,
            Query = case maps:get('query', Parts, <<>>) of
                <<>> -> <<>>;
                Q -> <<"?", Q/binary>>
            end,
            Url1 = iolist_to_binary([ Scheme, "://", Host, Port, Path, Query ]),
            {ok, {Host, Url1}};
        _ ->
            {error, url}
    end.

start_stream(Host, Method, Url, Request, Opts) ->
    SSLOptions = case proplists:get_value(insecure, Opts) of
        true ->
            [ {verify, verify_none} ];
        _ ->
            tls_certificate_check:options(Host)
    end,
    Timeout = proplists:get_value(timeout, Opts, ?HTTPC_TIMEOUT),
    HttpOptions = [
        {autoredirect, false},
        {relaxed, true},
        {timeout, Timeout},
        {connect_timeout, ?HTTPC_TIMEOUT_CONNECT},
        {ssl, SSLOptions}
     ],
    try
        httpc:request(Method,
                      Request,
                      HttpOptions,
                      [ {sync, false}, {body_format, binary}, {stream, {self, once}} ],
                      profile(Url))
    catch
        error:E -> {error, E};
        throw:E -> {error, E}
    end.


fetch_stream({ok, ReqId}, #fstate{ request_options = Opts } = FState) ->
    Timeout = proplists:get_value(timeout, Opts, ?HTTPC_TIMEOUT),
    receive
        {http, {ReqId, stream_end, Hs}} ->
            FState1 = FState#fstate{
                code = 200,
                headers = Hs ++ FState#fstate.headers
            },
            {ok, FState1};
        {http, {ReqId, stream_start, Hs, HandlerPid}} ->
            httpc:stream_next(HandlerPid),
            FState1 = FState#fstate{
                headers = Hs
            },
            fetch_stream_data(ReqId, HandlerPid, FState1);
        {http, {ReqId, {error, _} = Error}} ->
            Error;
        {http, {ReqId, {{_V, Code, _Msg}, Hs, Data}}} when is_binary(Data) ->
            case response_data(Code, Data, FState) of
                {ok, Data1} ->
                    FState1 = FState#fstate{
                        code = Code,
                        headers = Hs ++ FState#fstate.headers,
                        data = Data1
                    },
                    {ok, FState1};
                {error, _} = Error ->
                    Error
            end;
        {http, {ReqId, {{_V, Code, _Msg}, Hs, _Data}}} ->
            FState1 = FState#fstate{
                code = Code,
                headers = Hs ++ FState#fstate.headers
            },
            {ok, FState1}
    after Timeout ->
        httpc:cancel_request(ReqId, z_url_fetch),
        {error, timeout}
    end;
fetch_stream({error, _} = Error, _FState) ->
    Error.

fetch_stream_data(ReqId, HandlerPid, #fstate{ length = Length, max = Max } = FState) when
        Max =:= undefined; Length =< Max ->
    #fstate{
        request_options = Opts,
        data = Data
    } = FState,
    Timeout = proplists:get_value(timeout, Opts, ?HTTPC_TIMEOUT),
    receive
        {http, {ReqId, stream_end, EndHs}} ->
            FState1 = FState#fstate{
                code = 200,
                headers = EndHs ++ FState#fstate.headers
            },
            {ok, FState1};
        {http, {ReqId, stream, Part}} ->
            case append_data(Data, Part, FState#fstate.device) of
                {ok, Data1} ->
                    Length1 = Length + size(Part),
                    FState1 = FState#fstate{
                        length = Length1,
                        data = Data1
                    },
                    case Length1 =< Max of
                        true ->
                            httpc:stream_next(HandlerPid),
                            fetch_stream_data(ReqId, HandlerPid, FState1);
                        false ->
                            httpc:cancel_request(ReqId, z_url_fetch),
                            FState2 = FState1#fstate{
                                code = 200
                            },
                            {ok, FState2}
                    end;
                {error, _} = Error ->
                    httpc:cancel_request(ReqId, z_url_fetch),
                    Error
            end;
        {http, {ReqId, {error, socket_closed_remotely}}} ->
            % Remote closed the connection, this can happen at the moment
            % we received all data, then this error is received instead of
            % the expected data.
            % Return the data we received till now and pretend nothing is wrong.
            FState1 = FState#fstate{
                code = 200
            },
            {ok, FState1};
        {http, {ReqId, {error, _} = Error}} ->
            Error
    after Timeout ->
        httpc:cancel_request(ReqId, z_url_fetch),
        {error, timeout}
    end;
fetch_stream_data(ReqId, _HandlerPid, FState) ->
    receive
        {http, {ReqId, stream_end, EndHs}} ->
            FState1 = FState#fstate{
                code = 200,
                headers = EndHs ++ FState#fstate.headers
            },
            {ok, FState1};
        {http, _} ->
            httpc:cancel_request(ReqId, z_url_fetch),
            FState1 = FState#fstate{
                code = 200
            },
            {ok, FState1}
    after 100 ->
        httpc:cancel_request(ReqId, z_url_fetch),
        FState1 = FState#fstate{
            code = 200
        },
        {ok, FState1}
    end.

%% @doc Keep redirect policy above httpc, which always has autoredirect disabled.
maybe_redirect(#fstate{request_options = Options, code = Code} = FState)
        when Code >= 300, Code < 400 ->
    case proplists:get_value(autoredirect, Options, true) of
        true -> maybe_redirect_1(FState);
        false -> {ok, FState}
    end;
maybe_redirect(FState) ->
    maybe_redirect_1(FState).

maybe_redirect_1(#fstate{ code = Code } = FState) when Code >= 200, Code =< 299 ->
    {ok, FState};
maybe_redirect_1(#fstate{ code = 416, options = Opts, url = Url }) ->
    % 416 Range Not Satisfiable - if this is the first request then it might
    % be a picky server complaining that we requested beyond the size of the
    % document.
    Opts1 = proplists:delete(use_range, Opts),
    {redirect, get, Url, [ {use_range, false} | Opts1 ]};
maybe_redirect_1(#fstate{ code = Code, headers = Headers, url = Url, options = Options } = FState)
    when Code =:= 301; Code =:= 302; Code =:= 303; Code =:= 307; Code =:= 308 ->
    case proplists:get_value("location", Headers) of
        undefined ->
            {error, no_location_header};
        Location ->
            NextUrl = z_convert:to_binary(z_url:abs_link(Location, Url)),
            case redirect_options(z_convert:to_binary(Url), NextUrl, Options) of
                {ok, NextOptions} ->
                    NextMethod = case Code of
                        303 -> get;
                        _ -> FState#fstate.method
                    end,
                    {redirect, NextMethod, to_list(NextUrl), NextOptions};
                {error, _} = Error ->
                    Error
            end
    end;
maybe_redirect_1(FState) ->
    {ok, FState}.

%% Always start from the caller's options, never from the last callback result.
request_options(Method, Url, Options) ->
    BaseOptions = proplists:delete(options_fun, Options),
    RequestOptions = case proplists:get_value(options_fun, Options) of
        undefined -> BaseOptions;
        OptionsFun when is_function(OptionsFun, 3) -> OptionsFun(Method, Url, BaseOptions)
    end,
    EffectiveOptions = case proplists:lookup(authorization, Options) of
        {authorization, Auth} ->
            case is_empty(Auth) of
                true ->
                    [
                        {authorization, none}
                        | proplists:delete(authorization, RequestOptions)
                    ];
                false ->
                    RequestOptions
            end;
        _ ->
            RequestOptions
    end,
    cleanup_options(EffectiveOptions).

%% Caller-supplied authorization cannot follow redirects to another origin or
%% to cleartext HTTP. Anonymous intent persists, while callback credentials are
%% resolved independently for each destination from the original caller options.
redirect_options(Url, Next, Options) ->
    IsSameOrigin = origin(Url) =:= origin(Next),
    IsTLS = case origin(Next) of
        {<<"https">>, _, _} -> true;
        _ -> false
    end,
    case has_authorization(Options) andalso not (IsSameOrigin andalso IsTLS) of
        true ->
            {error, authorization_redirect};
        false when IsSameOrigin ->
            {ok, Options};
        false ->
            {ok, lists:map(
                fun
                    ({headers, Headers}) ->
                        {headers, [
                            Header || {Name, _} = Header <- Headers,
                                      not lists:member(
                                            string:lowercase(z_convert:to_binary(Name)),
                                            [<<"cookie">>, <<"proxy-authorization">>])
                        ]};
                    (Option) -> Option
                end,
                Options)}
    end.

has_authorization(Options) ->
    not is_empty(proplists:get_value(authorization, Options))
    orelse lists:any(
                fun({Name, _}) ->
                    string:lowercase(z_convert:to_binary(Name)) =:= <<"authorization">>
                end,
                proplists:get_value(headers, Options, [])).

is_empty(undefined) -> true;
is_empty(none) -> true;
is_empty(<<>>) -> true;
is_empty("") -> true;
is_empty(_) -> false.

origin(Url) ->
    case uri_string:parse(Url) of
        #{ scheme := Scheme, host := Host } = Parts ->
            NormalizedScheme = z_string:to_lower(Scheme),
            Port = maps:get(port, Parts, default_port(NormalizedScheme)),
            {NormalizedScheme, z_string:to_lower(Host), Port};
        _ ->
            undefined
    end.

default_port(<<"https">>) -> 443;
default_port(<<"http">>) -> 80;
default_port(_) -> undefined.

cleanup_options(Options) ->
    IsAuthAnonymous = case proplists:lookup(authorization, Options) of
        {authorization, Auth} -> is_empty(Auth);
        _ -> false
    end,
    lists:filtermap(
        fun
            ({authorization, _None}) when IsAuthAnonymous ->
                false;
            ({headers, Headers}) when IsAuthAnonymous ->
                {true, {headers, [
                    Header || {Name, _} = Header <- Headers,
                              z_string:to_lower(z_convert:to_binary(Name)) =/= <<"authorization">>
                ]}};
            (Option) ->
                {true, Option}
        end,
        Options).

%% Redirect/retry bodies must not be appended to the caller's output device.
%% Keep them in memory until the redirect decision; final responses still stream.
response_data(416, Data, #fstate{data = Previous}) ->
    {ok, <<Previous/binary, Data/binary>>};
response_data(Code, Data, #fstate{data = Previous, device = Device, request_options = Options})
    when Code =:= 301; Code =:= 302; Code =:= 303; Code =:= 307; Code =:= 308 ->
    case proplists:get_value(autoredirect, Options, true) of
        true -> {ok, <<Previous/binary, Data/binary>>};
        false -> append_data(Previous, Data, Device)
    end;
response_data(_, Data, #fstate{data = Previous, device = Device}) ->
    append_data(Previous, Data, Device).


append_data(Data, <<>>, _Device) ->
    {ok, Data};
append_data(Data, Part, undefined) ->
    {ok, <<Data/binary, Part/binary>>};
append_data(Data, Part, OutDev) ->
    case file:write(OutDev, Part) of
        ok -> {ok, Data};
        {error, _} = Error -> Error
    end.

%% @doc Some servers (Spotify) deliver gzip encoded content, even when we ask for identity.
maybe_handle_content_encoding(#fstate{ length = Length, headers = Hs, data = Data } = FState)
    when Length > 0, is_binary(Data), Data =/= <<>> ->
    CE = proplists:get_value("content-encoding", Hs, "identity"),
    handle_ce(CE, FState);
maybe_handle_content_encoding(FState) ->
    FState.

handle_ce("gzip", #fstate{ length = Length, data = Data } = FState)
    when Length > 0, is_binary(Data), Data =/= <<>> ->
    % Decode partial gzip data
    case partial_unzip(Data, FState#fstate.max) of
        {ok, Data1} ->
            FState1 = FState#fstate{
                length = size(Data1),
                data = Data1
            },
            FState1;
        {error, _} ->
            FState
    end;
handle_ce(_ContentEncoding, FState) ->
    FState.

partial_unzip(Compressed, MaxLength) ->
    Z = zlib:open(),
    zlib:inflateInit(Z, 16 + 15),
    try
        Uncompressed = unzip_loop(Z, <<>>, zlib:safeInflate(Z, Compressed), MaxLength),
        {ok, Uncompressed}
    catch
        _:_ ->
            {error, gunzip}
    after
        zlib:close(Z)
    end.

unzip_loop(_Z, Acc, _, MaxLength) when size(Acc) >= MaxLength ->
    Acc;
unzip_loop(Z, Acc, {continue, Output}, MaxLength) ->
    Out1 = iolist_to_binary(Output),
    Acc1 = <<Acc/binary, Out1/binary>>,
    Next = try
        zlib:safeInflate(Z, [])
    catch
        _:_ ->
            {finished, <<>>}
    end,
    unzip_loop(Z, Acc1, Next, MaxLength);
unzip_loop(_Z, Acc, {finished, Output}, _MaxLength) ->
    Out1 = iolist_to_binary(Output),
    <<Acc/binary, Out1/binary>>.

%% @doc Flush any late results from previous requests
httpc_flush() ->
    receive
        {http, _} -> httpc_flush()
    after 0 ->
        ok
    end.

%% @doc Some url shorteners return HTML+Javascript, except for simple text-only browsers
httpc_ua(Url) ->
    case is_url_shortener(Url) of
        true -> ?CURL_UA;
        false -> ?HTTPC_UA
    end.

is_url_shortener(Url) ->
    case string:tokens(Url, "://") of
        [_Proto, DomainPath | _] ->
            is_url_shortener_1(DomainPath);
        _ ->
            false
    end.

is_url_shortener_1("t.co/" ++ _) -> true;
is_url_shortener_1("bit.ly/" ++ _) -> true;
is_url_shortener_1("ow.ly/" ++ _) -> true;
is_url_shortener_1("goo.gl/" ++ _) -> true;
is_url_shortener_1("lnkd.in/" ++ _) -> true;
is_url_shortener_1("tinyurl.com/" ++ _) -> true;
is_url_shortener_1("j.mp/" ++ _) -> true;
is_url_shortener_1("fb.me/" ++ _) -> true;
is_url_shortener_1("wp.me/" ++ _) -> true;
is_url_shortener_1("gu.com/" ++ _) -> true;
is_url_shortener_1("nyti.ms/" ++ _) -> true;
is_url_shortener_1("s.vk.nl/" ++ _) -> true;
is_url_shortener_1(_) -> false.
