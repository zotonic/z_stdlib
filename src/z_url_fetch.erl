% @author Marc Worrell
%% @copyright 2014-2022 Marc Worrell
%% @doc Fetch (part of) the data of an Url, including its headers.

%% Copyright 2014-2022 Marc Worrell
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
                | {authorization, binary() | string()}
                | {accept, binary() | string()}
                | {user_agent, binary() | string()}
                | {language, atom()}
                | {content_type, binary() | string()}
                | insecure.

-type fetch_result() :: {ok, {string(), list({string(), string()}), pos_integer(), binary()}} | {error, term()}.

-export_type([
    options/0,
    option/0,
    fetch_result/0
]).

-define(is_method(M), (M =:= get orelse M =:= post orelse M =:= delete orelse M =:= put orelse M =:= patch)).

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


%% @doc Fetch the first kilobytes of data and headers from an url
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

%% @doc Fetch the first N bytes of data and headers from an url, optionally save to the file device
-spec fetch_partial(Method, Url, Payload, Options) -> fetch_result() when
    Method :: get | post | delete | put | patch,
    Url :: string()|binary(),
    Payload :: binary(),
    Options :: options().
fetch_partial(Method, Url, Payload, Options) when is_binary(Payload), ?is_method(Method) ->
    OutDevice = proplists:get_value(device, Options),
    MaxLength = proplists:get_value(max_length, Options, ?HTTPC_MAX_LENGTH),
    fetch_partial(Method, z_convert:to_list(Url), Payload, 0, MaxLength, OutDevice, Options).


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
                    {ok, {200, Headers, size(Bytes), Bytes}};
                Dev ->
                    file:write(Dev, Bytes),
                    {ok, {200, Headers, size(Bytes), <<>>}}
            end;
        {error, _} = Error ->
            Error
    end.

fetch_partial(_Method, Url0, _Payload, RedirectCount, _Max, _OutDev, _Opts) when RedirectCount >= ?HTTPC_REDIRECT_COUNT ->
    error_logger:warning_msg("Error fetching url, too many redirects ~p", [Url0]),
    {error, too_many_redirects};
fetch_partial(Method, Url0, Payload, RedirectCount, Max, OutDev, Opts) when is_binary(Payload) ->
    httpc_flush(),
    case normalize_url(Url0) of
        {ok, {Host, UrlBin}} ->
            Url = to_list(UrlBin),
            Language = z_convert:to_list(proplists:get_value(language, Opts, en)),
            Accept = z_convert:to_list(proplists:get_value(accept, Opts, ?HTTP_ACCEPT)),
            UserAgent = z_convert:to_list(proplists:get_value(user_agent, Opts, httpc_ua(Url))),
            ContentType = case proplists:get_value(content_type, Opts) of
                undefined -> "application/octet-stream";
                CT -> to_list(CT)
            end,
            Headers = [
                {"Accept", Accept},
                {"Accept-Encoding", "identity"},
                {"Accept-Charset", "UTF-8;q=1.0, ISO-8859-1;q=0.5, *;q=0"},
                {"Accept-Language", Language ++ ",*;q=0"},
                {"User-Agent", UserAgent}
            ] ++ case Max of
                undefined -> [];
                _ -> [ {"Range", "bytes=0-"++integer_to_list(Max-1)} ]
            end ++ case proplists:get_value(authorization, Opts) of
                undefined -> [];
                Auth -> [ {"Authorization", to_list(Auth)} ]
            end,
            Request = case Method of
                get -> {Url, Headers};
                delete when Payload =:= <<>> -> {Url, Headers};
                delete -> {Url, Headers, ContentType, Payload};
                post -> {Url, Headers, ContentType, Payload};
                put -> {Url, Headers, ContentType, Payload};
                patch -> {Url, Headers, ContentType, Payload}
            end,
            case fetch_stream(start_stream(Host, Method, Url, Request, Opts), Max, OutDev, Opts) of
                {ok, Result} ->
                    maybe_redirect(Result, Method, Url, Payload, RedirectCount, Max, OutDev, Opts);
                {error, _} = Error ->
                    error_logger:warning_msg("Error fetching url ~p error: ~p", [Url, Error]),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

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


fetch_stream({ok, ReqId}, Max, OutDev, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, ?HTTPC_TIMEOUT),
    receive
        {http, {ReqId, stream_end, Hs}} ->
            {ok, {200, Hs, 0, <<>>}};
        {http, {ReqId, stream_start, Hs, HandlerPid}} ->
            httpc:stream_next(HandlerPid),
            fetch_stream_data(ReqId, HandlerPid, Hs, <<>>, 0, Max, OutDev, Opts);
        {http, {ReqId, {error, _} = Error}} ->
            Error;
        {http, {_ReqId, {{_V, Code, _Msg}, Hs, Data}}} ->
            {ok, {Code, Hs, 0, Data}}
    after Timeout ->
        httpc:cancel_request(ReqId),
        {error, timeout}
    end;
fetch_stream({error, _} = Error, _Max, _OutDev, _Opts) ->
    Error.

fetch_stream_data(ReqId, HandlerPid, Hs, Data, N, Max, OutDev, Opts) when N =< Max ->
    Timeout = proplists:get_value(timeout, Opts, ?HTTPC_TIMEOUT),
    receive
        {http, {ReqId, stream_end, EndHs}} ->
            {ok, {200, EndHs++Hs, N, Data}};
        {http, {ReqId, stream, Part}} ->
            case append_data(Data, Part, OutDev) of
                {ok, Data1} ->
                    N1 = N + size(Part),
                    case N1 =< Max of
                        true ->
                            httpc:stream_next(HandlerPid),
                            fetch_stream_data(ReqId, HandlerPid, Hs, Data1, N1, Max, OutDev, Opts);
                        false ->
                            httpc:cancel_request(ReqId),
                            {ok, {200, Hs, N1, Data1}}
                    end;
                {error, _} = Error ->
                    httpc:cancel_request(ReqId),
                    Error
            end;
        {http, {ReqId, {error, socket_closed_remotely}}} ->
            % Remote closed the connection, this can happen at the moment
            % we received all data, then this error is received instead of
            % the expected data.
            % Return the data we received till now and pretend nothing is wrong.
            {ok, {200, Hs, N, Data}};
        {http, {ReqId, {error, _} = Error}} ->
            Error
    after Timeout ->
        httpc:cancel_request(ReqId),
        {error, timeout}
    end;
fetch_stream_data(ReqId, _HandlerPid, Hs, Data, N, _Max, _OutFile, _Opts) ->
    receive
        {http, {ReqId, stream_end, EndHs}} ->
            {ok, {200, EndHs++Hs, N, Data}};
        {http, _} ->
            httpc:cancel_request(ReqId),
            {ok, {200, Hs, N, Data}}
    after 100 ->
        httpc:cancel_request(ReqId),
        {ok, {200, Hs, N, Data}}
    end.

maybe_redirect({Code, Hs, Size, Data}, _Method, Url, _Payload, _RedirectCount, _Max, _OutDev, _Opts)
    when Code >= 200, Code =< 299 ->
    {ok, {Url, Hs, Size, Data}};
maybe_redirect({416, _Hs, _Size, _Data}, Method, Url, Payload, RedirectCount, _Max, OutDev, Opts) ->
    fetch_partial(Method, Url, Payload, RedirectCount+1, undefined, OutDev, Opts);
maybe_redirect({Code, Hs, _Size, _Data}, Method, BaseUrl, Payload, RedirectCount, Max, OutDev, Opts)
    when Code =:= 301; Code =:= 302; Code =:= 303; Code =:= 307 ->
    case proplists:get_value("location", Hs) of
        undefined ->
            {error, no_location_header};
        Location ->
            NewUrl = z_convert:to_list(z_url:abs_link(Location, BaseUrl)),
            fetch_partial(Method, NewUrl, Payload, RedirectCount+1, Max, OutDev, Opts)
    end;
maybe_redirect({Code, Hs, Size, Data}, _Method, Url, _Payload, _RedirectCount, _Max, _OutDev, _Opts) ->
    {error, {Code, Url, Hs, Size, Data}}.

append_data(Data, Part, undefined) ->
    {ok, <<Data/binary, Part/binary>>};
append_data(Data, Part, OutDev) ->
    case file:write(OutDev, Part) of
        ok -> {ok, Data};
        {error, _} = Error -> Error
    end.

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
