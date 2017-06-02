% @author Marc Worrell
%% @copyright 2014 Marc Worrell
%% @doc Fetch (part of) the data of an Url, including its headers.

%% Copyright 2014 Marc Worrell
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
-define(HTTPC_LENGTH, 32*1024).
-define(HTTPC_MAX_LENGTH, 1024*1024*100).  % Max 100MB

%% Number of redirects followed before giving up
-define(HTTPC_REDIRECT_COUNT, 10).

%% Total request timeout
-define(HTTPC_TIMEOUT, 20000).

%% Connect timeout, server has to respond before this
-define(HTTPC_TIMEOUT_CONNECT, 10000).

%% Some url shorteners return HTML+Javascript, except for simple text-only browsers
-define(CURL_UA, "curl/7.21.4 (universal-apple-darwin11.0) libcurl/7.21.4 OpenSSL/0.9.8r zlib/1.2.5").

%% Some servers handle Twitterbot extra nicely and give it better metadata.
-define(HTTPC_UA, "Twitterbot").


-export([
    fetch/2,
    fetch_partial/1,
    fetch_partial/2,

    profile/1,
    ensure_profiles/0,
    periodic_cleanup/0
    ]).

-type options() :: list(option()).

-type option() :: {device, pid()} | {timeout, pos_integer()} | {max_length, pos_integer()}.

%% @doc Fetch the data and headers from an url
-spec fetch(string()|binary(), options()) -> {ok, {string(), list(), pos_integer(), binary()}} | {error, term()}.
fetch(Url, Options) ->
    fetch_partial(Url, Options).


%% @doc Fetch the first kilobytes of data and headers from an url
-spec fetch_partial(string()|binary()) -> {ok, {string(), list(), pos_integer(), binary()}} | {error, term()}.
fetch_partial(Url) ->
    fetch_partial(Url, [{max_length, ?HTTPC_LENGTH}]).

%% @doc Fetch the first N bytes of data and headers from an url, optionally save to the file device
-spec fetch_partial(string()|binary(), options()) -> {ok, {string(), list(), pos_integer(), binary()}} | {error, term()}.
fetch_partial("data:" ++ _ = DataUrl, Options) ->
    fetch_data_url(DataUrl, Options);
fetch_partial(<<"data:", _/binary>> = DataUrl, Options) ->
    fetch_data_url(DataUrl, Options);
fetch_partial(Url, Options) ->
    OutDevice = proplists:get_value(device, Options),
    Length = proplists:get_value(max_length, Options, ?HTTPC_MAX_LENGTH),
    fetch_partial(z_convert:to_list(Url), 0, Length, OutDevice, Options).


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

fetch_data_url(DataUrl, Options) ->
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

fetch_partial(Url0, RedirectCount, _Max, _OutDev, _Opts) when RedirectCount >= ?HTTPC_REDIRECT_COUNT ->
    error_logger:warning_msg("Error fetching url, too many redirects ~p", [Url0]),
    {error, too_many_redirects};
fetch_partial(Url0, RedirectCount, Max, OutDev, Opts) ->
    httpc_flush(),
    Url = normalize_url(Url0),
    Headers = [
        {"Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"},
        {"Accept-Encoding", "identity"},
        {"Accept-Charset", "UTF-8;q=1.0, ISO-8859-1;q=0.5, *;q=0"},
        {"Accept-Language", "en,*;q=0"},
        {"User-Agent", httpc_ua(Url)}
    ] ++ case Max of
            undefined -> [];
            _ -> [ {"Range", "bytes=0-"++integer_to_list(Max-1)} ]
         end,
    case fetch_stream(start_stream(Url, Headers, Opts), Max, OutDev) of
        {ok, Result} ->
            maybe_redirect(Result, Url, RedirectCount, Max, OutDev, Opts);
        {error, _} = Error ->
            error_logger:warning_msg("Error fetching url ~p error: ~p", [Url, Error]),
            Error
    end.

normalize_url(Url) ->
    {Protocol, Host, Path, Qs, _Frag} = mochiweb_util:urlsplit(Url),
    lists:flatten([
            normalize_protocol(Protocol), "://", Host, Path,
            case Qs of
                [] -> [];
                _ -> [ $?, Qs ]
            end
        ]).

normalize_protocol("") -> "http";
normalize_protocol(Protocol) -> Protocol.

start_stream(Url, Headers, Opts) ->
    try
        Timeout = proplists:get_value(timeout, Opts, ?HTTPC_TIMEOUT),
        httpc:request(get,
                      {Url, Headers},
                      [ {autoredirect, false}, {relaxed, true}, {timeout, Timeout}, {connect_timeout, ?HTTPC_TIMEOUT_CONNECT} ],
                      [ {sync, false}, {body_format, binary}, {stream, {self, once}} ],
                      profile(Url))
    catch
        error:E -> {error, E};
        throw:E -> {error, E}
    end.


fetch_stream({ok, ReqId}, Max, OutDev) ->
    receive
        {http, {ReqId, stream_end, Hs}} ->
            {ok, {200, Hs, 0, <<>>}};
        {http, {ReqId, stream_start, Hs, HandlerPid}} ->
            httpc:stream_next(HandlerPid),
            fetch_stream_data(ReqId, HandlerPid, Hs, <<>>, 0, Max, OutDev);
        {http, {ReqId, {error, _} = Error}} ->
            Error;
        {http, {_ReqId, {{_V, Code, _Msg}, Hs, Data}}} ->
            {ok, {Code, Hs, 0, Data}}
    after ?HTTPC_TIMEOUT ->
        httpc:cancel_request(ReqId),
        {error, timeout}
    end;
fetch_stream({error, _} = Error, _Max, _OutDev) ->
    Error.

fetch_stream_data(ReqId, HandlerPid, Hs, Data, N, Max, OutDev) when N =< Max ->
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
                            fetch_stream_data(ReqId, HandlerPid, Hs, Data1, N1, Max, OutDev);
                        false ->
                            httpc:cancel_request(ReqId),
                            {ok, {200, Hs, N, Data1}}
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
    after ?HTTPC_TIMEOUT ->
        httpc:cancel_request(ReqId),
        {error, timeout}
    end;
fetch_stream_data(ReqId, _HandlerPid, Hs, Data, N, _Max, _OutFile) ->
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

maybe_redirect({200, Hs, Size, Data}, Url, _RedirectCount, _Max, _OutDev, _Opts) ->
    {ok, {Url, Hs, Size, Data}};
maybe_redirect({416, _Hs, _Size, _Data}, Url, RedirectCount, _Max, OutDev, Opts) ->
    fetch_partial(Url, RedirectCount+1, undefined, OutDev, Opts);
maybe_redirect({Code, Hs, _Size, _Data}, BaseUrl, RedirectCount, Max, OutDev, Opts)
    when Code =:= 301; Code =:= 302; Code =:= 303; Code =:= 307 ->
    case proplists:get_value("location", Hs) of
        undefined ->
            {error, no_location_header};
        Location ->
            NewUrl = z_convert:to_list(z_url:abs_link(Location, BaseUrl)),
            fetch_partial(NewUrl, RedirectCount+1, Max, OutDev, Opts)
    end;
maybe_redirect({Code, Hs, Size, Data}, Url, _RedirectCount, _Max, _OutDev, _Opts) ->
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
