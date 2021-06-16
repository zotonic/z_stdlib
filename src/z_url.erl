%% @author Marc Worrell
%% @copyright 2012 Marc Worrell
%% @doc Misc utility URL functions for zotonic

%% Copyright 2012 Marc Worrell
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

-module(z_url).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    url_encode/1,
    url_decode/1,
    url_path_encode/1,
    url_valid_char/1,
    url_reserved_char/1,
    url_unreserved_char/1,
    percent_encode/1,
    percent_encode/2,
    hex_encode/1,
    hex_decode/1,
    remove_protocol/1,
    location/1,
    abs_link/2,
    split_base_host/1,
    decode_data_url/1
]).


-define(is_uppercase_alpha(C), C >= $A, C =< $Z).
-define(is_lowercase_alpha(C), C >= $a, C =< $z).
-define(is_alpha(C), ?is_uppercase_alpha(C); ?is_lowercase_alpha(C)).
-define(is_digit(C), C >= $0, C =< $9).
-define(is_unreserved(C), ?is_alphanumeric(C); C =:= $-; C =:= $_; C =:= $.; C =:= $~).
-define(is_alphanumeric(C), ?is_alpha(C); ?is_digit(C)).

%%% URL ENCODE %%%

-spec url_encode( string() | atom() | float() | integer() | binary() ) -> binary().
url_encode(S) ->
    cow_qs:urlencode( z_convert:to_binary(S) ).

-spec url_decode( string() | binary() ) -> binary().
url_decode(S) ->
    cow_qs:urldecode( z_convert:to_binary(S) ).

%%% URL PATH ENCODE %%%

%% url spec for path part
url_path_encode(L) when is_list(L) ->
    url_path_encode(L, []);
url_path_encode(L) ->
    url_path_encode(z_convert:to_list(L)).

url_path_encode([], Acc) ->
    lists:reverse(Acc);
url_path_encode([$/|[$/|_]=R], Acc) ->
    url_path_encode(R, Acc);
url_path_encode([$/|R], Acc) ->
    url_path_encode(R, [$/|Acc]);
url_path_encode([C|R], Acc) when C=:=$&; C=:=$=; C=:=$+; C=:=$$; C=:=$,; C=:=$; ->
    url_path_encode(R, [C|Acc]);
url_path_encode([C|R], Acc)->
    case url_unreserved_char(C) of
        true ->
            url_path_encode(R, [C|Acc]);
        false ->
            <<Hi:4, Lo:4>> = <<C>>,
            url_path_encode(R, [hexdigit(Lo), hexdigit(Hi), $% | Acc])
    end.

% hexdigit is from Mochiweb.

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).


%%% PERCENT encode ENCODE %%%

%% @doc Percent encoding/decoding as defined by RFC 3986 (http://tools.ietf.org/html/rfc3986).
percent_encode(Chars) when is_list(Chars) ->
    percent_encode(Chars, []);
percent_encode(Chars) ->
    percent_encode(z_convert:to_list(Chars)).

percent_encode([], Encoded) ->
  lists:flatten(lists:reverse(Encoded));
percent_encode([C|Etc], Encoded) when ?is_unreserved(C) ->
  percent_encode(Etc, [C|Encoded]);
percent_encode([C|Etc], Encoded) ->
  Value = [io_lib:format("%~s", [encode([Char], 16)])
            || Char <- binary_to_list(unicode:characters_to_binary([C]))],
  percent_encode(Etc, [lists:flatten(Value)|Encoded]).


%% @doc Naive function to remove the protocol from an Url
-spec remove_protocol( string() | binary() ) -> string() | binary().
remove_protocol("://" ++ Rest) -> Rest;
remove_protocol(":" ++ Rest) -> Rest;
remove_protocol([_|T]) -> remove_protocol(T);
remove_protocol([]) -> [];

remove_protocol(<<"://", Rest/binary>>) -> Rest;
remove_protocol(<<":", Rest/binary>>) -> Rest;
remove_protocol(<<_, Rest/binary>>) -> remove_protocol(Rest);
remove_protocol(<<>>) -> <<>>.


%% VALID URL CHARACTERS
%% RFC 3986
url_valid_char(Char) ->
  url_reserved_char(Char) orelse url_unreserved_char(Char).

url_reserved_char($!) -> true;
url_reserved_char($*) -> true;
url_reserved_char($") -> true;
url_reserved_char($') -> true;
url_reserved_char($() -> true;
url_reserved_char($)) -> true;
url_reserved_char($;) -> true;
url_reserved_char($:) -> true;
url_reserved_char($@) -> true;
url_reserved_char($&) -> true;
url_reserved_char($=) -> true;
url_reserved_char($+) -> true;
url_reserved_char($$) -> true;
url_reserved_char($,) -> true;
url_reserved_char($/) -> true;
url_reserved_char($?) -> true;
url_reserved_char($%) -> true;
url_reserved_char($#) -> true;
url_reserved_char($[) -> true;
url_reserved_char($]) -> true;
url_reserved_char(_) -> false.

url_unreserved_char(Ch) when ?is_unreserved(Ch) ->
  true;
url_unreserved_char(_) ->
  false.


%% @doc Find the definitive location of an url, removing url shorteners in the process.
%%      Identify as Curl to prevent url shorteners returning HTML pages.
location(Url) when is_binary(Url) ->
    location(z_convert:to_list(Url));
location(Url) ->
    Headers = [
        {"Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"},
        {"Accept-Encoding", "identity"},
        {"Accept-Charset", "UTF-8;q=1.0, ISO-8859-1;q=0.5, *;q=0"},
        {"Accept-Language", "en,*;q=0"},
        {"User-Agent", "curl/7.21.4 (universal-apple-darwin11.0) libcurl/7.21.4 OpenSSL/0.9.8r zlib/1.2.5"},
        {"Connection", "close"}
    ],
    case httpc:request(head, {Url, Headers}, [{autoredirect, false}], []) of
        {ok, {{_HTTP, 301, _Moved}, Hs, _}} ->
            case proplists:get_value("location", Hs) of
                undefined -> Url;
                Url1 -> location(Url1)
            end;
        _ ->
            Url
    end.


%%% HEX ENCODE and HEX DECODE

hex_encode(Data) -> encode(Data, 16).
hex_decode(Data) -> decode(Data, 16).

encode(Data, Base) when is_binary(Data) -> encode(binary_to_list(Data), Base);
encode(Data, Base) when is_list(Data) ->
	F = fun(C) when is_integer(C) ->
		case erlang:integer_to_list(C, Base) of
			[C1, C2] -> [C1, C2];
			[C1]     -> [$0, C1]
		end
	end,
	[F(I) || I <- Data].

decode(Data, Base) when is_binary(Data) -> decode(binary_to_list(Data), Base);
decode(Data, Base) when is_list(Data) ->
	inner_decode(Data, Base).

inner_decode(Data, Base) when is_list(Data) ->
	case Data of
		[C1, C2|Rest] ->
			I = erlang:list_to_integer([C1, C2], Base),
			[I|inner_decode(Rest, Base)];
		[] ->
			[]
	end.


-spec split_base_host(string()|binary()) -> {binary(), binary()}.
split_base_host(Base) ->
    BaseB = z_convert:to_binary(Base),
    Parts = uri_string:parse(BaseB),
    case Parts of
        #{
            host := Host,
            path := Path
        } ->
            Path1 = case binary:split(Path, <<"/">>, [ global, trim_all ]) of
                [] -> <<>>;
                Ps ->
                    iolist_to_binary(
                        lists:map(
                            fun(P) -> [ P, $/ ] end,
                            lists:reverse( tl( lists:reverse(Ps) ) ) ))
            end,
            Scheme = maps:get(scheme, Parts, <<"http">>),
            BaseHost = <<Scheme/binary, "://", Host/binary>>,
            BaseHost1 = case maps:get(port, Parts, none) of
                none -> BaseHost;
                N -> <<BaseHost/binary, ":", (integer_to_binary(N))/binary>>
            end,
            {BaseHost1, <<BaseHost1/binary, "/", Path1/binary>>};
        _ ->
            {<<>>, BaseB}
    end.


%% @doc Given a relative URL and a base URL, calculate the absolute URL.
-spec abs_link(string()|binary(), string()|binary()) -> binary().
abs_link(RelativeUrl, BaseUrl) ->
    {BaseHost, BaseHostDir} = z_url:split_base_host(BaseUrl),
    ensure_protocol(iolist_to_binary(make_abs_link(z_convert:to_binary(RelativeUrl), BaseHost, BaseHostDir))).

ensure_protocol(<<"://", Url/binary>>) -> <<"http://", Url/binary>>;
ensure_protocol(Url) -> Url.

make_abs_link(<<"http:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"https:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"ws:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"wss:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"mailto:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"file:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"ftp:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"spdy:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"data:", _/binary>> = Url, _Host, _HostDir) -> Url;
make_abs_link(<<"./", Rest/binary>>, Host, HostDir) ->
    make_abs_link(Rest, Host, HostDir);
make_abs_link(<<"//", _/binary>> = Url, Host, _HostDir) ->
    case Host of
        <<"http:", _/binary>> -> <<"http:", Url/binary>>;
        <<"https:", _/binary>> -> <<"https:", Url/binary>>;
        _ -> <<"http:", Url/binary>>
    end;
make_abs_link(<<"/", _/binary>> = Url, Host, _HostDir) ->
    [Host, Url];
make_abs_link(<<"../", Rest/binary>>, Host, HostDir) ->
    HostDirOneUp = re:replace(HostDir, "^(.*//.*/)[^/]+/$", "\\1", [{return, binary}]),
    make_abs_link(Rest, Host, HostDirOneUp);
make_abs_link(Url, _Host, HostDir) ->
    [HostDir, Url].


%% @doc Decode a "data:" url to its parts.
%%      Crashes if the url doesn't have a "data:" protocol.
-spec decode_data_url(binary()) -> {ok, Mime::binary(), Charset::binary(), Data::binary()} | {error, unknown_encoding}.
decode_data_url(<<"data:", Data/binary>>) ->
    Parts = binary:split(Data, <<";">>, [global]),
    [Encoded|Args] = lists:reverse(Parts),
    case decode_url_data(Encoded) of
        {ok, Decoded} ->
            {Mime, Charset} = decode_data_url_args(Args),
            {ok, Mime, Charset, Decoded};
        {error, _} = Error ->
            Error
    end.

decode_url_data(<<"base64,", Data/binary>>) ->
    Data1 = << <<case C of $- -> $+; $_ -> $/; _ -> C end>> || <<C>> <= Data >>,
    Data2 = case byte_size(Data1) rem 4 of
        0 -> Data1;
        2 -> <<Data1/binary, "==">>;
        3 -> <<Data1/binary, "=">>
    end,
    {ok, base64:decode(Data2)};
decode_url_data(<<",", Data/binary>>) ->
    {ok, Data};
decode_url_data(_) ->
    {error, unknown_encoding}.

decode_data_url_args(Args) ->
    lists:foldl(fun(<<"charset=", Charset/binary>>, {Mime,_Charset}) ->
                        {Mime,Charset};
                    (Mime, {_Mime,Charset}) ->
                        {Mime,Charset}
                end,
                {<<"text/plain">>, <<"US-ASCII">>},
                Args).

