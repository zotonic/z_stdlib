%% @author Marc Worrell
%% @copyright 2012-2024 Marc Worrell
%% @doc Misc utility URL functions for Zotonic.
%% @end

%% Copyright 2012-2024 Marc Worrell
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
    hex_encode_lc/1,
    hex_decode/1,
    remove_protocol/1,
    location/1,
    abs_link/2,
    split_base_host/1,
    decode_data_url/1,
    encode_data_url/3
]).


-define(is_uppercase_alpha(C), C >= $A, C =< $Z).
-define(is_lowercase_alpha(C), C >= $a, C =< $z).
-define(is_alpha(C), ?is_uppercase_alpha(C); ?is_lowercase_alpha(C)).
-define(is_digit(C), C >= $0, C =< $9).
-define(is_unreserved(C), ?is_alphanumeric(C); C =:= $-; C =:= $_; C =:= $.; C =:= $~).
-define(is_alphanumeric(C), ?is_alpha(C); ?is_digit(C)).

%%% URL ENCODE %%%

-spec url_encode(Value) -> Encoded when
    Value :: string() | atom() | float() | integer() | binary() | iodata(),
    Encoded :: binary().
url_encode(S) ->
    Encoded = cow_qs:urlencode( z_convert:to_binary(S) ),
    case binary:match(Encoded, <<"+">>) of
        nomatch -> Encoded;
        _ -> binary:replace(Encoded, <<"+">>, <<"%20">>, [global])
    end.

-spec url_decode(Encoded) -> Data when
    Encoded :: iodata(),
    Data :: binary().
url_decode(S) ->
    cow_qs:urldecode( z_convert:to_binary(S) ).

%%% URL PATH ENCODE %%%

%% url spec for path part
-spec url_path_encode( iodata() ) -> binary().
url_path_encode(L) when is_list(L) ->
    url_path_encode(iolist_to_binary(L), <<>>);
url_path_encode(S) when is_binary(S) ->
    url_path_encode(S, <<>>).

url_path_encode(<<>>, Acc) ->
    Acc;
url_path_encode(<<$/, $/, R/binary>>, Acc) ->
    url_path_encode(<<$/, R/binary>>, Acc);
url_path_encode(<<$/, R/binary>>, Acc) ->
    url_path_encode(R, <<Acc/binary, $/>>);
url_path_encode(<<C, R/binary>>, Acc) when C =:= $&; C =:= $=; C =:= $$; C =:= $,; C =:= $; ->
    url_path_encode(R, <<Acc/binary, C>>);
url_path_encode(<<C, R/binary>>, Acc) when ?is_unreserved(C) ->
    url_path_encode(R, <<Acc/binary, C>>);
url_path_encode(<<C, R/binary>>, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    url_path_encode(R, <<Acc/binary,  $%, (hexdigit(Hi)), (hexdigit(Lo))>>).

% hexdigit is from Mochiweb.

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

hexdigit_lc(C) when C < 10 -> $0 + C;
hexdigit_lc(C) when C < 16 -> $a + (C - 10).

from_hexdigit(C) when C =< $9 -> C - $0;
from_hexdigit(C) when C =< $Z -> C - $A + 10;
from_hexdigit(C) when C =< $z -> C - $a + 10.


%%% PERCENT encode ENCODE %%%

%% @doc Percent encoding/decoding as defined by RFC 3986 (http://tools.ietf.org/html/rfc3986).
-spec percent_encode( iodata() ) -> binary().
percent_encode(Chars) when is_list(Chars) ->
    percent_encode(iolist_to_binary(Chars), <<>>);
percent_encode(Chars) when is_binary(Chars) ->
    percent_encode(Chars, <<>>).

percent_encode(<<>>, Acc) ->
    Acc;
percent_encode(<<C, R/binary>>, Acc) when ?is_unreserved(C) ->
    percent_encode(R, <<Acc/binary, C>>);
percent_encode(<<C, R/binary>>, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    percent_encode(R, <<Acc/binary,  $%, (hexdigit(Hi)), (hexdigit(Lo))>>).


%% @doc Naive function to remove the protocol from an Url
-spec remove_protocol( string() ) -> string();
                     ( binary() ) -> binary().
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
-spec url_valid_char(Char) -> boolean() when
    Char :: non_neg_integer().
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

-spec hex_encode( iodata() ) -> binary().
hex_encode(Data) ->
    hex_encode(iolist_to_binary(Data), <<>>).

hex_encode(<<>>, Acc) ->
    Acc;
hex_encode(<<C, R/binary>>, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    hex_encode(R, <<Acc/binary,  (hexdigit(Hi)), (hexdigit(Lo))>>).

-spec hex_encode_lc( iodata() ) -> binary().
hex_encode_lc(Data) ->
    hex_encode_lc(iolist_to_binary(Data), <<>>).

hex_encode_lc(<<>>, Acc) ->
    Acc;
hex_encode_lc(<<C, R/binary>>, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    hex_encode_lc(R, <<Acc/binary,  (hexdigit_lc(Hi)), (hexdigit_lc(Lo))>>).

-spec hex_decode( binary() ) -> binary().
hex_decode(Data) ->
    hex_decode(Data, <<>>).

hex_decode(<<>>, Acc) ->
    Acc;
hex_decode(<<Hi, Lo, R/binary>>, Acc) ->
    HiC = from_hexdigit(Hi),
    LoC = from_hexdigit(Lo),
    hex_decode(R, <<Acc/binary, HiC:4, LoC:4>>).


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
-spec abs_link(Url, BaseUrl) -> AbsUrl when
    Url :: string() | binary(),
    BaseUrl :: string() | binary(),
    AbsUrl :: binary().
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


%% @doc Decode a "data:" url to its parts. If the charset is not defined in the data
%% then it is returned as "US-ASCII". The mime type defaults to "text/plain".
-spec decode_data_url(DataUrl) -> {ok, Mime, Charset, Data} | {error, Reason} when
    DataUrl :: binary(),
    Mime :: binary(),
    Charset :: binary(),
    Data :: binary(),
    Reason :: unknown_encoding | nodata.
decode_data_url(<<"data:", Data/binary>>) ->
    case binary:split(Data, <<",">>) of
        [ MimeData, EncodedData ] ->
            MimeParts = binary:split(MimeData, <<";">>, [global]),
            Mime = find_mime(MimeParts),
            Charset = find_charset(MimeParts),
            DecodedData = case last(MimeParts) of
                <<"base64">> -> decode_base64(EncodedData);
                <<"utf8">> -> EncodedData;
                _ -> z_url:url_decode(EncodedData)
            end,
            {ok, Mime, Charset, DecodedData};
        [ _ ] ->
            {error, unknown_encoding}
    end;
decode_data_url(Url) when is_binary(Url) ->
    {error, nodata}.

last([]) -> undefined;
last(L) -> lists:last(L).

decode_base64(Data) ->
    Data1 = << <<case C of $- -> $+; $_ -> $/; _ -> C end>> || <<C>> <= Data >>,
    Data2 = case byte_size(Data1) rem 4 of
        0 -> Data1;
        2 -> <<Data1/binary, "==">>;
        3 -> <<Data1/binary, "=">>
    end,
    base64:decode(Data2).

find_mime([]) -> <<"text/plain">>;
find_mime([<<>>|_]) -> <<"text/plain">>;
find_mime([M|_]) ->
    case binary:match(M, <<"=">>) of
        nomatch -> M;
        {_,_} -> <<"text/plain">>
    end.

find_charset([]) -> <<"US-ASCII">>;
find_charset([ <<"charset=", Charset/binary>> | _ ]) -> Charset;
find_charset([ _ | Ms ]) -> find_charset(Ms).


%% Encode a data URL. If the charset is US-ASCII or empty then it is omitted.
%% Plain text (text/plain) is URL encoded, other data is base64 encoded.
-spec encode_data_url(Mime, Charset, Data) -> Encoded when
    Mime :: binary(),
    Charset :: binary() | undefined,
    Data :: binary(),
    Encoded :: binary().
encode_data_url(<<"text/plain">>, Charset, Data) when
    Charset =:= undefined;
    Charset =:= <<>>;
    Charset =:= <<"US-ASCII">> ->
    <<"data:,", (url_encode(Data))/binary>>;
encode_data_url(Mime, Charset, Data) when
    Charset =:= undefined;
    Charset =:= <<>>;
    Charset =:= <<"US-ASCII">> ->
    <<"data:", Mime/binary, ";base64,", (base64:encode(Data))/binary>>;
encode_data_url(Mime, Charset, Data) ->
    <<"data:", Mime/binary, ";charset=", Charset/binary, ";base64,", (base64:encode(Data))/binary>>.
