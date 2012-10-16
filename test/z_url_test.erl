%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_url_test).

-include_lib("eunit/include/eunit.hrl").

url_encode_decode_test() ->
    Url = "index.html?x=y&z=1",
    ?assertEqual(Url, z_url:url_decode(z_url:url_encode(Url))).


url_encode_test() ->
    ?assertEqual("foo+bar", z_url:url_encode("foo bar")),
    ?assertEqual("foo%26bar", z_url:url_encode("foo&bar")).

url_decode_test() ->
    ?assertEqual("foo&bar", z_url:url_decode("foo%26bar")).


percent_encode_test() ->
    ?assertEqual("foo%20bar", z_url:percent_encode("foo bar")),
    ?assertEqual("foo%26bar", z_url:percent_encode("foo&bar")).

