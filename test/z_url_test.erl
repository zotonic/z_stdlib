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

split_base_host_test() ->
    ?assertEqual(
        {<<"http://example.com">>, <<"http://example.com/bla/">>}, 
        z_url:split_base_host(<<"http://example.com/bla/hello.html?a=b#c">>)),
    
    ?assertEqual(
        {<<"spdy://example.com:8000">>, <<"spdy://example.com:8000/bla/">>}, 
        z_url:split_base_host(<<"spdy://example.com:8000/bla/hello.html?a=b#c">>)).
    
abs_link_test() ->
    Base = <<"http://example.com/folder/file.html">>,

    ?assertEqual(<<"http://example.com/">>,  z_url:abs_link("/", Base)),
    ?assertEqual(<<"http://example.com/foo">>,  z_url:abs_link("/foo", Base)),
    ?assertEqual(<<"http://example.com/folder/foo">>,  z_url:abs_link("foo", Base)),
    ?assertEqual(<<"http://example.com/folder/foo/">>,  z_url:abs_link("foo/", Base)),
    ?assertEqual(<<"http://example.com/folder/foo">>,  z_url:abs_link("./foo", Base)),
    ?assertEqual(<<"http://example.com/foo/">>,  z_url:abs_link("../foo/", Base)),
    ?assertEqual(<<"http://example.com/foo">>,  z_url:abs_link("../foo", Base)),
    ?assertEqual(<<"http://example.com/">>,  z_url:abs_link("../../", Base)),
    ?assertEqual(<<"http://example.com/">>,  z_url:abs_link("../../../", Base)),
    ?assertEqual(<<"http://example.com/bar">>,  z_url:abs_link("//example.com/bar", Base)),

    Base2 = <<"http://example.com/folder/folder2/file.html">>,
    ?assertEqual(<<"http://example.com/">>,  z_url:abs_link("../../", Base2)),
    ?assertEqual(<<"http://example.com/xx">>,  z_url:abs_link("../../xx", Base2)),
    ?assertEqual(<<"http://example.com/folder/xx">>,  z_url:abs_link("../xx", Base2)),
    
    ok.


abs_link2_test() ->
    Base = <<"http://example.com">>,

    ?assertEqual(<<"http://example.com/">>,  z_url:abs_link("/", Base)),
    ?assertEqual(<<"http://example.com/foo">>,  z_url:abs_link("/foo", Base)),
    ?assertEqual(<<"http://example.com/foo">>,  z_url:abs_link("foo", Base)),
    ?assertEqual(<<"http://example.com/foo/bar/baz.html">>,  z_url:abs_link("foo/bar/baz.html", Base)),
    ?assertEqual(<<"http://example.com/foo/bar/baz.html">>,  z_url:abs_link("/foo/bar/baz.html", Base)),
    ok.

