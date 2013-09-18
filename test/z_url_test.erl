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

data_url_test() ->
    DataUrl1 = <<"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUA",
                 "AAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO",
                 "9TXL0Y4OHwAAAABJRU5ErkJggg==">>,
    {ok, Mime1, Charset1, Data1} = z_url:decode_data_url(DataUrl1),
    ?assertEqual(<<"image/png">>, Mime1),
    ?assertEqual(<<"US-ASCII">>, Charset1),
    ?assertEqual(<<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,5,0,0,0,5,8,6,0,0,0,
                   141,111,38,229,0,0,0,28,73,68,65,84,8,215,99,248,255,255,63,195,127,6,
                   32,5,195,32,18,132,208,49,241,130,88,205,4,0,14,245,53,203,209,142,14,
                   31,0,0,0,0,73,69,78,68,174,66,96,130>>, Data1),

    DataUrl2 = <<"data:charset=utf-8;,Hello World">>,
    {ok, Mime2, Charset2, Data2} = z_url:decode_data_url(DataUrl2),
    ?assertEqual(<<"text/plain">>, Mime2),
    ?assertEqual(<<"utf-8">>, Charset2),
    ?assertEqual(<<"Hello World">>, Data2),

    DataUrl3 = <<"data:,Hello World">>,
    {ok, Mime3, Charset3, Data3} = z_url:decode_data_url(DataUrl3),
    ?assertEqual(<<"text/plain">>, Mime3),
    ?assertEqual(<<"US-ASCII">>, Charset3),
    ?assertEqual(<<"Hello World">>, Data3),
    ok.
