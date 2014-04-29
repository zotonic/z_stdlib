%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_html_test).

-include_lib("eunit/include/eunit.hrl").


link_elements_test() ->
    ?assertEqual([[{"rel", "foo"}]], z_html:scrape_link_elements("<p>This is text.<link rel=\"foo\" /></p>")),
    ?assertEqual([[{"rel", "Foo"}]], z_html:scrape_link_elements("<p>This is text.<LINK REL=\"Foo\" /></p>")),
    ok.


escape_props_test() ->
    ?assertEqual([{title, <<"Foo &amp; bar">>}],
                 z_html:escape_props([{title, <<"Foo & bar">>}])),

    ?assertEqual([{body, <<"Foo &amp; bar">>}],
                 z_html:escape_props([{body, <<"Foo & bar">>}])),
    ok.

ensure_check_test() ->
	?assertEqual(<<"&#1234; &lt;&gt;;">>, z_html:escape_check(<<"&#1234; <>;">>)),
	ok.

strip_test() ->
	?assertEqual(<<"Hello">>, z_html:strip(<<"<p class='hello'>Hello</p>">>)),
	ok.

abs_links_test() ->
    ?assertEqual(
        <<"Hello <a src=\"http://example.com/a/b/c.html\">">>, 
        z_html:abs_links(<<"Hello <a src=\"c.html\">">>, 
                         <<"http://example.com/a/b/d.html">>)),
    ?assertEqual(
        <<"Hello <a src=\"http://example.com/c.html\">">>, 
        z_html:abs_links(<<"Hello <a src=\"/c.html\">">>, 
                         <<"http://example.com/a/b/d.html">>)),
    ?assertEqual(
        <<"Hello <a src=\"http://b.org/c.html\">">>, 
        z_html:abs_links(<<"Hello <a src=\"http://b.org/c.html\">">>, 
                         <<"http://example.com/a/b/d.html">>)),
    ok.


unescape_test() ->
	?assertEqual(<<"<>">>, z_html:unescape(<<"&lt;&gt;">>)),
	?assertEqual(<<"aap & aap">>, z_html:unescape(<<"aap &amp; aap">>)),
    %%?assertEqual(<<"foo>çbar">>, z_html:unescape("foo&gt;&#231;bar")),
    ?assertEqual(<<"foo&unknown;bar">>, z_html:unescape("foo&unknown;bar")),

    ?assertEqual(<<"foo&&bar">>, z_html:unescape("foo&&bar")),
    ?assertEqual(<<"foo&&&bar">>, z_html:unescape("foo&&&bar")),
    ?assertEqual(<<"foo&;bar">>, z_html:unescape("foo&;bar")),
    
    ok.


sanitize_test() ->
    %% Html tags and attributes are case insensitive 
    ?assertEqual(<<"<img src=\"img.jpg\" />">>, 
        z_html:sanitize(<<"<img sRc='img.jpg'></img>">>)),

    ?assertEqual(<<"<img src=\"img.jpg\" />">>, 
        z_html:sanitize(<<"<IMG sRc='img.jpg'></img>">>)),

    ?assertEqual(<<"<img src=\"img.jpg\" />">>, 
        z_html:sanitize(<<"<img onload='bad_script()' src='img.jpg'></img>">>)),

    ?assertEqual(<<"<div><p></p></div>">>, 
        z_html:sanitize(<<"<div><p></P></div>">>)),

    %% XML data islands are removed
    ?assertEqual(<<"<div></div>">>, 
        z_html:sanitize(<<"<div><xml id='data'><Score Property='test' Player='me'></Score></xml></DIV>">>)),

    ?assertEqual(<<"<div>...</div>">>, 
        z_html:sanitize(<<"<div><script type='text/javascript'>bad_script()</script>...</div>">>)),



    ok.


filter_css_test() ->
    ?assertEqual(<<"">>, z_html:sanitize(<<"<style></style">>)),
    ?assertEqual(<<"<p style=\"color: red\">No</p>">>, 
        z_html:sanitize(<<"<p style=\"color: red\">No</p>">>)),
    ?assertEqual(<<"<p style=\"color: red\">No</p>">>, 
        z_html:sanitize(<<"<p style=\"color: red\">No</p>">>)),
    ?assertEqual(<<"<span style=\"font-family: L\\FC beck\">...</span>">>,
        z_html:sanitize(<<"<span style=\"font-family: L\\FC beck\">...</span>">>)),  

    ok.
    

