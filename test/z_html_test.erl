%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_html_test).

-include_lib("eunit/include/eunit.hrl").


link_elements_test() ->
    ?assertEqual([[{<<"rel">>, <<"foo">>}]], z_html:scrape_link_elements("<p>This is text.<link rel=\"foo\" /></p>")),
    ?assertEqual([[{<<"rel">>, <<"Foo">>}]], z_html:scrape_link_elements("<p>This is text.<LINK REL=\"Foo\" /></p>")),
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
    ?assertEqual(<<"Hello">>, z_html:strip(<<"Hello">>)),
    ?assertEqual(<<"">>, z_html:strip(<<"">>)),
    ?assertEqual([], z_html:strip([])),
    ?assertEqual(<<"1234">>, z_html:strip(1234)),
    ?assertEqual(<<"1234\n5678">>, z_html:strip(<<"<p>1234</p>\n<p>5678</p>">>)),
    ?assertEqual(<<"1234\n5678">>, z_html:strip(<<"<p>1234\n<span>5678</span></p>">>)),
    ?assertEqual(<<"This is a list item.">>, z_html:strip(<<"<ul><li>This is a list item.</li></ul>">>)),
    ?assertEqual(<<"1234 5678">>, z_html:strip(<<"<p>1234</p><p>5678</p>">>)),
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

nonhtml_test() ->
    ?assertEqual({error, nohtml}, z_html_parse:parse(<<"<??/N>">>)),
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


truncate_test() ->
    ?assertEqual(<<"12345">>, z_html:truncate(<<"12345">>, 6)),
    ?assertEqual(<<"12345">>, z_html:truncate(<<"12345">>, 5)),
    ?assertEqual(<<"1234">>, z_html:truncate(<<"12345">>, 4)),
    ?assertEqual(<<"<p>12345</p>">>, z_html:truncate(<<"<p>12345</p>">>, 5)),
    ?assertEqual(<<"<p>1234</p>">>, z_html:truncate(<<"<p>12345</p>">>, 4)),
    ?assertEqual(<<"<p>12<br/>34</p>">>, z_html:truncate(<<"<p>12<br/>345</p>">>, 4)),

    ?assertEqual(<<"12345">>, z_html:truncate(<<"12345">>, 6, <<"...">>)),
    ?assertEqual(<<"12345">>, z_html:truncate(<<"12345">>, 5, <<"...">>)),
    ?assertEqual(<<"1234...">>, z_html:truncate(<<"12345">>, 4, <<"...">>)),
    ?assertEqual(<<"<p>12345</p>">>, z_html:truncate(<<"<p>12345</p>">>, 5, <<"...">>)),
    ?assertEqual(<<"<p>1234...</p>">>, z_html:truncate(<<"<p>12345</p>">>, 4, <<"...">>)),
    ?assertEqual(<<"<p>12<br/>34...</p>">>, z_html:truncate(<<"<p>12<br/>345</p>">>, 4, <<"...">>)),

    ?assertEqual(<<"12<a href='x'>34...</a>">>, z_html:truncate(<<"12<a href='x'>345</a>">>, 4, <<"...">>)),

    ?assertEqual(<<"12&amp;45">>, z_html:truncate(<<"12&amp;45">>, 6, <<"...">>)),
    ?assertEqual(<<"12&amp;45">>, z_html:truncate(<<"12&amp;45">>, 5, <<"...">>)),
    ?assertEqual(<<"12&amp;4...">>, z_html:truncate(<<"12&amp;45">>, 4, <<"...">>)),
    ?assertEqual(<<"12&amp;...">>, z_html:truncate(<<"12&amp;45">>, 3, <<"...">>)),

    ?assertEqual(<<"12<!-- hello -->&amp;4...">>, z_html:truncate(<<"12<!-- hello -->&amp;45">>, 4, <<"...">>)),
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
    
    %% Custom tags.
    ?assertEqual(<<"<div><erlang>Let it crash!</erlang></div>">>, 
        z_html:sanitize(<<"<div><erlang scary='no'>Let it crash!</erlang></div>">>, [
                {elt_extra, [<<"erlang">>]}
            ])),

    %% Custom tags and attributes.
    ?assertEqual(<<"<div><erlang scary=\"no\" trap_exit=\"true\">Let it crash!</erlang></div>">>, 
        z_html:sanitize(<<"<div><erlang scary=\"no\" trap_exit=\"true\">Let it crash!</erlang></div>">>, [
                {elt_extra, [<<"erlang">>]},
                {attr_extra, [<<"trap_exit">>, <<"scary">>]}
            ])),

    % Data urls
    ?assertEqual(<<"<a href=\"\">Click me</a>">>,
        z_html:sanitize(<<"<a href=\"data:text/html;charset=utf8,randomhtmlstuff\">Click me</a>">>)),

    % Spaces after mailto:
    ?assertEqual(<<"<a href=\"mailto:jan@example.com\">a</a>">>, 
        z_html:sanitize(<<"<a href=\"mailto: jan@example.com\">a</a>">>)),

    % li without surround ul/ol
    ?assertEqual(<<"<ul><li>a</li></ul>">>,
        z_html:sanitize(<<"<li>a</li>">>)),

    ?assertEqual(<<"<div><ul><li>a</li></ul></div>">>,
        z_html:sanitize(<<"<div><li>a</li></div>">>)),

    ?assertEqual(<<"<ul><li>a</li></ul>">>,
        z_html:sanitize(<<"<ul><li>a</li></ul>">>)),

    ?assertEqual(<<"<ol><li>a</li></ol>">>,
        z_html:sanitize(<<"<ol><li>a</li></ol>">>)),

    ok.


filter_css_test() ->
    % Remove all <style/> tags
    ?assertEqual(
        <<"">>,
        z_html:sanitize(<<"<style>:before { content: 'xxxx'; }</style">>)),

    ?assertEqual(
        <<"<p style=\"color:red; \">No</p>">>, 
        z_html:sanitize(<<"<p style=\"color: red\">No</p>">>)),

    % Accept a font name with a css escaped character
    ?assertEqual(
        <<"<span style=\"font-family:L\\FC beck; \">...</span>">>,
        z_html:sanitize(<<"<span style=\"font-family: L\\FC beck\">...</span>">>)),

    % Quotes in the css must be escaped
    ?assertEqual(
        <<"<span style=\"font-family:&quot;L\\FC beck&quot;; \">...</span>">>,
        z_html:sanitize(<<"<span style=\"font-family: 'L\\FC beck'\">...</span>">>)),  
    ok.
    

