%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_html_test).

-include_lib("eunit/include/eunit.hrl").


link_elements_test() ->
    ?assertEqual([[{<<"rel">>, <<"foo">>}]], z_html:scrape_link_elements("<p>This is text.<link rel=\"foo\" /></p>")),
    ?assertEqual([[{<<"rel">>, <<"Foo">>}]], z_html:scrape_link_elements("<p>This is text.<LINK REL=\"Foo\" /></p>")),
    ok.

escape_link_test() ->
    <<"<a href=\"http://example.com/a\" rel=\"noopener nofollow noreferrer\">http://example.com/a</a> "/utf8>> 
        = z_html:escape_link(z_html:unescape(<<"http://example.com/a&nbsp;">>)),
    <<"foo <a href=\"https://www.example.com/~user/home_foo;c?a=123&amp;b=c#abc\" rel=\"noopener nofollow noreferrer\">www.example.com/~user/home_foo;c?a=123&amp;b=c#abc</a> bar"/utf8>> 
        = z_html:escape_link(<<"foo www.example.com/~user/home_foo;c?a=123&b=c#abc bar">>),
    <<"foo &lt;&gt;&amp;&quot;&#39; bar"/utf8>>
        = z_html:escape_link(<<"foo <>&\"' bar">>),
    <<"foo &lt;&gt;&amp;&quot;&#39; bar"/utf8>>
        = z_html:escape_link("foo <>&\"' bar"),
    <<"foo <a href=\"https://www.example.com\" rel=\"noopener nofollow noreferrer\">www.example.com</a> &lt;&gt;&amp;&quot;&#39; bar"/utf8>>
        = z_html:escape_link("foo www.example.com <>&\"' bar"),
    ok.

escape_props_test() ->
    ?assertEqual([{title, <<"Foo &amp; bar">>}],
                 z_html:escape_props([{title, <<"Foo & bar">>}])),

    ?assertEqual([{body, <<"Foo &amp; bar">>}],
                 z_html:escape_props([{body, <<"Foo & bar">>}])),
    ok.

escape_props_1_test() ->
    Ps1 = #{
        <<"foo_int_list">> => [ <<"1">>, <<"2">> ]
    },
    Ps1Out = #{
        <<"foo_int_list">> => [ 1, 2 ]
    },
    ?assertEqual(Ps1Out, z_html:escape_props(Ps1)),
    Ps2 = #{
        <<"foo_id_list">> => [ <<"1">>, <<"2">>, <<"foo&bar">>, <<>> ]
    },
    Ps2Out = #{
        <<"foo_id_list">> => [ 1, 2, <<"foo&amp;bar">>, undefined ]
    },
    ?assertEqual(Ps2Out, z_html:escape_props(Ps2)),
    Ps3 = #{
        <<"foo_id_list_list">> => [ [ <<"1">>, <<"2">> ], [ <<"foo&bar">>, <<>> ] ]
    },
    Ps3Out = #{
        <<"foo_id_list_list">> => [ [ 1, 2 ], [ <<"foo&amp;bar">>, undefined ] ]
    },
    ?assertEqual(Ps3Out, z_html:escape_props(Ps3)),
    ok.

ensure_check_test() ->
	?assertEqual(<<"&#1234; &lt;&gt;;">>, z_html:escape_check(<<"&#1234; <>;">>)),
	ok.

strip_test() ->
    ?assertEqual(<<"Hello">>, z_html:strip(<<"<p class='hello'>Hello</p>">>)),
    ?assertEqual(<<"Hello">>, z_html:strip(<<"Hello">>)),
    ?assertEqual(<<"">>, z_html:strip(<<"">>)),
    ?assertEqual(<<"">>, z_html:strip("")),
    ?assertEqual(<<"1234\n5678">>, z_html:strip(<<"<p>1234</p>\n<p>5678</p>">>)),
    ?assertEqual(<<"1234\n5678">>, z_html:strip(<<"<p>1234\n<span>5678</span></p>">>)),
    ?assertEqual(<<"This is a list item.">>, z_html:strip(<<"<ul><li>This is a list item.</li></ul>">>)),
    ?assertEqual(<<"1234 5678">>, z_html:strip(<<"<p>1234</p><p>5678</p>">>)),
    ?assertEqual(<<"1234 5678">>, z_html:strip(<<"<p>1234</p><!-- Hello --><p>5678</p>">>)),
    ?assertEqual(<<"12345678">>, z_html:strip(<<"<p>1234<!-- Hello -->5678</p>">>)),
    ?assertEqual(<<"1234 5678">>, z_html:strip(<<"<p>1234<style type=\"text/css\">foo: { a: x }</style>5678</p>">>)),
    ?assertEqual(<<"1234 5678">>, z_html:strip(<<"<p>1234<script type=\"text/javascript\">foo();</script>5678</p>">>)),
    ?assertEqual(<<"1234 5678">>, z_html:strip(<<"<p>1234<style>foo: { a: x }</style>5678</p>">>)),
    ?assertEqual(<<"1234 5678">>, z_html:strip(<<"<p>1234<script>foo();</script>5678</p>">>)),
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
    ?assertEqual({ok,{<<"p">>,[],[{pi,<<"?">>,[]}]}}, z_html_parse:parse(<<"<p><??/N></p>">>)),
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

noscript_test() ->
    ?assertEqual(<<"">>, z_html:noscript(<<"%25AA%25AA">>)),
    ?assertEqual(<<"#script-removed">>, z_html:noscript(<<"javascript:xyz">>)),
    ?assertEqual(<<"#script-removed">>, z_html:noscript(<<" jaVas Cript  :xyz">>)),
   ?assertEqual(<<"http://example.com/">>, z_html:noscript(<<" http://example.com/  ">>)).

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

    % Script urls
    ?assertEqual(<<"<a href=\"#script-removed\">Click me</a>">>,
        z_html:sanitize(<<"<a href=\"javascript:foobar()\">Click me</a>">>)),

    ?assertEqual(<<"<a href=\"#script-removed\">Click me</a>">>,
        z_html:sanitize(<<"<a href=\"j%41vascript:foobar()\">Click me</a>">>)),

    ?assertEqual(<<"<a href=\"#script-removed\">Click me</a>">>,
        z_html:sanitize(<<"<a href=\"j%41va scr%20iPt:foobar()\">Click me</a>">>)),

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

rel_sanitize_test() ->
    ?assertEqual(<<"<a rel=\"nofollow noopener noreferrer\" target=\"_blank\" href=\"https://example.com\">Click me</a>">>,
        z_html:sanitize(<<"<a target=\"_blank\" href=\"https://example.com\">Click me</a>">>)).

sanitize_trans_test() ->
    Tr = {trans, #{ <<"en">> => <<"<>&gt;">> }},
    ?assertEqual({trans, [ {en, <<"&lt;&gt;&amp;gt;">>}]}, z_html:escape(Tr)),
    ?assertEqual({trans, [ {en, <<"&lt;&gt;&gt;">>}]}, z_html:escape_check(Tr)).


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
    

