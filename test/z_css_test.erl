%% @author Marc Worrell <marc@worrell.nl>
%% @doc Tests voor de css lexer, parser and sanitizer.

-module(z_css_test).

-include_lib("eunit/include/eunit.hrl").

sanitize_css_test() ->
    ?assertEqual(
        {ok, <<"#a :xyz :c(x) {\nposition:absolute;\nbackground-image:url();\n}\n">>},
        z_css:sanitize(<<"#a :xyz :c(x) { position: fixed; background-image: URl(http://example.com) }">>)).

sanitize_css_style_test() ->
    ?assertEqual(
        {ok, <<"position:absolute; background-image:url(); ">>},
        z_css:sanitize_style(<<"position: fixed; background-image: URl(http://example.com)">>)).

sanitize_media_test() ->
    ?assertEqual(
        {ok, <<"@media screen {\np {\nbackground:url();\n}\n}\n">>},
        z_css:sanitize(<<"@media screen {p{background:url(http://example.com)}}">>)),
    ?assertEqual(
        {ok, <<"@media screen,print,foobar {\n}\n">>},
        z_css:sanitize(<<"@media screen,print,foobar { }">>)).

sanitize_content_test() ->
    ?assertEqual(
        {ok, <<":before {\ncontent:\"Hello &quot;\\&#39;world\";\n}\n">>},
        z_css:sanitize(<<":before { content: '<p>Hello \"\\'world' }">>)).

sanitize_unit_test() ->
    ?assertEqual(
        {ok,<<"a {\nc:100%;\nd:1em;\ne:2px;\nf:a,b,c;\n}\n">>},
        z_css:sanitize(<<"a {\nc:100%; d:1em; e:2px; f:a,b,c;\n}\n">>)).

