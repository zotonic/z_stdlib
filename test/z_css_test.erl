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
        z_css:sanitize(<<"@media screen,print,foobar { }">>)),
    ?assertEqual(
        {ok, <<"@media only screen and (max-width:600px) {\np {\nbackground-image:url();\n}\n}\n">>},
        z_css:sanitize(<<"@media only screen and (max-width: 600px) { p { background-image: url(https://example.com/x.png) } }">>)).

sanitize_external_references_test() ->
    ?assertEqual(
        {ok, <<"p {\ncolor:red;\n}\n">>},
        z_css:sanitize(<<"@import url(https://example.com/a.css) screen; @import \"https://example.com/b.css\" print; p { color: red }">>)),
    ?assertEqual(
        {ok, <<"p {\nfont-family:Example;\n}\n">>},
        z_css:sanitize(<<"@font-face { font-family: Example; src: url(https://example.com/font.woff2) format(\"woff2\"); } p { font-family: Example }">>)),
    ?assertEqual(
        {ok, <<"@media only screen and (max-width:600px) {\np {\ncolor:red;\n}\n}\n">>},
        z_css:sanitize(<<"@media only screen and (max-width: 600px) { @font-face { src: url(https://example.com/font.woff2); } p { color: red } }">>)).

sanitize_content_test() ->
    ?assertEqual(
        {ok, <<":before {\ncontent:\"Hello &quot;\\&#39;world\";\n}\n">>},
        z_css:sanitize(<<":before { content: '<p>Hello \"\\'world' }">>)).

sanitize_unit_test() ->
    ?assertEqual(
        {ok,<<"a {\nc:100%;\nd:1em;\ne:2px;\nf:a,b,c;\n}\n">>},
        z_css:sanitize(<<"a {\nc:100%; d:1em; e:2px; f:a,b,c;\n}\n">>)),
    Units = [
        <<"cap">>, <<"ch">>, <<"cm">>, <<"cqb">>, <<"cqh">>, <<"cqi">>, <<"cqmax">>, <<"cqmin">>, <<"cqw">>,
        <<"deg">>, <<"dpcm">>, <<"dpi">>, <<"dppx">>, <<"dvb">>, <<"dvh">>, <<"dvi">>, <<"dvmax">>,
        <<"dvmin">>, <<"dvw">>, <<"em">>, <<"ex">>, <<"fr">>, <<"grad">>, <<"Hz">>, <<"ic">>, <<"in">>,
        <<"kHz">>, <<"lh">>, <<"lvb">>, <<"lvh">>, <<"lvi">>, <<"lvmax">>, <<"lvmin">>, <<"lvw">>, <<"mm">>,
        <<"ms">>, <<"pc">>, <<"pt">>, <<"px">>, <<"Q">>, <<"rad">>, <<"rcap">>, <<"rch">>, <<"rem">>,
        <<"rex">>, <<"ric">>, <<"rlh">>, <<"s">>, <<"svb">>, <<"svh">>, <<"svi">>, <<"svmax">>,
        <<"svmin">>, <<"svw">>, <<"turn">>, <<"vb">>, <<"vh">>, <<"vi">>, <<"vmax">>, <<"vmin">>,
        <<"vw">>, <<"x">>
    ],
    Css = iolist_to_binary([
        <<"a {\n">>,
        [ [ <<"u">>, integer_to_binary(N), <<":1">>, Unit, <<";\n">> ]
            || {N, Unit} <- lists:zip(lists:seq(1, length(Units)), Units) ],
        <<"}\n">>
    ]),
    ?assertMatch({ok, _}, z_css:sanitize(Css)).
