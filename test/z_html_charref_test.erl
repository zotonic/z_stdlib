-module(z_html_charref_test).

-include_lib("eunit/include/eunit.hrl").

charref_test() ->
    ?assertEqual(1234, z_html_charref:charref("#1234")),
    ?assertEqual(1234, z_html_charref:charref(<<"#1234">>)),
    ?assertEqual(255, z_html_charref:charref("#xfF")),
    ?assertEqual(255, z_html_charref:charref(<<"#XFf">>)),
    ?assertEqual(38, z_html_charref:charref("amp")),
    ?assertEqual(38, z_html_charref:charref(<<"amp">>)),
    ?assertEqual(38, z_html_charref:charref("AMP")),
    ?assertEqual(38, z_html_charref:charref(<<"AMP">>)),
    ?assertEqual(undefined, z_html_charref:charref("not_an_entity")),
    ?assertEqual(undefined, z_html_charref:charref(<<"not_an_entity">>)),
    ?assertEqual(undefined, z_html_charref:charref("#not_an_entity")),
    ?assertEqual(undefined, z_html_charref:charref("#xnot_an_entity")),
    ?assertEqual(undefined, z_html_charref:charref(<<"#not_an_entity">>)),
    ?assertEqual(undefined, z_html_charref:charref(<<"#xnot_an_entity">>)).

entity_binary_compat_test() ->
    ?assertEqual(z_html_charref:charref("nbsp"), z_html_charref:charref(<<"nbsp">>)),
    ?assertEqual(z_html_charref:charref("amp"), z_html_charref:charref(<<"amp">>)),
    ?assertEqual(z_html_charref:charref("euro"), z_html_charref:charref(<<"euro">>)),
    ?assertEqual(z_html_charref:charref("Yuml"), z_html_charref:charref(<<"Yuml">>)),
    ?assertEqual(z_html_charref:charref("Abreve"), z_html_charref:charref(<<"Abreve">>)),
    ?assertEqual(z_html_charref:charref("CounterClockwiseContourIntegral"), z_html_charref:charref(<<"CounterClockwiseContourIntegral">>)).

html5_entity_test() ->
    ?assertEqual(258, z_html_charref:charref("Abreve")),
    ?assertEqual(8755, z_html_charref:charref("CounterClockwiseContourIntegral")),
    ?assertEqual([8766, 819], z_html_charref:charref("acE")),
    ?assertEqual([8923, 65024], z_html_charref:charref(<<"gesl">>)).
