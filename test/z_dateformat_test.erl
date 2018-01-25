%% @author Marc Worrell <marc@worrell.nl>

-module(z_dateformat_test).

-include_lib("eunit/include/eunit.hrl").


dateformat_year_y_test() ->
    ?assertEqual(<<"-0999">>,  z_dateformat:format({{  -999,1,1},{0,0,0}}, "Y", [])),
    ?assertEqual(<<"0999">>,   z_dateformat:format({{   999,1,1},{0,0,0}}, "Y", [])),
    ?assertEqual(<<"-1000">>,  z_dateformat:format({{ -1000,1,1},{0,0,0}}, "Y", [])),
    ?assertEqual(<<"1000">>,   z_dateformat:format({{  1000,1,1},{0,0,0}}, "Y", [])),
    ?assertEqual(<<"-10000">>, z_dateformat:format({{-10000,1,1},{0,0,0}}, "Y", [])),
    ?assertEqual(<<"10000">>,  z_dateformat:format({{ 10000,1,1},{0,0,0}}, "Y", [])),
    ok.

dateformat_year_x_test() ->
    ?assertEqual(<<"0100">>,  z_dateformat:format({{  -100,1,1},{0,0,0}}, "x", [])),
    ?assertEqual(<<"0100">>,  z_dateformat:format({{   100,1,1},{0,0,0}}, "x", [])),
    ?assertEqual(<<"10000">>, z_dateformat:format({{-10000,1,1},{0,0,0}}, "x", [])),
    ?assertEqual(<<"10000">>, z_dateformat:format({{ 10000,1,1},{0,0,0}}, "x", [])),
    ok.

