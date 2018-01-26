%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_convert_test).

-include_lib("eunit/include/eunit.hrl").


-define(assertDatetime(A, B), ?assertEqual(A, z_convert:to_datetime(B))).

convert_float_test() ->
    ?assertEqual("10.0", z_convert:to_list(10.0)),
    ?assertEqual("10.12345", z_convert:to_list(10.12345)),
    ?assertEqual(<<"100.001">>, z_convert:to_binary(100.001)),
    ?assertEqual(<<"0.001">>, z_convert:to_binary(0.001)).

convert_date_test() ->
    ?assertEqual({2010,12,12}, z_convert:to_date("2010-12-12")),
    ?assertEqual({2010,12,12}, z_convert:to_date("2010/12/12")),

    %% wrong input format; should not crash
    ?assertEqual(undefined, z_convert:to_date("Mon Sep 07 2015 12:08:04 GMT+0200 (West-Europa (zomertijd))")),
    
    ok.

convert_time_test() ->
    ?assertEqual({1,23,0}, z_convert:to_time("01:23:00")),
    ?assertEqual({1,23,0}, z_convert:to_time("01:23")),
    ?assertEqual({1,23,59}, z_convert:to_time("01:23:59")),
    ok.

convert_datetime_test() ->

    %% generic format
    ?assertDatetime({{2010,1,1},{18,23,50}}, "2010-01-01 18:23:50"),
    ?assertDatetime({{2010,1,1},{18,23,0}}, "2010-01-01 18:23"),
    ?assertDatetime({{2010,1,1},{0,0,0}}, "2010-01-01"),

    %% xsd:datetime
    ?assertDatetime({{2010,1,1},{18,29,39}}, "2010-01-01T18:29:39"),
    ?assertDatetime({{2010,1,1},{18,29,39}}, "2010-01-01T18:29:39+00:00"),
    ?assertDatetime({{2010,1,1},{18,29,39}}, "2010-01-01T18:29:39Z"),
    ?assertDatetime({{2010,1,1},{17,29,39}}, "2010-01-01T18:29:39+01:00"),
    ?assertDatetime({{2010,1,1},{20,29,39}}, "2010-01-01T18:29:39-02:00"),

    ?assertDatetime({{2011,10,6},{14,44,0}}, "2011-10-06T16:44:00+0200"),
    ok.

datetime_to_iso_test() ->
    ?assertEqual("2010-09-02T10:11:56Z", z_convert:to_isotime({{2010,9,2},{10,11,56}})),
    ?assertEqual("2010-09-02T01:01:01Z", z_convert:to_isotime({{2010,9,2},{1,1,1}})),
    ?assertEqual("2010-09-02T01:01:01Z", z_convert:to_isotime({{2010,9,2},{1,1,1}})),
    ?assertEqual("0500-09-02T10:11:56Z", z_convert:to_isotime({{500,9,2},{10,11,56}})),
    ?assertEqual("-0500-09-02T10:11:56Z", z_convert:to_isotime({{-500,9,2},{10,11,56}})),
    ok.
