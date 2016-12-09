-module(z_ubf_test).

-include_lib("eunit/include/eunit.hrl").

list_test() ->
    L = [1,2,3],
    {ok, Enc} = z_ubf:encode(L),
    {ok, L1, _} = z_ubf:decode(Enc),
    ?assertEqual(L, L1),
    ?assertEqual(Enc, <<"#3&2&1&$">>).

float_test() ->
    F = 3.14159,
    {ok, Enc} = z_ubf:encode(F),
    {ok, F1, _} = z_ubf:decode(Enc),
    TF = round(F*100000),
    TF1 = round(F1*100000),
    ?assertEqual(TF,TF1),
    ?assertEqual(Enc, <<"\"3.14159\"`f`$">>).

date_test() ->
    Date = {{2008,12,10},{15,30,0}},
    {ok, Enc} = z_ubf:encode(Date),
    {ok, Date1, _} = z_ubf:decode(Enc),
    ?assertEqual(Date, Date1),
    ?assertEqual(Enc, <<"1228923000`dt`$">>).

stjuttemis_test() ->
    Date = {{9999,8,17},{12,0,0}},
    {ok, Enc} = z_ubf:encode(Date),
    {ok, Date1, _} = z_ubf:decode(Enc),
    ?assertEqual(undefined, Date1),
    ?assertEqual(Enc, <<"'undefined'$">>).

proplist_test() ->
    L = [{a,1},{b,2}],
    {ok, Enc} = z_ubf:encode(L),
    {ok, L1, _} = z_ubf:decode(Enc),
    ?assertEqual(L, L1),
    ?assertEqual(Enc, <<"#{'b',2}&{'a',1}&`plist`$">>).
