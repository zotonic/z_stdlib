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

map_test() ->
    M = #{a => 1, b => 2},
    {ok, Enc} = z_ubf:encode(M),
    {ok, M1, _} = z_ubf:decode(Enc),
    ?assertEqual(M, M1),
    ?assertEqual(<<"#{'b',2}&{'a',1}&`map`$">>, Enc).

empty_map_test() ->
    M = #{},
    {ok, Enc} = z_ubf:encode(M),
    {ok, M1, _} = z_ubf:decode(Enc),
    ?assertEqual(M, M1),
    ?assertEqual(<<"#`map`$">>, Enc).

nested_map_test() ->
    M = #{
        a => #{b => 2},
        <<"list">> => [#{c => 3}]
    },
    {ok, Enc} = z_ubf:encode(M),
    {ok, M1, _} = z_ubf:decode(Enc),
    ?assertEqual(M, M1).

map_deabstract_test() ->
    ?assertEqual(
        #{
            a => #{b => 2},
            "key" => "value"
        },
        z_ubf:deabstract(#{
            a => #{b => 2},
            {'#S', "key"} => {'#S', "value"}
        })).

map_stream_decode_test() ->
    {more, Cont} = z_ubf:decode(<<"#{'b',2}&">>),
    {done, M, Rest} = z_ubf:decode(<<"{'a',1}&`map`$rest">>, {more, Cont}),
    ?assertEqual(#{a => 1, b => 2}, M),
    ?assertEqual(<<"rest">>, Rest).

recordlist_test() ->
    L = [{a,1},{b,2},{a,3},{b,4}],
    {ok, Enc} = z_ubf:encode(L, [{record_names, [a, b]}]),
    {ok, L1, _} = z_ubf:decode(Enc),
    ?assertEqual(L, L1),
    ?assertEqual(<<"#{'b',4}&{'a',3}&{'b',2}&{'a',1}&$">>, Enc).

% bug_test() ->
%  %% TODO: this was on the original code as a bug/0 function. 
%    %% was never called, but points to a bug.
%    C = z_ubf:decode("{'abc"),
%    z_ubf:decode("d'}$", C).
