%% Temporary; see https://github.com/manopapad/proper/issues/49 for
%% pull request of this into proper itself.
-module(proper_utils).

-export([opts/0, qc/1, qc/2, qc_/1, qc_/2, print_stdout/1]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Run a function and output to stdout. Useful for PropEr within eunit.
-spec print_stdout(fun(() -> A)) -> A.
print_stdout(Fun) ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Ret = Fun(),
    erlang:group_leader(EunitLeader, self()),
    Ret.

qc(Proper) ->
    qc(Proper, opts()).

%% @doc like proper:quickcheck/2, but print to stdout.
%%
%% Removes `timeout' from `Opts'.
qc(Proper, Opts) ->
    Opts2 = proplists:delete(timeout, Opts),
    print_stdout(fun() -> proper:quickcheck(Proper, Opts2) end).

qc_(Proper) ->
    qc_(Proper, opts()).

%% @doc proper:quickcheck/1 helper for eunit. Accepts additional Arg `timeout'.
qc_(Proper, Opts) ->
    Timeout = proplists:get_value(timeout, Opts),
    if
        is_integer(Timeout) ->
            {timeout, Timeout, ?_assertEqual(true, qc(Proper, Opts))};
        true ->
            ?_assertEqual(true, qc(Proper, Opts))
    end.

%% @doc Default options for PropEr runs
opts() ->
    [{constraint_tries, 1000}, {numtests, 1000}, {timeout, 3600}].
