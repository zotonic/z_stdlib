%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_tempfile_test).

-include_lib("eunit/include/eunit.hrl").

tempfile_new_test() ->
    File = z_tempfile:tempfile(<<".tmp">>),
    true = z_tempfile:is_tempfile(File),
    <<".tmp">> = filename:extension(File),
    ok.

is_tempfile_test() ->
    F = filename:join(z_tempfile:temppath(), <<"foo">>),
    false = z_tempfile:is_tempfile(F),
    ok.

tempfile_monitor_test() ->
    {ok, {MPid, F}} = z_tempfile:monitored_new(),
    MRef = erlang:monitor(process, MPid),
    true = z_tempfile:is_tempfile(F),
    true = erlang:is_process_alive(MPid),
    ok = file:write_file(F, <<"hello">>),
    MPid ! {detach_delete, self()},
    receive
        {'DOWN', MRef, process, MPid, normal} ->
            ok
    end,
    false = filelib:is_file(F),
    ok.
