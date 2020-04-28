%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2020 Marc Worrell
%% @doc Simple temporary file handling, deletes the file when the calling process stops or crashes.

%% Copyright 2011-2020 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_tempfile).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	new/0,
	new/1,

    monitored_new/0,
    monitored_new/1,
    monitored_attach/1,
    monitored_detach/1,

	tempfile/0,
	tempfile/1,
	is_tempfile/1,
	temppath/0,

    cleanup/0
]).

% Export for tmp file monitor
-export([
    tmpfile_monitor_loop/2
]).

-include_lib("kernel/include/file.hrl").


% Threshold for tmp files to be old and deleted if cleanup/0 is called.
-define(CLEANUP_SECS, 3600*24).


%% @doc Return a new unique filename, start a monitoring process to clean it up after use.
%       The file must be created and written within 10 seconds, or it will be deleted.
-spec new() -> file:filename_all().
new() ->
	new(<<>>).

%% @doc Return a new unique filename, start a monitoring process to clean it up after use.
%       The file must be created and written within 10 seconds, or it will be deleted.
-spec new( string() | binary() ) -> file:filename_all().
new(Extension) ->
    {ok, {_Pid, Filename}} = monitored_new(Extension),
    Filename.

%% @doc Like new/0 but also return yhe Pid of the monitoring process.
-spec monitored_new() -> {ok, {pid(), file:filename_all()}}.
monitored_new() ->
    monitored_new(<<>>).

%% @doc Like new/1 but also return yhe Pid of the monitoring process.
-spec monitored_new( string()|binary() ) -> {ok, {pid(), file:filename_all()}}.
monitored_new(Extension) ->
    Filename = tempfile(Extension),
    Self = self(),
    Pid = erlang:spawn(fun() -> tmpfile_monitor(Filename, Self) end),
    receive
        {is_monitoring, Pid} ->
            {ok, {Pid, Filename}}
    end.

%% @doc Add a process to the tempfile monitor. The tempfile is deleted after all
%%      attached processes stopped or are detached.
-spec monitored_attach( pid() ) -> ok.
monitored_attach(MonitorPid) when is_pid(MonitorPid) ->
    MonitorPid ! {attach, self()},
    ok.

%% @doc Remove a process from the tempfile monitor. The tempfile is deleted after all
%%      attached processes stopped or are detached.
-spec monitored_detach( pid() ) -> ok.
monitored_detach(MonitorPid) when is_pid(MonitorPid) ->
    MonitorPid ! {detach, self()},
    ok.

%% @hidden Monitoring process, delete file when requesting process stops or crashes
tmpfile_monitor(Filename, OwnerPid) ->
	process_flag(trap_exit, true),
    erlang:monitor(process, OwnerPid),
    OwnerPid ! {is_monitoring, self()},
    ?MODULE:tmpfile_monitor_loop(Filename, [OwnerPid]).

%% @private
tmpfile_monitor_loop(Filename, AttachedPids) ->
    Timeout = case AttachedPids of
        [] -> 10000;
        _ -> 100000
    end,
	receive
		{'DOWN', _MRef, process, Pid, _Reason} ->
            ?MODULE:tmpfile_monitor_loop(Filename, lists:delete(Pid, AttachedPids));
        {detach, Pid} ->
            ?MODULE:tmpfile_monitor_loop(Filename, lists:delete(Pid, AttachedPids));
        {attach, Pid} ->
            case lists:member(Pid, AttachedPids) of
                false ->
                    erlang:monitor(process, Pid),
                    ?MODULE:tmpfile_monitor_loop(Filename, [ Pid | AttachedPids ]);
                true ->
                    ?MODULE:tmpfile_monitor_loop(Filename, AttachedPids)
            end;
        {detach_delete, Pid} ->
            case lists:delete(Pid, AttachedPids) of
                [] ->
                    file:delete(Filename);
                OtherPids ->
                    ?MODULE:tmpfile_monitor_loop(Filename, OtherPids)
            end
    after Timeout ->
        case AttachedPids of
            [] -> file:delete(Filename);
            _ -> ?MODULE:tmpfile_monitor_loop(Filename, AttachedPids)
        end
	end.

%% @doc return a unique temporary filename located in the TMP directory.
-spec tempfile() -> file:filename_all().
tempfile() ->
	tempfile(<<>>).

%% @doc return a unique temporary filename with the given extension.
-spec tempfile( string()|binary() ) -> file:filename_all().
tempfile(Extension) ->
	A = rand:uniform(1000000000),
	B = rand:uniform(1000000000),
    Filename = filename:join(
        temppath(),
        iolist_to_binary( io_lib:format("ztmp-~s-~p.~p~s",[node(),A,B,Extension]) )
    ),
    case filelib:is_file(Filename) of
    	true -> tempfile(Extension);
    	false -> Filename
    end.

%% @doc Check if the file is a temporary filename.
-spec is_tempfile( file:filename_all() ) -> boolean().
is_tempfile(Filename) ->
    FilenameParts = filename:split( unicode:characters_to_binary(Filename) ),
    TempPathParts = filename:split( temppath() ),
    is_tempfile(FilenameParts, TempPathParts).

is_tempfile([ <<"ztmp-", _/binary>> ], []) ->
    true;
is_tempfile([ A | As], [ A | Bs ]) ->
    is_tempfile(As, Bs);
is_tempfile(_, _) ->
    false.

%% @doc Returns the path where to store temporary files.
-spec temppath() -> file:filename_all().
temppath() ->
    lists:foldl(
        fun
            (false, TmpPath) ->
                TmpPath;
            (Good, _) ->
                unicode:characters_to_binary(Good)
        end,
        <<"/tmp">>,
        [ os:getenv("TMP"), os:getenv("TEMP") ]).


%% @doc Delete all tempfiles not modified in the last day.
-spec cleanup() -> ok.
cleanup() ->
    Old = calendar:datetime_to_gregorian_seconds( calendar:universal_time() ) - ?CLEANUP_SECS,
    Tmp = filename:join(temppath(), <<"/ztmp-*">>),
    Files = filelib:wildcard(Tmp),
    lists:foreach(
        fun(F) ->
            case file:read_file_info(F, [ {time, universal} ]) of
                {ok, #file_info{
                    type = regular,
                    mtime = ModDT
                }} ->
                    ModSecs = calendar:datetime_to_gregorian_seconds( ModDT ),
                    case ModSecs < Old of
                        true ->
                            file:delete(F);
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end
        end,
        Files).
