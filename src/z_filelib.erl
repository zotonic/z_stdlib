%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2025 Marc Worrell
%% @doc Extra file functions.
%% @end

%% Copyright 2017-2025 Marc Worrell
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

-module(z_filelib).

-export([
    rename/2,
    ensure_dir/1,
    os_filename/1,
    os_escape/1
    ]).


%% @doc Rename a file. Copy the file on a cross-fs error.
-spec rename(From, To) -> ok | {error, term()} when
    From :: file:filename_all(),
    To :: file:filename_all().
rename(From, To) ->
    case file:rename(From, To) of
        ok ->
            ok;
        {error, exdev} ->
            % cross-fs rename is not supported by erlang, so copy and delete the file
            case file:copy(From, To) of
                {ok, _BytesCopied} ->
                    ok = file:delete(From);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Ensure the directory of a file is present. This will still work
%%      if a soft-link in the path refers to a missing directory.
-spec ensure_dir(file:filename_all()) -> ok | {error, term()}.
ensure_dir(Filename) ->
    case filelib:ensure_dir(Filename) of
        ok -> ok;
        {error, enoent} ->
            % Could be that there is a softlink to a missing directory in the path
            {LinkFile, Rest} = first_missing(filename:split(Filename), []),
            case file:read_link(LinkFile) of
                {ok, Link} ->
                    case make_link_target(Link, LinkFile, Rest) of
                        ok -> ensure_dir(Filename);
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

make_link_target(Link, LinkFile, Rest) ->
    Target = case is_relative(Link) of
        true -> filename:join([ filename:dirname(LinkFile), Link ]);
        false -> Link
    end,
    case Rest of
        [] -> filelib:ensure_dir(filename:join([ Target ]));
        _ -> filelib:ensure_dir(filename:join([ Target, hd(Rest) ]))
    end.

is_relative(Path) ->
    case filename:split(Path) of
        [".."|_] -> true;
        ["."|_] -> true;
        [<<"..">>|_] -> true;
        [<<".">>|_] -> true;
        _ -> false
    end.


first_missing([], Acc) ->
    lists:reverse(Acc);
first_missing([P|Ps], Acc) ->
    Acc1 = [P|Acc],
    Path = filename:join(lists:reverse(Acc1)),
    case filelib:is_file(Path) of
        true -> first_missing(Ps, [P|Acc]);
        false -> {Path, Ps}
    end.


%% @doc Simple escape function for filenames as commandline arguments.
%% foo/"bar.jpg -> "foo/\"bar.jpg"; on windows "foo\\\"bar.jpg" (both including quotes!)
-spec os_filename( string()|binary() ) -> string().
os_filename(A) when is_binary(A) ->
    os_filename(binary_to_list(A));
os_filename(A) when is_list(A) ->
    os_filename(lists:flatten(A), []).

os_filename([], Acc) ->
    filename:nativename([$'] ++ lists:reverse(Acc) ++ [$']);
os_filename([$\\|Rest], Acc) ->
    os_filename_bs(Rest, Acc);
os_filename([$'|Rest], Acc) ->
    os_filename(Rest, [$', $\\ | Acc]);
os_filename([C|Rest], Acc) ->
    os_filename(Rest, [C|Acc]).

os_filename_bs([$\\|Rest], Acc) ->
    os_filename(Rest, [$\\,$\\|Acc]);
os_filename_bs([$'|Rest], Acc) ->
    os_filename(Rest, [$',$\\,$\\,$\\|Acc]);
os_filename_bs([C|Rest], Acc) ->
    os_filename(Rest, [C,$\\|Acc]).


%% @doc Simple escape function for command line arguments
-spec os_escape(string()|binary()|undefined) -> string().
os_escape(undefined) ->
    "";
os_escape(A) when is_binary(A) ->
    os_escape(binary_to_list(A));
os_escape(A) when is_list(A) ->
    {Family, _} = os:type(),
    os_escape(Family, lists:flatten(A), []).

os_escape(_, [], Acc) ->
    lists:reverse(Acc);
os_escape(unix, [C|Rest], Acc) when
      (C >= $A andalso C =< $Z)
      orelse (C >= $a andalso C =< $z)
      orelse (C >= $0 andalso C =< $9)
      orelse C == $_
      orelse C == $.
      orelse C == $-
      orelse C == $+
      orelse C == $/
      ->
    os_escape(unix, Rest, [C|Acc]);
os_escape(unix, [C|Rest], Acc) when
      C >= 32
      orelse  C == $\r
      orelse  C == $\n
      orelse  C == $\t
      ->
    os_escape(unix, Rest, [C,$\\|Acc]);

%% Win32 escaping, see: http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/ntcmds_shelloverview.mspx
os_escape(win32, [C|Rest], Acc) when
      C == $&
      orelse C == $|
      orelse C == $;
      orelse C == $,
      orelse C == $%
      orelse C == $(
      orelse C == $)
      orelse C == $"
      orelse C == $'
      orelse C == $=
      orelse C == $^
      orelse C == 32
      ->
    os_escape(win32, Rest, [C,$^|Acc]);
os_escape(win32, [C|Rest], Acc) ->
    os_escape(win32, Rest, [C|Acc]).
