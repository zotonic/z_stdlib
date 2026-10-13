%% @copyright 2026 Marc Worrell
%% @doc Verify redirect policy and cancellation on the fetcher's own HTTP profile.
%% @end

%% Copyright 2014-2025 Marc Worrell
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

-module(z_url_fetch_test).
-include_lib("eunit/include/eunit.hrl").

redirect_policy_test() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(tls_certificate_check),
    lists:foreach(fun({Code, Follow}) ->
        with_listener(fun(Listen, Url) ->
            Parent = self(),
            Server = spawn_monitor(fun() ->
                {ok, Socket} = gen_tcp:accept(Listen, 2000),
                _ = headers(Socket, <<>>),
                ok = gen_tcp:send(Socket, ["HTTP/1.1 ", integer_to_list(Code), <<" Found\r\nLocation: /target\r\nContent-Length: 0\r\nConnection: close\r\n\r\n">>]),
                gen_tcp:close(Socket),
                case Follow of
                    true ->
                        {ok, Target} = gen_tcp:accept(Listen, 2000),
                        <<"GET /target ", _/binary>> = headers(Target, <<>>),
                        gen_tcp:send(Target, <<"HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok">>),
                        gen_tcp:close(Target);
                    false ->
                        ?assertEqual({error, timeout}, gen_tcp:accept(Listen, 200))
                end,
                Parent ! checked
            end),
            Options = case Follow of true -> []; false -> [{autoredirect, false}] end,
            Reply = z_url_fetch:fetch(Url, [{use_range, false} | Options]),
            case Follow of
                true -> ?assertMatch({ok, {_, _, 2, <<"ok">>}}, Reply);
                false -> ?assertMatch({error, {Code, _, _, _, <<>>}}, Reply)
            end,
            await(Server)
        end)
    end, [{Code, Follow} || Code <- [301, 302, 303, 307, 308], Follow <- [true, false]]).

cancel_profile_test() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(tls_certificate_check),
    with_listener(fun(Listen, Url) ->
        Parent = self(),
        Server = spawn_monitor(fun() ->
            {ok, Socket} = gen_tcp:accept(Listen, 2000),
            _ = headers(Socket, <<>>),
            gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\nContent-Length: 20\r\n\r\nx">>),
            ?assertEqual({error, closed}, gen_tcp:recv(Socket, 0, 2000)),
            Parent ! checked
        end),
        ?assertEqual({error, timeout}, z_url_fetch:fetch(Url, [{timeout, 100}, {use_range, false}])),
        await(Server)
    end).

with_listener(Test) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(tls_certificate_check),
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, {_, Port}} = inet:sockname(Listen),
    try Test(Listen, "http://localhost:" ++ integer_to_list(Port) ++ "/start")
    after gen_tcp:close(Listen) end.

headers(Socket, Buffer) ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, Data} = gen_tcp:recv(Socket, 0, 2000),
            headers(Socket, <<Buffer/binary, Data/binary>>);
        _ -> Buffer
    end.

await({Pid, Ref}) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            receive checked -> ok after 0 -> error(missing_server_check) end;
        {'DOWN', Ref, process, Pid, Reason} -> error({server_failed, Reason})
    after 3000 -> exit(Pid, kill), error(server_timeout)
    end.

callback_redirect_test() ->
    with_listener(fun(Listen, Url) ->
        Parent = self(),
        TargetUrl = string:replace(Url, "localhost", "127.0.0.1", all),
        OptionsFun = fun(get, RequestUrl, Options) ->
            ExpectedAuthorization = case uri_string:parse(RequestUrl) of
                #{host := <<"localhost">>} -> undefined;
                #{host := <<"127.0.0.1">>} -> none
            end,
            ?assertEqual(ExpectedAuthorization, proplists:get_value(authorization, Options)),
            Token = case uri_string:parse(RequestUrl) of
                #{host := <<"localhost">>} -> <<"Bearer first">>;
                #{host := <<"127.0.0.1">>} -> <<"Bearer second">>
            end,
            [{authorization, Token} | Options]
        end,
        Server = spawn_monitor(fun() ->
            {ok, Socket} = gen_tcp:accept(Listen, 2000),
            FirstHeaders = string:lowercase(headers(Socket, <<>>)),
            ?assertNotEqual(nomatch, binary:match(FirstHeaders, <<"authorization: bearer first">>)),
            respond(Socket, 302, [{"Location", TargetUrl}], <<"redirect body">>),
            {ok, Target} = gen_tcp:accept(Listen, 2000),
            TargetHeaders = string:lowercase(headers(Target, <<>>)),
            ?assertNotEqual(nomatch, binary:match(TargetHeaders, <<"authorization: bearer second">>)),
            ?assertEqual(nomatch, binary:match(TargetHeaders, <<"cookie:">>)),
            respond(Target, 200, [], <<"ok">>),
            Parent ! checked
        end),
        ?assertMatch({ok, {_, _, 2, <<"ok">>}}, z_url_fetch:fetch(Url,
            [{options_fun, OptionsFun}, {headers, [{"Cookie", "private"}]}])),
        await(Server)
    end).

anonymous_redirect_test() ->
    with_listener(fun(Listen, Url) ->
        Parent = self(),
        OptionsFun = fun(_, _, Options) ->
            ?assertEqual(none, proplists:get_value(authorization, Options)),
            %% The library must enforce anonymous intent even if a callback ignores it.
            [{authorization, <<"unwanted">>}, {headers, [{"AUTHORIZATION", "unwanted"}]}]
        end,
        Server = spawn_monitor(fun() ->
            {ok, Socket} = gen_tcp:accept(Listen, 2000),
            assert_anonymous(Socket),
            respond(Socket, 302, [{"Location", string:replace(Url, "localhost", "127.0.0.1", all)}], <<>>),
            {ok, Target} = gen_tcp:accept(Listen, 2000),
            assert_anonymous(Target),
            respond(Target, 200, [], <<"ok">>),
            Parent ! checked
        end),
        ?assertMatch({ok, {_, _, 2, <<"ok">>}}, z_url_fetch:fetch(Url,
            [{authorization, none}, {options_fun, OptionsFun}])),
        await(Server)
    end).

explicit_authorization_redirect_test() ->
    lists:foreach(fun(Authorization) ->
        lists:foreach(fun(Location) ->
            with_listener(fun(Listen, Url) ->
                Parent = self(),
                Server = spawn_monitor(fun() ->
                    {ok, Socket} = gen_tcp:accept(Listen, 2000),
                    _ = headers(Socket, <<>>),
                    respond(Socket, 302, [{"Location", Location}], <<>>),
                    ?assertEqual({error, timeout}, gen_tcp:accept(Listen, 100)),
                    Parent ! checked
                end),
                ?assertEqual({error, authorization_redirect}, z_url_fetch:fetch(Url, [{authorization_redirect, error}, Authorization])),
                await(Server)
            end)
        end, ["/target", "https://other.invalid/target"])
    end, [{authorization, <<"Bearer explicit">>}, {headers, [{"Authorization", "Bearer explicit"}]}]).

cross_origin_anonymous_default_test() ->
    lists:foreach(fun(Authorization) ->
        with_listener(fun(Listen, Url) ->
            Parent = self(),
            TargetUrl = string:replace(Url, "localhost", "127.0.0.1", all),
            Server = spawn_monitor(fun() ->
                {ok, Socket} = gen_tcp:accept(Listen, 2000),
                Request = string:lowercase(headers(Socket, <<>>)),
                ?assertNotEqual(nomatch, binary:match(Request, <<"authorization: bearer explicit">>)),
                respond(Socket, 302, [{"Location", TargetUrl}], <<>>),
                {ok, Target} = gen_tcp:accept(Listen, 2000),
                assert_anonymous(Target),
                respond(Target, 302, [{"Location", "/final"}], <<>>),
                {ok, Final} = gen_tcp:accept(Listen, 2000),
                assert_anonymous(Final),
                respond(Final, 200, [], <<"ok">>),
                Parent ! checked
            end),
            ?assertMatch({ok, {_, _, 2, <<"ok">>}}, z_url_fetch:fetch(Url, [Authorization])),
            await(Server)
        end)
    end, [{authorization, <<"Bearer explicit">>},
          {headers, [{"AUTHORIZATION", "Bearer explicit"}]}]).

streamed_redirect_test() ->
    with_listener(fun(Listen, Url) ->
        Parent = self(),
        Server = spawn_monitor(fun() ->
            {ok, Socket} = gen_tcp:accept(Listen, 2000),
            _ = headers(Socket, <<>>),
            respond(Socket, 302, [{"Location", "/target"}], <<"discard this redirect body">>),
            {ok, Target} = gen_tcp:accept(Listen, 2000),
            _ = headers(Target, <<>>),
            respond(Target, 200, [], <<"ok">>),
            Parent ! checked
        end),
        Path = z_tempfile:new(),
        {ok, Device} = file:open(Path, [write, binary]),
        try
            ok = file:write(Device, <<"prefix:">>),
            ?assertMatch({ok, {_, _, 2, <<>>}}, z_url_fetch:fetch_partial(Url, [{device, Device}])),
            ?assertEqual({ok, <<"prefix:ok">>}, file:read_file(Path)),
            await(Server)
        after ok = file:close(Device), ok = file:delete(Path) end
    end).

redirect_method_test() ->
    lists:foreach(fun(Code) ->
        with_listener(fun(Listen, Url) ->
            Parent = self(),
            Server = spawn_monitor(fun() ->
                {ok, Socket} = gen_tcp:accept(Listen, 2000),
                <<"POST /start ", _/binary>> = headers(Socket, <<>>),
                respond(Socket, Code, [{"Location", "/target"}], <<>>),
                {ok, Target} = gen_tcp:accept(Listen, 2000),
                Request = headers(Target, <<>>),
                ExpectedMethod = case Code of 303 -> <<"GET">>; _ -> <<"POST">> end,
                ?assertEqual(ExpectedMethod, hd(binary:split(Request, <<" ">>))),
                respond(Target, 200, [], <<"ok">>),
                Parent ! checked
            end),
            ?assertMatch({ok, {_, _, _, <<"ok">>}}, z_url_fetch:fetch(post, Url, <<"payload">>, [])),
            await(Server)
        end)
    end, [301, 302, 303, 307, 308]).

range_retry_test() ->
    with_listener(fun(Listen, Url) ->
        Parent = self(),
        Server = spawn_monitor(fun() ->
            {ok, Socket} = gen_tcp:accept(Listen, 2000),
            ?assertNotEqual(nomatch, binary:match(string:lowercase(headers(Socket, <<>>)), <<"range:">>)),
            respond(Socket, 416, [], <<"range not supported">>),
            {ok, Target} = gen_tcp:accept(Listen, 2000),
            ?assertEqual(nomatch, binary:match(string:lowercase(headers(Target, <<>>)), <<"range:">>)),
            respond(Target, 200, [], <<"ok">>),
            Parent ! checked
        end),
        ?assertMatch({ok, {_, _, 2, <<"ok">>}}, z_url_fetch:fetch(Url, [])),
        await(Server)
    end).

assert_anonymous(Socket) ->
    ?assertEqual(nomatch, binary:match(string:lowercase(headers(Socket, <<>>)), <<"authorization:">>)).

respond(Socket, Code, Headers, Body) ->
    ok = gen_tcp:send(Socket, ["HTTP/1.1 ", integer_to_list(Code), " Response\r\n",
        [[Name, ": ", Value, "\r\n"] || {Name, Value} <- Headers],
        "Content-Length: ", integer_to_list(byte_size(Body)), "\r\nConnection: close\r\n\r\n", Body]),
    ok = gen_tcp:close(Socket).
