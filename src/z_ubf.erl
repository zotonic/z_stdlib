%%% see: http://www.sics.se/~joe/ubf/site/home.html
%%% Written by: Joe Armstrong
%%% Adapted for binary strings and data by Marc Worrell

%%% Copyright 2002-2003 Joe Armstrong.
%%% Copyright 2013 Marc Worrell. 
%%%
%%% All rights reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to
%%% permit persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(z_ubf).

-compile(export_all).

-export([decode_init/0, decode/1, decode/2, encode/1, encode/2]).
-export([encode_print/1, deabstract/1]).

-import(lists, [foldl/3, reverse/1, map/2, seq/2, sort/1]).


bug() ->
    C = decode("{'abc"),
    decode("d'}$", C).

%% Decoding rules
%% {'#S', String} -> String
%% Int            -> Int
%% [ ]            -> List
%% {...}          -> Tuple

%% decode_init() -> Cont
%% decode(Str, Cont) -> {more, Cont'} | {done, Term, Str}
%% encode(Str) -> Bytes

%% intro() -> single line terminated with \n

decode(Str) when is_binary(Str) ->
    case catch decode(Str, decode_init()) of
        {done, Term, Rest} -> {ok, Term, Rest};
        {more, _} = More -> More;
        {error, _} = Error -> Error;
        {'EXIT', Error} -> {error, Error}
    end.

decode_init() -> {more, fun(I) -> decode(I, [[]], dict:new()) end}.

decode(S, {more, Fun}) -> 
    Fun(S).

decode1(S, Stack, D) ->
    decode(S, Stack, D).

decode(<<$',T/binary>>, Stack, Dict) ->
    get_stuff(T, $', <<>>, Stack, Dict);
decode(<<$~,T/binary>>, [[Int|Stack]|S1], Dict) when is_integer(Int), Int >= 0 ->
    collect_binary(Int, T, <<>>, [Stack|S1], Dict);
decode(<<$~,_/binary>>, _Stack, _Dict) ->
    exit(tilde);
decode(<<$%, T/binary>>, Stack, Dict) ->
    get_stuff(T, $%, <<>>, Stack, Dict);
decode(<<$", T/binary>>, Stack, Dict) ->
    get_stuff(T, $", <<>>, Stack, Dict);
decode(<<$`, T/binary>>, Stack, Dict) ->
    get_stuff(T, $`, <<>>, Stack, Dict);
decode(<<$-, T/binary>>, Stack, Dict) ->
    collect_int(T, 0, '-', Stack, Dict);
decode(<<H, T/binary>>, Stack, Dict) when $0 =< H, H =< $9 ->
    collect_int(T, H-$0, '+', Stack, Dict);
decode(<<${, T/binary>>, Stack, Dict) ->
    decode1(T, [[]|Stack], Dict);
decode(<<$}, T/binary>>, [H|Stack], Dict) ->
    decode1(T, push(list_to_tuple(reverse(H)),Stack), Dict);
decode(<<$&, T/binary>>, [ [H1,H2|T1] | Stack], Dict) ->
    decode1(T, [[[H1|H2]|T1]|Stack], Dict);
decode(<<$#, T/binary>>, Stack, Dict) ->
    decode1(T, push([], Stack), Dict);
decode(<<$$, T/binary>>, [[X]], _Dict) ->
    {done, X, T};
decode(<<$>,Key, T/binary>>, [[Val|R]|Stack], Dict) ->
    decode1(T, [R|Stack], dict:store(Key,Val,Dict));
decode(<<H, T/binary>>, Stack, Dict) ->
    case special(H) of
	true ->
	    decode1(T, Stack, Dict);
	false ->
	    decode1(T, push(dict:fetch(H, Dict), Stack), Dict)
    end;
decode(<<>>, Stack, Dict) ->
    {more, fun(I) -> decode1(I, Stack, Dict) end};
decode(X, _Stack, _Dict) ->
    {error, {eof, X}}.
		   
get_stuff(<<$\\,H,T/binary>>, Stop, L, Stack, Dict) -> 
    get_stuff(T, Stop, <<L/binary,H>>, Stack, Dict);
get_stuff(<<$',T/binary>>, $', L, Stack, Dict)  -> 
    decode1(T, push(list_to_existing_atom(z_convert:to_list(L)),Stack), Dict);
get_stuff(<<$",T/binary>>, $", L, Stack, Dict)  -> 
    decode1(T, push(L,Stack), Dict);
get_stuff(<<$`,T/binary>>, $`, L, [Top|Stack], Dict)  -> 
    decode1(T, push({'$TYPE', L, Top},Stack), Dict);
get_stuff(<<$%,T/binary>>, $%, _L, Stack, Dict)  -> 
    decode1(T, Stack, Dict);
get_stuff(<<H,T/binary>>, Stop, L, Stack, Dict) -> 
    get_stuff(T, Stop, <<L/binary, H>>, Stack, Dict);
get_stuff(<<>>, Stop, L, Stack, Dict) ->
    {more, fun(I) ->		   
		   get_stuff(I, Stop, L, Stack, Dict) end}.

collect_binary(0, T, L, Stack, Dict) ->
    expect_tilde(T, push(L, Stack), Dict);
collect_binary(N, <<H,T/binary>>, L, Stack, Dict) ->
    collect_binary(N-1, T, <<L/binary, H>>, Stack, Dict);
collect_binary(N, <<>>, L, Stack, Dict) ->
    {more, fun(I) -> collect_binary(N, I, L, Stack, Dict) end}.

expect_tilde(<<$~, T/binary>>, Stack, Dict) ->
    decode(T, Stack, Dict);
expect_tilde(<<>>, Stack, Dict) ->
    {more, fun(I) -> expect_tilde(I, Stack, Dict) end};
expect_tilde(<<H,_/binary>>, _, _) ->
    exit({expect_tilde, H}).

push(X, [Top|Rest]) -> 
    [[X|Top]|Rest];
push(X, Y) ->
    exit({bad_push, X, Y}).

special($ )  -> true;
special(${)  -> true;
special($})  -> true;
special($,)  -> true;
special($#)  -> true;
special($&)  -> true;
special($%)  -> true;
special($>)  -> true;
special($\n) -> true;
special($\r) -> true;
special($\t) -> true;
special($$)  -> true;
special($")  -> true;
special($')  -> true;
special($~)  -> true;
special(_)   -> false.

special_chars() ->    
    " 0123456789{},~%#>\n\r\s\t\"'-&$".

collect_int([H|T], N, Sign, Stack, Dict) when  $0 =< H, H =< $9 ->
    collect_int(T, N*10 + H - $0, Sign, Stack, Dict);
collect_int([], N, Sign, Stack, Dict) ->
    {more, fun(I) -> collect_int(I, N, Sign, Stack, Dict) end};
collect_int(T, N, '+', Stack, Dict) ->
    decode1(T, push(N, Stack), Dict);
collect_int(T, N, '-', Stack, Dict) ->
    decode1(T, push(-N, Stack), Dict).

%%---------------------------------------------------------------------

 
encode_print(X) ->
    io:format("~s~n",[encode(X)]).

encode(X) ->
    case encode(X, dict:new()) of
        {ok, Bin, _Dict} -> {ok, Bin};
        {error, _} = Error -> Error
    end.

encode(X, Dict0) ->
    {Dict1, L1} = initial_dict(X, Dict0),
    case (catch do_encode(X, Dict1)) of
	{'EXIT', What} ->
	    {error, What};
	L ->
	    {ok, iolist_to_binary([L1, L,$$]), Dict1}
    end.

initial_dict(X, Dict0) ->
    Free = seq(32,255) -- special_chars(),
    Most = analyse(X),
    %% io:format("Analysis:~p~n",[Most]),
    load_dict(Most, Free, Dict0, []).

load_dict([{N,X}|T], [Key|T1], Dict0, L) when N > 0->
    load_dict(T, T1, dict:store(X, Key, Dict0), 
	      [encode_obj(X),">",Key|L]);
load_dict(_, _, Dict, L) ->
    {Dict, L}.

analyse(T) ->
    KV = dict:to_list(analyse(T, dict:new())),
    %% The Range is the Number of things times its size
    %% If the size is greater than 0
    KV1 = map(fun rank/1, KV),
    reverse(sort(KV1)).
		     
rank({X, K}) when is_atom(X) ->
    case length(atom_to_list(X)) of
	N when N > 1, K > 1 ->
	    {(N-1) * K, X};
	_ ->
	    {0, X}
    end;
rank({X, K}) when is_integer(X) ->
    case length(integer_to_list(X)) of
	N when N > 1, K > 1 ->
	    {(N-1) * K, X};
	_ ->
	    {0, X}
    end;
rank({X, _}) ->
    {0, X}.

analyse({'#S', Str}, Dict) ->
    analyse(Str, Dict);
analyse(T, Dict) when is_tuple(T) ->
    foldl(fun analyse/2, Dict, tuple_to_list(T)); 
analyse(X, Dict) ->
    case dict:find(X, Dict) of
	{ok, Val} ->
	    dict:store(X, Val+1, Dict);
	error ->
	    dict:store(X, 1, Dict)
    end.

encode_obj(X) when is_atom(X) -> encode_atom(X);
encode_obj(X) when is_integer(X) -> integer_to_list(X);
encode_obj(X) when is_binary(X) -> encode_binary(X).

encode_string(S) -> [$",add_string(S, $"), $"].
encode_atom(X)   -> [$',add_string(atom_to_list(X), $'), $'].
encode_binary(X) -> [integer_to_list(size(X)), $~,X,$~].
    
do_encode(X, Dict) when is_atom(X); is_integer(X); is_binary(X) ->
    case dict:find(X, Dict) of
	{ok, Y} ->
	    Y;
	error ->
	    encode_obj(X)
    end;
do_encode({'#S', Str}, _Dict) ->
    %% This *is* a string
    encode_string(Str);
do_encode([H|T], Dict) ->
    S1  = do_encode(T, Dict),
    S2  = do_encode(H, Dict),
    [S1,S2,$&];
do_encode(T, Dict) when is_tuple(T) ->
    S1 = encode_tuple(1, T, Dict),
    [${,S1,$}];
do_encode([], _Dict) ->
    $#.

encode_list([H|T], Dict, L) ->
    encode_list(T, Dict, [$&,do_encode(H, Dict)|L]);
encode_list([], _Dict, L) ->
    reverse(L).

encode_tuple(N, T, _Dict) when N > size(T) ->
    "";
encode_tuple(N, T, Dict) ->
    S1 = do_encode(element(N, T), Dict),
    S2 = encode_tuple(N+1, T,  Dict),
    [S1,possible_comma(N, T),S2].

possible_comma(N, T) when N < size(T) -> $,;
possible_comma(_, _)                  -> [].

%% The ascii printables are in the range 32..126 includive

add_string([$\\|T], Quote)   -> [$\\,$\\|add_string(T, Quote)];
add_string([Quote|T], Quote) -> [$\\,Quote|add_string(T, Quote)];
add_string([H|T], Quote) when H >= 0,  H=< 255 -> [H|add_string(T, Quote)];
add_string([H|_], _Quote) -> exit({string_character,H});
add_string([], _)            -> [].
    
deabstract({'#S',S}) -> S;
deabstract(T) when is_tuple(T) ->
    list_to_tuple(map(fun deabstract/1, tuple_to_list(T)));
deabstract([H|T]) -> [deabstract(H)|deabstract(T)];
deabstract(T) -> T.
