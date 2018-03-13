%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Javascript and CSS minifier. Based on jsmin.c

%% Copyright 2018 Marc Worrell
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

%% jsmin is Copyright (c) 2002 Douglas Crockford  (www.crockford.com)
%% see https://github.com/douglascrockford/JSMin/blob/master/jsmin.c

-module(z_jsmin).

-export([
    minify/1
    ]).

-define(is_alnum(C), (
    (C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $A) orelse
    (C >= $0 andalso C =< $9) orelse
    C =:= $_ orelse
    C =:= $\\ orelse
    C =:= $$ orelse
    C > 126)).

-define(is_pre_regexp(C), (
    C =:= $( orelse C =:= $, orelse C =:= $= orelse C =:= $: orelse
    C =:= $[ orelse C =:= $! orelse C =:= $& orelse C =:= $| orelse
    C =:= $? orelse C =:= $+ orelse C =:= $- orelse C =:= $~ orelse
    C =:= $* orelse C =:= $/ orelse C =:= ${ orelse C =:= $\n)).

-define(isspace(C), (C =:= 32 orelse C =:= $\n)).

-spec minify( binary() ) -> binary().
minify( JS ) ->
    minify(next(JS), []).

minify(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
minify(<<$\n, JS/binary>>, []) ->
    minify(next(JS), []);
minify(<<32, JS/binary>>, []) ->
    minify(next(JS), []);
minify(<<$\n, JS/binary>>, [ $\n | _ ] = Acc) ->
    minify(next(JS), Acc);
minify(<<32, JS/binary>>, [ C | _ ] = Acc) when ?isspace(C) ->
    minify(next(JS), Acc);
minify(<<32, B, JS/binary>>, [ A | _ ] = Acc)
    when ?is_alnum(B) andalso ?is_alnum(A) ->
    minify(<<B, JS/binary>>, [ 32, A | Acc ]);
minify(<<32, $-, JS/binary>>, [ $- | _ ] = Acc) ->
    minify(<<$-, JS/binary>>, [ 32 | Acc ]);
minify(<<32, $+, JS/binary>>, [ $+ | _ ] = Acc) ->
    minify(<<$+, JS/binary>>, [ 32 | Acc ]);
minify(<<32, JS/binary>>, Acc) ->
    minify(next(JS), Acc);
minify(<<$\n, B, JS/binary>>, [ A | Acc ])
    when (?is_alnum(B) orelse B =:= ${ orelse B =:= $[ orelse B =:= $( orelse B =:= $+ orelse B =:= $-) andalso
         (?is_alnum(A) orelse A =:= $} orelse A =:= $] orelse A =:= $) orelse B =:= $+ orelse B =:= $- orelse B =:= $" orelse B =:= $' orelse B =:= $`) ->
    minify(<<B, JS/binary>>, [ $\n, A | Acc ]);
minify(<<$\n, JS/binary>>, Acc) ->
    minify(next(JS), Acc);
minify(<<Q, JS/binary>>, Acc) when Q =:= $'; Q =:= $"; Q =:= $` ->
    {JS1, Acc1} = string(Q, JS, Acc),
    minify(next(JS1), Acc1);
minify(<<$/, JS/binary>>, [ C | _] = Acc) when ?is_pre_regexp(C) ->
    Acc1 = case C of
        $/ -> [ $/, 32 | Acc ];
        $* -> [ $/, 32 | Acc ];
        _ -> [ $/ | Acc ]
    end,
    {JS1, Acc2} = regexp(JS, Acc1),
    minify(next(JS1), Acc2);
minify(<<C, JS/binary>>, Acc) ->
    minify(next(JS), [ C | Acc ]).


string(Q, <<Q, JS/binary>>, Acc) ->
    {JS, [ Q | Acc ]};
string(Q, <<$\\, C, JS/binary>>, Acc) ->
    string(Q, JS, [ C, $\\ | Acc ]);
string(Q, <<C, JS/binary>>, Acc) ->
    string(Q, JS, [ C | Acc ]);
string(_Q, <<>>, _Acc) ->
    throw('Unterminated string').


regexp(<<$[, JS/binary>>, Acc) ->
    {JS1, Acc1} = regexp_set(JS, [ $[ | Acc ]),
    regexp(JS1, Acc1);
regexp(<<$/, C, _/binary>>, _Acc) when C =:= $/; C =:= $* ->
    throw('Unterminated regexp');
regexp(<<$/, JS/binary>>, Acc) ->
    {JS, [ $/ | Acc ]};
regexp(<<$\\, C, JS/binary>>, Acc) ->
    regexp(JS, [ C, $\\ | Acc ]);
regexp(<<>>, _Acc) ->
    throw('Unterminated regexp');
regexp(<<C, JS/binary>>, Acc) ->
    regexp(JS, [ C | Acc ]).

regexp_set(<<$], JS/binary>>, Acc) ->
    {JS, [ $] | Acc ]};
regexp_set(<<$\\, C, JS/binary>>, Acc) ->
    regexp_set(JS, [ C, $\\, Acc ]);
regexp_set(<<C, JS/binary>>, Acc) ->
    regexp_set(JS, [ C | Acc ]);
regexp_set(<<>>, _Acc) ->
    throw('Unterminated set in regexp').


-spec next(binary()) -> binary().
next(<<>>) -> <<>>;
next(<<"//", A/binary>>) -> next(skip_to_eol(A));
next(<<"/*", A/binary>>) -> next(skip_comment(A));
next(<<$\r, A/binary>>) -> <<$\n, A/binary>>;
next(<<$\n, _/binary>> = A) -> A;
next(<<C, _/binary>> = A) when C >= 32 -> A;
next(<<_, A/binary>>) -> <<" ", A/binary>>.


% -spec is_alnum( ch() ) -> boolean.
% is_alnum(C) when C >= $a, C =< $z -> true;
% is_alnum(C) when C >= $A, C =< $Z -> true;
% is_alnum(C) when C >= $0, C =< $9 -> true;
% is_alnum($_) -> true;
% is_alnum($$) -> true;
% is_alnum($\\) -> true;
% is_alnum(eof) -> false;
% is_alnum(C) -> C > 126.


-spec skip_to_eol(binary()) -> binary().
skip_to_eol(<<>>) -> <<>>;
skip_to_eol(<<C, _/binary>> = A) when C =< $\n -> A;
skip_to_eol(<<_, A/binary>>) -> skip_to_eol(A).

-spec skip_comment(binary()) -> binary().
skip_comment(<<>>) -> <<>>;
skip_comment(<<"*/", A/binary>>) -> <<" ", A/binary>>;
skip_comment(<<_, A/binary>>) -> skip_comment(A).

