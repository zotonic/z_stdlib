% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% coding: utf-8

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc String related functions
%% @end

%% @todo Check valid chars for filenames, allow chinese, japanese, etc?
%% CJK Unified Ideographs Extension A: Range: 3400-4DBF
%% CJK Unified Ideographs: Range: 4E00-9FAF
%% Kangxi Radicals: Range 2F00-2FDF
%% See also: http://www.utf8-chartable.de/

%% Copyright 2009-2024 Marc Worrell
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

-module(z_string).
-author("Marc Worrell <marc@worrell.nl").

%% Used to append to a string when truncating the string.
-define(DOTS_UTF8, <<226,128,166>>).

%% interface functions
-export([
    trim/1,
    trim_left/1,
    trim_right/1,
    trim/2,
    trim_left/2,
    trim_right/2,
    trim_left_func/2,
    is_string/1,
    is_whitespace/1,
    first_char/1,
    last_char/1,
    unquote/1,
    unquote/2,
    nospaces/1,
    line/1,
    len/1,
    unaccent/1,
    normalize/1,
    to_rootname/1,
    to_name/1,
    to_slug/1,
    to_lower/1,
    to_upper/1,
    replace/3,
    sanitize_utf8/1,
    truncate/2,
    truncate/3,
    truncatechars/2,
    truncatechars/3,
    truncatewords/2,
    truncatewords/3,
    split_lines/1,
    escape_ical/1,
    starts_with/2,
    ends_with/2,
    contains/2,
    split/2,
    concat/2,
    distance/2
]).


%% @doc Remove whitespace at the start and end of the string
-spec trim(iodata()) -> binary().
trim(B) when is_binary(B) ->
    trim_right(trim_left(B));
trim(L) when is_list(L) ->
    trim(iolist_to_binary(L)).

%% @doc Remove all occurrences of a character at the start and end of a string.
-spec trim(iodata(), integer()) -> binary().
trim(B, Char) when is_binary(B) ->
    trim_right(trim_left(B, Char), Char);
trim(L, Char) when is_list(L) ->
    trim(iolist_to_binary(L), Char).


%% @doc Remove whitespace at the start the string
-spec trim_left(binary()|list()) -> binary()|list().
trim_left(S) ->
    trim_left_func(S, fun is_whitespace/1).

%% @doc Remove all occurrences of a char at the start of a string
-spec trim_left(binary()|list(), integer()) -> binary()|list().
trim_left(S, Char) ->
    trim_left_func(S, fun(C) -> C == Char end).


trim_left_func(<<Char/utf8, Rest/binary>> = Bin, F) ->
    case F(Char) of
        true -> trim_left_func(Rest, F);
        false -> Bin
    end;
trim_left_func([Char|Rest] = L, F) when is_integer(Char) ->
    case F(Char) of
        true -> trim_left_func(Rest, F);
        false -> L
    end;
trim_left_func([L|Rest], F) when is_list(L); is_binary(L) ->
    case trim_left_func(L, F) of
        [] -> trim_left_func(Rest, F);
        <<>> -> trim_left_func(Rest, F);
        Other -> [Other|Rest]
    end;
trim_left_func(Other, _F) ->
    Other.

%% @doc Remove whitespace at the end of the string
-spec trim_right(iodata()) -> binary().
trim_right(B) when is_binary(B) ->
    trim_right(B, <<>>, <<>>);
trim_right(L) ->
    trim_right(iolist_to_binary(L)).

trim_right(<<C/utf8, Rest/binary>>, WS, Acc) ->
    case is_whitespace(C) of
        true -> trim_right(Rest, <<WS/binary, C/utf8>>, Acc);
        false -> trim_right(Rest, <<>>, <<Acc/binary, WS/binary, C/utf8>>)
    end;
trim_right(<<>>, _WS, Acc) ->
    Acc.

%% @doc Remove all occurrences of a char at the end of the string
-spec trim_right(iodata(), integer()) -> binary().
trim_right(B, Char) when is_binary(B) ->
    trim_right(B, Char, <<>>, <<>>);
trim_right(L, Char) ->
    trim_right(iolist_to_binary(L), Char).

trim_right(<<C/utf8, Rest/binary>>, Char, WS, Acc) ->
    case C of
        Char -> trim_right(Rest, Char, <<WS/binary, C/utf8>>, Acc);
        _ -> trim_right(Rest, Char, <<>>, <<Acc/binary, WS/binary, C/utf8>>)
    end;
trim_right(<<>>, _Char, _WS, Acc) ->
    Acc.

%% @doc Check if the variable is a one dimensional list of bytes, or a valid
%% utf-8 binary.
-spec is_string(iodata()) -> boolean().
is_string(<<>>) ->
    true;
is_string(<<C/utf8, Rest/binary>>) when
        C >= 32 orelse C =:= 9 orelse C =:= 10 orelse C =:= 12 orelse C =:= 13 ->
    is_string(Rest);
is_string(<<_, _/binary>>) ->
    false;
is_string([]) ->
    true;
is_string([C|Rest]) when
        is_integer(C)
        andalso C =< 255
        andalso (C >= 32 orelse C =:= 9 orelse C =:= 10 orelse C =:= 12 orelse C =:= 13) ->
    is_string(Rest);
is_string(_) ->
    false.

%% @doc Is the character an ASCII or Unicode whitespace character?
%%      See @link https://en.wikipedia.org/wiki/Whitespace_character
-spec is_whitespace(non_neg_integer()) -> boolean().
is_whitespace(C) when C =< 32 -> true;
is_whitespace(133) -> true;
is_whitespace(160) -> true;
is_whitespace(8203) -> true;
is_whitespace(C) when C >= 8192 andalso C =< 8205 -> true;
is_whitespace(8232) -> true;
is_whitespace(8233) -> true;
is_whitespace(8239) -> true;
is_whitespace(8287) -> true;
is_whitespace(12288) -> true;
is_whitespace(65279) -> true;
is_whitespace(_) -> false.

%% @doc Return the first character of a string.
-spec first_char(binary()|list()) -> non_neg_integer().
first_char(<<>>) -> undefined;
first_char(<<C/utf8, _/binary>>) -> C;
first_char(<<C, _/binary>>) -> C;   %% illegal UTF-8, do not crash
first_char([H|_]) when is_integer(H), H >= 0 -> H;
first_char(_) -> undefined.


%% @doc Return the last character of a string
-spec last_char(binary()|string()) -> non_neg_integer().
last_char([]) -> undefined;
last_char(L) when is_list(L) ->
    case lists:last(L) of
        C when is_integer(C), C >= 0 -> C;
        _ -> undefined
    end;
last_char(<<>>) -> undefined;
last_char(<<C/utf8>>) -> C;
last_char(<<_, R/binary>>) -> last_char(R).


%% @doc Remove the first and last char if they are double quotes.
-spec unquote( iodata() ) -> iodata().
unquote(S) ->
    unquote(S, $").

unquote(S, Q) ->
    case S of
        <<Q, R/binary>> -> unquote1(R, <<>>, Q, S);
        [Q|R] -> unquote1(R, [], Q, S);
        _ -> S
    end.

unquote1([], _Acc, _Q, S) -> S;
unquote1([Q], Acc, Q, _S) -> lists:reverse(Acc);
unquote1([H|T], Acc, Q, S) -> unquote1(T, [H|Acc], Q, S);

unquote1(<<>>, _Acc, _Q, S) -> S;
unquote1(<<Q>>, Acc, Q, _S) -> Acc;
unquote1(<<C,R/binary>>, Acc, Q, S) -> unquote1(R, <<Acc/binary, C>>, Q, S).


%% @doc Remove all spaces and control characters from a string.
-spec nospaces(iodata()) -> iodata().
nospaces(B) when is_binary(B) ->
    nospaces_bin(B, <<>>);
nospaces(L) when is_list(L) ->
    nospaces_list(L, []).

nospaces_list([], Acc) ->
    lists:reverse(Acc);
nospaces_list([C|Rest], Acc) when C =< 32 ->
    nospaces_list(Rest, Acc);
nospaces_list([B|Rest], Acc) when is_binary(B) ->
    nospaces_list(Rest, [nospaces_bin(B, <<>>)|Acc]);
nospaces_list([L|Rest], Acc) when is_list(L) ->
    nospaces_list(Rest, [nospaces_list(L,[])|Acc]);
nospaces_list([C|Rest], Acc) ->
    nospaces_list(Rest, [C|Acc]).

nospaces_bin(<<>>, Acc) ->
    Acc;
nospaces_bin(<<C,Rest/binary>>, Acc) when C =< 32 ->
    nospaces_bin(Rest, Acc);
nospaces_bin(<<C,Rest/binary>>, Acc) ->
    nospaces_bin(Rest, <<Acc/binary,C>>).


%% @doc Make sure that the string is on one line only, replace control characters with spaces
-spec line( string() | binary() ) -> string() | binary().
line(B) when is_binary(B) ->
    << <<(if C < 32 -> 32; true -> C end)>> || <<C>> <= B >>;
line(L) when is_list(L) ->
    [ if C < 32 -> 32; true -> C end || C <- L ].

-spec len(binary()|string()|undefined) -> integer().
len(undefined) -> 0;
len(L) when is_list(L) -> erlang:length(L);
len(B) when is_binary(B) -> len(B, 0).

len(<<>>, N) -> N;
len(<<_C/utf8, B/binary>>, N) -> len(B, N+1);
len(<<_C, B/binary>>, N) -> len(B, N+1).

%% @doc Return a lowercase binary string for the input.
-spec to_lower(string()|binary()|atom()) -> binary().
to_lower(None) when None =:= undefined; None =:= <<>>; None =:= "" ->
    <<>>;
to_lower(B) when is_binary(B) ->
    string:casefold(B);
to_lower(A) when is_atom(A) ->
    to_lower(atom_to_binary(A, utf8));
to_lower(L) when is_list(L) ->
    to_lower(unicode:characters_to_binary(L, utf8)).

%% @doc Return a uppercase binary string for the input.
-spec to_upper(string()|binary()|atom()) -> binary().
to_upper(None) when None =:= undefined; None =:= <<>>; None =:= "" ->
    <<>>;
to_upper(B) when is_binary(B) ->
    string:uppercase(B);
to_upper(A) when is_atom(A) ->
    to_upper(atom_to_binary(A, utf8));
to_upper(L) when is_list(L) ->
    to_upper(unicode:characters_to_binary(L, utf8)).

%% @doc Filter a filename so that we obtain a basename that is safe to use.
-spec to_rootname( file:filename_all() ) -> binary().
to_rootname(Filename) ->
    to_slug(filename:rootname(filename:basename(Filename))).

%% @doc Map a string to a slug that can be used in the uri of a page. Same as a name, but then
%% with dashes instead of underscores.
-spec to_slug( string() | binary() | atom() ) -> binary().
to_slug(Title) ->
    binary:replace(to_name(Title), <<$_>>, <<$->>, [global]).


%% @doc Map a string to a value that can be used as a name or slug. Maps all characters to
%% lowercase and remove non digalpha characters.
-spec to_name( string() | binary() | atom() | {trans, list()} ) -> binary().
to_name(V) ->
    Norm = to_name1(normalize(V), <<>>),
    Norm1 = name_cleanup(Norm),
    case trim(Norm1, $_) of
        <<>> -> <<"_">>;
        Name -> Name
    end.

name_cleanup(V) ->
    case binary:replace(V, <<"__">>, <<"_">>, [global]) of
        V -> V;
        V1 -> name_cleanup(V1)
    end.

to_name1(<<>>, Acc) ->
    Acc;
to_name1(<<V/utf8, Rest/binary>>, Acc) when V >= $a, V =< $z ->
    to_name1(Rest, <<Acc/binary, V/utf8>>);
to_name1(<<V/utf8, Rest/binary>>, Acc) when V >= $0, V =< $9 ->
    to_name1(Rest, <<Acc/binary, V/utf8>>);
to_name1(<<"@", Rest/binary>>, Acc) ->
    to_name1(Rest, <<Acc/binary,$_,$a,$t,$_>>);
to_name1(<<_/utf8, Rest/binary>>, Acc) ->
    to_name1(Rest, <<Acc/binary, "_">>).


%% @doc Remove all accents from all characters.
-spec unaccent(S) -> S1 when
    S :: unicode:chardata(),
    S1 :: binary().
unaccent(S) ->
    {ok, Re} = re:compile(<<"\\p{Mn}">>, [unicode]),
    NFD = unicode:characters_to_nfd_binary(S),
    WithoutAccents = re:replace(NFD, Re, <<>>, [global]),
    unicode:characters_to_nfc_binary(WithoutAccents).


%% @doc Transliterate an unicode string to an ascii string with lowercase characters.
%% Tries to transliterate some characters to a..z
-spec normalize(string() | binary() | atom() | {trans, list()}) -> binary().
normalize(S) ->
    z_string_normalize:normalize(S).

%% @doc Replace a string inside another string
%% Copyright 2008 Rusty Klophaus  (Nitrogen, MIT License)
replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
    Length = length(S1),
    case string:substr(String, 1, Length) of
        S1 ->
            S2 ++ replace(string:substr(String, Length + 1), S1, S2);
        _ ->
            [hd(String)|replace(tl(String), S1, S2)]
    end.

%% @doc Sanitize an utf-8 string, remove all non-utf-8 characters.
-spec sanitize_utf8( string() | binary() ) -> binary().
sanitize_utf8(L) when is_list(L) -> sanitize_utf8(iolist_to_binary(L));
sanitize_utf8(B) when is_binary(B) -> s_utf8(B, <<>>).

s_utf8(<<>>, Acc) ->
    Acc;

%% 1 byte
s_utf8(<<0, Rest/binary>>, Acc) ->
    s_utf8(Rest, Acc);
s_utf8(<<X, Rest/binary>>, Acc) when X < 128 ->
    s_utf8(Rest, <<Acc/binary, X>>);

%% 2 bytes
s_utf8(<<2#110:3, A:5, 2#10:2, B:6, Rest/binary>>, Acc) when
        <<0:5, A:5, B:6>> >= <<16#80:16>>,
        <<0:5, A:5, B:6>> =< <<16#7FF:16>> ->
    s_utf8(Rest, <<Acc/binary, 2#110:3, A:5, 2#10:2, B:6>>);

%% 3 bytes
s_utf8(<<2#1110:4, A:4, 2#10:2, B:6, 2#10:2, C:6, Rest/binary>>, Acc) when
        <<0:7, A:5, B:6, C:6>> >= <<16#800:24>> andalso
        <<0:7, A:5, B:6, C:6>> =< <<16#D7FF:24>>
        orelse
        <<0:7, A:5, B:6, C:6>> >= <<16#E000:24>> andalso
        <<0:7, A:5, B:6, C:6>> =< <<16#FFFD:24>> ->
    s_utf8(Rest, <<Acc/binary, 2#1110:4, A:4, 2#10:2, B:6, 2#10:2, C:6>>);

%% 4 bytes
s_utf8(<<2#11110:5, A:3, 2#10:2, B:6, 2#10:2, C:6, 2#10:2, D:6, Rest/binary>>,
        Acc) when
        <<0:3, A:3, B:6, C:6, D:6>> >= <<16#10000:24>> andalso
        <<0:3, A:3, B:6, C:6, D:6>> =< <<16#10FFFF:24>> ->
    s_utf8(Rest, <<Acc/binary, 2#11110:5, A:3, 2#10:2, B:6, 2#10:2, C:6, 2#10:2, D:6>>);

%% Drop illegal utf-8 character.
s_utf8(<<_, Rest/binary>>, Acc) ->
    s_utf8(Rest, Acc).

%% @doc Truncate a string.  Append the '...' character at the place of break off.
-spec truncate( String :: undefined | string() | binary(), Length :: integer() ) -> undefined | binary().
truncate(undefined, _) ->
    undefined;
truncate(L, N) ->
    truncate(L, N, ?DOTS_UTF8).

-spec truncate( String :: undefined | string() | binary(), Length :: integer(), Append :: binary() | string() ) -> binary().
truncate(_L, N, _Append) when N =< 0 ->
    <<>>;
truncate(B, N, Append) when is_binary(B), is_binary(Append) ->
    truncate(B, N, Append, in_word, <<>>, in_word, <<>>);
truncate(L, N, Append) ->
    truncate(z_convert:to_binary(L), N, z_convert:to_binary(Append)).


truncate(<<>>, _, _Append, _LastState, _Last, _AccState, Acc) ->
    Acc;
truncate(_, 0, _Append, sentence, Last, _AccState, _Acc) ->
    Last;
truncate(_, 0, Append, _, <<>>, _AccState, Acc) ->
    <<Acc/binary, Append/binary>>;
truncate(_, 0, Append, _LastState, Last, _AccState, _Acc) ->
    <<Last/binary, Append/binary>>;

%% HTML element (we only allow self closing elements like <br> and <hr>)
truncate(<<$>,Rest/binary>>, N, Append, _LastState, Last, in_element, Acc) ->
    truncate(Rest, N, Append, sentence, Last, in_word, <<Acc/binary,$>>>);

truncate(<<C/utf8,Rest/binary>>, N, Append, LastState, Last, in_element, Acc) ->
    truncate(Rest, N, Append, LastState, Last, in_element, <<Acc/binary,C/utf8>>);

truncate(<<$<,Rest/binary>>, N, Append, LastState, _Last, _AccState, Acc) ->
    truncate(Rest, N, Append, LastState, Acc, in_element, <<Acc/binary,$<>>);

truncate(<<C,Rest/binary>>, N, Append, LastState, Last, AccState, Acc)
    when C =:= $.; C =:= $!; C =:= $? ->
        case AccState of
            in_word -> truncate(Rest, N-1, Append, sentence, <<Acc/binary,C>>, sentence, <<Acc/binary,C>>);
            word    -> truncate(Rest, N-1, Append, sentence, <<Acc/binary,C>>, sentence, <<Acc/binary,C>>);
            _       -> truncate(Rest, N-1, Append, LastState, Last,   sentence, <<Acc/binary,C>>)
        end;
truncate(<<C,Rest/binary>>, N, Append, LastState, Last, AccState, Acc)
    when C =:= $;; C =:= $-; C =:= $, ->
        case AccState of
            in_word -> truncate(Rest, N-1, Append, sentence,  Acc,  word, <<Acc/binary,C>>);
            _       -> truncate(Rest, N-1, Append, LastState, Last, word, <<Acc/binary,C>>)
        end;
truncate(<<C,Rest/binary>>, N, Append, LastState, Last, AccState, Acc)
    when C =:= 32; C =:= 9; C =:= 10; C =:= 13; C =:= $/; C =:= $|; C =:= $(; C =:= $); C =:= $" ->
        case AccState of
            in_word -> truncate(Rest, N-1, Append, word, Acc, word, <<Acc/binary,C>>);
            _       -> truncate(Rest, N-1, Append, LastState, Last, word, <<Acc/binary,C>>)
        end;
truncate(<<$&,_/binary>>=Input, N, Append, LastState, Last, AccState, Acc) ->
    {Rest1, _EntityLen, Acc1} = get_entity(Input, 0, Acc),
    case AccState of
        in_word -> truncate(Rest1, N-1, Append, word, Acc1, word, Acc1);
        _       -> truncate(Rest1, N-1, Append, LastState, Last, word, Acc1)
    end;
truncate(<<C/utf8,Rest/binary>>, N, Append, LastState, Last, _AccState, Acc) ->
    truncate(Rest, N-1, Append, LastState, Last, in_word, <<Acc/binary,C/utf8>>);

truncate(<<_, Rest/binary>>, N, Append, LastState, Last, AccState, Acc) ->
    % Silently drop non-utf-8 characters
    truncate(Rest, N, Append, LastState, Last, AccState, Acc).


get_entity(<<>>, N, Acc) ->
    {<<>>, N, Acc};
get_entity(<<$;,Rest/binary>>, N, Acc) ->
    {Rest, N+1, <<Acc/binary,$;>>};
get_entity(<<C/utf8,Rest/binary>>, N, Acc) ->
    get_entity(Rest, N+1, <<Acc/binary,C/utf8>>).


%% @doc Truncate a string, count all characters. No special handling of entities and tags.
-spec truncatechars( String :: undefined | string() | binary(), Length :: integer() ) -> undefined | binary().
truncatechars(undefined, _) ->
    undefined;
truncatechars(S, N) ->
    truncatechars(S, N, <<>>).

-spec truncatechars( String :: undefined | string() | binary(), Length :: integer(), Append :: binary() | string() ) -> binary().
truncatechars(_L, N, _Append) when N =< 0 ->
    <<>>;
truncatechars(B, N, Append) when is_binary(B), is_binary(Append) ->
    truncatechars(B, N, Append, <<>>);
truncatechars(L, N, Append) ->
    truncatechars(z_convert:to_binary(L), N, z_convert:to_binary(Append)).

truncatechars(<<>>, _, _Append, Acc) ->
    Acc;
truncatechars(_, 0, Append, Acc) ->
    <<Acc/binary, Append/binary>>;
truncatechars(<<C/utf8,Rest/binary>>, N, Append, Acc) ->
    truncatechars(Rest, N-1, Append, <<Acc/binary,C/utf8>>);
truncatechars(<<_, Rest/binary>>, N, Append, Acc) ->
    % Silently drop non-utf-8 characters
    truncatechars(Rest, N, Append, Acc).


%% @doc Truncate a string, count all words. No special handling of entities and tags.
truncatewords(undefined, _) ->
    undefined;
truncatewords(S, Words) ->
    truncatewords(S, Words, ?DOTS_UTF8).
truncatewords(S, Words, Append) when is_binary(S) ->
    truncatewords(S, in_space, Words, Append, <<>>);
truncatewords(S, Words, Append) when is_list(S) ->
    truncatewords(iolist_to_binary(S), in_space, Words, Append, <<>>).

truncatewords(_S, _State, 0, Append, Acc) ->
    Append1 = z_convert:to_binary(Append),
    trim_left_func(<<Acc/binary,Append1/binary>>, fun iswordsep/1);
truncatewords(<<>>, _State, _Words, _Append, Acc) ->
    Acc;
truncatewords(<<C/utf8,Rest/binary>>, in_space, Words, Append, Acc) ->
    case iswordsep(C) of
        true -> truncatewords(Rest, in_space, Words, Append, <<Acc/binary,C/utf8>>);
        false -> truncatewords(Rest, in_word, Words, Append, <<Acc/binary,C/utf8>>)
    end;
truncatewords(<<C/utf8,Rest/binary>>, in_word, Words, Append, Acc) ->
    case iswordsep(C) of
        true -> truncatewords(Rest, in_space, Words-1, Append, <<Acc/binary,C/utf8>>);
        false -> truncatewords(Rest, in_word, Words, Append, <<Acc/binary,C/utf8>>)
    end.

iswordsep($\s) -> true;
iswordsep($\n) -> true;
iswordsep($\r) -> true;
iswordsep($\t) -> true;
iswordsep($,) -> true;
iswordsep($:) -> true;
iswordsep($;) -> true;
iswordsep(_) -> false.


%% @doc Split the binary into lines. Line separators can be \r, \n or \r\n.
-spec split_lines(binary()) -> list(binary()).
split_lines(B) when is_binary(B) ->
    split_lines(B, <<>>, []).

    split_lines(<<>>, Line, Acc) ->
        lists:reverse([Line|Acc]);
    split_lines(<<13,10,Rest/binary>>, Line, Acc) ->
        split_lines(Rest, <<>>, [Line|Acc]);
    split_lines(<<13,Rest/binary>>, Line, Acc) ->
        split_lines(Rest, <<>>, [Line|Acc]);
    split_lines(<<10,Rest/binary>>, Line, Acc) ->
        split_lines(Rest, <<>>, [Line|Acc]);
    split_lines(<<C, Rest/binary>>, Line, Acc) ->
        split_lines(Rest, <<Line/binary, C>>, Acc).


%% @doc Escape special characters for ical RFC2445 elements
escape_ical(L) when is_list(L) ->
    escape_ical(iolist_to_binary(L));
escape_ical(B) when is_binary(B) ->
    escape_ical(B, <<>>, 0);
escape_ical(A) when is_atom(A) ->
    escape_ical(atom_to_list(A)).

    escape_ical(<<>>, Acc, _N) -> Acc;
    escape_ical(B, Acc, N) when N >= 70 -> escape_ical(B, <<Acc/binary, 13, 10, 32>>, 0);
    escape_ical(<<13, 10, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $n>>, N+2);
    escape_ical(<<10, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $n>>, N+2);
    escape_ical(<<9, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, 32>>, N+1);
    escape_ical(<<$", Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $">>, N+2);
    escape_ical(<<$,, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $,>>, N+2);
    escape_ical(<<$:, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $:>>, N+3);
    escape_ical(<<$;, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $;>>, N+2);
    escape_ical(<<$\\, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, $\\, $\\>>, N+2);
    escape_ical(<<C, Rest/binary>>, Acc, N) -> escape_ical(Rest, <<Acc/binary, C>>, N+1).

%% @doc Return true if Start is a prefix of Word
-spec starts_with(iodata(), iodata()) -> boolean().
starts_with(Start, B) when is_binary(Start), is_binary(B) ->
    StartSize = size(Start),
    case B of
        <<Start:StartSize/binary, _/binary>> -> true;
        _ -> false
    end;
starts_with(Start, String) ->
    starts_with(iolist_to_binary(Start), iolist_to_binary(String)).


%% @doc Return true if Word ends with End
-spec ends_with(iodata(), iodata()) -> boolean().
ends_with(End, B) when is_binary(End), is_binary(B) ->
    StartSize = size(B) - size(End),
    case B of
        <<_:StartSize/binary, End/binary>> -> true;
        _ ->false
    end;
ends_with(End, String) ->
    ends_with(iolist_to_binary(End), iolist_to_binary(String)).


%% @doc Return true if What is found in the string
-spec contains(iodata(), iodata()) -> boolean().
contains(What, B) when is_binary(What), is_binary(B) ->
    contains(What, size(What), B, 0);
contains(What, String) ->
    contains(iolist_to_binary(What), iolist_to_binary(String)).

    contains(_What, _SizeWhat, B, C) when C > size(B) ->
        false;
    contains(What, SizeWhat, B, C) ->
        case B of
            <<_:C/binary, What:SizeWhat/binary, _/binary>> ->true;
            _ ->contains(What, SizeWhat, B, C + 1)
        end.

%% @doc Split a string, see http://www.erlang.org/pipermail/erlang-questions/2008-October/038896.html
%% @spec split(String, String) -> list()
split(String, []) ->
     split0(String);
split(String, [Sep]) when is_integer(Sep) ->
     split1(String, Sep);
split(String, [C1,C2|L]) when is_integer(C1), is_integer(C2) ->
     split2(String, C1, C2, L).

%% Split a string at "", which is deemed to occur _between_
%% adjacent characters, but queerly, not at the beginning
%% or the end.

split0([C|Cs]) ->
     [[C] | split0(Cs)];
split0([]) ->
     [].

%% Split a string at a single character separator.

split1(String, Sep) ->
     split1_loop(String, Sep, "").

split1_loop([Sep|String], Sep, Rev) ->
     [lists:reverse(Rev) | split1(String, Sep)];
split1_loop([Chr|String], Sep, Rev) ->
     split1_loop(String, Sep, [Chr|Rev]);
split1_loop([], _, Rev) ->
     [lists:reverse(Rev)].

%% Split a string at a multi-character separator
%% [C1,C2|L].  These components are split out for
%% a fast match.

split2(String, C1, C2, L) ->
     split2_loop(String, C1, C2, L, "").

split2_loop([C1|S = [C2|String]], C1, C2, L, Rev) ->
     case split_prefix(L, String)
       of no   -> split2_loop(S, C1, C2, L, [C1|Rev])
        ; Rest -> [lists:reverse(Rev) | split2(Rest, C1, C2, L)]
     end;
split2_loop([Chr|String], C1, C2, L, Rev) ->
     split2_loop(String, C1, C2, L, [Chr|Rev]);
split2_loop([], _, _, _, Rev) ->
     [lists:reverse(Rev)].

split_prefix([C|L], [C|S]) -> split_prefix(L, S);
split_prefix([],    S)     -> S;
split_prefix(_,     _)     -> no.


%% @doc Concatenate two strings (list or binary). Return type matching the first part.
-spec concat( string() | binary(), string() | binary() ) -> string() | binary().
concat(A, B) when is_binary(A) ->
    B1 = z_convert:to_binary(B),
    <<A/binary, B1/binary>>;
concat(A, B) when is_list(A) ->
    A ++ z_convert:to_flatlist(B).


%% @doc Calculate the Levensthein distance between two strings.
%% Adapted from https://rosettacode.org/wiki/Levenshtein_distance#Erlang to use
%% binary utf8 strings and maps. The smaller the returned integer the closer the
%% strings are. Distance 0 means the two strings are equal.
-spec distance(S, T) -> Distance when
    S :: binary() | string(),
    T :: binary() | string(),
    Distance :: non_neg_integer().
distance(S, T) when is_binary(S), is_binary(T) ->
    {L,_} = ld(S, T, #{}),
    L;
distance(S, T) ->
    distance(unicode:characters_to_binary(S), unicode:characters_to_binary(T)).

ld(<<>> = S, T, Cache) ->
    {len(T), Cache#{ {S,T} => len(T) }};
ld(S, <<>> = T, Cache) ->
    {len(S), Cache#{ {S,T} => len(S) }};
ld(<<X/utf8, S/binary>>, <<X/utf8, T/binary>>, Cache) ->
    ld(S, T, Cache);
ld(<<_/utf8, ST/binary>> = S, <<_/utf8, TT/binary>> = T, Cache) ->
    case maps:get({S, T}, Cache, undefined) of
        undefined ->
            {L1, C1} = ld(S, TT, Cache),
            {L2, C2} = ld(ST, T, C1),
            {L3, C3} = ld(ST, TT, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, C3#{ {S, T} => L }};
        Dist ->
            {Dist, Cache}
    end.
