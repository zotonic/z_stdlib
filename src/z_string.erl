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


%% @doc Return a lowercase string for the input
-spec to_lower(string()|binary()|atom()) -> binary().
to_lower(B) when is_binary(B) ->
    to_lower(B,<<>>);
to_lower(undefined) ->
    <<>>;
to_lower(A) when is_atom(A) ->
    to_lower(z_convert:to_binary(A));
to_lower(L) when is_list(L) ->
    to_lower(unicode:characters_to_binary(L)).

to_lower(<<>>, Acc) ->
    Acc;
to_lower(<<H,T/binary>>, Acc) when H >= $A andalso H =< $Z ->
    H1 = H + 32,
    to_lower(T,<<Acc/binary,H1>>);
to_lower(<<H,T/binary>>, Acc) when H < 128 ->
    to_lower(T,<<Acc/binary,H>>);
to_lower(<<"Å"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,165>>);
to_lower(<<"Ä"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,164>>);
to_lower(<<"Á"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,161>>);
to_lower(<<"À"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,160>>);
to_lower(<<"Ë"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,171>>);
to_lower(<<"Ê"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,170>>);
to_lower(<<"É"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,169>>);
to_lower(<<"È"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,168>>);
to_lower(<<"Ï"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,175>>);
to_lower(<<"Î"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,174>>);
to_lower(<<"Í"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,173>>);
to_lower(<<"Ì"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,172>>);
to_lower(<<"Ü"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,188>>);
to_lower(<<"Û"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,187>>);
to_lower(<<"Ú"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,186>>);
to_lower(<<"Ù"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,185>>);
to_lower(<<"Ö"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,182>>);
to_lower(<<"Ô"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,180>>);
to_lower(<<"Ó"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,179>>);
to_lower(<<"Ò"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,178>>);
to_lower(<<"Ø"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,184>>);
to_lower(<<"Ç"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,167>>);
to_lower(<<"Æ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,166>>);
to_lower(<<"Œ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,197,147>>);
to_lower(<<"Ã"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,163>>);
to_lower(<<"Ñ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,177>>);
to_lower(<<"Õ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,195,181>>);
% Cyrillic support
to_lower(<<"А"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,176>>);
to_lower(<<"Б"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,177>>);
to_lower(<<"В"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,178>>);
to_lower(<<"Г"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,179>>);
to_lower(<<"Д"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,180>>);
to_lower(<<"Е"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,181>>);
to_lower(<<"Ё"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,145>>);
to_lower(<<"Ж"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,182>>);
to_lower(<<"З"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,183>>);
to_lower(<<"И"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,184>>);
to_lower(<<"Й"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,185>>);
to_lower(<<"К"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,186>>);
to_lower(<<"Л"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,187>>);
to_lower(<<"М"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,188>>);
to_lower(<<"Н"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,189>>);
to_lower(<<"О"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,190>>);
to_lower(<<"П"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,208,191>>);
to_lower(<<"Р"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,128>>);
to_lower(<<"С"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,129>>);
to_lower(<<"Т"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,130>>);
to_lower(<<"У"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,131>>);
to_lower(<<"Ф"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,132>>);
to_lower(<<"Х"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,133>>);
to_lower(<<"Ц"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,134>>);
to_lower(<<"Ч"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,135>>);
to_lower(<<"Ш"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,136>>);
to_lower(<<"Щ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,137>>);
to_lower(<<"Ъ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,138>>);
to_lower(<<"Ы"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,139>>);
to_lower(<<"Ь"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,140>>);
to_lower(<<"Э"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,141>>);
to_lower(<<"Ю"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,142>>);
to_lower(<<"Я"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,143>>);
% Extra Ukrainian characters
to_lower(<<"Ґ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,210,145>>);
to_lower(<<"Ї"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,151>>);
to_lower(<<"І"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,150>>);
to_lower(<<"Є"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,209,148>>);
% Polish support
to_lower(<<"Ą"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,196,133>>);
to_lower(<<"Ę"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,196,153>>);
to_lower(<<"Ć"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,196,135>>);
to_lower(<<"Ł"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,197,130>>);
to_lower(<<"Ń"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,197,132>>);
to_lower(<<"Ś"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,197,155>>);
to_lower(<<"Ź"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,197,186>>);
to_lower(<<"Ż"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,197,188>>);
% Turkish support
to_lower(<<"Ş"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,197,159>>);
to_lower(<<"Ğ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,196,159>>);
to_lower(<<"İ"/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,196,177>>);
% Other characters are taken as-is
to_lower(<<H/utf8,T/binary>>, Acc) -> to_lower(T, <<Acc/binary,H/utf8>>).


%% @doc Return a uppercase string for the input
-spec to_upper(string()|binary()|atom()) -> binary().
to_upper(B) when is_binary(B) ->
    to_upper(B,<<>>);
to_upper(undefined) ->
    <<>>;
to_upper(A) when is_atom(A) ->
    to_upper(z_convert:to_binary(A));
to_upper(L) when is_list(L) ->
    to_upper(unicode:characters_to_binary(L), <<>>).

to_upper(<<>>, Acc) ->
    Acc;
to_upper(<<H,T/binary>>, Acc) when H >= $a andalso H =< $z ->
    H1 = H - 32,
    to_upper(T,<<Acc/binary,H1>>);
to_upper(<<H,T/binary>>, Acc) when H < 128 ->
    to_upper(T,<<Acc/binary,H>>);
to_upper(<<"å"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,133>>);
to_upper(<<"ä"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,132>>);
to_upper(<<"á"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,129>>);
to_upper(<<"à"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,128>>);
to_upper(<<"ë"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,139>>);
to_upper(<<"ê"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,138>>);
to_upper(<<"é"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,137>>);
to_upper(<<"è"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,136>>);
to_upper(<<"ï"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,143>>);
to_upper(<<"Î"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,142>>);
to_upper(<<"í"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,141>>);
to_upper(<<"ì"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,140>>);
to_upper(<<"ü"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,156>>);
to_upper(<<"û"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,155>>);
to_upper(<<"ú"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,154>>);
to_upper(<<"ù"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,153>>);
to_upper(<<"ö"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,150>>);
to_upper(<<"ô"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,148>>);
to_upper(<<"ó"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,147>>);
to_upper(<<"ò"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,146>>);
to_upper(<<"ø"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,152>>);
to_upper(<<"ç"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,135>>);
to_upper(<<"æ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,134>>);
to_upper(<<"œ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,197,146>>);
to_upper(<<"ã"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,131>>);
to_upper(<<"ñ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,145>>);
to_upper(<<"õ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,195,149>>);
% Cyrillic support
to_upper(<<"а"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,144>>);
to_upper(<<"б"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,145>>);
to_upper(<<"в"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,146>>);
to_upper(<<"г"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,147>>);
to_upper(<<"д"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,148>>);
to_upper(<<"е"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,149>>);
to_upper(<<"ё"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,129>>);
to_upper(<<"ж"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,150>>);
to_upper(<<"з"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,151>>);
to_upper(<<"и"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,152>>);
to_upper(<<"й"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,153>>);
to_upper(<<"к"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,154>>);
to_upper(<<"л"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,155>>);
to_upper(<<"м"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,156>>);
to_upper(<<"н"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,157>>);
to_upper(<<"о"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,158>>);
to_upper(<<"п"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,159>>);
to_upper(<<"р"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,160>>);
to_upper(<<"с"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,161>>);
to_upper(<<"т"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,162>>);
to_upper(<<"у"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,163>>);
to_upper(<<"ф"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,164>>);
to_upper(<<"х"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,165>>);
to_upper(<<"ц"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,166>>);
to_upper(<<"ч"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,167>>);
to_upper(<<"ш"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,168>>);
to_upper(<<"щ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,169>>);
to_upper(<<"ъ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,170>>);
to_upper(<<"ы"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,171>>);
to_upper(<<"ь"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,172>>);
to_upper(<<"э"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,173>>);
to_upper(<<"ю"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,174>>);
to_upper(<<"я"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,175>>);
% Extra Ukrainian characters
to_upper(<<"ґ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,210,144>>);
to_upper(<<"ї"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,135>>);
to_upper(<<"і"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,143>>);
to_upper(<<"є"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,208,132>>);
% Polish support
to_upper(<<"ą"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,196,132>>);
to_upper(<<"ę"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,196,152>>);
to_upper(<<"ć"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,196,134>>);
to_upper(<<"ł"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,197,129>>);
to_upper(<<"ń"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,197,131>>);
to_upper(<<"ś"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,197,154>>);
to_upper(<<"ź"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,197,185>>);
to_upper(<<"ż"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,197,187>>);
% Turkish support
to_upper(<<"ş"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,197,158>>);
to_upper(<<"ğ"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,196,158>>);
to_upper(<<"ı"/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,196,176>>);

% Other chars are taken as-is
to_upper(<<H/utf8,T/binary>>, Acc) -> to_upper(T, <<Acc/binary,H/utf8>>).


%% @doc Filter a filename so that we obtain a basename that is safe to use.
-spec to_rootname( file:filename_all() ) -> binary().
to_rootname(Filename) ->
    to_slug(filename:rootname(filename:basename(Filename))).


%% @doc Map a string to a slug that can be used in the uri of a page. Same as a name, but then with dashes instead of underscores.
-spec to_slug( string() | binary() | atom() ) -> binary().
to_slug(Title) ->
    binary:replace(to_name(Title), <<$_>>, <<$->>, [global]).


%% @doc Map a string to a value that can be used as a name or slug. Maps all characters to lowercase and remove non digalpha chars
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



%% @doc Transliterate an unicode string to an ascii string with lowercase characters.
%% Tries to transliterate some characters to a..z
-spec normalize(string() | binary() | atom() | {trans, list()}) -> binary().
normalize(B) when is_binary(B) ->
    normalize(B, <<>>);
normalize(undefined) ->
    <<>>;
normalize(A) when is_atom(A) ->
    normalize(z_convert:to_binary(A));
normalize(L) when is_list(L) ->
    normalize(unicode:characters_to_binary(L), <<>>);
normalize({trans, Tr}) ->
    case proplists:get_value(en, Tr) of
        undefined ->
            case Tr of
                [{_,V}|_] -> normalize(V);
                _ -> <<>>
            end;
        V ->
            normalize(V)
    end.

normalize(<<>>, Acc) ->
    Acc;
normalize(<<C, T/binary>>, Acc) when C >= $A andalso C =< $Z ->
    normalize(T, <<Acc/binary,(C + 32)>>);
normalize(<<C,T/binary>>, Acc) when (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_ ->
    normalize(T, <<Acc/binary,C>>);
normalize(<<"ä"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"ë"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ï"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ü"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"ö"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"Ä"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"Ë"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"Ï"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"Ü"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"Ö"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"é"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"è"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"É"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"È"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"í"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ì"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"Í"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"Ì"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ú"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"ù"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"Ú"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"Ù"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"ó"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"ò"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"Ó"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"Ò"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"ß"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$s>>);
normalize(<<"ç"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"Ç"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"ø"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"Ø"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"å"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"Å"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"€"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ÿ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"ã"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"ñ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"õ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"Ã"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"Ñ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"Õ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
% Cyrillic support (from http://en.wikipedia.org/wiki/Romanization_of_Russian)
normalize(<<"А"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"а"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"Б"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$b>>);
normalize(<<"б"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$b>>);
normalize(<<"В"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"в"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"Г"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"г"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"Д"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d>>);
normalize(<<"д"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d>>);
normalize(<<"Е"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"е"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"Ё"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o,$y>>);
normalize(<<"ё"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o,$y>>);
normalize(<<"Ж"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$z>>);
normalize(<<"ж"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$z>>);
normalize(<<"З"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"з"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"И"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"и"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"Й"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$j>>);
normalize(<<"й"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$j>>);
normalize(<<"К"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"к"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"Л"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"л"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"М"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$m>>);
normalize(<<"м"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$m>>);
normalize(<<"Н"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"н"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"О"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"о"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"П"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$p>>);
normalize(<<"п"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$p>>);
normalize(<<"Р"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$r>>);
normalize(<<"р"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$r>>);
normalize(<<"С"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"с"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"Т"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"т"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"У"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"у"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"Ф"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$f>>);
normalize(<<"ф"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$f>>);
normalize(<<"Х"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h>>);
normalize(<<"х"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h>>);
normalize(<<"Ц"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"ц"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"Ч"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$c>>);
normalize(<<"ч"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$c>>);
normalize(<<"Ш"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$s>>);
normalize(<<"ш"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$s>>);
normalize(<<"Щ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$h,$s>>);
normalize(<<"щ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$h,$s>>);
normalize(<<"Ъ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$_>>);
normalize(<<"ъ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$_>>);
normalize(<<"Ы"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"ы"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"Ь"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$_>>);
normalize(<<"ь"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$_>>);
normalize(<<"Э"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$e>>);
normalize(<<"э"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h,$e>>);
normalize(<<"Ю"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u,$y>>);
normalize(<<"ю"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u,$y>>);
normalize(<<"Я"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$y>>);
normalize(<<"я"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$y>>);
% Ukrainian support
normalize(<<"Ґ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"ґ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"Ї"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ї"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"І"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"і"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"Є"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e,$y>>);
normalize(<<"є"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e,$y>>);
% Polish support
normalize(<<"Ą"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"ą"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"Ę"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ę"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"Ć"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"ć"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"Ł"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"ł"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"Ń"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"ń"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"Ś"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ś"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"Ź"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"ź"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"Ż"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"ż"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
% Turkish support
normalize(<<"Ş"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ş"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"Ğ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"ğ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"İ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ı"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);

% Hebrew support (simplified) https://en.wikipedia.org/wiki/Romanization_of_Hebrew
% TODO: check this, as it seems quite broken/incomplete
normalize(<<"א"/utf8,T/binary>>, Acc) -> normalize(T, Acc);
normalize(<<"ב"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"בּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$b>>);
normalize(<<"ג"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"גּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"ג׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$j>>);
normalize(<<"ד"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d>>);
normalize(<<"דּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d>>);
normalize(<<"ד׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d,$h>>);
normalize(<<"ה"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h>>);
normalize(<<"הּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h>>);
normalize(<<"ו"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"וּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"ז"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"זּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"ז׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z,$h>>);
normalize(<<"ח"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c,$h>>);
normalize(<<"ט"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"טּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"י"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"יּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"ךכ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c,$h>>);
normalize(<<"ךּ כּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"ל"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"לּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"םמ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$m>>);
normalize(<<"מּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$m>>);
normalize(<<"ןנ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"נּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"ס"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"סּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ע"/utf8,T/binary>>, Acc) -> normalize(T, Acc);
normalize(<<"ףפ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$f>>);
normalize(<<"ףּ פּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$p>>);
normalize(<<"ץצ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$z>>);
normalize(<<"צּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$z>>);
normalize(<<"ץ׳צ׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$s,$h>>);
normalize(<<"ק"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"קּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"ר"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$r>>);
normalize(<<"רּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$r>>);
normalize(<<"ש"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h>>);
normalize(<<"שׁ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h>>);
normalize(<<"שּׁ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h>>);
normalize(<<"שׂ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"שּׂ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ת"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"תּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"ת׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$h>>);
% Hebrew forms used in translitearion from Arabic
normalize(<<"ח׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"ט׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"ע׳ר׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
% Hebrew vowels
normalize(<<"צ"/utf8, T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$a>>);
normalize(<<"טְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"חֱ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"חֲ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"חֳ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"טִ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"טֵ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"טֶ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"טַ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"טָ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"טֹ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"טֻ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"טוּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"טֵי"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e,$i>>);
normalize(<<"טֶי"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e,$i>>);
normalize(<<"טַיטַיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$i>>);
normalize(<<"טָיטָיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$i>>);
normalize(<<"טֹיטֹיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o,$i>>);
normalize(<<"טֻיטֻיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u,$i>>);
normalize(<<"טוּיטוּיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u,$i>>);

% Some entities - we might want to add generic code here, depends
% on where normalize/1 is used (can we assume that the input is always html?)
normalize(<<"&amp;", T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&lt;",  T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&gt;",  T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&#39;", T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&quot;",T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&nbsp;",T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&mdash;",T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<"&ndash;",T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<"—"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<"–"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<C/utf8,T/binary>>, Acc) when C >= 32, C =< 126 -> normalize(T, <<Acc/binary, C/utf8>>);
normalize(<<C/utf8,T/binary>>, Acc) when C =:= 8023 ->
    % Zero width space
    normalize(T, Acc);
normalize(<<C/utf8,T/binary>>, Acc) ->
    % Keep rest as-is
    normalize(T, <<Acc/binary, C/utf8>>);
normalize(<<_C,T/binary>>, Acc) ->
    % Drop non-utf8
    normalize(T, Acc).



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
