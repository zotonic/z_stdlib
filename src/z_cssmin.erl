%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2018-2025 Maas-Maarten Zeeman
%% @doc Javascript minifier. Based on cssmin.c. Minimizes a Javascript binary
%% by removing comments and unnecessary whitespace.
%% @end

%% Copyright 2018-2025 Maas-Maarten Zeeman
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

%% cssmin is Copyright (c) 2010  (www.ryanday.org)
%% see https://github.com/soldair/cssmin

-module(z_cssmin).

-export([
    minify/1
    ]).

%% @doc Minify a binary containing CSS.
-spec minify( binary() ) -> binary().
minify( CSS ) ->
    machine(CSS, <<>>).

machine(<<>>, Acc) -> Acc;
machine(<<$/, $*, Rest/binary>>, Acc) -> machine(skip_comment(Rest), Acc);
machine(<<C, Rest/binary>>, Acc) when C =:= 32 orelse C =:= 10 -> machine(Rest, Acc);
machine(<<$@, Rest/binary>>, Acc) -> atrule(Rest, <<Acc/binary, $@>>);
machine(Bin, Acc) -> selector(Bin, Acc).

selector(<<>>, Acc) -> Acc;
selector(<<$/, $*, Rest/binary>>, Acc) -> selector(skip_comment(Rest), Acc);
selector(<<${, Rest/binary>>, Acc) -> block(Rest, <<Acc/binary, ${>>);
selector(<<$@, Rest/binary>>, Acc) -> atrule(Rest, <<Acc/binary, $@>>);
selector(<<10, Rest/binary>>, Acc) -> selector(Rest, Acc);
selector(<<32, Rest/binary>>, Acc) -> continue_selector(skip_space(Rest), Acc); 
selector(<<C, Rest/binary>>, Acc) -> selector(Rest, <<Acc/binary, C>>).

continue_selector(<<${, _/binary>> = Rest, Acc) -> selector(Rest, Acc);
continue_selector(Rest, Acc) -> 
    case binary:last(Acc) of
        32 -> selector(Rest, Acc);
        _ -> selector(Rest, <<Acc/binary, 32>>)
    end.

atrule(<<>>, Acc) -> Acc;
atrule(<<$/, $*, Rest/binary>>, Acc) -> atrule(skip_comment(Rest), Acc);
atrule(<<10, Rest/binary>>, Acc) ->
    case Rest of
        <<10, _/binary>> ->
            machine(Rest, <<Acc/binary, $;>>);
        _ ->
            atrule(Rest, <<Acc/binary, 32>>)
    end;
atrule(<<$;, Rest/binary>>, Acc) -> machine(Rest, <<Acc/binary, $;>>);
atrule(<<${, Rest/binary>>, Acc) -> block(Rest, <<Acc/binary, ${>>);
atrule(<<C, Rest/binary>>, Acc) -> atrule(Rest, <<Acc/binary, C>>).
 
block(<<>>, Acc) -> Acc;
block(<<$/, $*, Rest/binary>>, Acc) -> block(skip_comment(Rest), Acc);
block(<<$}, Rest/binary>>, Acc) -> machine(Rest, <<Acc/binary, $}>>);
block(Bin, Acc) -> declaration(skip_whitespace(Bin), Acc).

declaration(<<>>, Acc) -> Acc;

declaration(<<$/, $*, Rest/binary>>, Acc) -> declaration(skip_comment(Rest), Acc);
declaration(<<$(, Rest/binary>>, Acc) -> declaration_in_paren(Rest, <<Acc/binary, $(>>);
declaration(<<$;, Rest/binary>>, Acc) ->
    case skip_whitespace(Rest) of
        <<$}, _/binary>> = Rest1 -> block(Rest1, Acc);
        <<$;, _/binary>> = Rest1 -> block(Rest1, Acc);
        _ -> block(Rest, <<Acc/binary, $;>>)
    end;
declaration(<<$}, Rest/binary>>, Acc) -> machine(Rest, <<Acc/binary, $}>>);
declaration(<<32, Rest/binary>>, Acc) ->
    case binary:last(Acc) of
        32 -> declaration(Rest, Acc);
        _ -> declaration(Rest, <<Acc/binary, 32>>)
    end;
declaration(<<C, Rest/binary>>, Acc) ->
    declaration(Rest, <<Acc/binary, C>>).
 
declaration_in_paren(<<>>, Acc) -> Acc;
declaration_in_paren(<<$), Rest/binary>>, Acc) -> declaration(Rest, <<Acc/binary, $)>>);
declaration_in_paren(<<C, Rest/binary>>, Acc) -> declaration_in_paren(Rest, <<Acc/binary, C>>).

skip_comment(<<>>) -> <<>>;
skip_comment(<<$*, $/, Rest/binary>>) -> Rest;
skip_comment(<<_C, Rest/binary>>) -> skip_comment(Rest).

skip_space(<<32, Rest/binary>>) -> skip_space(Rest);
skip_space(Bin) -> Bin.

skip_whitespace(<<C, Rest/binary>>) when C =:= 32 orelse C =:= 10 -> skip_whitespace(Rest);
skip_whitespace(Bin) -> Bin.

    



  
