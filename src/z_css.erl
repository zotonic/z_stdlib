%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%%
%% @doc Utility functions for CSS processing. Also used for sanitizing HTML.

%% Copyright 2014 Marc Worrell
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

-module(z_css).

-export([
    scan/1,
    parse/1,
    sanitize/1,
    sanitize_style/1
    ]).

scan(Bs) when is_binary(Bs) ->
    scan(unicode:characters_to_list(Bs));
scan(L) when is_list(L) ->
    {ok, Ts, _} = z_css_lexer:string(L),
    {ok, Ts}.

parse(B) when is_binary(B) ->
    {ok, Ts} = scan(B),
    parse(Ts);
parse(Ts) when is_list(Ts) -> 
    z_css_parser:parse(Ts).


sanitize(Css) when is_binary(Css) ->
    {ok, Ts} = scan(Css),
    case z_css_parser:parse(Ts) of
        {error, {Line, z_css_parser, Error}} ->
            {error, {Line, unicode:characters_to_binary(Error)}};
        {ok, {stylesheet, Charset, Import, Rules}} ->
            Charset1 = sanitize_charset(Charset),
            Import1 = sanitize_import(Import),
            Rules1 = [ sanitize_rule(R) || R <- Rules ],
            {ok, unicode:characters_to_binary([
                serialize_charset(Charset1),
                serialize_import(Import1),
                [ serialize_rule(R) || R <- Rules1 ]
            ])}
    end.

sanitize_style(Css) when is_list(Css) ->
    sanitize_style(unicode:characters_to_binary(Css));
sanitize_style(Css) when is_binary(Css) ->
    {ok, Ts} = scan(<<"a { ",Css/binary," }">>),
    case z_css_parser:parse(Ts) of
        {ok, {stylesheet, no_charset, no_import, [{rule, _Sel, Declarations}]}} ->
            SanitizedDs = [ sanitize_declaration(D) || D <- Declarations ],
            Ts1 = unicode:characters_to_binary([ serialize_declaration(D) || D <- SanitizedDs ]),
            {ok, binary:replace(Ts1, <<"\n">>, <<" ">>, [global])};
        {error, {Line, z_css_parser, Error}} ->
            {error, {Line, unicode:characters_to_binary(Error)}}
    end.


%%% -------------------------------------------------------- 
%%% Sanitize a CSS parse tree
%%% -------------------------------------------------------- 

sanitize_charset(_Charset) -> no_charset.

sanitize_import(_Import) -> no_import.

sanitize_rule({rule, Selector, Declarations}) ->
    {rule, Selector, [ sanitize_declaration(D) || D <- Declarations ]};
sanitize_rule({media, MediaList, Rules}) ->
    {media, MediaList, [ sanitize_rule(R) || R <- Rules ]};
sanitize_rule({page, PseudoPage, Declarations}) ->
    {page, PseudoPage, [ sanitize_declaration(D) || D <- Declarations ]}.

sanitize_declaration({declaration, Ident, Expr, Prio}) ->
    {declaration, Ident, sanitize_expr(Expr), Prio}.

sanitize_expr({ident, Line, Ident}) ->
    % Don't allow anything to escape its bounding box.
    case string:to_lower(Ident) of
        "fixed" -> {ident, Line, "absolute"};
        _ -> {ident, Line, Ident}
    end;
sanitize_expr({uri, Line, _Uri}) -> 
    % No external url references
    {uri, Line, "url()"};
sanitize_expr({function, _Func, Expr}) -> 
    % No unchecked functions
    sanitize_expr(Expr);
sanitize_expr({number, _, _} = E) -> E;
sanitize_expr({length, _, _} = E) -> E;
sanitize_expr({ems, _, _} = E) -> E;
sanitize_expr({exs, _, _} = E) -> E;
sanitize_expr({angle, _, _} = E) -> E;
sanitize_expr({time, _, _} = E) -> E;
sanitize_expr({freq, _, _} = E) -> E;
sanitize_expr({dimension, _, _} = E) -> E;
sanitize_expr({percentage, _, _} = E) -> E;
sanitize_expr({string, Line, S}) -> {string, Line, sanitize_string(S)};
sanitize_expr({hash, _, _} = E) -> E;
sanitize_expr({operator, Op, E1, E2}) -> {operator, Op, sanitize_expr(E1), sanitize_expr(E2)};
sanitize_expr({operator, Op, E1}) -> {operator, Op, sanitize_expr(E1)}.

sanitize_string([Quot|S]) when Quot =:= $"; Quot =:= $' ->
    S1 = lists:sublist(S, length(S)-1),
    [ $", z_html:escape_check(z_html:strip(unicode:characters_to_binary(S1))), $"].


%%% -------------------------------------------------------- 
%%% Serialize the sanitized parse tree
%%% -------------------------------------------------------- 

serialize_charset(no_charset) -> <<>>;
serialize_charset({charset, {string,_,S}}) -> [ <<"@charset ">>, S, $; ].

serialize_import(no_import) -> <<>>;
serialize_import({charset, Location, MediaList}) -> 
    [ <<"@charset ">>,
      serialize_location(Location),
      serialize_medialist(MediaList)
    ].

serialize_location({string, _, S}) -> S;
serialize_location({url, _, Url}) -> Url.

serialize_medialist([]) -> 
    [];
serialize_medialist([M|Rest]) ->
    [ 
        serialize_media(M),
        case Rest of
            [] -> [];
            Ms -> [ $, , serialize_medialist(Ms) ]
        end
    ].

serialize_media({ident, _, Ident}) -> Ident.

serialize_rule({rule, SelectorList, Declarations}) ->
    [
        serialize_selectorlist(SelectorList),
        ${, $\n, [ serialize_declaration(D) || D <- Declarations ], $}, $\n
    ];
serialize_rule({media, MediaList, Rules}) ->
    [ 
        <<"@media ">>, 
        serialize_medialist(MediaList),
        32, ${, $\n,
        [ serialize_rule(R) || R <- Rules ],
        $}, $\n
    ];
serialize_rule({page, PseudoPage, Declarations}) ->
    [ 
        <<"@page ">>, 
        serialize_pseudo_page(PseudoPage),
        32, ${, $\n,
        [ serialize_declaration(R) || R <- Declarations ],
        $}, $\n
    ].

serialize_pseudo_page(undefined) -> <<>>;
serialize_pseudo_page({ident, _, V}) -> [ $:, V ].

serialize_selectorlist([S|Rest]) ->
    [ 
        serialize_selector(S),
        case Rest of
            [] -> [];
            Ms -> [ $,, $\n, serialize_selectorlist(Ms) ]
        end
    ].

serialize_selector(Sels) ->
    [ serialize_selector_1(S) || S <- Sels ].

serialize_selector_1({none, S}) -> [ serialize_simpleselector(S), 32 ];
serialize_selector_1({'+', S})  -> [ "+ ", serialize_simpleselector(S), 32 ];
serialize_selector_1({'>', S})  -> [ "> ", serialize_simpleselector(S), 32 ].

serialize_simpleselector('*') -> $*;
serialize_simpleselector({ident, _Line, V}) -> V;
serialize_simpleselector({hash, _Line, V}) -> V;
serialize_simpleselector({class, {ident, _Line, V}}) -> [ $., V ];
serialize_simpleselector({attrib, {ident, _Line, V}, AttrOpVal}) -> [ $[, V, serialize_attr_opval(AttrOpVal), $] ];
serialize_simpleselector({pseudo, {ident, _Line, V}}) -> [ $:, V ];
serialize_simpleselector({pseudo, {function, {function, _Line, F}, {ident, _Line, V}}}) -> [ $:, F, V, $) ].

serialize_attr_opval(undefined) -> <<>>;
serialize_attr_opval({'=', AttrVal}) -> [ $=, serialize_attr_val(AttrVal) ];
serialize_attr_opval({includes, AttrVal}) -> [ $~, $=, serialize_attr_val(AttrVal) ];
serialize_attr_opval({dashmatch, AttrVal}) -> [ $|, $=, serialize_attr_val(AttrVal) ].

serialize_attr_val({ident, _, V}) -> V;
serialize_attr_val({string, _, V}) -> V.

serialize_declaration({declaration, {ident, _, Idn}, Expr, Prio}) ->
    [
        Idn, $:,
        serialize_expr(Expr),
        case Prio of
            important -> <<" !important">>;
            normal -> <<>>
        end,
        $;, $\n
    ].

serialize_expr({ident, _Line, Ident}) ->
    Ident;
serialize_expr({uri, _Line, Uri}) -> 
    Uri;
serialize_expr({function, {function,_Line,Fun}, Expr}) -> 
    [ Fun, serialize_expr(Expr), $) ];
serialize_expr({number, _, V}) -> V;
serialize_expr({length, _, V}) -> V;
serialize_expr({ems, _, V}) -> V;
serialize_expr({exs, _, V}) -> V;
serialize_expr({angle, _, V}) -> V;
serialize_expr({time, _, V}) -> V;
serialize_expr({freq, _, V}) -> V;
serialize_expr({dimension, _, V}) -> V;
serialize_expr({percentage, _, V}) -> V;
serialize_expr({string, _, V}) -> V;
serialize_expr({hash, _, V}) -> V;
serialize_expr({operator, Op, E1, E2}) ->
    [ serialize_expr(E1), z_convert:to_list(Op), serialize_expr(E2) ];
serialize_expr({operator, Op, E1}) ->
    [ z_convert:to_list(Op), serialize_expr(E1) ].

