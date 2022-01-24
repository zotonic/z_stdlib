%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.; copyright 2018-2021 Maas-Maarten Zeeman

%% @doc Loosely tokenizes and generates parse trees for (X)HTML and XML.
%% Adapted by Maas-Maarten Zeeman
%% Extended for basic XML parsing by Marc Worrell

-module(z_html_parse).
-export([
    tokens/1, tokens/2, parse/1, parse/2,
    parse_to_map/1, parse_to_map/2,
    parse_tokens/1, to_tokens/1, to_tokens/2,
    escape/1, escape_attr/1,
    to_html/1, to_html/2]).

%% Exports for tests in test/z_html_parse_test.tpl
-ifdef(TEST).
-export([
    is_singleton/2,
    destack/1,
    destack/3
    ]).
-endif.

-type html_node()    :: {html_tag(), [html_attr()], [ html_element() ]}.
-type html_attr()    :: {html_attr_name(), html_attr_value()}.
-type html_tag() :: binary()
                  | string()
                  | atom().
-type html_attr_name() :: binary()
                        | string()
                        | atom().
-type html_attr_value() :: binary()
                         | string()
                         | atom()
                         | number().
-type html_element() :: html_node()
                      | html_comment()
                      | html_nop()
                      | pi_tag()
                      | inline_html()
                      | {html_tag()}
                      | {html_tag(), [ html_element() ]}
                      | binary().
-type html_comment() :: {comment, Comment::binary()}.
-type html_nop()     :: {nop, [ html_element() ]}.      % Special node used by sanitizer for unwanted elements
-type pi_tag()       :: {pi, binary()}
                      | {pi, Tag::binary(), [html_attr()]}.

-type html_data()    :: {data, binary(), Whitespace::boolean()}.
-type start_tag()    :: {start_tag, Name::binary(), [ html_attr() ], Singleton::boolean()}.
-type end_tag()      :: {end_tag, Name::binary()}.
-type html_doctype() :: {doctype, [ Doctype::any() ]}.
-type inline_html()  :: {'=', binary()}.

-type html_token() :: html_data()
                    | start_tag()
                    | end_tag()
                    | pi_tag()
                    | inline_html()
                    | html_comment()
                    | html_doctype().

-type html_tree() :: html_doctype()
                   | html_node()
                   | html_comment()
                   | inline_html()
                   | {html_tag()}
                   | {html_tag(), [ html_element() ]}
                   | pi_tag().

-type options() :: #{
        mode => xml | html,
        escape => boolean(),
        lowercase => boolean()
    }.

-export_type([
    html_tree/0,
    html_node/0,
    html_element/0,
    html_attr/0,
    html_data/0,
    html_comment/0,
    html_doctype/0,
    start_tag/0,
    end_tag/0,
    html_token/0,
    html_tag/0,
    html_attr_name/0,
    html_attr_value/0
    ]).

%% This is a macro to placate syntax highlighters..
-define(QUOTE, $\").
-define(SQUOTE, $\').
-define(ADV_COL(S, N),
        S#decoder{column=N+S#decoder.column,
                  offset=N+S#decoder.offset}).
-define(INC_COL(S),
        S#decoder{column=1+S#decoder.column,
                  offset=1+S#decoder.offset}).
% -define(INC_LINE(S),
%         S#decoder{column=1,
%                   line=1+S#decoder.line,
%                   offset=1+S#decoder.offset}).
-define(INC_CHAR(S, C),
        case C of
            $\n ->
                S#decoder{column=1,
                          line=1+S#decoder.line,
                          offset=1+S#decoder.offset};
            _ ->
                S#decoder{column=1+S#decoder.column,
                          offset=1+S#decoder.offset}
        end).

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).
-define(IS_LITERAL_SAFE(C),
        ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z)
         orelse (C >= $0 andalso C =< $9))).
-define(IS_START_LITERAL_SAFE(C),
        ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) 
         orelse (C == $_))).
-define(PROBABLE_CLOSE(C),
        (C =:= $> orelse ?IS_WHITESPACE(C))).

-record(decoder, {line = 1,
                  column = 1,
                  offset = 0}).


%% External API.

%% @doc tokenize and then transform the token stream into a HTML tree.
-spec parse( iodata() ) -> {ok, html_node()} | {error, nohtml}.
parse(Input) ->
    parse(Input, #{ mode => html, escape => true }).

-spec parse( iodata(), options() ) -> {ok, html_node()} | {error, nohtml}.
parse(Input, Options) ->
    Options1 = opts(Options),
    parse_tokens(tokens(Input, Options1), Options1).

%% @doc Parse an HTML/XML document to a JSON compatible map. Attributes will be added
%% as keys in an <tt>@attributes</tt> key. Elements will be mapped to keys with value lists.
%% all keys are lowercased.
-spec parse_to_map( Input :: iodata() | {binary, list(), list()} ) -> {ok, map()} | {error, term()}.
parse_to_map(Input) ->
    parse_to_map(Input, #{ mode => html, escape => true }).

%% @doc Parse an HTML/XML document to a JSON compatible map. Attributes will be added
%% as keys in an <tt>@attributes</tt> key. Elements will be mapped to keys with value lists.
%% all keys are lowercased.
-spec parse_to_map( Input :: iodata() | {binary, list(), list()}, options() ) -> {ok, map()} | {error, term()}.
parse_to_map({_, _, _} = Tree, _Options) ->
    {ok, tree_to_map(Tree, #{})};
parse_to_map(Input, Options) ->
    Options1 = opts(Options),
    case parse(Input, Options1) of
        {ok, Tree} ->
            {ok, tree_to_map(Tree, #{})};
        {error, _} = Error ->
            Error
    end.

opts(Options) ->
    Options#{
        mode => maps:get(mode, Options, html),
        escape => maps:get(escape, Options, true)
    }.

%% @doc Transform the output of tokens(Doc) into a HTML tree.
-spec parse_tokens( [ html_token() ] ) -> {ok, html_node()} | {error, nohtml}.
parse_tokens(Tokens) ->
    parse_tokens(Tokens, #{ mode => html, escape => true }).

-spec parse_tokens( [ html_token() ], options() ) -> {ok, html_node()} | {error, nohtml}.
parse_tokens(Tokens, #{ mode := Mode } = Options) when is_list(Tokens) ->
    %% Skip over doctype, processing instructions
    F = fun (X) ->
                case X of
                    {start_tag, _, _, false} ->
                        false;
                    {start_tag, _, _, true} when Mode =:= xml ->
                        false;
                    _ ->
                        true
                end
        end,
    case lists:dropwhile(F, Tokens) of
        [{start_tag, Tag, Attrs, false} | Rest] ->
            {Tree, _} = tree(Rest, [ norm({Tag, Attrs}, Options) ], Options),
            {ok, Tree};
        [{start_tag, Tag, Attrs, true} ] when Mode =:= xml ->
            {Tree, _} = tree([], [ norm({Tag, Attrs}, Options) ], Options),
            {ok, Tree};
        [] ->
            {error, nohtml}
    end.

%% @doc Transform the input UTF-8 HTML into a token stream.
-spec tokens( iodata() ) -> [ html_token() ].
tokens(Input) ->
    tokens(Input, #{ mode => html, escape => true }).

-spec tokens( iodata(), options() ) -> [ html_token() ].
tokens(Input, Options) ->
    tokens(iolist_to_binary(Input), #decoder{}, [], Options).

%% @doc Convert a html_node() tree to a list of tokens.
-spec to_tokens( html_tree() ) -> [ html_token() ].
to_tokens(HtmlNode) ->
    to_tokens(HtmlNode, #{ mode => html, escape => true }).

-spec to_tokens( html_tree(), options() ) -> [ html_token() ].
to_tokens({Tag0}, Options) ->
    to_tokens({Tag0, [], []}, Options);
to_tokens(T={'=', _}, _Options) ->
    [T];
to_tokens(T={doctype, _}, _Options) ->
    [T];
to_tokens(T={comment, _}, _Options) ->
    [T];
to_tokens({Tag0, Acc}, Options) ->
    %% This is only allowed in sub-tags: {p, [{"class", "foo"}]}
    to_tokens({Tag0, [], Acc}, Options);
to_tokens({Tag0, Attrs, []}, #{ mode := xml } = Options) ->
    Tag = to_tag(Tag0, Options),
    to_tokens_1([], [{start_tag, Tag, Attrs, true}], Options);
to_tokens({Tag0, Attrs, Acc}, Options) ->
    Tag = to_tag(Tag0, Options),
    case is_singleton(Tag, Options) of
        true ->
            to_tokens_1([], [{start_tag, Tag, Attrs, true}], Options);
        false ->
            to_tokens_1([{Tag, Acc}], [{start_tag, Tag, Attrs, false}], Options)
    end.

%% @doc Convert a list of html_token() to a HTML document.
-spec to_html([ html_token() ] | html_tree() ) -> iodata().
to_html(Node) ->
    to_html(Node, #{ mode => html, escape => true }).

-spec to_html([ html_token() ] | html_tree(), options() ) -> iodata().
to_html(Node, Options) when is_tuple(Node) ->
    Options1 = opts(Options),
    to_html(to_tokens(Node, Options1), Options1);
to_html(Tokens, Options) when is_list(Tokens) ->
    Options1 = opts(Options),
    to_html_1(Tokens, [], Options1).

%% @spec escape(string() | atom() | binary()) -> binary()
%% @doc Escape a string such that it's safe for HTML (amp; lt; gt;).
escape(B) when is_binary(B) ->
    escape(binary_to_list(B), []);
escape(A) when is_atom(A) ->
    escape(atom_to_list(A), []);
escape(S) when is_list(S) ->
    escape(S, []).

%% @spec escape_attr(string() | binary() | atom() | integer() | float()) -> binary()
%% @doc Escape a string such that it's safe for HTML attrs
%%      (amp; lt; gt; quot;).
escape_attr(B) when is_binary(B) ->
    escape_attr(binary_to_list(B), []);
escape_attr(A) when is_atom(A) ->
    escape_attr(atom_to_list(A), []);
escape_attr(S) when is_list(S) ->
    escape_attr(S, []);
escape_attr(I) when is_integer(I) ->
    escape_attr(integer_to_list(I), []);
escape_attr(F) when is_float(F) ->
    escape_attr(z_mochinum:digits(F), []).

to_html_1([], Acc, _Options) ->
    lists:reverse(Acc);
to_html_1([{'=', Content} | Rest], Acc, Options) ->
    to_html_1(Rest, [Content | Acc], Options);
to_html_1([{pi, Bin} | Rest], Acc, Options) ->
    Open = [<<"<?">>,
            Bin,
            <<"?>">>],
    to_html_1(Rest, [Open | Acc], Options);
to_html_1([{pi, Tag, Attrs} | Rest], Acc, Options) ->
    Open = [<<"<?">>,
            Tag,
            attrs_to_html(Attrs, []),
            <<"?>">>],
    to_html_1(Rest, [Open | Acc], Options);
to_html_1([{comment, Comment} | Rest], Acc, Options) ->
    to_html_1(Rest, [[<<"<!--">>, Comment, <<"-->">>] | Acc], Options);
to_html_1([{doctype, Parts} | Rest], Acc, Options) ->
    Inside = doctype_to_html(Parts, Acc),
    to_html_1(Rest, [[<<"<!DOCTYPE">>, Inside, <<">">>] | Acc], Options);
to_html_1([{data, Data, _Whitespace} | Rest], Acc, #{ escape := true } = Options) ->
    to_html_1(Rest, [escape(Data) | Acc], Options);
to_html_1([{data, Data, _Whitespace} | Rest], Acc, #{ escape := false } = Options) ->
    to_html_1(Rest, [Data | Acc], Options);
to_html_1([{start_tag, Tag, Attrs, Singleton} | Rest], Acc, #{ mode := html } = Options) ->
    EscapeData = case Tag of
                     <<"script">> -> false;
                     _ -> true
                 end,
    Open = [<<"<">>,
            Tag,
            attrs_to_html(Attrs, []),
            case Singleton of
                true -> <<" />">>;
                false -> <<">">>
            end],
    to_html_1(Rest, [Open | Acc], Options#{ escape := EscapeData });
to_html_1([{start_tag, Tag, Attrs, Singleton} | Rest], Acc, #{ mode := xml } = Options) ->
    Open = [<<"<">>,
            Tag,
            attrs_to_html(Attrs, []),
            case Singleton of
                true -> <<" />">>;
                false -> <<">">>
            end],
    to_html_1(Rest, [Open | Acc], Options#{ escape := true });
to_html_1([{end_tag, Tag} | Rest], Acc, Options) ->
    to_html_1(Rest, [[<<"</">>, Tag, <<">">>] | Acc], Options#{ escape => false }).

doctype_to_html([], Acc) ->
    lists:reverse(Acc);
doctype_to_html([Word | Rest], Acc) ->
    case lists:all(fun (C) -> ?IS_LITERAL_SAFE(C) end,
                   binary_to_list(iolist_to_binary(Word))) of
        true ->
            doctype_to_html(Rest, [[<<" ">>, Word] | Acc]);
        false ->
            doctype_to_html(Rest, [[<<" \"">>, escape_attr(Word), ?QUOTE] | Acc])
    end.

attrs_to_html([], Acc) ->
    lists:reverse(Acc);
attrs_to_html([{K, V} | Rest], Acc) ->
    attrs_to_html(Rest,
                  [[<<" ">>, escape(K), <<"=\"">>,
                    escape_attr(V), <<"\"">>] | Acc]).

escape([], Acc) ->
    list_to_binary(lists:reverse(Acc));
escape("<" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape(">" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape("&" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape([16#c2, 16#a0] ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&nbsp;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).

escape_attr([], Acc) ->
    list_to_binary(lists:reverse(Acc));
escape_attr("<" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&lt;", Acc));
escape_attr(">" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&gt;", Acc));
escape_attr("&" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&amp;", Acc));
escape_attr([?QUOTE | Rest], Acc) ->
    escape_attr(Rest, lists:reverse("&quot;", Acc));
escape_attr([16#c2, 16#a0] ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&nbsp;", Acc));
escape_attr([C | Rest], Acc) ->
    escape_attr(Rest, [C | Acc]).

to_tag(A, Options) when is_atom(A) ->
    norm(atom_to_binary(A, utf8), Options);
to_tag(L, Options) ->
    norm(L, Options).

to_tokens_1([], Acc, _Options) ->
    lists:reverse(Acc);
to_tokens_1([{Tag, []} | Rest], Acc, Options) ->
    to_tokens_1(Rest, [{end_tag, to_tag(Tag, Options)} | Acc], Options);
to_tokens_1([{Tag0, [{T0} | R1]} | Rest], Acc, Options) ->
    %% Allow {br}
    to_tokens_1([{Tag0, [{T0, [], []} | R1]} | Rest], Acc, Options);
to_tokens_1([{Tag0, [T0={'=', _C0} | R1]} | Rest], Acc, Options) ->
    %% Allow {'=', iolist()}
    to_tokens_1([{Tag0, R1} | Rest], [T0 | Acc], Options);
to_tokens_1([{Tag0, [T0={comment, _C0} | R1]} | Rest], Acc, Options) ->
    %% Allow {comment, iolist()}
    to_tokens_1([{Tag0, R1} | Rest], [T0 | Acc], Options);
to_tokens_1([{Tag0, [T0={pi, _S0} | R1]} | Rest], Acc, Options) ->
    %% Allow {pi, binary()}
    to_tokens_1([{Tag0, R1} | Rest], [T0 | Acc], Options);
to_tokens_1([{Tag0, [T0={pi, _S0, _A0} | R1]} | Rest], Acc, Options) ->
    %% Allow {pi, binary(), list()}
    to_tokens_1([{Tag0, R1} | Rest], [T0 | Acc], Options);
to_tokens_1([{Tag0, [{T0, A0=[{_, _} | _]} | R1]} | Rest], Acc, Options) ->
    %% Allow {p, [{"class", "foo"}]}
    to_tokens_1([{Tag0, [{T0, A0, []} | R1]} | Rest], Acc, Options);
to_tokens_1([{Tag0, [{T0, C0} | R1]} | Rest], Acc, Options) ->
    %% Allow {p, "content"} and {p, <<"content">>}
    to_tokens_1([{Tag0, [{T0, [], C0} | R1]} | Rest], Acc, Options);
to_tokens_1([{Tag0, [{T0, A1, C0} | R1]} | Rest], Acc, Options) when is_binary(C0) ->
    %% Allow {"p", [{"class", "foo"}], <<"content">>}
    to_tokens_1([{Tag0, [{T0, A1, binary_to_list(C0)} | R1]} | Rest], Acc, Options);
to_tokens_1([{Tag0, [{T0, A1, C0=[C | _]} | R1]} | Rest], Acc, Options)
  when is_integer(C) ->
    %% Allow {"p", [{"class", "foo"}], "content"}
    to_tokens_1([{Tag0, [{T0, A1, [C0]} | R1]} | Rest], Acc, Options);
to_tokens_1([{Tag0, [{T0, A1, []} | R1]} | Rest], Acc, #{ mode := xml } = Options) ->
    Tag = to_tag(Tag0, Options),
    T1 = to_tag(T0, Options),
    to_tokens_1([{Tag, R1} | Rest],
                [{start_tag, T1, A1, true} | Acc],
                Options);
to_tokens_1([{Tag0, [{T0, A1, C1} | R1]} | Rest], Acc, Options) ->
    %% Native {"p", [{"class", "foo"}], ["content"]}
    Tag = to_tag(Tag0, Options),
    T1 = to_tag(T0, Options),
    case is_singleton(norm(T1, Options), Options) of
        true ->
            to_tokens_1([{Tag, R1} | Rest],
                        [{start_tag, T1, A1, true} | Acc],
                        Options);
        false ->
            to_tokens_1([{T1, C1}, {Tag, R1} | Rest],
                        [{start_tag, T1, A1, false} | Acc],
                        Options)
    end;
to_tokens_1([{Tag0, [L | R1]} | Rest], Acc, Options) when is_list(L) ->
    %% List text
    Tag = to_tag(Tag0, Options),
    to_tokens_1([{Tag, R1} | Rest], [{data, iolist_to_binary(L), false} | Acc], Options);
to_tokens_1([{Tag0, [B | R1]} | Rest], Acc, Options) when is_binary(B) ->
    %% Binary text
    Tag = to_tag(Tag0, Options),
    to_tokens_1([{Tag, R1} | Rest], [{data, B, false} | Acc], Options).

tokens(B, S=#decoder{offset=O}, Acc, #{ mode := Mode } = Options) ->
    case B of
        <<_:O/binary>> ->
            lists:reverse(Acc);
        _ when Mode =:= xml ->
            {Tag, S1} = tokenize(B, S, Options),
            tokens(B, S1, [Tag | Acc], Options);
        _ when Mode =:= html ->
            {Tag, S1} = tokenize(B, S, Options),
            case parse_flag(Tag) of
                script ->
                    {Tag2, S2} = tokenize_script(B, S1),
                    tokens(B, S2, [Tag2, Tag | Acc], Options);
                textarea ->
                    {Tag2, S2} = tokenize_textarea(B, S1),
                    tokens(B, S2, [Tag2, Tag | Acc], Options);
                none ->
                    tokens(B, S1, [Tag | Acc], Options)
            end
    end.

parse_flag({start_tag, B, _, false}) ->
    case z_string:to_lower(B) of
        <<"script">> ->
            script;
        <<"textarea">> ->
            textarea;
        _ ->
            none
    end;
parse_flag(_) ->
    none.

tokenize(B, S=#decoder{offset=O}, Options) ->
    case B of
        <<_:O/binary, "<!--", _/binary>> ->
            tokenize_comment(B, ?ADV_COL(S, 4));
        <<_:O/binary, "<!DOCTYPE", _/binary>> ->
            tokenize_doctype(B, ?ADV_COL(S, 10), Options);
        <<_:O/binary, "<!doctype", _/binary>> ->
            tokenize_doctype(B, ?ADV_COL(S, 10), Options);
        <<_:O/binary, "<![CDATA[", _/binary>> ->
            tokenize_cdata(B, ?ADV_COL(S, 9));
        <<_:O/binary, "<?php", _/binary>> ->
            {Body, S1} = raw_qgt(B, ?ADV_COL(S, 2)),
            {{pi, Body}, S1};
        <<_:O/binary, "<?", _/binary>> ->
            {Tag, S1} = tokenize_literal(B, ?ADV_COL(S, 2), tag, Options),
            {Attrs, S2} = tokenize_attributes(B, S1, Options),
            S3 = find_qgt(B, S2),
            {{pi, Tag, Attrs}, S3};
        <<_:O/binary, "&", _/binary>> ->
            tokenize_charref(B, ?INC_COL(S));
        <<_:O/binary, "</", _/binary>> ->
            {Tag, S1} = tokenize_literal(B, ?ADV_COL(S, 2), tag, Options),
            {S2, _} = find_gt(B, S1),
            {{end_tag, Tag}, S2};
        <<_:O/binary, "<", C, _/binary>>
                when ?IS_WHITESPACE(C); not ?IS_START_LITERAL_SAFE(C) ->
            %% This isn't really strict HTML
            {{data, Data, _Whitespace}, S1} = tokenize_data(B, ?INC_COL(S)),
            {{data, <<$<, Data/binary>>, false}, S1};
        <<_:O/binary, "<", _/binary>> ->
            {Tag, S1} = tokenize_literal(B, ?INC_COL(S), tag, Options),
            {Attrs, S2} = tokenize_attributes(B, S1, Options),
            {S3, HasSlash} = find_gt(B, S2),
            Singleton = HasSlash orelse is_singleton(Tag, Options),
            {{start_tag, Tag, Attrs, Singleton}, S3};
        _ ->
            tokenize_data(B, S, false)
    end.

tree_data([{data, Data, Whitespace} | Rest], AllWhitespace, Acc) ->
    tree_data(Rest, (Whitespace andalso AllWhitespace), [Data | Acc]);
tree_data(Rest, AllWhitespace, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), AllWhitespace, Rest}.

tree([], Stack, _Options) ->
    {destack(Stack), []};
tree([{end_tag, Tag} | Rest], Stack, Options) ->
    case destack(norm(Tag, Options), Stack, Options) of
        S when is_list(S) ->
            tree(Rest, S, Options);
        Result ->
            {Result, []}
    end;
tree([{start_tag, Tag, Attrs, true} | Rest], S, Options) ->
    tree(Rest, append_stack_child(norm({Tag, Attrs}, Options), S), Options);
tree([{start_tag, Tag, Attrs, false} | Rest], S, Options) ->
    tree(Rest, stack(norm({Tag, Attrs}, Options), S, Options), Options);
tree([T={pi, _Raw} | Rest], S, Options) ->
    tree(Rest, append_stack_child(T, S), Options);
tree([T={pi, _Tag, _Attrs} | Rest], S, Options) ->
    tree(Rest, append_stack_child(T, S), Options);
tree([T={comment, _Comment} | Rest], S, Options) ->
    tree(Rest, append_stack_child(T, S), Options);
tree(L=[{data, _Data, _Whitespace} | _], S, Options) ->
    case tree_data(L, true, []) of
        {_, true, Rest} ->
            tree(Rest, S, Options);
        {Data, false, Rest} ->
            tree(Rest, append_stack_child(Data, S), Options)
    end;
tree([{doctype, _} | Rest], Stack, Options) ->
    tree(Rest, Stack, Options).

norm({Tag, Attrs}, #{ mode := xml }) ->
    {iolist_to_binary(Tag), [{iolist_to_binary(K), iolist_to_binary(V)} || {K, V} <- Attrs], []};
norm(Tag, #{ lowercase := true }) when is_binary(Tag); is_list(Tag) ->
    z_string:to_lower(iolist_to_binary(Tag));
norm(Tag, #{ mode := xml }) ->
    iolist_to_binary(Tag);
norm({Tag, Attrs}, Options) ->
    {norm(Tag, Options), [{norm(K, Options), iolist_to_binary(V)} || {K, V} <- Attrs], []};
norm(Tag, _Options) when is_binary(Tag) ->
    Tag;
norm(Tag, _Options) ->
    BTag = iolist_to_binary(Tag),
    LTag = z_string:to_lower(BTag),
    case is_html_tag(LTag) of
        true -> LTag;
        false -> BTag
    end.

stack(T1, Stack, #{ mode := xml }) ->
    [T1 | Stack];
stack(T1={TN, _, _}, Stack=[{TN, _, _} | _Rest], Options)
    when       TN =:= <<"li">>
        orelse TN =:= <<"option">> ->
    [T1 | destack(TN, Stack, Options)];
stack(T1={TN, _, _}, Stack, Options) when TN =:= <<"td">> orelse TN =:= <<"th">> ->
    case find_in_stack([<<"td">>, <<"th">>], <<"table">>, Stack) of
        none -> Stack;
        undefined -> [T1 | Stack];
        Tag -> [T1 | destack(Tag, Stack, Options)]
    end;
stack(T1={TN, _, _}, Stack, Options)
    when TN =:= <<"tr">> ->
    case find_in_stack([<<"tr">>], <<"table">>, Stack) of
        none -> Stack;
        undefined -> [T1 | Stack];
        Tag -> [T1 | destack(Tag, Stack, Options)]
    end;
stack(T1={TN, _, _}, Stack, Options)
    when       TN =:= <<"tbody">>
        orelse TN =:= <<"thead">>
        orelse TN =:= <<"tfoot">>
        orelse TN =:= <<"colgroup">> ->
    case find_in_stack([<<"tbody">>, <<"thead">>, <<"tfoot">>, <<"colgroup">>], <<"table">>, Stack) of
        none -> Stack;
        undefined ->
            %% Make sure we are not destacking a tr of a parent table
            case find_in_stack([<<"tr">>], <<"table">>, Stack) of
                %% none case is not possible.
                undefined -> [T1 | Stack];
                <<"tr">> -> [T1 | destack(<<"tr">>, Stack, Options)]
            end;
        Tag -> [T1 | destack(Tag, Stack, Options)]
    end;
stack(T1={TN0, _, _}, Stack=[{TN1, _, _} | _Rest], Options)
  when (TN0 =:= <<"dd">> orelse TN0 =:= <<"dt">>) andalso
       (TN1 =:= <<"dd">> orelse TN1 =:= <<"dt">>) ->
    [T1 | destack(TN1, Stack, Options)];
stack(T1, Stack, _Options) ->
    [T1 | Stack].

find_in_stack(_CanClose, _Until, []) ->
    none;
find_in_stack(_CanClose, Until, [{Until, _,_}|_Rest]) ->
    undefined;
find_in_stack(CanClose, Until, [{TN, _,_}|Rest]) ->
    case lists:member(TN, CanClose) of
        false -> find_in_stack(CanClose, Until, Rest);
        true -> TN
    end;
find_in_stack(CanClose, Until, [_|Rest]) ->
    find_in_stack(CanClose, Until, Rest).

append_stack_child(StartTag, [{Name, Attrs, Acc} | Stack]) ->
    [{Name, Attrs, [StartTag | Acc]} | Stack].

destack(<<"br">>, Stack, #{ mode := html }) ->
    %% This is an ugly hack to make dumb_br_test() pass,
    %% this makes it such that br can never have children.
    Stack;
destack(TagName, Stack, Options) when is_list(Stack) ->
    F = fun
            ({X, _, _}) when X =:= TagName -> false;
            (_) -> true
        end,
    case lists:splitwith(F, Stack) of
        {_, []} ->
            %% If we're parsing something like XML we might find
            %% a <link>tag</link> that is normally a singleton
            %% in HTML but isn't here
            case {is_singleton(TagName, Options), Stack} of
                {true, [{T0, A0, Acc0} | Post0]} ->
                    case lists:splitwith(F, Acc0) of
                        {_, []} ->
                            %% Actually was a singleton
                            Stack;
                        {Pre, [{T1, A1, Acc1} | Post1]} ->
                            [{T0, A0, [{T1, A1, Acc1 ++ lists:reverse(Pre)} | Post1]}
                             | Post0]
                    end;
                _ ->
                    %% No match, no state change
                    Stack
            end;
        {_Pre, [_T]} ->
            %% Unfurl the whole stack, we're done
            destack(Stack);
        {Pre, [T, {T0, A0, Acc0} | Post]} ->
            %% Unfurl up to the tag, then accumulate it
            [{T0, A0, [destack(Pre ++ [T]) | Acc0]} | Post]
    end.

destack([{Tag, Attrs, Acc}]) ->
    {Tag, Attrs, lists:reverse(Acc)};
destack([{T1, A1, Acc1}, {T0, A0, Acc0} | Rest]) ->
    destack([{T0, A0, [{T1, A1, lists:reverse(Acc1)} | Acc0]} | Rest]).

is_singleton(_, #{ mode := xml }) -> false;
is_singleton(<<"area">>, _Options) -> true;
is_singleton(<<"base">>, _Options) -> true;
is_singleton(<<"br">>, _Options) -> true;
is_singleton(<<"col">>, _Options) -> true;
is_singleton(<<"embed">>, _Options) -> true;
is_singleton(<<"hr">>, _Options) -> true;
is_singleton(<<"img">>, _Options) -> true;
is_singleton(<<"input">>, _Options) -> true;
is_singleton(<<"link">>, _Options) -> true;
is_singleton(<<"meta">>, _Options) -> true;
is_singleton(<<"param">>, _Options) -> true;
is_singleton(<<"source">>, _Options) -> true;
is_singleton(<<"wbr">>, _Options) -> true;
is_singleton(_, _Options) -> false.

tokenize_data(B, S=#decoder{offset=O}) ->
    tokenize_data(B, S, O, true).

tokenize_data(B, S=#decoder{offset=O}, WhiteSpace) ->
    tokenize_data(B, S, O, WhiteSpace).

tokenize_data(B, S=#decoder{offset=O}, Start, Whitespace) ->
    case B of
        <<_:O/binary, C, _/binary>> when (C =/= $< andalso C =/= $&) ->
            tokenize_data(B, ?INC_CHAR(S, C), Start,
                          (Whitespace andalso ?IS_WHITESPACE(C)));
        _ ->
            Len = O - Start,
            <<_:Start/binary, Data:Len/binary, _/binary>> = B,
            {{data, Data, Whitespace}, S}
    end.

tokenize_attributes(B, S, Options) ->
    tokenize_attributes(B, S, [], Options).

tokenize_attributes(B, S=#decoder{offset=O}, Acc, Options) ->
    case B of
        <<_:O/binary>> ->
            {lists:reverse(Acc), S};
        <<_:O/binary, C, _/binary>> when (C =:= $> orelse C =:= $/) ->
            {lists:reverse(Acc), S};
        <<_:O/binary, "?>", _/binary>> ->
            {lists:reverse(Acc), S};
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            tokenize_attributes(B, ?INC_CHAR(S, C), Acc, Options);
        _ ->
            {Attr, S1} = tokenize_literal(B, S, attribute, Options),
            {Value, S2} = tokenize_attr_value(Attr, B, S1),
            tokenize_attributes(B, S2, [{Attr, Value} | Acc], Options)
    end.

tokenize_attr_value(Attr, B, S) ->
    S1 = skip_whitespace(B, S),
    O = S1#decoder.offset,
    case B of
        <<_:O/binary, "=", _/binary>> ->
            S2 = skip_whitespace(B, ?INC_COL(S1)),
            tokenize_quoted_or_unquoted_attr_value(B, S2);
        _ ->
            {Attr, S1}
    end.

tokenize_quoted_or_unquoted_attr_value(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary>> ->
            { [], S };
        <<_:O/binary, Q, _/binary>> when Q =:= ?QUOTE orelse
                                         Q =:= ?SQUOTE ->
            tokenize_quoted_attr_value(B, ?INC_COL(S), [], Q);
        <<_:O/binary, _/binary>> ->
            tokenize_unquoted_attr_value(B, S, [])
    end.

tokenize_quoted_attr_value(B, S=#decoder{offset=O}, Acc, Q) ->
    case B of
        <<_:O/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(B, ?INC_COL(S)),
            tokenize_quoted_attr_value(B, S1, [Data|Acc], Q);
        <<_:O/binary, Q, _/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), ?INC_COL(S) };
        <<_:O/binary, C, _/binary>> ->
            tokenize_quoted_attr_value(B, ?INC_COL(S), [C|Acc], Q)
    end.

tokenize_unquoted_attr_value(B, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(B, ?INC_COL(S)),
            tokenize_unquoted_attr_value(B, S1, [Data|Acc]);
        <<_:O/binary, $/, $>, _/binary>> ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, C, _/binary>> when ?PROBABLE_CLOSE(C) ->
            { iolist_to_binary(lists:reverse(Acc)), S };
        <<_:O/binary, C, _/binary>> ->
            tokenize_unquoted_attr_value(B, ?INC_COL(S), [C|Acc])
    end.

skip_whitespace(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            skip_whitespace(B, ?INC_CHAR(S, C));
        _ ->
            S
    end.

tokenize_literal(Bin, S=#decoder{offset=O}, Type, Options) ->
    case Bin of
        <<_:O/binary, C, _/binary>> when C =:= $>
                                    orelse C =:= $/
                                    orelse C =:= $= ->
            %% Handle case where tokenize_literal would consume
            %% 0 chars. http://github.com/mochi/mochiweb/pull/13
            {[C], ?INC_COL(S)};
        _ ->
            tokenize_literal(Bin, S, Type, <<>>, Options)
    end.

tokenize_literal(Bin, S=#decoder{offset=O}, Type, Acc, Options) ->
    case Bin of
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(Bin, ?INC_COL(S)),
            tokenize_literal(Bin, S1, Type, <<Acc/binary, Data/binary>>, Options);
        <<_:O/binary, C, _/binary>> when not (?IS_WHITESPACE(C)
                                              orelse C =:= $>
                                              orelse C =:= $/
                                              orelse C =:= $=) ->
            tokenize_literal(Bin, ?INC_COL(S), Type, <<Acc/binary, C>>, Options);
        _ ->
            Acc1 = case Type of
                tag ->
                    tokenize_tag(Acc, Options);
                attribute ->
                    tokenize_attribute_name(Acc, Options)
            end,
            {Acc1, S}
    end.

tokenize_tag(Tag, #{ lowercase := true }) ->
    z_string:to_lower(Tag);
tokenize_tag(Tag, #{ mode := xml }) ->
    Tag;
tokenize_tag(Tag, _Options) ->
    LTag = z_string:to_lower(Tag),
    case is_html_tag(LTag) of
        true -> LTag;
        false -> Tag
    end.

tokenize_attribute_name(Name, #{ lowercase := true }) ->
    z_string:to_lower(Name);
tokenize_attribute_name(Name, #{ mode := xml }) ->
    Name;
tokenize_attribute_name(Name, _Options) ->
    LName = z_string:to_lower(Name),
    case is_html_attr(LName) of
        true -> LName;
        false -> Name
    end.

raw_qgt(Bin, S=#decoder{offset=O}) ->
    raw_qgt(Bin, S, O).

raw_qgt(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary, "?>", _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {Raw, ?ADV_COL(S, 2)};
        <<_:O/binary, C, _/binary>> ->
            raw_qgt(Bin, ?INC_CHAR(S, C), Start);
        <<_:O/binary>> ->
            <<_:Start/binary, Raw/binary>> = Bin,
            {Raw, S}
    end.

find_qgt(Bin, S=#decoder{offset=O}) ->
    case Bin of
        <<_:O/binary, "?>", _/binary>> ->
            ?ADV_COL(S, 2);
        <<_:O/binary, ">", _/binary>> ->
            ?ADV_COL(S, 1);
        <<_:O/binary, "/>", _/binary>> ->
            ?ADV_COL(S, 2);
        %% tokenize_attributes takes care of this state:
        <<_:O/binary, C, _/binary>> ->
            find_qgt(Bin, ?INC_CHAR(S, C));
        <<_:O/binary>> ->
            S
    end.

find_gt(Bin, S) ->
    find_gt(Bin, S, false).

find_gt(Bin, S=#decoder{offset=O}, HasSlash) ->
    case Bin of
        <<_:O/binary, $/, _/binary>> ->
            find_gt(Bin, ?INC_COL(S), true);
        <<_:O/binary, $>, _/binary>> ->
            {?INC_COL(S), HasSlash};
        <<_:O/binary, C, _/binary>> ->
            find_gt(Bin, ?INC_CHAR(S, C), HasSlash);
        _ ->
            {S, HasSlash}
    end.

tokenize_charref(Bin, S=#decoder{offset=O}) ->
    try
        tokenize_charref(Bin, S, O)
    catch
        throw:invalid_charref ->
            {{data, <<"&">>, false}, S}
    end.

tokenize_charref(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary>> ->
            throw(invalid_charref);
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C)
                                         orelse C =:= ?SQUOTE
                                         orelse C =:= ?QUOTE
                                         orelse C =:= $/
                                         orelse C =:= $< 
                                         orelse C =:= $> 
                                         orelse C =:= $& ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
        Data = case z_html_charref:charref(Raw) of
                       undefined ->
                           Start1 = Start - 1,
                           Len1 = Len + 1,
                           <<_:Start1/binary, R:Len1/binary, _/binary>> = Bin,
                           R;
                       Unichar ->
                            codepoint_to_bytes(Unichar)
                   end,
            {{data, Data, false}, S};
        <<_:O/binary, $;, _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            Data = case z_html_charref:charref(Raw) of
                       undefined ->
                           throw(invalid_charref);
                       Unichar ->
                           codepoint_to_bytes(Unichar)
                   end,
            {{data, Data, false}, ?INC_COL(S)};
        _ ->
            tokenize_charref(Bin, ?INC_COL(S), Start)
    end.

codepoint_to_bytes(Unichar) when is_integer(Unichar) ->
    codepoint_to_bytes([Unichar]);
codepoint_to_bytes(Unichars) when is_list(Unichars) ->
    case unicode:characters_to_binary(Unichars) of
        B when is_binary(B) -> B;
        {error, _, _} -> throw(invalid_charref)
    end.


tokenize_doctype(Bin, S, Options) ->
    tokenize_doctype(Bin, S, [], Options).

tokenize_doctype(Bin, S=#decoder{offset=O}, Acc, Options) ->
    case Bin of
        <<_:O/binary>> ->
            {{doctype, lists:reverse(Acc)}, S};
        <<_:O/binary, $>, _/binary>> ->
            {{doctype, lists:reverse(Acc)}, ?INC_COL(S)};
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            tokenize_doctype(Bin, ?INC_CHAR(S, C), Acc);
        _ ->
            {Word, S1} = tokenize_word_or_literal(Bin, S, Options),
            tokenize_doctype(Bin, S1, [Word | Acc], Options)
    end.

tokenize_word_or_literal(Bin, S=#decoder{offset=O}, Options) ->
    case Bin of
        <<_:O/binary, C, _/binary>> when C =:= ?QUOTE orelse C =:= ?SQUOTE ->
            tokenize_word(Bin, ?INC_COL(S), C);
        <<_:O/binary, C, _/binary>> when not ?IS_WHITESPACE(C) ->
            %% Sanity check for whitespace
            tokenize_literal(Bin, S, tag, Options)
    end.

tokenize_word(Bin, S, Quote) ->
    tokenize_word(Bin, S, Quote, []).

tokenize_word(Bin, S=#decoder{offset=O}, Quote, Acc) ->
    case Bin of
        <<_:O/binary>> ->
            {iolist_to_binary(lists:reverse(Acc)), S};
        <<_:O/binary, Quote, _/binary>> ->
            {iolist_to_binary(lists:reverse(Acc)), ?INC_COL(S)};
        <<_:O/binary, $&, _/binary>> ->
            {{data, Data, false}, S1} = tokenize_charref(Bin, ?INC_COL(S)),
            tokenize_word(Bin, S1, Quote, [Data | Acc]);
        <<_:O/binary, C, _/binary>> ->
            tokenize_word(Bin, ?INC_CHAR(S, C), Quote, [C | Acc])
    end.

tokenize_cdata(Bin, S=#decoder{offset=O}) ->
    tokenize_cdata(Bin, S, O).

tokenize_cdata(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary, "]]>", _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{data, Raw, false}, ?ADV_COL(S, 3)};
        <<_:O/binary, C, _/binary>> ->
            tokenize_cdata(Bin, ?INC_CHAR(S, C), Start);
        _ ->
            <<_:O/binary, Raw/binary>> = Bin,
            {{data, Raw, false}, S}
    end.

tokenize_comment(Bin, S=#decoder{offset=O}) ->
    tokenize_comment(Bin, S, O).

tokenize_comment(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        <<_:O/binary, "-->", _/binary>> ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{comment, Raw}, ?ADV_COL(S, 3)};
        <<_:O/binary, C, _/binary>> ->
            tokenize_comment(Bin, ?INC_CHAR(S, C), Start);
        <<_:Start/binary, Raw/binary>> ->
            {{comment, Raw}, S}
    end.

tokenize_script(Bin, S=#decoder{offset=O}) ->
    tokenize_script(Bin, S, O).

tokenize_script(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        %% Just a look-ahead, we want the end_tag separately
        <<_:O/binary, $<, $/, SS, CC, RR, II, PP, TT, ZZ, _/binary>>
        when (SS =:= $s orelse SS =:= $S) andalso
             (CC =:= $c orelse CC =:= $C) andalso
             (RR =:= $r orelse RR =:= $R) andalso
             (II =:= $i orelse II =:= $I) andalso
             (PP =:= $p orelse PP =:= $P) andalso
             (TT=:= $t orelse TT =:= $T) andalso
             ?PROBABLE_CLOSE(ZZ) ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{data, Raw, false}, S};
        <<_:O/binary, C, _/binary>> ->
            tokenize_script(Bin, ?INC_CHAR(S, C), Start);
        <<_:Start/binary, Raw/binary>> ->
            {{data, Raw, false}, S}
    end.

tokenize_textarea(Bin, S=#decoder{offset=O}) ->
    tokenize_textarea(Bin, S, O).

tokenize_textarea(Bin, S=#decoder{offset=O}, Start) ->
    case Bin of
        %% Just a look-ahead, we want the end_tag separately
        <<_:O/binary, $<, $/, TT, EE, XX, TT2, AA, RR, EE2, AA2, ZZ, _/binary>>
        when (TT =:= $t orelse TT =:= $T) andalso
             (EE =:= $e orelse EE =:= $E) andalso
             (XX =:= $x orelse XX =:= $X) andalso
             (TT2 =:= $t orelse TT2 =:= $T) andalso
             (AA =:= $a orelse AA =:= $A) andalso
             (RR =:= $r orelse RR =:= $R) andalso
             (EE2 =:= $e orelse EE2 =:= $E) andalso
             (AA2 =:= $a orelse AA2 =:= $A) andalso
             ?PROBABLE_CLOSE(ZZ) ->
            Len = O - Start,
            <<_:Start/binary, Raw:Len/binary, _/binary>> = Bin,
            {{data, Raw, false}, S};
        <<_:O/binary, C, _/binary>> ->
            tokenize_textarea(Bin, ?INC_CHAR(S, C), Start);
        <<_:Start/binary, Raw/binary>> ->
            {{data, Raw, false}, S}
    end.


tree_to_map({Tag, [], Elts}, TagMap) when is_list(Elts) ->
    case lists:any(fun is_element/1, Elts) of
        true ->
            EltMap = lists:foldr(
                fun tree_to_map/2,
                #{},
                Elts),
            TagMap#{
                Tag => [ EltMap | maps:get(Tag, TagMap, []) ]
            };
        false ->
            % Leave node, sub elements are values
            Vs = map_values(Elts),
            TagMap#{
                Tag => lists:flatten([ Vs | maps:get(Tag, TagMap, []) ])
            }
    end;
tree_to_map({Tag, Attrs, Elts}, TagMap) when is_list(Elts) ->
    case lists:any(fun is_element/1, Elts) of
        true ->
            EltMap = lists:foldr(
                fun tree_to_map/2,
                #{},
                Elts),
            EltMap1 = EltMap#{
                <<"@attributes">> => Attrs
            },
            TagMap#{
                Tag => [ EltMap1 | maps:get(Tag, TagMap, []) ]
            };
        false ->
            % Leave node, but with attributes
            V = #{
                <<"@attributes">> => Attrs,
                <<"value">> => map_values(Elts)
            },
            TagMap#{
                Tag => [ V | maps:get(Tag, TagMap, []) ]
            }
    end;
tree_to_map(_, TagMap) ->
    TagMap.

map_values(Vs) ->
    lists:filtermap(fun map_value/1, Vs).

map_value(B) when is_binary(B) -> {true, B};
map_value(_) -> false.

is_element({Tag, Attrs, Elts}) when is_binary(Tag), is_list(Attrs), is_list(Elts) ->
    true;
is_element(_) ->
    false.


% @doc Return true when Tag is a html tag.
%

% A
is_html_tag(<<"a">>) -> true;
is_html_tag(<<"abbr">>) -> true;
is_html_tag(<<"acronym">>) -> true;
is_html_tag(<<"address">>) -> true;
is_html_tag(<<"applet">>) -> true;
is_html_tag(<<"area">>) -> true;
is_html_tag(<<"article">>) -> true;
is_html_tag(<<"aside">>) -> true;
is_html_tag(<<"audio">>) -> true;
% B
is_html_tag(<<"b">>) -> true;
is_html_tag(<<"base">>) -> true;
is_html_tag(<<"basefont">>) -> true;
is_html_tag(<<"bdi">>) -> true;
is_html_tag(<<"bdo">>) -> true;
is_html_tag(<<"bgsound">>) -> true;
is_html_tag(<<"big">>) -> true;
is_html_tag(<<"blink">>) -> true;
is_html_tag(<<"blockquote">>) -> true;
is_html_tag(<<"body">>) -> true;
is_html_tag(<<"br">>) -> true;
is_html_tag(<<"button">>) -> true;
% C
is_html_tag(<<"canvas">>) -> true;
is_html_tag(<<"caption">>) -> true;
is_html_tag(<<"center">>) -> true;
is_html_tag(<<"cite">>) -> true;
is_html_tag(<<"code">>) -> true;
is_html_tag(<<"col">>) -> true;
is_html_tag(<<"colgroup">>) -> true;
is_html_tag(<<"content">>) -> true;
% D
is_html_tag(<<"data">>) -> true;
is_html_tag(<<"datalist">>) -> true;
is_html_tag(<<"dd">>) -> true;
is_html_tag(<<"decorator">>) -> true;
is_html_tag(<<"del">>) -> true;
is_html_tag(<<"details">>) -> true;
is_html_tag(<<"dfn">>) -> true;
is_html_tag(<<"dir">>) -> true;
is_html_tag(<<"div">>) -> true;
is_html_tag(<<"dl">>) -> true;
is_html_tag(<<"dt">>) -> true;
% E
is_html_tag(<<"element">>) -> true;
is_html_tag(<<"em">>) -> true;
is_html_tag(<<"embed">>) -> true;
% F
is_html_tag(<<"fieldset">>) -> true;
is_html_tag(<<"figcaption">>) -> true;
is_html_tag(<<"figure">>) -> true;
is_html_tag(<<"font">>) -> true;
is_html_tag(<<"footer">>) -> true;
is_html_tag(<<"form">>) -> true;
is_html_tag(<<"frame">>) -> true;
is_html_tag(<<"frameset">>) -> true;
% G H
is_html_tag(<<"h1">>) -> true;
is_html_tag(<<"h2">>) -> true;
is_html_tag(<<"h3">>) -> true;
is_html_tag(<<"h4">>) -> true;
is_html_tag(<<"h5">>) -> true;
is_html_tag(<<"h6">>) -> true;
is_html_tag(<<"head">>) -> true;
is_html_tag(<<"header">>) -> true;
is_html_tag(<<"hgroup">>) -> true;
is_html_tag(<<"hr">>) -> true;
is_html_tag(<<"html">>) -> true;
% I
is_html_tag(<<"i">>) -> true;
is_html_tag(<<"iframe">>) -> true;
is_html_tag(<<"img">>) -> true;
is_html_tag(<<"input">>) -> true;
is_html_tag(<<"ins">>) -> true;
is_html_tag(<<"isindex">>) -> true;
% J K
is_html_tag(<<"kbd">>) -> true;
is_html_tag(<<"keygen">>) -> true;
% L
is_html_tag(<<"label">>) -> true;
is_html_tag(<<"legend">>) -> true;
is_html_tag(<<"li">>) -> true;
is_html_tag(<<"link">>) -> true;
is_html_tag(<<"listing">>) -> true;
% M
is_html_tag(<<"main">>) -> true;
is_html_tag(<<"map">>) -> true;
is_html_tag(<<"mark">>) -> true;
is_html_tag(<<"marquee">>) -> true;
is_html_tag(<<"menu">>) -> true;
is_html_tag(<<"menuitem">>) -> true;
is_html_tag(<<"meta">>) -> true;
is_html_tag(<<"meter">>) -> true;
% N
is_html_tag(<<"nav">>) -> true;
is_html_tag(<<"nobr">>) -> true;
is_html_tag(<<"noframes">>) -> true;
is_html_tag(<<"noscript">>) -> true;
% O
is_html_tag(<<"object">>) -> true;
is_html_tag(<<"ol">>) -> true;
is_html_tag(<<"optgroup">>) -> true;
is_html_tag(<<"option">>) -> true;
is_html_tag(<<"output">>) -> true;
% P
is_html_tag(<<"p">>) -> true;
is_html_tag(<<"param">>) -> true;
is_html_tag(<<"plaintext">>) -> true;
is_html_tag(<<"pre">>) -> true;
is_html_tag(<<"progress">>) -> true;
% Q
is_html_tag(<<"q">>) -> true;
% R
is_html_tag(<<"rp">>) -> true;
is_html_tag(<<"rt">>) -> true;
is_html_tag(<<"ruby">>) -> true;
% S
is_html_tag(<<"s">>) -> true;
is_html_tag(<<"samp">>) -> true;
is_html_tag(<<"script">>) -> true;
is_html_tag(<<"section">>) -> true;
is_html_tag(<<"select">>) -> true;
is_html_tag(<<"shadow">>) -> true;
is_html_tag(<<"small">>) -> true;
is_html_tag(<<"source">>) -> true;
is_html_tag(<<"spacer">>) -> true;
is_html_tag(<<"span">>) -> true;
is_html_tag(<<"strike">>) -> true;
is_html_tag(<<"strong">>) -> true;
is_html_tag(<<"style">>) -> true;
is_html_tag(<<"sub">>) -> true;
is_html_tag(<<"summary">>) -> true;
is_html_tag(<<"sup">>) -> true;
% T
is_html_tag(<<"table">>) -> true;
is_html_tag(<<"tbody">>) -> true;
is_html_tag(<<"td">>) -> true;
is_html_tag(<<"template">>) -> true;
is_html_tag(<<"textarea">>) -> true;
is_html_tag(<<"tfoot">>) -> true;
is_html_tag(<<"th">>) -> true;
is_html_tag(<<"thead">>) -> true;
is_html_tag(<<"time">>) -> true;
is_html_tag(<<"title">>) -> true;
is_html_tag(<<"tr">>) -> true;
is_html_tag(<<"track">>) -> true;
is_html_tag(<<"tt">>) -> true;
% U
is_html_tag(<<"u">>) -> true;
is_html_tag(<<"ul">>) -> true;
% V
is_html_tag(<<"var">>) -> true;
is_html_tag(<<"video">>) -> true;
% W
is_html_tag(<<"wbr">>) -> true;
% X Y Z
is_html_tag(<<"xmp">>) -> true;
% Everything else
is_html_tag(_) -> false.

% @doc Returns true when Attr is a html attribute.
is_html_attr(<<"accept">>) -> true; 
is_html_attr(<<"accept-charset">>) -> true;
is_html_attr(<<"accesskey">>) -> true;
is_html_attr(<<"action">>) -> true;  
is_html_attr(<<"align">>) -> true;  
is_html_attr(<<"alt">>) -> true; 
is_html_attr(<<"async">>) -> true;   
is_html_attr(<<"autocomplete">>) -> true;
is_html_attr(<<"autofocus">>) -> true;
is_html_attr(<<"autoplay">>) -> true;
% B
is_html_attr(<<"bgcolor">>) -> true;
is_html_attr(<<"border">>) -> true;
is_html_attr(<<"buffered">>) -> true;
% C
is_html_attr(<<"challenge">>) -> true;
is_html_attr(<<"charset">>) -> true;
is_html_attr(<<"checked">>) -> true;
is_html_attr(<<"cite">>) -> true;
is_html_attr(<<"class">>) -> true;
is_html_attr(<<"code">>) -> true;
is_html_attr(<<"codebase">>) -> true;
is_html_attr(<<"color">>) -> true;
is_html_attr(<<"cols">>) -> true;   
is_html_attr(<<"colspan">>) -> true;
is_html_attr(<<"content">>) -> true;
is_html_attr(<<"contenteditable">>) -> true;
is_html_attr(<<"contextmenu">>) -> true;
is_html_attr(<<"controls">>) -> true;
is_html_attr(<<"coords">>) -> true;
% D
is_html_attr(<<"data">>) -> true;
is_html_attr(<<"data-", _Rest/binary>>) -> true;
is_html_attr(<<"datetime">>) -> true;
is_html_attr(<<"default">>) -> true;
is_html_attr(<<"defer">>) -> true;
is_html_attr(<<"dir">>) -> true;
is_html_attr(<<"dirname">>) -> true;
is_html_attr(<<"disabled">>) -> true;
is_html_attr(<<"download">>) -> true;
is_html_attr(<<"draggable">>) -> true;
is_html_attr(<<"dropzone">>) -> true;
% E
is_html_attr(<<"enctype">>) -> true;
% F
is_html_attr(<<"for">>) -> true;
is_html_attr(<<"form">>) -> true;
is_html_attr(<<"headers">>) -> true;
is_html_attr(<<"height">>) -> true;
is_html_attr(<<"hidden">>) -> true;
is_html_attr(<<"high">>) -> true;
is_html_attr(<<"href">>) -> true;
is_html_attr(<<"hreflang">>) -> true;
is_html_attr(<<"http-equiv">>) -> true;
% G H I
is_html_attr(<<"icon">>) -> true;
is_html_attr(<<"id">>) -> true;
is_html_attr(<<"ismap">>) -> true;
is_html_attr(<<"itemprop">>) -> true;
% K
is_html_attr(<<"keytype">>) -> true;
is_html_attr(<<"kind">>) -> true;
is_html_attr(<<"label">>) -> true;
% L
is_html_attr(<<"lang">>) -> true;
is_html_attr(<<"language">>) -> true;
is_html_attr(<<"list">>) -> true;
is_html_attr(<<"loop">>) -> true;
is_html_attr(<<"low">>) -> true; 
% M
is_html_attr(<<"manifest">>) -> true;
is_html_attr(<<"max">>) -> true;
is_html_attr(<<"maxlength">>) -> true;
is_html_attr(<<"media">>) -> true;
is_html_attr(<<"method">>) -> true;
is_html_attr(<<"min">>) -> true;
% N
is_html_attr(<<"multiple">>) -> true;
is_html_attr(<<"name">>) -> true;    
is_html_attr(<<"novalidate">>) -> true;
% O
is_html_attr(<<"open">>) -> true;
is_html_attr(<<"optimum">>) -> true;
% P
is_html_attr(<<"pattern">>) -> true;
is_html_attr(<<"ping">>) -> true;
is_html_attr(<<"placeholder">>) -> true;
is_html_attr(<<"poster">>) -> true;
is_html_attr(<<"preload">>) -> true; 
is_html_attr(<<"pubdate">>) -> true;
% Q R
is_html_attr(<<"radiogroup">>) -> true;
is_html_attr(<<"readonly">>) -> true;
is_html_attr(<<"rel">>) -> true;
is_html_attr(<<"required">>) -> true;
is_html_attr(<<"reversed">>) -> true;
is_html_attr(<<"rows">>) -> true;
is_html_attr(<<"rowspan">>) -> true; 
% S
is_html_attr(<<"sandbox">>) -> true;
is_html_attr(<<"spellcheck">>) -> true;
is_html_attr(<<"scope">>) -> true;
is_html_attr(<<"scoped">>) -> true; 
is_html_attr(<<"seamless">>) -> true; 
is_html_attr(<<"selected">>) -> true;
is_html_attr(<<"shape">>) -> true;
is_html_attr(<<"size">>) -> true;
is_html_attr(<<"sizes">>) -> true; 
is_html_attr(<<"span">>) -> true; 
is_html_attr(<<"src">>) -> true;
is_html_attr(<<"srcdoc">>) -> true;
is_html_attr(<<"srclang">>) -> true;
is_html_attr(<<"start">>) -> true;
is_html_attr(<<"step">>) -> true;
is_html_attr(<<"style">>) -> true;   
is_html_attr(<<"summary">>) -> true; 
% T
is_html_attr(<<"tabindex">>) -> true; 
is_html_attr(<<"target">>) -> true;  
is_html_attr(<<"title">>) -> true;  
is_html_attr(<<"type">>) -> true;  
% U
is_html_attr(<<"usemap">>) -> true;
is_html_attr(<<"value">>) -> true;
% W
is_html_attr(<<"width">>) -> true; 
is_html_attr(<<"wrap">>) -> true;   
is_html_attr(_) -> false.
