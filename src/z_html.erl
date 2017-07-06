%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%% Date: 2009-04-17
%%
%% @doc Utility functions for html processing.  Also used for property filtering (by m_rsc_update).

%% Copyright 2009-2014 Marc Worrell
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

-module(z_html).
-author("Marc Worrell <marc@worrell.nl").

-export([
    escape_props/1,
    escape_props/2,
    escape/1,
    escape_props_check/1,
    escape_props_check/2,
    escape_check/1,
    unescape/1,
    strip/1,
    truncate/2,
    truncate/3,
    sanitize/1,
    sanitize/2,
    sanitize/4,
    noscript/1,
    sanitize_uri/1,
    escape_link/1,
    nl2br/1,
    br2nl/1,
    scrape_link_elements/1,
    ensure_escaped_amp/1,
    abs_links/2
]).

% Used by z_svg.erl
% @todo: move this to separate erlang module
-export([
    flatten_attr/1,
    escape_html_text/2,
    escape_html_comment/2
    ]).


%% @doc Escape all properties used for an update statement. Only leaves the body property intact.
-spec escape_props(list()) -> list().
escape_props(Props) ->
    escape_props(Props, []).

-spec escape_props(list(), Options::list()) -> list().
escape_props(Props, Options) ->
    [ escape_props1(P, Options) || P <- Props ].

escape_props1({_K,V} = Prop, _Options) when is_float(V); is_integer(V); is_atom(V) -> 
    Prop;
escape_props1({body, V}, Options) ->
    {body, sanitize(V, Options)};
escape_props1({body_extra, V}, Options) ->
    {body_extra, sanitize(V, Options)};
escape_props1({summary, Summary}, _Options) ->
    {summary, nl2br(escape_value(Summary))};
escape_props1({blocks, V}, Options) when is_list(V) ->
    V1 = [ escape_props(L, Options) || L <- V ],
    {blocks, V1};
escape_props1({website, V}, _Options) ->
    V1 = escape_value(sanitize_uri(V)),
    {website, V1};
escape_props1({K, V}, Options) ->
    case z_convert:to_list(K) of
        "is_" ++ _ ->
            {K, z_convert:to_bool(V)};
        KS ->
            EscapeFun = case lists:reverse(KS) of
                            "lmth_" ++ _ -> fun(A) -> sanitize(A, Options) end; %% prop ends in '_html'
                            "iru_" ++ _ -> fun(A) -> escape_value(sanitize_uri(A)) end; %% prop ends in '_uri'
                            "lru_" ++ _ -> fun(A) -> escape_value(sanitize_uri(A)) end; %% prop ends in '_url'
                            _ -> fun escape_value/1
                        end,
            {K, EscapeFun(V)}
    end;
escape_props1(P, _Options) ->
    P.

escape_value({trans, _Ts} = Tr) ->
    escape(Tr);
escape_value(V) when is_list(V) ->
    try
        escape_value(iolist_to_binary(V))
    catch _:_ ->
        V
    end;
escape_value(B) when is_binary(B) ->
    escape(B);
escape_value(V) -> 
    V.


%% @doc Checks if all properties are properly escaped
-spec escape_props_check(list()) -> list().
escape_props_check(Props) ->
    escape_props_check(Props, undefined).

-spec escape_props_check(list(), Options::list()) -> list().
escape_props_check(Props, Options) ->
    [ escape_props_check1(P, Options) || P <- Props ].

escape_props_check1({_K,V} = Prop, _Options) when is_float(V); is_integer(V); is_atom(V) -> 
    Prop;
escape_props_check1({body, V}, Options) ->
    {body, sanitize(V, Options)};
escape_props_check1({body_extra, V}, Options) ->
    {body_extra, sanitize(V, Options)};
escape_props_check1({summary, Summary}, _Options) ->
    {summary, nl2br(escape_check(br2nl(Summary)))};
escape_props_check1({blocks, V}, Options) when is_list(V) ->
    V1 = [ escape_props_check(L, Options) || L <- V ],
    {blocks, V1};
escape_props_check1({website, V}, _Options) ->
    {website, escape_value(sanitize_uri(unescape(V)))};
escape_props_check1({K, V}, Options) ->
    EscapeFun = case lists:reverse(z_convert:to_list(K)) of
                    "lmth_" ++ _ -> fun(A) -> sanitize(A, Options) end; %% prop ends in '_html'
                    "iru_" ++ _ -> fun(A) -> escape_value(sanitize_uri(unescape(A))) end; %% prop ends in '_uri'
                    "lru_" ++ _ -> fun(A) -> escape_value(sanitize_uri(unescape(A))) end; %% prop ends in '_url'
                    _ -> fun escape_value_check/1
                end,
    {K, EscapeFun(V)};
escape_props_check1(P, _Options) ->
    P.

escape_value_check({trans, _Ts} = Tr) ->
    escape_check(Tr);
escape_value_check(V) when is_list(V) ->
    try
        escape_check(iolist_to_binary(V))
    catch _:_ ->
        V
    end;
escape_value_check(B) when is_binary(B) ->
    escape_check(B);
escape_value_check(V) -> 
    V.


%% @doc Escape a string so that it is valid within HTML/ XML.
%% @spec escape(iolist()) -> binary()
-spec escape(list()|binary()|{trans, list()}) -> binary() | undefined.
escape({trans, Tr}) ->
    {trans, [{Lang, escape(V)} || {Lang,V} <- Tr]};
escape(undefined) -> 
    undefined;
escape(<<>>) -> 
    <<>>;
escape([]) ->
    <<>>;
escape(L) when is_list(L) ->
    escape(list_to_binary(L));
escape(B) when is_binary(B) ->
    escape1(B, <<>>).

escape1(<<>>, Acc) -> 
    Acc;
escape1(<<"&euro;", T/binary>>, Acc) ->
    escape1(T, <<Acc/binary, "€">>);
escape1(<<$&, T/binary>>, Acc) ->
    escape1(T, <<Acc/binary, "&amp;">>);
escape1(<<$<, T/binary>>, Acc) ->
    escape1(T, <<Acc/binary, "&lt;">>);
escape1(<<$>, T/binary>>, Acc) ->
    escape1(T, <<Acc/binary, "&gt;">>);
escape1(<<$", T/binary>>, Acc) ->
    escape1(T, <<Acc/binary, "&quot;">>);
escape1(<<$', T/binary>>, Acc) ->
    escape1(T, <<Acc/binary, "&#39;">>);
escape1(<<C, T/binary>>, Acc) ->
    escape1(T, <<Acc/binary, C>>).


%% @doc Escape a string so that it is valid within HTML/ XML.
-spec escape_check(list()|binary()|{trans, list()}) -> binary() | undefined.
escape_check({trans, Tr}) ->
    {trans, [{Lang, escape_check(V)} || {Lang,V} <- Tr]};
escape_check(undefined) -> 
    undefined;
escape_check(<<>>) -> 
    <<>>;
escape_check([]) ->
    <<>>;
escape_check(L) when is_list(L) ->
    escape_check1(iolist_to_binary(L), <<>>);
escape_check(B) when is_binary(B) ->
    escape_check1(B, <<>>);
escape_check(Other) ->
    Other.

escape_check1(<<>>, Acc) -> 
    Acc;
escape_check1(<<"&euro;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "€">>);
escape_check1(<<"&amp;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&amp;">>);
escape_check1(<<"&lt;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&lt;">>);
escape_check1(<<"&gt;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&gt;">>);
escape_check1(<<"&quot;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&quot;">>);
escape_check1(<<"&#39;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&#39;">>);
escape_check1(<<"&#x27;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&#x27;">>);
escape_check1(<<"&#x2F;", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&#x2F;">>);
escape_check1(<<$&, Rest/binary>>, Acc) ->
    case try_amp(Rest, in_amp, <<>>) of
        {Amp,Rest1} -> escape_check1(Rest1, <<Acc/binary, $&, Amp/binary>>);
        false -> escape_check1(Rest, <<Acc/binary, "&amp;">>)
    end;
escape_check1(<<$<, T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&lt;">>);
escape_check1(<<$>, T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&gt;">>);
escape_check1(<<$", T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&quot;">>);
escape_check1(<<$', T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, "&#39;">>);
escape_check1(<<C, T/binary>>, Acc) ->
    escape_check1(T, <<Acc/binary, C>>).



%% @doc Unescape - reverses the effect of escape.
%% @spec unescape(iolist()) -> binary()
unescape({trans, Tr}) ->
    {trans, [{Lang, unescape(V)} || {Lang,V} <- Tr]};
unescape(undefined) -> 
    undefined;
unescape(<<>>) -> 
    <<>>;
unescape([]) ->
    <<>>;
unescape(L) when is_list(L) ->
    unescape(list_to_binary(L));
unescape(B) when is_binary(B) ->
    unescape(B, <<>>).

unescape(<<>>, Acc) -> 
    Acc;
unescape(<<"&", Rest/binary>>, Acc) ->
    unescape_in_charref(Rest, <<>>, Acc);
unescape(<<C, T/binary>>, Acc) ->
    unescape(T, <<Acc/binary, C>>).

unescape_in_charref(<<>>, CharAcc, ContAcc) ->
    <<ContAcc/binary, $&, CharAcc/binary>>; %% premature end of string; keep.
unescape_in_charref(<<$;, Rest/binary>>, CharAcc, ContAcc) ->
    case z_html_charref:charref(CharAcc) of
        undefined ->
            %% keep original code
            unescape(Rest, <<ContAcc/binary, $&, CharAcc/binary, $;>>);
        Ch ->
            %% replace the real char
            ChBin = unicode:characters_to_binary([Ch]),
            unescape(Rest, <<ContAcc/binary, ChBin/binary>>)
    end;

unescape_in_charref(<<Ch/integer, Rest/binary>>, CharAcc, ContAcc) ->
    unescape_in_charref(Rest, <<CharAcc/binary, Ch>>, ContAcc).
    

%% @doc Escape a text. Expands any urls to links with a nofollow attribute.
%% @spec escape_link(Text) -> binary()
escape_link(undefined) ->
    undefined;
escape_link(<<>>) ->
    <<>>;
escape_link([]) ->
    <<>>;
escape_link(Text) ->
    case re:run(Text, "\\b(([\\w-]+://?|www[.])[^\\s()<>]+(?:\\([\\w\\d]+\\)|([^[:punct:]\\s]|/)))", [{capture, first, index}, global]) of
        {match, Matches} ->
            Matches1 = [ hd(M) || M <- Matches ],
            nl2br(iolist_to_binary(make_links1(0, Matches1, z_convert:to_list(Text), [])));
        nomatch ->
            nl2br(escape(Text))
    end.

make_links1(_Offset, [], Text, Acc) ->
    lists:reverse([escape(Text) | Acc]);
make_links1(Offset, [{Offset, Len}|Rest], Text, Acc) ->
    {Link, Text1} = lists:split(Len, Text),
    NoScript = noscript(Link, true),
    Link1 = escape(NoScript),
    Link2 = escape(ensure_protocol(NoScript)),
    make_links1(Offset+Len, Rest, Text1, [["<a href=\"",Link2,"\" rel=\"nofollow\">",Link1,"</a>"] | Acc]);
make_links1(Offset, [{MatchOffs,_}|_] = Matches, Text, Acc) ->
    {Text1,Text2} = lists:split(MatchOffs-Offset, Text),
    make_links1(MatchOffs, Matches, Text2, [escape(Text1)|Acc]).

ensure_protocol([]) -> [];
ensure_protocol("#" ++ _ = Link) -> Link;
ensure_protocol("/" ++ _ = Link) -> Link;
ensure_protocol("://" ++ _ = Link) -> ["http", Link];
ensure_protocol("http://" ++ _ = Link) -> Link;
ensure_protocol("https://" ++ _ = Link) -> Link;
ensure_protocol("mailto:" ++ Rest) -> "mailto:"++z_string:trim(Rest);
ensure_protocol("www." ++ Rest) -> ["http://www.", Rest];
ensure_protocol(<<>>) -> <<>>;
ensure_protocol(<<"#", _/binary>> = Link) -> Link;
ensure_protocol(<<"/", _/binary>> = Link) -> Link;
ensure_protocol(<<"://", _/binary>> = Link) -> ["http", Link];
ensure_protocol(<<"http://", _/binary>> = Link) -> Link;
ensure_protocol(<<"https://", _/binary>> = Link) -> Link;
ensure_protocol(<<"mailto:", Rest/binary>>) ->
    <<"mailto:", (z_string:trim(Rest))/binary>>;
ensure_protocol(<<"www.", _/binary>> = Link) -> <<"http://", Link/binary>>;
ensure_protocol(Link) ->
    B = iolist_to_binary(Link),
    case binary:match(B, <<"://">>) of
        nomatch ->
            [First|_] = binary:split(B, <<"/">>),
            case binary:match(First, <<".">>) of
                nomatch -> [$/, Link];
                _Match -> ["http://", Link]
            end;
        _ ->
            B
    end.

%% @doc Ensure that an uri is (quite) harmless by removing any script reference
sanitize_uri(undefined) ->
    undefined;
sanitize_uri(<<>>) ->
    <<>>;
sanitize_uri([]) ->
    <<>>;
sanitize_uri(Uri) ->
    B = iolist_to_binary(ensure_protocol(noscript(z_string:trim(Uri), true))),
    cleanup_uri_chars(B, <<>>).

cleanup_uri_chars(<<>>, Acc) -> 
    Acc;
cleanup_uri_chars(<<$%, A, B, C/binary>>, Acc)
    when      ((A >= $0 andalso A =< $9) orelse (A >= $A andalso A =< $Z))
      andalso ((B >= $0 andalso B =< $9) orelse (B >= $A andalso B =< $Z)) ->
    cleanup_uri_chars(C, <<Acc/binary, $%, A, B>>);
cleanup_uri_chars(<<C, B/binary>>, Acc)
    when C =:= $.; C =:= $&; C =:= $:; C =:= $/; 
         C =:= $=; C =:= $?; C =:= $#; C =:= $+ ->
    cleanup_uri_chars(B, <<Acc/binary, C>>);
cleanup_uri_chars(<<C, B/binary>>, Acc) ->
    case z_url:url_unreserved_char(C) of
        false ->
            C1 = iolist_to_binary(z_url:hex_encode([C])),
            cleanup_uri_chars(B, <<Acc/binary, $%, C1/binary>>);
        true ->
            cleanup_uri_chars(B, <<Acc/binary, C>>)
    end.

%% @doc Strip all html elements from the text. Simple parsing is applied to find the elements. Does not escape the end result.
%% @spec strip(iolist()) -> iolist()
strip({trans, Tr}) ->
    {trans, [{Lang, strip(V)} || {Lang,V} <- Tr]};
strip(undefined) ->
    [];
strip(<<>>) ->
    <<>>;
strip([]) ->
    [];
strip(Html) when is_binary(Html) ->
    strip(Html, in_text, <<>>);
strip(L) when is_list(L) ->
    strip(list_to_binary(L));
strip(N) when is_integer(N) ->
    strip(integer_to_list(N)).

strip(<<>>, _, Acc) -> 
    Acc;
strip(<<$<,T/binary>>, in_text, Acc) ->
    strip(T, in_tag, Acc);
strip(<<$>,T/binary>>, in_tag, <<>>) ->
    strip(T, in_text, <<>>);
strip(<<$>>>, in_tag, Acc) -> 
    Acc;
strip(<<$>, WS, T/binary>>, in_tag, Acc) when WS =< 32 ->
    strip(T, in_text, <<Acc/binary, WS>>);
strip(<<$>, T/binary>>, in_tag, Acc) ->
    case T of
        <<$<, $/, _/binary>> ->
            strip(T, in_text, Acc);
        _ ->
            strip(T, in_text, maybe_add_space(Acc))
    end;
strip(<<$>,T/binary>>, State, Acc) ->
    strip(T, State, Acc);
strip(<<$<,T/binary>>, State, Acc) ->
    strip(T, State, Acc);
strip(<<$\\,_,T/binary>>, in_dstring, Acc) ->
    strip(T, in_dstring, Acc);
strip(<<$\\,_,T/binary>>, in_sstring, Acc) ->
    strip(T, in_sstring, Acc);
strip(<<$",T/binary>>, in_tag, Acc) ->
    strip(T, in_dstring, Acc);
strip(<<$",T/binary>>, in_dstring, Acc) ->
    strip(T, in_tag, Acc);
strip(<<$',T/binary>>, in_tag, Acc) ->
    strip(T, in_sstring, Acc);
strip(<<$',T/binary>>, in_sstring, Acc) ->
    strip(T, in_tag, Acc);
strip(<<H,T/binary>>, in_text, Acc) ->
    strip(T, in_text, <<Acc/binary, H>>);
strip(<<_,T/binary>>, State, Acc) ->
    strip(T, State, Acc).

maybe_add_space(Bin) ->
    case binary:last(Bin) of
        C when C =< 32 ->
            Bin;
        _ ->
            <<Bin/binary, 32>>
    end.

%% @doc Truncate a previously sanitized HTML string.
truncate(Html,Length) ->
    truncate(Html, Length, <<>>).

truncate(_, Length, _Append) when Length =< 0 ->
    <<>>;
truncate({trans, Tr}, Length, Append) ->
    {trans, [{Lang,truncate(V,Length, Append)} || {Lang,V} <- Tr]};
truncate(Html, Length, Append) when is_list(Html) ->
    truncate(unicode:characters_to_binary(Html), Length, Append);
truncate(Html, Length, Append) when is_binary(Html) ->
    case size(Html) of
        N when N =< Length ->
            Html;
        _ ->
            truncate(Html, in_text, [], <<>>, <<>>, Length, Append)
    end.

truncate(<<>>, _State, _Stack, _TagAcc, Acc, _Length, _Append) ->
    Acc;

truncate(<<"<!--", Rest/binary>>, in_text, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_comment, Stack, <<>>, <<Acc/binary,"<!--">>, Length, Append);
truncate(<<"-->", Rest/binary>>, in_comment, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_text, Stack, <<>>, <<Acc/binary,"-->">>, Length, Append);
truncate(<<C/utf8, Rest/binary>>, in_comment, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_comment, Stack, <<>>, <<Acc/binary,C/utf8>>, Length, Append);

truncate(<<$<, $/, Rest/binary>>, in_text, Stack, <<>>, Acc, 0, Append) ->
    truncate(Rest, in_tag, Stack, <<$<,$/>>, Acc, 0, Append);
truncate(_Rest, in_text, [], _TagAcc, Acc, 0, Append) ->
    <<Acc/binary,Append/binary>>;
truncate(Rest, in_text, [Tag|Stack], _TagAcc, Acc, 0, Append) ->
    CloseTag = make_closetag(Tag),
    truncate(Rest, in_text, Stack, <<>>, <<Acc/binary, Append/binary, CloseTag/binary>>, 0, <<>>);

truncate(<<$/,$>, Rest/binary>>, in_tag, Stack, Tag, Acc, Length, Append) ->
    truncate(Rest, in_text, Stack, <<>>, <<Acc/binary,Tag/binary, $/, $>>>, Length, Append);
truncate(<<$>, Rest/binary>>, in_tag, [_Tag|Stack], <<$<,$/,_/binary>> = CloseTag, Acc, Length, Append) ->
    truncate(Rest, in_text, Stack, <<>>, <<Acc/binary,CloseTag/binary,$>>>, Length, Append);
truncate(<<$>, Rest/binary>>, in_tag, Stack, Tag, Acc, Length, Append) ->
    truncate(Rest, in_text, [Tag|Stack], <<>>, <<Acc/binary,Tag/binary,$>>>, Length, Append);
truncate(<<$<, Rest/binary>>, in_text, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_tag, Stack, <<$<>>, Acc, Length, Append);
truncate(<<C/utf8, Rest/binary>>, in_tag, Stack, Tag, Acc, Length, Append) ->
    truncate(Rest, in_tag, Stack, <<Tag/binary,C/utf8>>, Acc, Length, Append);

truncate(<<$&, Rest/binary>>, in_text, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_element, Stack, <<>>, <<Acc/binary,$&>>, Length, Append);
truncate(<<$;, Rest/binary>>, in_element, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_text, Stack, <<>>, <<Acc/binary,$;>>, Length-1, Append);
truncate(<<C, Rest/binary>>, in_element, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_element, Stack, <<>>, <<Acc/binary,C>>, Length, Append);

truncate(<<C/utf8, Rest/binary>>, in_text, Stack, <<>>, Acc, Length, Append) ->
    truncate(Rest, in_text, Stack, <<>>, <<Acc/binary, C/utf8>>, Length-1, Append).

make_closetag(<<$<, Rest/binary>>) ->
    case binary:split(Rest, <<" ">>) of
        [Tag,_] ->
            << $<,$/,Tag/binary,$> >>;
        _ ->
            [Tag|_] = binary:split(Rest, <<">">>),
            << $<,$/,Tag/binary,$> >>
    end.

%% @doc Sanitize a (X)HTML string. Remove elements and attributes that might be harmful.
%% @spec sanitize(binary()) -> binary()
sanitize(Html) ->
    sanitize(Html, []).


sanitize({trans, Tr}, Options) ->
    {trans, [{Lang, sanitize(V, Options)} || {Lang,V} <- Tr]};
sanitize(Html, Options) when is_binary(Html) ->
    sanitize_opts(<<"<sanitize>",Html/binary,"</sanitize>">>, Options);
sanitize(Html, Options) when is_list(Html) ->
    sanitize_opts(iolist_to_binary(["<sanitize>", Html, "</sanitize>"]), Options).

sanitize_opts(Html, Options) ->
    sanitize1(Html, proplists:get_value(elt_extra, Options, []), 
        proplists:get_value(attr_extra, Options, []), Options).

sanitize1(Html, ExtraElts, ExtraAttrs, Options) ->
    Parsed = z_html_parse:parse(ensure_escaped_amp(Html)),
    Sanitized = sanitize(Parsed, ExtraElts, ExtraAttrs, Options),
    flatten(Sanitized).

%% @doc Sanitize a mochiwebparse tree. Remove harmful elements and attributes.
%% @spec sanitize(mochiweb_html:html_node(), binary() | list(), binary() | list(), any())
sanitize(ParseTree, ExtraElts, ExtraAttrs, Options) when is_binary(ExtraElts) ->
    sanitize(ParseTree, binary:split(ExtraElts, <<",">>, [global]), ExtraAttrs, Options);
sanitize(ParseTree, ExtraElts, ExtraAttrs, Options) when is_binary(ExtraAttrs) ->
    sanitize(ParseTree, ExtraElts, binary:split(ExtraAttrs, <<",">>, [global]), Options);
sanitize(ParseTree, ExtraElts, ExtraAttrs, Options) ->
    sanitize(ParseTree, [], ExtraElts, ExtraAttrs, Options).

sanitize(B, Stack, _ExtraElts, _ExtraAttrs, Options) when is_binary(B) ->
    case sanitize_element({'TextNode', B}, Stack, Options) of
        {'TextNode', B1} -> escape(iolist_to_binary(B1));
        Other -> Other
    end; 
sanitize({comment, _Text} = Comment, Stack, _ExtraElts, _ExtraAttrs, Options) ->
    sanitize_element(Comment, Stack, Options);
sanitize({pi, _Raw}, _Stack, _ExtraElts, _ExtraAttrs, _Options) ->
    <<>>;
sanitize({pi, _Tag, _Attrs}, _Stack, _ExtraElts, _ExtraAttrs, _Options) ->
    <<>>;
sanitize({<<"svg">>, _Attrs, _Enclosed} = Element, _Stack, ExtraElts, _ExtraAttrs, _Options) ->
    case allow_elt(<<"svg">>, ExtraElts) of
        true ->
            z_svg:sanitize_element(Element);
        false ->
            {nop, []}
    end;
sanitize({Elt,Attrs,Enclosed}, Stack, ExtraElts, ExtraAttrs, Options) ->
    case allow_elt(Elt, ExtraElts) orelse (not lists:member(Elt, Stack) andalso allow_once(Elt)) of
        true ->
            Attrs1 = lists:filter(fun({A,_}) -> allow_attr(A, ExtraAttrs) end, Attrs),
            Stack1 = [Elt|Stack],
            Tag = { Elt, 
                    Attrs1,
                    [ sanitize(Encl, Stack1, ExtraElts, ExtraAttrs, Options) || Encl <- Enclosed ]},
            sanitize_element(Tag, Stack, Options);
        false ->
            case skip_contents(Elt) of
                false ->
                    {nop, [ sanitize(Encl, Stack, ExtraElts, ExtraAttrs, Options) || Encl <- Enclosed ]};
                true ->
                    {nop, []}
            end
    end.

sanitize_element(Element, Stack, Options) ->
    Callback = proplists:get_value(element, Options, fun(E) -> E end),
    sanitize_element(Callback, Element, Stack, Options).

sanitize_element(F, Element, _Stack, _Options) when is_function(F, 1) ->
    F(Element);
sanitize_element(F, Element, _Stack, Options) when is_function(F, 2) ->
    F(Element, Options);
sanitize_element(F, Element, Stack, Options) when is_function(F, 3) ->
    F(Element, Stack, Options);
sanitize_element({M, F, A}, Element, Stack, _Options) ->
    erlang:apply(M, F, [Element, Stack|A]).


%% @doc Flatten the sanitized html tree to a binary 
flatten(B) when is_binary(B) ->
    escape_html_text(B, <<>>);
flatten({nop, Enclosed}) ->
    flatten(Enclosed);
flatten({comment, Text}) ->
    Comment = binary:replace(Text, <<"-->">>, <<"-- >">>, [global]),
    <<"<!--", Comment/binary, "-->">>;
flatten({sanitized_html, Html}) ->
    Html;
flatten({Elt, Attrs, Enclosed}) ->
    EncBin = flatten(Enclosed),
    Attrs1 = [flatten_attr(Attr) || Attr <- Attrs ],
    Attrs2 = iolist_to_binary(prefix(32, Attrs1)),
    case is_selfclosing(Elt) andalso EncBin == <<>> of
        true ->  <<$<, Elt/binary, Attrs2/binary, 32, $/, $>>>;
        false -> <<$<, Elt/binary, Attrs2/binary, $>, EncBin/binary, $<, $/, Elt/binary, $>>>
    end;
flatten(L) when is_list(L) -> 
    iolist_to_binary([ flatten(A) || A <- L ]).

prefix(Sep, List) -> prefix(Sep,List,[]).
prefix(_Sep, [], Acc) -> lists:reverse(Acc);
prefix(Sep, [H|T], Acc) -> prefix(Sep, T, [H,Sep|Acc]).

%% @doc Flatten an attribute to a binary, filter urls and css.
flatten_attr({<<"style">>,Value}) ->
    Value1 = escape(filter_css(Value)),
    <<"style=\"", Value1/binary, $">>;
flatten_attr({<<"class">>,Value}) ->
    % Remove all do_xxxx widget manager classes
    Value1 = escape(filter_widget_class(Value)),
    <<"class=\"", Value1/binary, $">>;
flatten_attr({Attr,Value}) ->
    Value1 = case is_url_attr(Attr) of
                true -> noscript(Value, Attr =:= <<"href">>);
                false -> Value
            end,
    Value2 = escape(Value1),
    <<Attr/binary, $=, $", Value2/binary, $">>.

%% @doc Escape smaller-than, greater-than, single and double quotes in texts (&amp; is already removed or escaped).
escape_html_text(<<>>, Acc) -> 
    Acc;
escape_html_text(<<$<, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&lt;">>);
escape_html_text(<<$>, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&gt;">>);
escape_html_text(<<$", T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&quot;">>);
escape_html_text(<<$', T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, "&#39;">>);
escape_html_text(<<C, T/binary>>, Acc) ->
    escape_html_text(T, <<Acc/binary, C>>).

%% @doc Escape smaller-than, greater-than (for in comments)
escape_html_comment(<<>>, Acc) -> 
    Acc;
escape_html_comment(<<$<, T/binary>>, Acc) ->
    escape_html_comment(T, <<Acc/binary, "&lt;">>);
escape_html_comment(<<$>, T/binary>>, Acc) ->
    escape_html_comment(T, <<Acc/binary, "&gt;">>);
escape_html_comment(<<C, T/binary>>, Acc) ->
    escape_html_comment(T, <<Acc/binary, C>>).


%% @doc Elements that can only occur once in a nesting.
%% Used for cleaning up code from html editors.
allow_once(<<"a">>) -> true;
allow_once(<<"abbr">>) -> true;
allow_once(<<"area">>) -> true;
allow_once(<<"article">>) -> true;
allow_once(<<"b">>) -> true;
allow_once(<<"bdo">>) -> true;
allow_once(<<"big">>) -> true;
allow_once(<<"br">>) -> true;
allow_once(<<"cite">>) -> true;
allow_once(<<"code">>) -> true;
allow_once(<<"del">>) -> true;
allow_once(<<"dfn">>) -> true;
allow_once(<<"em">>) -> true;
allow_once(<<"hr">>) -> true;
allow_once(<<"i">>) -> true;
allow_once(<<"ins">>) -> true;
allow_once(<<"nav">>) -> true;
allow_once(<<"p">>) -> true;
allow_once(<<"pre">>) -> true;
allow_once(<<"q">>) -> true;
allow_once(<<"s">>) -> true;
allow_once(<<"small">>) -> true;
allow_once(<<"sub">>) -> true;
allow_once(<<"sup">>) -> true;
allow_once(<<"strong">>) -> true;
allow_once(<<"strike">>) -> true;
allow_once(<<"tt">>) -> true;
allow_once(<<"u">>) -> true;
allow_once(<<"var">>) -> true;
allow_once(_) -> false.

%% @doc Allowed elements (see also allow_once/1 above)
allow_elt(Elt, Extra) ->
    allow_elt(Elt) orelse lists:member(Elt, Extra).

allow_elt(<<"audio">>) -> true;
allow_elt(<<"address">>) -> true;
allow_elt(<<"bdo">>) -> true;
allow_elt(<<"blockquote">>) -> true;
allow_elt(<<"caption">>) -> true;
allow_elt(<<"col">>) -> true;
allow_elt(<<"colgroup">>) -> true;
allow_elt(<<"dd">>) -> true;
allow_elt(<<"dl">>) -> true;
allow_elt(<<"dt">>) -> true;
allow_elt(<<"div">>) -> true;
allow_elt(<<"h1">>) -> true;
allow_elt(<<"h2">>) -> true;
allow_elt(<<"h3">>) -> true;
allow_elt(<<"h4">>) -> true;
allow_elt(<<"h5">>) -> true;
allow_elt(<<"h6">>) -> true;
allow_elt(<<"header">>) -> true;
allow_elt(<<"img">>) -> true;
allow_elt(<<"li">>) -> true;
allow_elt(<<"legend">>) -> true;
allow_elt(<<"map">>) -> true;
allow_elt(<<"ol">>) -> true;
allow_elt(<<"samp">>) -> true;
allow_elt(<<"section">>) -> true;
allow_elt(<<"source">>) -> true;
allow_elt(<<"span">>) -> true;
allow_elt(<<"table">>) -> true;
allow_elt(<<"tbody">>) -> true;
allow_elt(<<"tfoot">>) -> true;
allow_elt(<<"thead">>) -> true;
allow_elt(<<"td">>) -> true;
allow_elt(<<"th">>) -> true;
allow_elt(<<"tr">>) -> true;
allow_elt(<<"ul">>) -> true;
allow_elt(<<"video">>) -> true;
allow_elt(_) -> false.

%% @doc Allowed attributes
allow_attr(Attr, Extra) ->
    allow_attr(Attr) orelse lists:member(Attr, Extra).

allow_attr(<<"align">>) -> true;
allow_attr(<<"alt">>) -> true;
allow_attr(<<"autoplay">>) -> true;
allow_attr(<<"border">>) -> true;
allow_attr(<<"borderspacing">>) -> true;
allow_attr(<<"cellpadding">>) -> true;
allow_attr(<<"cellspacing">>) -> true;
allow_attr(<<"class">>) -> true;
allow_attr(<<"colspan">>) -> true;
allow_attr(<<"controls">>) -> true;
allow_attr(<<"coords">>) -> true;
allow_attr(<<"dir">>) -> true;
allow_attr(<<"height">>) -> true;
allow_attr(<<"href">>) -> true;
%allow_attr(<<"id">>) -> true;
allow_attr(<<"loop">>) -> true;
allow_attr(<<"name">>) -> true;
allow_attr(<<"poster">>) -> true;
allow_attr(<<"preload">>) -> true;
allow_attr(<<"rel">>) -> true;
allow_attr(<<"rowspan">>) -> true;
allow_attr(<<"shape">>) -> true;
allow_attr(<<"src">>) -> true;
allow_attr(<<"style">>) -> true;
allow_attr(<<"target">>) -> true;
allow_attr(<<"title">>) -> true;
allow_attr(<<"usemap">>) -> true;
allow_attr(<<"valign">>) -> true;
allow_attr(<<"width">>) -> true;
allow_attr(_) -> false.

%% @doc Check if the attribute might contain an url
is_url_attr(<<"src">>) -> true;
is_url_attr(<<"href">>) -> true;
is_url_attr(<<"poster">>) -> true;
is_url_attr(_) -> false.

%% @doc Elements that shouldn't use a open and close tag.
is_selfclosing(<<"br">>) -> true;
is_selfclosing(<<"hr">>) -> true;
is_selfclosing(<<"img">>) -> true;
is_selfclosing(_) -> false.

%% @doc Disallowed elements whose contents should be skipped
skip_contents(<<"style">>) -> true;
skip_contents(<<"script">>) -> true;
skip_contents(<<"deleteme">>) -> true;
skip_contents(<<"head">>) -> true;
skip_contents(_) -> false.

%% @doc Run the CSS sanitizer over 'style' attributes. This is a strict sanitizer, all
%%      non-conforming css is rejected.
filter_css(undefined) -> [];
filter_css(<<>>) -> <<>>;
filter_css([]) -> [];
filter_css(Css) when is_binary(Css) -> 
    case z_css:sanitize_style(Css) of
        {ok, Css1} -> 
            Css1;
        {error, _Error} ->
            <<>>
    end.

%% @doc Remove all do_xxxx classes to prevent widget manager invocations
filter_widget_class(Class) ->
    z_convert:to_binary(re:replace(Class, <<"do_[0-9a-zA-Z_]+">>, <<>>, [global])).

%% @doc Filter a url, remove any "javascript:" and "data:" (as data can be text/html).
noscript(Url) ->
    noscript(Url, true).

%% @doc Filter an url, if strict then also remove "data:" (as data can be text/html).
noscript(Url, IsStrict) -> 
    case nows(z_convert:to_binary(Url), <<>>) of
        <<"script:", _/binary>> -> <<"#script-removed">>;
        <<"vbscript:", _/binary>> -> <<"#script-removed">>;
        <<"javascript:", _/binary>> -> <<"#script-removed">>;
        <<"data:", _/binary>> when IsStrict -> <<>>;
        <<"data:", Data/binary>> ->
            case noscript_data(Data) of
                <<>> -> <<>>;
                Data1 -> <<"data:", Data1/binary>>
            end;
        <<"mailto:", Rest/binary>> -> <<"mailto:", (z_string:trim(Rest))/binary>>;
        _ -> Url
    end.

%% @doc Remove whitespace and make lowercase till we find a colon or slash.
nows(<<>>, Acc) -> Acc;
nows(<<$:, Rest/binary>>, Acc) -> <<Acc/binary, $:, Rest/binary>>;
nows(<<$/, Rest/binary>>, Acc) -> <<Acc/binary, $/, Rest/binary>>;
nows(<<$\\, Rest/binary>>, Acc) -> nows(Rest, Acc);
nows(<<C, Rest/binary>>, Acc) when C =< 32 -> nows(Rest, Acc);
nows(<<C, Rest/binary>>, Acc) when C >= $A, C =< $Z -> nows(Rest, <<Acc/binary, (C+32)>>);
nows(<<C/utf8, Rest/binary>>, Acc) -> nows(Rest, <<Acc/binary, C/utf8>>).

%% @doc Sanitize the data link, drop anything suspected to be a script, or that could contain a script.
%% @todo Parse SVG with the svg sanitizer
noscript_data(<<"image/svg", _/binary>>) -> <<>>;
noscript_data(<<"image/", _/binary>> = Data) -> Data;
noscript_data(<<"audio/", _/binary>> = Data) -> Data;
noscript_data(<<"video/", _/binary>> = Data) -> Data;
noscript_data(<<"text/plain;", _/binary>> = Data) -> Data;
noscript_data(_) -> <<>>.


%% @doc Translate any html br entities to newlines.
br2nl(undefined) ->
    undefined;
br2nl({trans, Ts}) ->
    {trans, [ {Iso,br2nl(T)} || {Iso,T} <- Ts ]};
br2nl(B) when is_binary(B) ->
    br2nl_bin(B, <<>>);
br2nl(L) ->
    br2nl(L, []).

br2nl([], Acc) ->
    lists:reverse(Acc);
br2nl("<br/>" ++ Rest, Acc) ->
    br2nl(Rest, [$\n|Acc]);
br2nl("<br />" ++ Rest, Acc) ->
    br2nl(Rest, [$\n|Acc]);
br2nl([C | Rest], Acc) ->
    br2nl(Rest, [C | Acc]).

br2nl_bin(<<>>, Acc) ->
    Acc;
br2nl_bin(<<"<br/>", Post/binary>>, Acc) ->
    br2nl_bin(Post, <<Acc/binary, $\n>>);
br2nl_bin(<<"<br />", Post/binary>>, Acc) ->
    br2nl_bin(Post, <<Acc/binary, $\n>>);
br2nl_bin(<<C, Post/binary>>, Acc) ->
    br2nl_bin(Post, <<Acc/binary, C>>).


%% @doc Translate any newlines to html br entities.
nl2br(undefined) ->
    undefined;
nl2br({trans, Ts}) ->
    {trans, [ {Iso,nl2br(T)} || {Iso,T} <- Ts ]};
nl2br(B) when is_binary(B) ->
    nl2br_bin(B, <<>>);
nl2br(L) ->
    nl2br(L, []).

nl2br([], Acc) ->
    lists:reverse(Acc);
nl2br("\r\n" ++ Rest, Acc) ->
    nl2br(Rest, lists:reverse("<br />", Acc));
nl2br("\n" ++ Rest, Acc) ->
    nl2br(Rest, lists:reverse("<br />", Acc));
nl2br([C | Rest], Acc) ->
    nl2br(Rest, [C | Acc]).

nl2br_bin(<<>>, Acc) ->
    Acc;
nl2br_bin(<<$\r, $\n, Post/binary>>, Acc) ->
    nl2br_bin(Post, <<Acc/binary, "<br />">>);
nl2br_bin(<<$\r, Post/binary>>, Acc) ->
    nl2br_bin(Post, <<Acc/binary, "<br />">>);
nl2br_bin(<<$\n, Post/binary>>, Acc) ->
    nl2br_bin(Post, <<Acc/binary, "<br />">>);
nl2br_bin(<<C, Post/binary>>, Acc) ->
    nl2br_bin(Post, <<Acc/binary, C>>).


%% @doc Given a HTML list, scrape all `<link>' elements and return their attributes. Attribute names are lowercased.
%% @spec scrape_link_elements(string()) -> [LinkAttributes]
scrape_link_elements(Html) ->
    case re:run(Html, "<link[^>]+>", [global, caseless, {capture,all,binary}]) of
        {match, Elements} ->
            F = fun(El) ->
                        H = iolist_to_binary(["<p>", El, "</p>"]),
                        {<<"p">>, [], [{_, Attrs, []}]} = z_html_parse:parse(H),
                        [{z_string:to_lower(K),V} || {K,V} <- lists:flatten(Attrs)]
                end,
            [F(El) || [El] <- Elements];
        nomatch ->
            []
    end.


%% @doc Ensure that `&'-characters are properly escaped inside a html string.
ensure_escaped_amp(B) ->
    ensure_escaped_amp(B, <<>>).

ensure_escaped_amp(<<>>, Acc) ->
    Acc;
ensure_escaped_amp(<<"<!--", Rest/binary>>, Acc) ->
    case try_comment(Rest, <<Acc/binary, "<!--">>) of
        false -> Acc;
        {Rest1, Acc1} -> ensure_escaped_amp(Rest1, Acc1)
    end;
ensure_escaped_amp(<<$&, Rest/binary>>, Acc) ->
    case try_amp(Rest, in_amp, <<>>) of
        {Amp,Rest1} -> ensure_escaped_amp(Rest1, <<Acc/binary, $&, Amp/binary>>);
        false -> ensure_escaped_amp(Rest, <<Acc/binary, "&amp;">>)
    end;
ensure_escaped_amp(<<C, Rest/binary>>, Acc) ->
    ensure_escaped_amp(Rest, <<Acc/binary, C>>).


try_amp(<<$;,Rest/binary>>, in_ent_name, Acc) ->
    {<<Acc/binary,$;>>, Rest};
try_amp(<<$;,Rest/binary>>, in_ent_val, Acc) ->
    {<<Acc/binary,$;>>, Rest};
try_amp(<<$#,Rest/binary>>, in_amp, <<>>) -> 
    try_amp(Rest, in_ent_val, <<$#>>);
try_amp(<<C,Rest/binary>>, in_ent_val, Acc) ->
    case is_valid_ent_val(C) of
        true -> try_amp(Rest, in_ent_val, <<Acc/binary,C>>);
        false -> false
    end;
try_amp(<<C,Rest/binary>>, in_amp, <<>>) ->
    case is_valid_ent_char(C) of
        true -> try_amp(Rest, in_ent_name, <<C>>);
        false -> false
    end;
try_amp(<<C,Rest/binary>>, in_ent_name, Acc) ->
    case is_valid_ent_char(C) of
        true -> try_amp(Rest, in_ent_name, <<Acc/binary, C>>);
        false -> false
    end;
try_amp(_B, _, _Acc) -> 
    false.

try_comment(<<"-->", Rest/binary>>, Acc) ->
    {Rest, <<Acc/binary, "-->">>};
try_comment(<<C/utf8, Rest/binary>>, Acc) ->
    try_comment(Rest, <<Acc/binary, C/utf8>>);
try_comment(_B, _Acc) ->
    false.

is_valid_ent_char(C) ->
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z).

is_valid_ent_val(C) -> 
    (C >= $a andalso C =< $f) orelse (C >= $A andalso C =< $F)
    orelse (C >= $0 andalso C =< $9).

%% @doc Make all links (href/src) in the html absolute to the base URL
%%      For now this takes a shortcut by checking all ' (src|href)=".."'
abs_links(Html, Base) ->
    case re:run(Html, 
                <<"(src|href)=\"([^\"]*)\"">>,
                [global, notempty, {capture, all, binary}])
    of
        {match, Matches} -> replace_matched_links(Html, Matches, Base);
        nomatch -> Html
    end.

replace_matched_links(Html, [], _Base) ->
    Html;
replace_matched_links(Html, [[Found, Attr, Link]|Matches], Base) ->
    Html1 = case z_url:abs_link(Link, Base) of
                Link -> 
                    Html;
                AbsLink ->
                    New = iolist_to_binary([Attr, $=, $", AbsLink, $"]),
                    binary:replace(Html, Found, New)
            end,
    replace_matched_links(Html1, Matches, Base).
