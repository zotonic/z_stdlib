%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% Date: 2009-04-17
%%
%% @doc Utility functions for html processing.  Also used for property filtering (by m_rsc_update).

%% Copyright 2009-2012 Marc Worrell
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
    escape/2,
    escape/1,
    escape_props_check/1,
    escape_props_check/2,
    escape_check/2,
    escape_check/1,
    unescape/1,
    strip/1,
    sanitize/1,
    sanitize/2,
    noscript/1,
    sanitize_uri/1,
    escape_link/2,
    escape_link/1,
    nl2br/1,
    br2nl/1,
    scrape_link_elements/1,
    ensure_escaped_amp/1,
    abs_links/2
]).


%%% This is a Zotonic #context{}, which is still here for compatibility with Zotonic.
-opaque context() :: tuple().


%% @doc Escape all properties used for an update statement. Only leaves the body property intact.
-spec escape_props(list()) -> list().
escape_props(Props) ->
    escape_props1(Props, [], []).

-spec escape_props(list(), Options::list()|context()) -> list().
escape_props(Props, Context) ->
    escape_props1(Props, [], Context).

escape_props1([], Acc, _Options) ->
    Acc;
escape_props1([{_K,V} = Prop|T], Acc, Options) when is_float(V); is_integer(V); is_atom(V) -> 
    escape_props1(T, [Prop|Acc], Options);
escape_props1([{K, V}|T], Acc, Options) when K =:= body orelse K =:= body_extra->
    escape_props1(T, [{K, sanitize(V, Options)} | Acc], Options);
escape_props1([{summary, Summary}|T], Acc, Options) ->
    escape_props1(T, [{summary, nl2br(escape_value(Summary))} | Acc], Options);
escape_props1([{blocks, V}|T], Acc, Options) when is_list(V) ->
    V1 = [ escape_props1(L, [], Options) || L <- V ],
    escape_props1(T, [{blocks, V1}|Acc], Options);
escape_props1([{website, V}|T], Acc, Options) ->
    V1 = escape_value(sanitize_uri(V)),
    escape_props1(T, [{website, V1} | Acc], Options);
escape_props1([{K, V}|T], Acc, Options) ->
    EscapeFun = case lists:reverse(z_convert:to_list(K)) of
                    "lmth_" ++ _ -> fun(A) -> sanitize(A, Options) end; %% prop ends in '_html'
                    "iru_" ++ _ -> fun(A) -> escape_value(sanitize_uri(A)) end; %% prop ends in '_uri'
                    "lru_" ++ _ -> fun(A) -> escape_value(sanitize_uri(A)) end; %% prop ends in '_url'
                    _ -> fun escape_value/1
                end,
    escape_props1(T, [{K, EscapeFun(V)} | Acc], Options).

escape_value({trans, Texts}) ->
    {trans, escape_props(Texts)};
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
    escape_props_check1(Props, [], undefined).

-spec escape_props_check(list(), Options::list()|context()) -> list().
escape_props_check(Props, Options) ->
    escape_props_check1(Props, [], Options).

escape_props_check1([], Acc, _Options) ->
    Acc;
escape_props_check1([{_K,V} = Prop|T], Acc, Options) when is_float(V); is_integer(V); is_atom(V) -> 
    escape_props_check1(T, [Prop|Acc], Options);
escape_props_check1([{K, V}|T], Acc, Options) when K =:= body orelse K =:= body_extra->
    escape_props_check1(T, [{K, sanitize(V, Options)} | Acc], Options);
escape_props_check1([{summary, Summary}|T], Acc, Options) ->
    escape_props_check1(T, [{summary, nl2br(escape_value_check(br2nl(Summary)))} | Acc], Options);
escape_props_check1([{blocks, V}|T], Acc, Options) when is_list(V) ->
    V1 = [ escape_props_check1(L, [], Options) || L <- V ],
    escape_props_check1(T, [{blocks, V1}|Acc], Options);
escape_props_check1([{K, V}|T], Acc, Options) ->
    EscapeFun = case lists:reverse(z_convert:to_list(K)) of
                    "lmth_" ++ _ -> fun(A) -> sanitize(A, Options) end; %% prop ends in '_html'
                    _ -> fun escape_value/1
                end,
    escape_props_check1(T, [{K, EscapeFun(V)} | Acc], Options).

escape_value_check({trans, Texts}) ->
    {trans, escape_props_check(Texts)};
escape_value_check(V) when is_list(V) ->
    try
        escape_value_check(iolist_to_binary(V))
    catch _:_ ->
        V
    end;
escape_value_check(B) when is_binary(B) ->
    escape_check(B);
escape_value_check(V) -> 
    V.



%% @doc Escape a string so that it is valid within HTML/ XML.
-spec escape(list()|binary()|{trans, list()}, Options::list()) -> binary() | undefined.
escape(V, Options) when is_tuple(Options), element(1, Options) =:= context ->
    escape(z_trans:lookup_fallback(V, Options));
escape(V, _) ->
    escape(V).

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


%% @doc Checks if a string is properly escaped.
-spec escape_check(list()|binary()|{trans, list()}, Options::list()|context()) -> binary() | undefined.
escape_check(V, Options) when is_tuple(Options), element(1, Options) =:= context ->
    escape_check(z_trans:lookup_fallback(V, Options));
escape_check(V, _Options) ->
    escape_check(V).

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
    escape(list_to_binary(L));
escape_check(B) when is_binary(B) ->
    escape_check1(B, <<>>).

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
-spec escape_link(list()|binary()|{trans, list()}, Options::list()|context()) -> binary() | undefined.
escape_link(V, Options) when is_tuple(Options), element(1, Options) =:= context ->
    escape_link(z_trans:lookup_fallback(V, Options));
escape_link(V, _Options) ->
    escape_link(V).

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
    NoScript = noscript(Link),
    Link1 = escape(NoScript),
    Link2 = escape(ensure_protocol(NoScript)),
    make_links1(Offset+Len, Rest, Text1, [["<a href=\"",Link2,"\" rel=\"nofollow\">",Link1,"</a>"] | Acc]);
make_links1(Offset, [{MatchOffs,_}|_] = Matches, Text, Acc) ->
    {Text1,Text2} = lists:split(MatchOffs-Offset, Text),
    make_links1(MatchOffs, Matches, Text2, [escape(Text1)|Acc]).

ensure_protocol([]) ->
    [];
ensure_protocol("#" ++ _ = Link) ->
    Link;
ensure_protocol("www" ++ Rest) ->
    ["http://www", Rest];
ensure_protocol(Link) ->
    Link.


%% @doc Ensure that an uri is (quite) harmless by removing any script reference
sanitize_uri(Uri) ->
    ensure_protocol(noscript(Uri)).


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
    strip(list_to_binary(L)).

strip(<<>>, _, Acc) -> Acc;
strip(<<$<,T/binary>>, in_text, Acc) ->
    strip(T, in_tag, Acc);
strip(<<$>,T/binary>>, in_tag, <<>>) ->
    strip(T, in_text, <<>>);
strip(<<$>>>, in_tag, Acc) -> 
    Acc;
strip(<<$>,T/binary>>, in_tag, Acc) ->
    strip(T, in_text, <<Acc/binary, 32>>);
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
    ExtraAttrs = case Options of
                    T when is_tuple(T), element(1, T) =:= context -> 
                        m_config:get_value(site, html_attr_extra, <<>>, Options);
                    _ ->
                        proplists:get_value(attr_extra, Options, [])
                 end,
    ExtraElts =  case Options of
                    T2 when is_tuple(T2), element(1, T2) =:= context -> 
                         m_config:get_value(site, html_elt_extra, <<>>, Options);
                    _ ->
                        proplists:get_value(elt_extra, Options, [])
                 end,
    ExtraAttrs1 = case is_binary(ExtraAttrs) of
                      true -> binary:split(ExtraAttrs, <<",">>, [global]);
                      false -> ExtraAttrs
                  end,
    ExtraElts1 = case is_binary(ExtraElts) of
                      true -> binary:split(ExtraElts, <<",">>, [global]);
                      false -> ExtraElts
                  end,
    sanitize1(Html, ExtraElts1, ExtraAttrs1, Options).

sanitize1(Html, ExtraElts, ExtraAttrs, Options) ->
    Parsed = mochiweb_html:parse(ensure_escaped_amp(Html)),
    Sanitized = sanitize(Parsed, [], ExtraElts, ExtraAttrs, Options),
    flatten(Sanitized).

sanitize(B, _Stack, _ExtraElts, _ExtraAttrs, _Options) when is_binary(B) ->
    escape(B);
sanitize({comment, _Text} = Comment, _Stack, _ExtraElts, _ExtraAttrs, Options) ->
    case Options of
        T when is_tuple(T), element(1, T) =:= context -> 
            z_notifier:foldl(sanitize_element, Comment, Options);
        _ ->
            case proplists:get_value(element, Options) of
                undefined -> Comment;
                F when is_function(F) -> F(Comment);
                {M,F,A} -> erlang:apply(M, F, [Comment|A])
            end
    end;
sanitize({pi, _Raw}, _Stack, _ExtraElts, _ExtraAttrs, _Options) ->
    <<>>;
sanitize({pi, _Tag, _Attrs}, _Stack, _ExtraElts, _ExtraAttrs, _Options) ->
    <<>>;
sanitize({Elt,Attrs,Enclosed}, Stack, ExtraElts, ExtraAttrs, Options) ->
    Lower = list_to_binary(z_string:to_lower(Elt)),
    case allow_elt(Lower, ExtraElts) orelse (not lists:member(Lower, Stack) andalso allow_once(Lower)) of
        true ->
            Attrs1 = lists:filter(fun({A,_}) -> allow_attr(A, ExtraAttrs) end, Attrs),
            Attrs2 = [ {list_to_binary(z_string:to_lower(A)), V} || {A,V} <- Attrs1 ],
            Stack1 = [Lower|Stack],
            Tag = { Lower, 
                    Attrs2,
                    [ sanitize(Encl, Stack1, ExtraElts, ExtraAttrs, Options) || Encl <- Enclosed ]},
            case Options of
                T when is_tuple(T), element(1, T) =:= context -> 
                    z_notifier:foldl(sanitize_element, Tag, Options);
                _ ->
                    case proplists:get_value(element, Options) of
                        undefined -> Tag;
                        F when is_function(F) -> F(Tag);
                        {M,F,A} -> erlang:apply(M, F, [Tag|A])
                    end
            end;
        false ->
            case skip_contents(Lower) of
                false ->
                    {nop, [ sanitize(Encl, Stack, ExtraElts, ExtraAttrs, Options) || Encl <- Enclosed ]};
                true ->
                    {nop, []}
            end
    end.

%% @doc Flatten the sanitized html tree to a binary 
flatten(B) when is_binary(B) ->
    escape_html_text(B, <<>>);
flatten({nop, Enclosed}) ->
    flatten(Enclosed);
flatten({comment, Text}) ->
    Comment = escape_html_comment(Text, <<>>),
    <<"<!--", Comment/binary, "-->">>;
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

%% @doc Flatten an attribute to a binary
%% @todo Filter javascript from the value (when there is a ':' then only allow http/https)
%% @todo Strip scripting and text css attributes
%% css: anything within () should be removed
flatten_attr({<<"style">>,Value}) ->
    Value1 = escape(filter_css(Value)),
    <<"style=\"", Value1/binary, $">>;
flatten_attr({<<"class">>,Value}) ->
    % Remove all do_xxxx widget manager classes
    Value1 = escape(filter_widget_class(Value)),
    <<"class=\"", Value1/binary, $">>;
flatten_attr({Attr,Value}) ->
    Value1 = case is_url_attr(Attr) of
                true -> noscript(Value);
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

%% @doc Simple filter for css. Removes parts between () and quoted strings. 
filter_css(undefined) ->
    [];
filter_css(<<>>) ->
    <<>>;
filter_css([]) ->
    [];
filter_css(Html) when is_binary(Html) ->
    filter_css(Html, in_text, <<>>);
filter_css(L) when is_list(L) ->
    filter_css(list_to_binary(L)).

filter_css(<<>>, _, Acc) -> Acc;
filter_css(<<$(,T/binary>>, in_text, Acc) ->
    filter_css(T, in_paren, <<Acc/binary,$(>>);
filter_css(<<$),T/binary>>, in_paren, Acc) ->
    filter_css(T, in_text, <<Acc/binary,$)>>);
filter_css(<<$),T/binary>>, State, Acc) ->
    filter_css(T, State, Acc);
filter_css(<<_,T/binary>>, in_paren, Acc) ->
    filter_css(T, in_paren, Acc);
filter_css(<<$",T/binary>>, in_text, Acc) ->
    filter_css(T, in_dstring, <<Acc/binary,$">>);
filter_css(<<$",T/binary>>, in_dstring, Acc) ->
    filter_css(T, in_text, <<Acc/binary,$">>);
filter_css(<<$',T/binary>>, in_text, Acc) ->
    filter_css(T, in_sstring, <<Acc/binary,$'>>);
filter_css(<<$',T/binary>>, in_sstring, Acc) ->
    filter_css(T, in_text, <<Acc/binary,$'>>);
filter_css(<<$\\,_,T/binary>>, in_sstring, Acc) ->
    filter_css(T, in_sstring, Acc);
filter_css(<<$\\,_,T/binary>>, in_dstring, Acc) ->
    filter_css(T, in_dstring, Acc);
filter_css(<<$\\,H,T/binary>>, in_text, Acc) ->
    filter_css(T, in_text, <<Acc/binary,H>>);
filter_css(<<H,T/binary>>, in_text, Acc) ->
    filter_css(T, in_text, <<Acc/binary, H>>);
filter_css(<<_,T/binary>>, State, Acc) ->
    filter_css(T, State, Acc).

%% @doc Remove all do_xxxx classes to prevent widget manager invocations
filter_widget_class(Class) ->
    z_convert:to_binary(re:replace(Class, <<"do_[0-9a-zA-Z_]+">>, <<>>, [global])).

%% @doc Filter a url, remove any javascript.
noscript(Url) -> 
    case nows(z_convert:to_list(Url), []) of
        "script:" ++ _ -> <<"#script-removed">>;
        "javascript:" ++ _ -> <<"#script-removed">>;
        _ -> Url
    end.

%% @doc Remove whitespace and make lowercase till we find a colon or slash.
nows([], Acc) -> lists:reverse(Acc);
nows([C|_] = L, Acc) when C =:= $:; C =:= $/ -> lists:reverse(Acc, L);
nows([C|T], Acc) when C =< 32 -> nows(T,Acc);
nows([C|T], Acc) when C >= $A, C =< $Z -> nows(T, [C+32|Acc]);
nows([$\\|T], Acc) -> nows(T, Acc);
nows([C|T], Acc) -> nows(T, [C|Acc]).


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
    case re:run(Html, "<link[^>]+>", [global, caseless, {capture,all,list}]) of
        {match, Elements} ->
            F = fun(El) ->
                        H = iolist_to_binary(["<p>", El, "</p>"]),
                        {<<"p">>, [], [{_, Attrs, []}]} = mochiweb_html:parse(H),
                        [{z_string:to_lower(binary_to_list(K)),binary_to_list(V)} || {K,V} <- lists:flatten(Attrs)]
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
