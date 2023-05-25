%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell
%% @doc Utility functions for html processing.  Also used for property filtering (by m_rsc_update).
%% @end

%% Copyright 2009-2022 Marc Worrell
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
    strip/2,
    truncate/2,
    truncate/3,
    sanitize/1,
    sanitize/2,
    sanitize/4,
    noscript/1,
    noscript/2,
    sanitize_uri/1,
    escape_link/1,
    nl2br/1,
    br2nl/1,
    scrape_link_elements/1,
    ensure_escaped_amp/1,
    abs_links/2
]).

-type text() :: iodata() | {trans, list( {atom(), binary()} )}.
-type unsafe_text() :: iodata()
                     | {trans, list( {atom(), iodata()} )}
                     | {trans, list( {binary(), iodata()} )}
                     | {trans, map()}.
-type maybe_text() :: undefined | text().
-type maybe_unsafe_text() :: undefined | unsafe_text().
-type maybe_binary() :: undefined | binary().
-type maybe_iodata() :: undefined | iodata().

-type sanitize_options() :: [ sanitize_option() ].
-type sanitize_option() :: {elt_extra, list( binary() )}
                         | {attr_extra, list( binary() )}
                         | {element, function()}.

-export_type([
    text/0,
    unsafe_text/0,
    maybe_text/0,
    maybe_unsafe_text/0,
    maybe_binary/0,
    maybe_iodata/0,

    sanitize_options/0,
    sanitize_option/0
]).


% Used by z_svg.erl
% @todo: move this to separate erlang module
-export([
    flatten_attr/1,
    sanitize_attr_value/2,
    escape_html_text/2,
    escape_html_comment/2
    ]).


%% @doc Escape all properties used for an update statement. Only leaves the body property intact.
-spec escape_props(list() | map()) -> list() | map().
escape_props(Props) ->
    escape_props(Props, []).

-spec escape_props(list() | map(), Options::list()) -> list() | map().
escape_props(Props, Options) when is_list(Props) ->
    lists:map(
        fun({P, V}) ->
            V1 = escape_props1(z_convert:to_binary(P), V, Options),
            {P, V1}
        end,
        Props);
escape_props(Props, Options) when is_map(Props) ->
    maps:fold(
        fun(K, V, Acc) ->
            K1 = z_convert:to_binary(K),
            Acc#{ K1 => escape_props1(K1, V, Options)}
        end,
        #{},
        Props).

escape_props1(_K, null, _Options) ->
    null;
escape_props1(_K, undefined, _Options) ->
    undefined;
escape_props1(_K, V, _Options) when is_number(V); is_boolean(V) ->
    V;
escape_props1(K, V, Options) when is_atom(V) ->
    V1 = atom_to_binary(V, utf8),
    case escape_props1(K, V1, Options) of
        V1 -> V;
        _ -> undefined
    end;
escape_props1(<<"body", _/binary>>, V, Options) ->
    sanitize(V, Options);
escape_props1(<<"summary">>, Summary, _Options) ->
    nl2br(escape_value(Summary));
escape_props1(<<"blocks">>, V, Options) when is_list(V) ->
    [ escape_props(L, Options) || L <- V ];
escape_props1(<<"website">>, V, _Options) ->
    escape_value(sanitize_uri(V));
escape_props1(<<"@id">>, V, _Options) ->
    escape_value(sanitize_uri(V));
escape_props1(<<"is_a", _/binary>>, V, Options) ->
    sanitize_list([], V, Options);
escape_props1(<<"is_", _/binary>>, V, _Options) ->
    z_convert:to_bool(V);
escape_props1(K, V, Options) ->
    [Type|Ks] = lists:reverse(binary:split(K, <<"_">>, [global])),
    sanitize_type(Type, Ks, V, Options).

sanitize_type(<<"html">>, _Ks, V, Options) -> sanitize(V, Options);
sanitize_type(<<"uri">>, _Ks, V, _Options) -> escape_value(sanitize_uri(V));
sanitize_type(<<"url">>, _Ks, V, _Options) -> escape_value(sanitize_uri(V));
sanitize_type(<<"list">>, Ks, V, Options) -> sanitize_list(Ks, V, Options);
sanitize_type(<<"int">>, _Ks, V, _Options) -> sanitize_int(V);
sanitize_type(<<"id">>, _Ks, undefined, _Options) -> undefined;
sanitize_type(<<"id">>, _Ks, <<>>, _Options) -> undefined;
sanitize_type(<<"id">>, _Ks, V, _Options) when V =/= undefined ->
    try
        z_convert:to_integer(V)
    catch
        _:_ -> escape_value(V)
    end;
sanitize_type(<<"unsafe">>, _Ks, V, _Options) -> V;
sanitize_type(_, _Ks, V, Options) when is_map(V) -> escape_props(V, Options);
sanitize_type(_, Ks, V, Options) when is_list(V) -> sanitize_list(Ks, V, Options);
sanitize_type(_, _Ks, V, _Options) -> escape_value(V).

sanitize_list(Ks, L, Options) when is_list(L) ->
    lists:map(
        fun
            ({P, V}) ->
                P1 = z_convert:to_binary(P),
                V1 = escape_props1(P1, V, Options),
                {P1, V1};
            (V) when is_list(V), Ks =:= [] ->
                escape_props(V, Options);
            (V) when is_map(V) ->
                escape_props(V, Options);
            (V) when Ks =:= [] ->
                escape_value(V);
            (V) ->
                [Type|Ks1] = Ks,
                sanitize_type(Type, Ks1, V, Options)
        end,
        L);
sanitize_list(Ks, Map, Options) when is_map(Map) ->
    sanitize_list(Ks, maps:to_list(Map), Options);
sanitize_list(_Ks, undefined, _Options) ->
    undefined;
sanitize_list(Ks, V, Options) ->
    sanitize_list(Ks, [V], Options).

sanitize_int(V) ->
    try
        z_convert:to_integer(V)
    catch
        _:_ -> undefined
    end.

escape_value(undefined) -> undefined;
escape_value(null) -> null;
escape_value(V) when is_boolean(V) -> V;
escape_value(V) when is_number(V) -> V;
escape_value(V) when is_atom(V) ->
    V1 = atom_to_binary(V, utf8),
    case escape_value(V1) of
        V1 -> V;
        _ -> undefined
    end;
escape_value({trans, _Ts} = Tr) ->
    escape(Tr);
escape_value(V) when is_list(V) ->
    try
        escape_value(unicode:characters_to_binary(V))
    catch _:_ ->
        V
    end;
escape_value(B) when is_binary(B) ->
    escape(B);
escape_value(V) ->
    V.

%% @doc Checks if all properties are properly escaped
-spec escape_props_check(list() | map()) -> list() | map().
escape_props_check(Props) ->
    escape_props_check(Props, []).

-spec escape_props_check(list() | map(), Options::list()) -> list() | map().
escape_props_check(Props, Options) when is_list(Props) ->
    lists:map(
        fun
            ({P, V}) ->
                V1 = escape_props_check1(z_convert:to_binary(P), V, Options),
                {P, V1};
            (V) when is_list(V); is_map(V)->
                escape_props_check(V, Options);
            (V) ->
                escape_value_check(V)
        end,
        Props);
escape_props_check(Props, Options) when is_map(Props) ->
    maps:map(
        fun(P, V) ->
            escape_props_check1(z_convert:to_binary(P), V, Options)
        end,
        Props).


escape_props_check1(_K, null, _Options) ->
    null;
escape_props_check1(_K, undefined, _Options) ->
    undefined;
escape_props_check1(_K, V, _Options) when is_number(V); is_boolean(V) ->
    V;
escape_props_check1(K, V, Options) when is_atom(V) ->
    V1 = atom_to_binary(V, utf8),
    case escape_props_check1(K, V1, Options) of
        V1 -> V;
        _ -> undefined
    end;
escape_props_check1(<<"body", _/binary>>, V, Options) ->
    sanitize(V, Options);
escape_props_check1(<<"summary">>, Summary, _Options) ->
    nl2br(escape_check(br2nl(Summary)));
escape_props_check1(<<"blocks">>, V, Options) when is_list(V) ->
    [ escape_props_check(L, Options) || L <- V ];
escape_props_check1(<<"website">>, V, _Options) ->
    escape_value(sanitize_uri(unescape(V)));
escape_props_check1(<<"@id">>, V, _Options) ->
    escape_value(sanitize_uri(unescape(V)));
escape_props_check1(<<"is_a">>, L, Options) when is_list(L) ->
    sanitize_list_check(L, Options);
escape_props_check1(<<"is_", _/binary>>, V, _Options) ->
    z_convert:to_bool(V);
escape_props_check1(K, V, Options) ->
    Type = lists:last(binary:split(K, <<"_">>, [global])),
    sanitize_type_check(Type, V, Options).

sanitize_type_check(<<"html">>, V, Options) -> sanitize(V, Options);
sanitize_type_check(<<"uri">>, V, _Options) -> escape_value(sanitize_uri(unescape(V)));
sanitize_type_check(<<"url">>, V, _Options) -> escape_value(sanitize_uri(unescape(V)));
sanitize_type_check(<<"list">>, V, Options) -> sanitize_list_check(V, Options);
sanitize_type_check(<<"int">>, V, _Options) -> sanitize_int(V);
sanitize_type_check(<<"unsafe">>, V, _Options) -> V;
sanitize_type_check(_, V, Options) when is_map(V) -> escape_props_check(V, Options);
sanitize_type_check(_, V, Options) when is_list(V) -> escape_props_check(V, Options);
sanitize_type_check(_, V, _Options) -> escape_value_check(V).


sanitize_list_check(L, Options) when is_list(L) ->
    lists:map(
        fun
            ({P, V}) ->
                P1 = z_convert:to_binary(P),
                V1 = escape_props_check1(P1, V, Options),
                {P1, V1};
            (V) when is_list(V); is_map(V)->
                escape_props_check(V, Options);
            (V) ->
                escape_value_check(V)
        end,
        L);
sanitize_list_check(Map, Options) when is_map(Map) ->
    sanitize_list_check(maps:to_list(Map), Options);
sanitize_list_check(undefined, _Options) ->
    undefined;
sanitize_list_check(V, Options) ->
    sanitize_list_check([V], Options).


escape_value_check(undefined) -> undefined;
escape_value_check(null) -> null;
escape_value_check(V) when is_boolean(V) -> V;
escape_value_check(V) when is_number(V) -> V;
escape_value_check(V) when is_atom(V) ->
    V1 = atom_to_binary(V, utf8),
    case escape_check(V1) of
        V1 -> V;
        _ -> undefined
    end;
escape_value_check({trans, _Ts} = Tr) ->
    escape_check(Tr);
escape_value_check(V) when is_list(V) ->
    try
        escape_check(unicode:characters_to_binary(V))
    catch _:_ ->
        V
    end;
escape_value_check(B) when is_binary(B) ->
    escape_check(B);
escape_value_check(V) ->
    V.


%% @doc Escape a string so that it is valid within HTML/ XML.
-spec escape( maybe_unsafe_text() ) -> maybe_text().
escape({trans, Tr}) when is_list(Tr) ->
    Tr1 = lists:filtermap(
        fun
            ({Lang, V}) when is_atom(Lang) ->
                V1 = z_convert:to_binary(V),
                {true, {Lang, escape(V1)}};
            ({Lang, V}) when is_binary(Lang) ->
                try
                    Lang1 = binary_to_existing_atom(Lang, utf8),
                    V1 = z_convert:to_binary(V),
                    {true, {Lang1, escape(V1)}}
                catch _:_ ->
                    false
                end;
            (_) ->
                false
        end,
        Tr),
    {trans, Tr1};
escape({trans, Tr}) when is_map(Tr) ->
    escape({trans, maps:to_list(Tr)});
escape({trans, _}) ->
    <<>>;
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


%% @doc Ensure that a string is escaped so that it is valid within HTML/ XML.
-spec escape_check( maybe_unsafe_text() ) -> maybe_text().
escape_check({trans, Tr}) when is_list(Tr) ->
    Tr1 = lists:filtermap(
        fun
            ({Lang, V}) when is_atom(Lang) ->
                V1 = z_convert:to_binary(V),
                {true, {Lang, escape_check(V1)}};
            ({Lang, V}) when is_binary(Lang) ->
                try
                    Lang1 = binary_to_existing_atom(Lang, utf8),
                    V1 = z_convert:to_binary(V),
                    {true, {Lang1, escape_check(V1)}}
                catch _:_ ->
                    false
                end;
            (_) ->
                false
        end,
        Tr),
    {trans, Tr1};
escape_check({trans, Tr}) when is_map(Tr) ->
    escape_check({trans, maps:to_list(Tr)});
escape_check({trans, _}) ->
    <<>>;
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
-spec unescape( maybe_text() ) -> maybe_text().
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
-spec escape_link( maybe_iodata() ) -> maybe_binary().
escape_link(undefined) ->
    undefined;
escape_link(<<>>) ->
    <<>>;
escape_link("") ->
    <<>>;
escape_link(Text) when is_binary(Text) ->
    case re:run(Text,
            "(("
                "mailto:"
                    "[-a-zA-Z0-9_\\.\\(\\)\\+=%~]+"
                    "@"
                "|"
                    "ftp://|http://|https://|www\\."
            ")"
            "[-a-zA-Z0-9]+(\\.[-a-zA-Z0-9]+)+"
            "(/[-/_a-zA-Z0-9\\.:\\+%;~]*)?"
            "(\\?[-_a-zA-Z0-9\\.:\\+%=&;\\$/~]*)?"
              "(#[-_a-zA-Z0-9\\.:\\+%=&;\\$/~]*)?)",
            [{capture, first}, global])
    of
        {match, Matches} ->
            Matches1 = [ hd(M) || M <- Matches ],
            Parts = split_in_links(lists:reverse(Matches1), Text, []),
            Linked = lists:map(fun make_link/1, Parts),
            nl2br(iolist_to_binary(Linked));
        nomatch ->
            nl2br(escape(Text))
    end;
escape_link(Text) ->
    escape_link(iolist_to_binary(Text)).

make_link(B) when is_binary(B) ->
    escape(B);
make_link({link, Link}) ->
    NoScript = noscript(Link, true),
    LinkText = escape(NoScript),
    LinkUrl = escape(ensure_protocol(NoScript)),
    <<
        "<a href=\"",
        LinkUrl/binary,
        "\" rel=\"noopener nofollow noreferrer\">",
        LinkText/binary,
        "</a>"
    >>.

split_in_links([], Text, Acc) ->
    [ Text | Acc ];
split_in_links([ {Offset, Len}|Matches ], Text, Acc) ->
    <<Before:Offset/binary, Link:Len/binary, Rest/binary>> = Text,
    Acc1 = [ {link, Link}, Rest | Acc ],
    split_in_links(Matches, Before, Acc1).

ensure_protocol(<<>>) -> <<>>;
ensure_protocol(<<"#", _/binary>> = Link) -> Link;
ensure_protocol(<<"?", _/binary>> = Link) -> Link;
ensure_protocol(<<"/", _/binary>> = Link) -> Link;
ensure_protocol(<<"://", _/binary>> = Link) -> <<"http", Link/binary>>;
ensure_protocol(<<"http:", _/binary>> = Link) -> Link;
ensure_protocol(<<"https:", _/binary>> = Link) -> Link;
ensure_protocol(<<"data:", _/binary>> = Link) -> Link;
ensure_protocol(<<"urn:", _/binary>> = Link) -> Link;
ensure_protocol(<<"ftp:", _/binary>> = Link) -> Link;
ensure_protocol(<<"mailto:", Rest/binary>>) -> <<"mailto:", (z_string:trim(Rest))/binary>>;
ensure_protocol(<<"www.", _/binary>> = Link) -> <<"https://", Link/binary>>;
ensure_protocol(Link) when is_binary(Link) ->
    case binary:match(Link, <<":">>) of
        nomatch ->
            % If the first path element looks like a domain name then
            % make it a https: link. Otherwise add a '/' in front.
            [First|_] = binary:split(Link, <<"/">>),
            case binary:match(First, <<".">>) of
                nomatch -> <<$/, Link/binary>>;
                _Match -> <<"https://", Link/binary>>
            end;
        _ ->
            Link
    end.

%% @doc Ensure that an uri is (quite) harmless by removing any script reference
-spec sanitize_uri( maybe_iodata() ) -> maybe_binary().
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

%% @doc Strip all html elements from the text. Simple parsing is applied to find the elements.
%%      Does not escape the end result.
-spec strip( maybe_text() ) -> binary().
strip(Text) ->
    strip(Text, nolimit).

%% @doc Strip all html elements from the text. Simple parsing is applied to find the elements.
%%      Does not escape the end result. Limit the length of the result string to N characters.
-spec strip( maybe_text(), integer() | nolimit ) -> binary().
strip(undefined, _N) ->
    <<>>;
strip(<<>>, _N) ->
    <<>>;
strip("", _N) ->
    <<>>;
strip({trans, Tr}, N) ->
    {trans, [{Lang, strip(V, N)} || {Lang,V} <- Tr]};
strip(Html, N) when is_binary(Html) ->
    strip(Html, <<>>, N);
strip(L, N) when is_list(L) ->
    strip(iolist_to_binary(L), N);
strip(V, N) ->
    strip(z_convert:to_binary(V), N).

strip(_, Acc, N) when is_integer(N), N =< 0 ->
    Acc;
strip(<<>>, Acc, _N) ->
    Acc;
strip(<<"<wbr>",T/binary>>, Acc, N) ->
    strip(T, Acc, N);
strip(<<"</span>",T/binary>>, Acc, N) ->
    strip(T, Acc, N);
strip(<<"</a>",T/binary>>, Acc, N) ->
    strip(T, Acc, N);
strip(<<"<",T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N);
strip(<<H/utf8,T/binary>>, Acc, N) ->
    strip(T, <<Acc/binary, H/utf8>>, sub1(N));
strip(<<_,T/binary>>, Acc, N) ->
    % Drop non-utf8 data
    strip(T, Acc, N).

strip_tag(<<>>, Acc, _N) ->
    Acc;
strip_tag(<<">">>, Acc, _N) ->
    Acc;
strip_tag(<<">", WS, T/binary>>, Acc, N) when WS =< 32 ->
    strip(T, <<Acc/binary, WS>>, sub1(N));
strip_tag(<<">", T/binary>>, <<>>, N) ->
    strip(T, <<>>, N);
strip_tag(<<">", T/binary>>, Acc, N) ->
    case T of
        <<"</", _/binary>> ->
            strip(T, Acc, N);
        _ ->
            case binary:last(Acc) of
                C when C =< 32 ->
                    strip(T, Acc, N);
                _ ->
                    strip(T, <<Acc/binary, " ">>, sub1(N))
            end
    end;
strip_tag(<<$",T/binary>>, Acc, N) ->
    strip_dstring(T, Acc, N);
strip_tag(<<$',T/binary>>, Acc, N) ->
    strip_sstring(T, Acc, N);
strip_tag(<<_, T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N).

strip_dstring(<<>>, Acc, _) ->
    Acc;
strip_dstring(<<$\\, _, T/binary>>, Acc, N) ->
    strip_dstring(T, Acc, N);
strip_dstring(<<$",T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N);
strip_dstring(<<_,T/binary>>, Acc, N) ->
    strip_dstring(T, Acc, N).

strip_sstring(<<>>, Acc, _) ->
    Acc;
strip_sstring(<<$\\, _, T/binary>>, Acc, N) ->
    strip_sstring(T, Acc, N);
strip_sstring(<<$',T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N);
strip_sstring(<<_,T/binary>>, Acc, N) ->
    strip_sstring(T, Acc, N).

sub1(nolimit) -> nolimit;
sub1(N) -> N-1.


%% @doc Truncate a previously sanitized HTML string.
-spec truncate( maybe_text(), integer() ) -> maybe_text().
truncate(Html,Length) ->
    truncate(Html, Length, <<>>).

-spec truncate( maybe_text(), integer(), iodata() ) -> maybe_text().
truncate(undefined, _Length, _Append) ->
    undefined;
truncate(_, Length, _Append) when Length =< 0 ->
    <<>>;
truncate(<<>>, _Length, _Append) ->
    <<>>;
truncate("", _Length, _Append) ->
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
-spec sanitize( maybe_text() ) -> maybe_text().
sanitize(Html) ->
    sanitize(Html, []).


-spec sanitize( maybe_text(), sanitize_options() ) -> maybe_text().
sanitize(undefined, _Options) ->
    undefined;
sanitize({trans, Tr}, Options) ->
    {trans, [{Lang, sanitize(V, Options)} || {Lang,V} <- Tr]};
sanitize(<<>>, _Options) ->
    <<>>;
sanitize("", _Options) ->
    <<>>;
sanitize(Html, Options) when is_binary(Html) ->
    sanitize_opts(<<"<sanitize>",Html/binary,"</sanitize>">>, Options);
sanitize(Html, Options) when is_list(Html) ->
    sanitize_opts(iolist_to_binary(["<sanitize>", Html, "</sanitize>"]), Options).

sanitize_opts(Html, Options) ->
    sanitize1(Html, proplists:get_value(elt_extra, Options, []),
        proplists:get_value(attr_extra, Options, []), Options).

sanitize1(Html, ExtraElts, ExtraAttrs, Options) ->
    case z_html_parse:parse(ensure_escaped_amp(Html)) of
        {ok, Parsed} ->
            Sanitized = sanitize(Parsed, ExtraElts, ExtraAttrs, Options),
            flatten(Sanitized);
        {error, _} ->
            <<>>
    end.

%% @doc Sanitize a mochiwebparse tree. Remove harmful elements and attributes.
-spec sanitize( z_html_parse:html_element(), binary() | list(), binary() | list(), any()) -> z_html_parse:html_element().
sanitize(ParseTree, ExtraElts, ExtraAttrs, Options) when is_binary(ExtraElts) ->
    sanitize(ParseTree, binary:split(ExtraElts, <<",">>, [global]), ExtraAttrs, Options);
sanitize(ParseTree, ExtraElts, ExtraAttrs, Options) when is_binary(ExtraAttrs) ->
    sanitize(ParseTree, ExtraElts, binary:split(ExtraAttrs, <<",">>, [global]), Options);
sanitize(ParseTree, ExtraElts, ExtraAttrs, Options) ->
    sanitize(ParseTree, [], ExtraElts, ExtraAttrs, Options).

sanitize({<<"li">>, _, _} = Elt, [], ExtraElts, ExtraAttrs, Options) ->
    sanitize({<<"ul">>, [], [ Elt ]}, [], ExtraElts, ExtraAttrs, Options);
sanitize({<<"li">>, _, _} = Elt, [ParentElt | _ ] = Stack, ExtraElts, ExtraAttrs, Options)
    when ParentElt =/= <<"ul">>, ParentElt =/= <<"ol">> ->
    sanitize({<<"ul">>, [], [ Elt ]}, Stack, ExtraElts, ExtraAttrs, Options);
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


%% @doc Flatten the sanitized html tree to a binary - the attributes are already filtered
%% using the allow_attr/1 whitelist.
-spec flatten( z_html_parse:html_element() ) -> binary().
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
    Attrs1 = sanitize_attrs(Attrs),
    Attrs2 = [flatten_attr(Attr) || Attr <- Attrs1 ],
    Attrs3 = iolist_to_binary(prefix(32, Attrs2)),
    case is_selfclosing(Elt) andalso EncBin == <<>> of
        true ->  <<$<, Elt/binary, Attrs3/binary, 32, $/, $>>>;
        false -> <<$<, Elt/binary, Attrs3/binary, $>, EncBin/binary, $<, $/, Elt/binary, $>>>
    end;
flatten(L) when is_list(L) ->
    iolist_to_binary([ flatten(A) || A <- L ]).

prefix(Sep, List) -> prefix(Sep,List,[]).
prefix(_Sep, [], Acc) -> lists:reverse(Acc);
prefix(Sep, [H|T], Acc) -> prefix(Sep, T, [H,Sep|Acc]).


sanitize_attrs(Attrs) ->
    Attrs1 = lists:map(
        fun({Attr, Value}) ->
            {Attr, sanitize_attr_value(Attr, Value)}
        end,
        Attrs),
    case lists:keymember(<<"target">>, 1, Attrs1) of
        true ->
            % Add 'rel="nofollow noopener noreferrer"' to all
            % elements with a 'target' attribute, where the href
            % is not a local link.
            case proplists:get_value(<<"href">>, Attrs1) of
                <<"#", _/binary>> -> Attrs1;
                <<"/", _/binary>> -> Attrs1;
                _ ->
                    Rel = proplists:get_value(<<"rel">>, Attrs1, <<>>),
                    Rels = re:split(Rel, <<"\\s">>),
                    Rels1 = [ R || R <- Rels, R =/= <<>> ],
                    Rels2 = Rels1 -- [
                        <<"follow">>,
                        <<"opener">>,
                        <<"referrer">>,
                        <<"nofollow">>,
                        <<"noopener">>,
                        <<"noreferrer">>
                    ],
                    Rels3 = [ <<"nofollow">>, <<"noopener">>, <<"noreferrer">> | Rels2 ],
                    Rels4 = iolist_to_binary( lists:join(32, Rels3) ),
                    [ {<<"rel">>, Rels4} | proplists:delete(<<"rel">>, Attrs1) ]
            end;
        false ->
            Attrs1
    end.

sanitize_attr_value(<<"style">>, V) ->
    filter_css(V);
sanitize_attr_value(<<"class">>, V) ->
    % Remove all do_xxxx widget manager classes
    filter_widget_class(V);
sanitize_attr_value(<<"href">>, V) ->
    noscript(V, true);
sanitize_attr_value(Attr, V) ->
    case is_url_attr(Attr) of
        true -> noscript(V, false);
        false -> V
    end.

%% @doc Flatten an attribute, attributes have been whitelisted and
%% the values have been sanitized.
flatten_attr({Attr,Value}) ->
    Value1 = escape(Value),
    <<Attr/binary, $=, $", Value1/binary, $">>.

%% @doc Escape smaller-than, greater-than, single and double quotes in texts
%% (&amp; is already removed or escaped).
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
allow_once(<<"aside">>) -> true;
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
allow_once(<<"kbd">>) -> true;
allow_once(<<"nav">>) -> true;
allow_once(<<"p">>) -> true;
allow_once(<<"pre">>) -> true;
allow_once(<<"q">>) -> true;
allow_once(<<"s">>) -> true;
allow_once(<<"samp">>) -> true;
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
allow_elt(<<"figcaption">>) -> true;
allow_elt(<<"figure">>) -> true;
allow_elt(<<"h1">>) -> true;
allow_elt(<<"h2">>) -> true;
allow_elt(<<"h3">>) -> true;
allow_elt(<<"h4">>) -> true;
allow_elt(<<"h5">>) -> true;
allow_elt(<<"h6">>) -> true;
allow_elt(<<"header">>) -> true;
allow_elt(<<"img">>) -> true;
allow_elt(<<"legend">>) -> true;
allow_elt(<<"li">>) -> true;
allow_elt(<<"map">>) -> true;
allow_elt(<<"ol">>) -> true;
allow_elt(<<"picture">>) -> true;
allow_elt(<<"samp">>) -> true;
allow_elt(<<"section">>) -> true;
allow_elt(<<"source">>) -> true;
allow_elt(<<"span">>) -> true;
allow_elt(<<"time">>) -> true;
allow_elt(<<"table">>) -> true;
allow_elt(<<"tbody">>) -> true;
allow_elt(<<"tfoot">>) -> true;
allow_elt(<<"thead">>) -> true;
allow_elt(<<"td">>) -> true;
allow_elt(<<"th">>) -> true;
allow_elt(<<"tr">>) -> true;
allow_elt(<<"ul">>) -> true;
allow_elt(<<"video">>) -> true;
allow_elt(<<"wbr">>) -> true;
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

%% @doc Elements that shouldn't use an open and close tag.
is_selfclosing(<<"br">>) -> true;
is_selfclosing(<<"hr">>) -> true;
is_selfclosing(<<"img">>) -> true;
is_selfclosing(<<"wbr">>) -> true;
is_selfclosing(_) -> false.

%% @doc Disallowed elements whose contents should be skipped
skip_contents(<<"style">>) -> true;
skip_contents(<<"script">>) -> true;
skip_contents(<<"deleteme">>) -> true;
skip_contents(<<"head">>) -> true;
skip_contents(_) -> false.

%% @doc Run the CSS sanitizer over 'style' attributes. This is a strict sanitizer, all
%%      non-conforming css is rejected.
-spec filter_css( maybe_iodata() ) -> binary().
filter_css(undefined) -> <<>>;
filter_css(<<>>) -> <<>>;
filter_css("") -> <<>>;
filter_css(L) when is_list(L) ->
    filter_css(iolist_to_binary(L));
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
-spec noscript(Url) -> SafeUrl when
    Url :: string() | binary(),
    SafeUrl :: binary().
noscript(Url) ->
    noscript(Url, true).

%% @doc Filter an url, if strict then also remove "data:" (as data can be text/html).
-spec noscript(Url, IsStrict) -> SafeUrl when
    Url :: string() |  binary(),
    IsStrict :: boolean(),
    SafeUrl :: binary().
noscript(Url0, IsStrict) ->
    Url = z_string:trim(z_string:sanitize_utf8(z_convert:to_binary(Url0))),
    case nows(Url, <<>>) of
        {<<"javascript">>, _} -> <<"#script-removed">>;
        {<<"script">>, _} -> <<"#script-removed">>;
        {<<"vbscript">>, _} -> <<"#script-removed">>;
        {<<"data">>, _} when IsStrict -> <<>>;
        {<<"data">>, Data} ->
            case noscript_data(Data) of
                <<>> -> <<>>;
                Data1 -> <<"data:", Data1/binary>>
            end;
        {<<"mailto">>, Rest} -> <<"mailto:", (z_string:trim(Rest))/binary>>;
        {Protocol, Rest} when is_binary(Protocol) -> <<Protocol/binary, $:, Rest/binary>>;
        {undefined, <<>>} -> <<>>;
        {undefined, _} -> Url
    end.

%% @doc Remove whitespace and make lowercase till we find a colon, slash or pound-sign. Also
%% deletes all invalid utf8 characters.
-spec nows( binary(), binary() ) -> {binary()|undefined, binary()}.
nows(<<>>, Acc) -> {undefined, Acc};
nows(<<$:, Rest/binary>>, Acc) -> {Acc, Rest};
nows(<<$/, Rest/binary>>, Acc) -> {undefined, <<Acc/binary, $/, Rest/binary>>};
nows(<<$#, Rest/binary>>, Acc) -> {undefined, <<Acc/binary, $#, Rest/binary>>};
nows(<<$\\, Rest/binary>>, Acc) -> nows(Rest, Acc);
nows(<<$%, A, B, Rest/binary>>, Acc) ->
    case catch erlang:binary_to_integer(<<A, B>>, 16) of
        V when is_integer(V) -> nows(<<V, Rest/binary>>, Acc);
        _ -> {undefined, <<>>}
    end;
nows(<<$%, _/binary>>, _Acc) ->
    % Illegal: not enough characters left for escape sequence
    {undefined, <<>>};
nows(<<C, Rest/binary>>, Acc) when C =< 32 ->
    % Discard control characters
    nows(Rest, Acc);
nows(<<C, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
    % Ensure lowercase a-z
    nows(Rest, <<Acc/binary, (C+32)>>);
nows(<<C/utf8, Rest/binary>>, Acc) ->
    nows(Rest, <<Acc/binary, C/utf8>>);
nows(<<_, Rest/binary>>, Acc) ->
    % Discard non utf8 characters
    nows(Rest, Acc).

%% @doc Sanitize the data link, drop anything suspected to be a script, or that could contain a script.
%% @todo Parse SVG with the svg sanitizer
noscript_data(<<"image/svg", _/binary>>) -> <<>>;
noscript_data(<<"image/", _/binary>> = Data) -> Data;
noscript_data(<<"audio/", _/binary>> = Data) -> Data;
noscript_data(<<"video/", _/binary>> = Data) -> Data;
noscript_data(<<"text/plain;", _/binary>> = Data) -> Data;
noscript_data(_) -> <<>>.


%% @doc Translate any html br entities to newlines.
-spec br2nl( maybe_text() ) -> maybe_text().
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
-spec nl2br( maybe_text() ) -> maybe_text().
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
-spec scrape_link_elements( iodata() ) -> list( [ z_html_parse:html_attr() ] ).
scrape_link_elements(Html) ->
    case re:run(Html, "<link[^>]+>", [global, caseless, {capture,all,binary}]) of
        {match, Elements} ->
            F = fun(El) ->
                    H = iolist_to_binary(["<p>", El, "</p>"]),
                    case z_html_parse:parse(H) of
                        {ok, {<<"p">>, [], [{_, Attrs, []}]}} ->
                            [ {z_string:to_lower(K),V} || {K,V} <- lists:flatten(Attrs) ];
                        {error, _} ->
                            []
                    end
                end,
            [ F(El) || [El] <- Elements ];
        nomatch ->
            []
    end.


%% @doc Ensure that `&'-characters are properly escaped inside a html string.
-spec ensure_escaped_amp( maybe_binary() ) -> binary().
ensure_escaped_amp(undefined) ->
    <<>>;
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
%%      This takes a shortcut by checking all ' (src|href)=".."'
-spec abs_links( maybe_iodata(), binary() ) -> iodata().
abs_links(undefined, _Base) ->
    <<>>;
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
