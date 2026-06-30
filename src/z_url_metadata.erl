%% @author Marc Worrell
%% @copyright 2014-2026 Marc Worrell
%% @doc Discover metadata about an url. Follows redirects
%% and URL shorteners, and then fetches the data at the final URL
%% to inspect for metadata tags, content headers and the first part of the HTML.
%%
%% The returned opaque metadata can be questioned for properties using p/2.
%%
%% The Slackbot user-agent is used for fetching URLs so that the URL shorteners
%% return a location header and other sites are coerced to give correct metadata.
%%
%% Only the first MB of data is fetched, this prevents fetching large objects.
%% @end

%% Copyright 2014-2026 Marc Worrell
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

-module(z_url_metadata).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    fetch/1,
    fetch/2,
    fetch_data/2,
    fetch_data/3,
    html_meta/1,
    p/2,
    header/2,
    filename/2
    ]).

-include("../include/z_url_metadata.hrl").


-type metadata() :: #url_metadata{}.
-type property() :: mime | mime_options | site_name | content_length |
    url | canonical_url | short_url | final_url | links |
    headers | title | h1 | summary | tags | filename | json_ld |
    mtitle | description | keywords | author | charset | language |
    image | image_nav | thumbnail |
    icon | icon_nav | icon_shortcut | icon_touch |
    binary().

-export_type([ metadata/0 ]).

% User-agent used for metadata sniffing - we pretend to be Slack so
% that some websites with bot-protection allow us to sniff the metadata.
-define(USER_AGENT, <<"Slackbot-LinkExpanding 1.0 (+https://api.slack.com/robots)">>).

% Per default we fetch max 1MB of data to analyze.
% We need to fetch this much as (for example) Youtube adds a lot of css/scripts
% above the metadata of the page.
-define(FETCH_LENGTH, 1024*1024).

% Below this size an image is considered too small to be a representative image or icon.
-define(IMG_SMALL_SIZE, 16).


%% @doc Fetch metadata information for the URL with default fetch options.
-spec fetch(binary()|string()) -> {ok, metadata()} | {error, term()}.
fetch(Url) ->
    fetch(Url, []).

%% @doc Fetch metadata information for the URL, with url fetch options. The data of the
%% URL is fetched partially, with a default maximum length of 1MB. The returned metadata
%% is extracted from the fetched data and http headers.
-spec fetch(binary()|string(), z_url_fetch:options()) -> {ok, metadata()} | {error, term()}.
fetch(Url, Options) ->
    Options1 = case proplists:is_defined(max_length, Options) of
        true -> Options;
        false -> [ {max_length, ?FETCH_LENGTH} | Options ]
    end,
    Options2 = case proplists:is_defined(user_agent, Options1) of
        true -> Options1;
        false -> [ {user_agent, ?USER_AGENT} | Options1 ]
    end,
    case z_url_fetch:fetch_partial(Url, Options2) of
        {ok, {FinalUrl, Headers, _Size, Data}} ->
            {ok, partial_metadata(FinalUrl, Headers, Data)};
        {error, _} = Error ->
            Error
    end.

%% @doc Parse metadata from the given headers and data, if an empty header
%% list is given, then a header with content-type html is added.
%%
%% This compatibility variant has no source URL, so callers that need correct
%% normalization of relative metadata values should use fetch_data/3 and pass
%% the final/base URL of the fetched content.
-spec fetch_data(Headers, Data) -> {ok, metadata()} when
    Headers :: list(),
    Data :: binary().
fetch_data([], Data) ->
    Hs = [ {<<"content-type">>, <<"text/html">>} ],
    fetch_data(<<"https://example.com/">>, Hs, Data);
fetch_data(Hs, Data) ->
    fetch_data(<<"https://example.com/">>, Hs, Data).

%% @doc Parse metadata from the given base/final URL, headers and data.
%% If an empty header list is given, then a header with content-type html is added.
-spec fetch_data(binary()|string(), Headers, Data) -> {ok, metadata()} when
    Headers :: list(),
    Data :: binary().
fetch_data(BaseUrl, [], Data) ->
    Hs = [ {<<"content-type">>, <<"text/html">>} ],
    fetch_data(BaseUrl, Hs, Data);
fetch_data(BaseUrl, Hs, Data) when is_list(BaseUrl) ->
    fetch_data(unicode:characters_to_binary(BaseUrl), Hs, Data);
fetch_data(BaseUrl, Hs, Data) ->
    {ok, partial_metadata(BaseUrl, Hs, Data)}.


%% @doc Fetch properties of the fetched metadata
-spec p(Property, Metadata) -> Value when
    Property :: property() | [ property() ],
    Metadata :: metadata(),
    Value :: binary() | list( binary() ) | Headers | Links | JsonLDs | undefined,
    Headers :: list({binary(), binary()}),
    Links :: #{binary() => [map()]},
    JsonLDs :: [map()].
p(mime, MD) ->
    MD#url_metadata.content_type;
p(mime_options, MD) ->
    MD#url_metadata.content_type_options;
p(final_url, MD) ->
    MD#url_metadata.final_url;
p(url, MD) ->
    case p1([<<"og:url">>, <<"twitter:url">>, canonical_url, short_url], MD) of
        undefined -> MD#url_metadata.final_url;
        PrefUrl -> z_url:abs_link(PrefUrl, MD#url_metadata.final_url)
    end;
p(site_name, MD) ->
    case p1([<<"og:site_name">>, <<"twitter:site">>], MD) of
        undefined ->
            Url = case p1([canonical_url], MD) of
                undefined -> MD#url_metadata.final_url;
                Canonical -> Canonical
            end,
            case uri_string:parse(Url) of
                #{ host := Host } ->
                    case unicode:characters_to_binary(Host) of
                        <<"www.", H/binary>> -> H;
                        H -> H
                    end;
                {error, _, _} ->
                    undefined
            end;
        Sitename ->
            Sitename
    end;
p(content_length, MD) ->
    MD#url_metadata.content_length;
p(headers, MD) ->
    MD#url_metadata.headers;
p(links, MD) ->
    MD#url_metadata.links;
p(json_ld, MD) ->
    MD#url_metadata.json_ld;
p(title, MD) ->
    case p1([<<"og:title">>, <<"twitter:title">>, mtitle, h1, title], MD) of
        undefined -> p(filename, MD);
        Title -> Title
    end;
p(summary, MD) ->
    p1([<<"og:description">>, <<"twitter:description">>, description], MD);
p(image, MD) ->
    case MD#url_metadata.content_type of
        <<"image/", _/binary>> ->
            MD#url_metadata.final_url;
        _ ->
            Ps = case MD#url_metadata.is_index_page of
                    true ->
                        [<<"twitter:image:src">>, <<"twitter:image">>, <<"og:image">>,
                         image_nav, image];
                    false ->
                        [<<"twitter:image:src">>, <<"twitter:image">>, <<"og:image">>,
                         image, image_nav]
                 end,
            case p1(Ps, MD) of
                undefined -> undefined;
                ImgSrc -> z_url:abs_link(ImgSrc, MD#url_metadata.final_url)
            end
    end;
p(icon, MD) ->
    case MD#url_metadata.content_type of
        <<"image/", _/binary>> ->
            MD#url_metadata.final_url;
        _ ->
            Ps = case MD#url_metadata.is_index_page of
                    true ->
                        [image_nav, icon_touch, icon_shortcut, icon_fav];
                    false ->
                        [icon_touch, image_nav, icon_shortcut, icon_fav]
                 end,
            case p1(Ps, MD) of
                undefined -> undefined;
                ImgSrc -> z_url:abs_link(ImgSrc, MD#url_metadata.final_url)
            end
    end;
p(tags, MD) ->
    case p1([keywords], MD) of
        undefined ->
            % Check og tags? (youtube uses og:video:tag)
            [];
        KW ->
            Ks = [ z_string:trim(K) || K <- binary:split(KW, <<",">>, [global]) ],
            [ K || K <- Ks, K =/= <<>> ]
    end;
p(filename, MD) ->
    filename(MD#url_metadata.final_url, MD#url_metadata.headers);
p(Ks, MD) when is_list(Ks) ->
    p1(Ks, MD);
p(K, MD) ->
    p1([K], MD).

-spec header( binary() | string(), metadata() ) -> binary() | undefined.
header(H, #url_metadata{ headers = Hs }) ->
    proplists:get_value(z_convert:to_binary(H), Hs).

-spec filename(binary()|string(), list()) -> binary() | undefined.
filename(Url, Hs) ->
    case content_disp_filename(proplists:get_value(<<"content-disposition">>, Hs)) of
        undefined -> basename(Url);
        FN -> z_convert:to_binary(FN)
    end.

%% ------------------------------------------------ Internal Functions ------------------------------------------------

%% Find the first defined property
p1([], _MD) ->
    undefined;
p1([P|Ps], MD) ->
    case proplists:get_value(P, MD#url_metadata.metadata) of
        undefined -> p1(Ps, MD);
        Value ->
            case z_string:trim(Value) of
                <<>> -> p1(Ps, MD);
                Trimmed -> maybe_abs_link(is_link_property(P), Trimmed, MD#url_metadata.final_url)
            end
    end.

maybe_abs_link(false, Value, _FinalUrl) ->
    Value;
maybe_abs_link(true, <<>>, _FinalUrl) ->
    undefined;
maybe_abs_link(true, Value, FinalUrl) ->
    Url1 = z_url:abs_link(Value, FinalUrl),
    z_html:sanitize_uri(Url1).

is_link_property(canonical_url) -> true;
is_link_property(short_url) -> true;
is_link_property(image_nav) -> true;
is_link_property(image) -> true;
is_link_property(icon_nav) -> true;
is_link_property(icon_shortcut) -> true;
is_link_property(_) -> false.


content_disp_filename(undefined) ->
    undefined;
content_disp_filename(Vs) ->
    {_Disp, Options} = parse_header(Vs),
    case proplists:get_value(<<"filename">>, Options) of
        undefined -> undefined;
        <<>> -> undefined;
        FN -> FN
    end.

basename(<<"data:", _/binary>>) ->
    undefined;
basename("data:" ++ _) ->
    undefined;
basename(Url) ->
    #{ path := Path } = uri_string:parse( z_convert:to_binary(Url) ),
    case Path of
        <<>> -> undefined;
        <<"/">> -> undefined;
        _ ->
            case lists:last( binary:split(Path, <<"/">>, [ global ]) ) of
                <<>> ->
                    undefined;
                Basename ->
                    % Perform percent-decode of the path
                    try
                        z_url:url_decode(Basename)
                    catch
                        _:_ ->
                            Basename
                    end
            end
    end.


%% ------------------------------------------------ From Mochiweb ------------------------------------------------

%% author Bob Ippolito <bob@mochimedia.com>
%% copyright 2007 Mochi Media, Inc.

%% @doc  Parse a Content-Type like header, return the main Content-Type
%%       and a property list of options.
-spec parse_header( binary() ) -> {binary(), [ {binary(), binary()} ]}.
parse_header(String) ->
    %% TODO: This is exactly as broken as Python's cgi module.
    %%       Should parse properly like mochiweb_cookies.

    [Type | Parts] = [z_string:trim(S) || S <- binary:split(String, <<";">>, [ global ])],
    F = fun (S, Acc) ->
                case binary:split(S, <<"=">>) of
                    [<<>>, _] ->
                        %% Skip anything with no name
                        Acc;
                    [_, <<>>] ->
                        %% Skip anything with no value
                        Acc;
                    [_] ->
                        Acc;
                    [Name, Value] ->
                        [{z_string:to_lower(z_string:trim(Name)),
                          unquote_header(z_string:trim(Value))} | Acc]
                end
        end,
    {Type, lists:foldr(F, [], Parts)}.

unquote_header(<<"\"", Rest/binary>>) ->
    unquote_header(Rest, <<>>);
unquote_header(S) ->
    S.

unquote_header(<<>>, Acc) ->
    Acc;
unquote_header(<<"\"">>, Acc) ->
    Acc;
unquote_header(<<$\\, C, Rest/binary>>, Acc) ->
    unquote_header(Rest, <<Acc/binary, C>>);
unquote_header(<<C, Rest/binary>>, Acc) ->
    unquote_header(Rest, <<Acc/binary, C>>).


%% -------------------------------------- Analyze fetched data -----------------------------------------

-record(ps, { in_nav = false }).

partial_metadata(Url, Hs, Data) ->
    HsBin = lists:foldr(
        fun({H, V}, Acc) ->
            HBin = z_convert:to_binary(H),
            VBin = z_convert:to_binary(V),
            HLower = z_string:to_lower(HBin),
            case HLower =:= HBin of
                true ->
                    [{HLower, VBin} | Acc];
                false ->
                    [{HLower, VBin}, {HBin, VBin} | Acc]
            end
        end,
        [],
        Hs
    ),
    {CT, CTOpts} = content_type(HsBin),
    IsText = is_text(CT, Data),
    IsHTML = IsText andalso is_html(CT),
    Data1 = maybe_convert_utf8(IsText, IsHTML, proplists:get_value(<<"charset">>, CTOpts), Data),
    MetadataList = html_meta(IsHTML, Data1),
    {JsonLDList, MetadataList0} = lists:partition(fun({P, _}) -> P =:= json_ld end, MetadataList),
    {LinkList, MetadataList1} = lists:partition(fun({P, _}) -> P =:= link end, MetadataList0),
    JsonLDs = lists:append([ LDs || {json_ld, LDs} <- JsonLDList ]),
    Links = lists:foldr(
        fun({link, {Rel, As}}, Acc) ->
            Acc#{
                Rel => [ As | maps:get(Rel, Acc, []) ]
            }
        end,
        #{},
        LinkList),
    Links1 = header_links(HsBin, Links),
    #url_metadata{
        final_url = z_convert:to_binary(Url),
        content_type = CT,
        content_type_options = CTOpts,
        content_length = content_length(HsBin),
        metadata = MetadataList1,
        links = Links1,
        json_ld = JsonLDs,
        is_index_page = is_index_page(Url),
        headers = HsBin,
        partial_data = Data
    }.

is_index_page(Url) ->
    case uri_string:parse( z_convert:to_binary(Url) ) of
        #{ query := _ } -> false;
        #{ path := <<>> } -> true;
        #{ path := <<"/">> } -> true;
        #{ path := <<"/index.", _/binary>> } -> true;
        #{ path := <<"/default.htm">> } -> true;
        #{ path := <<"/Default.htm">> } -> true;
        #{ path := <<"index.", _/binary>> } -> true;
        #{ path := <<"default.htm">> } -> true;
        #{ path := <<"Default.htm">> } -> true;
        _ -> false
    end.

html_meta(Data) ->
    html_meta(true, Data).

html_meta(true, PartialData) ->
    case parse(PartialData) of
        {ok, Parsed} ->
            JsonLDs = json_ld(Parsed),
            Metadata = lists:reverse(html(Parsed, [], #ps{})),
            case JsonLDs of
                [] -> Metadata;
                _ -> [{json_ld, JsonLDs} | Metadata]
            end;
        {error, _} ->
            []
    end;
html_meta(false, _PartialData) ->
    [].

parse(PartialData) when is_binary(PartialData) ->
    parse_html(<<"<partial>", PartialData/binary, "</partial>">>);
parse(PartialData) when is_list(PartialData) ->
    parse_html(iolist_to_binary([<<"<partial>">>, PartialData, <<"</partial>">>])).

parse_html(Html) ->
    z_html_parse:parse(Html).


html([], MD, _P) ->
    MD;
html([B|Es], MD, P) when is_binary(B) ->
    html(Es, MD, P);
html([{comment, _}|Es], MD, P) ->
    % <!-- ... -->
    html(Es, MD, P);
html([{pi, _Xml, _Attrs}|Es], MD, P) ->
    % <?xml version="1.0" encoding="UTF-8"?>
    html(Es, MD, P);
html([Tag|Es], MD, P) ->
    {MD1, P1} = tag(Tag, MD, P),
    html(Es, MD1, P1);
html(Tag, MD, P) when is_tuple(Tag) ->
    {MD1, _} = tag(Tag, MD, P),
    MD1.

tag({<<"html">>, As, Es}, MD, P) ->
    MD1 = case proplists:get_value(<<"lang">>, As) of
              undefined -> MD;
              Lang -> [{language, Lang} | MD]
          end,
    {html(Es, MD1, P), P};
tag({<<"meta">>, As, _}, MD, P) ->
    Name = z_string:to_lower(proplists:get_value(<<"name">>, As)),
    Property = proplists:get_value(<<"property">>, As),
    HttpEquiv = proplists:get_value(<<"http-equiv">>, As),
    Value = proplists:get_value(<<"value">>, As),
    Content = proplists:get_value(<<"content">>, As, Value),
    case first([Name, Property, HttpEquiv]) of
        undefined ->
            case proplists:get_value(<<"charset">>, As) of
                undefined -> {MD, P};
                Charset -> {[{charset,Charset} | MD], P}
            end;
        Prop ->
            {meta_tag(Prop, Content, MD), P}
    end;
tag({<<"title">>, _As, Es}, MD, P) ->
    Text = z_string:trim(fetch_text(Es, <<>>)),
    {[{title, Text} | MD], P};
tag({<<"link">>, As, _}, MD, P) ->
    Rel = z_string:to_lower(proplists:get_value(<<"rel">>, As)),
    case Rel of
        <<>> ->
            {MD, P};
        _ ->
            HRef = case proplists:get_value(<<"href">>, As) of
                undefined -> undefined;
                H -> z_string:trim(H)
            end,
            MD1 = meta_link(Rel, HRef, As, MD),
            MD2 = links(Rel, HRef, As, MD1),
            {MD2, P}
    end;
tag({<<"img">>, As, _}, MD, P) ->
    case proplists:get_value(<<"src">>, As, <<>>) of
        <<>> ->
            {MD, P};
        Src ->
            case is_img_allowed(Src, As) of
                true ->
                    case P#ps.in_nav of
                        true -> {[{image_nav, Src} | MD], P};
                        false -> {[{image, Src} | MD], P}
                    end;
                false ->
                    {MD, P}
            end
    end;
tag({<<"h1">>, _As, Es}, MD, #ps{in_nav=false} = P) ->
    case proplists:is_defined(h1, MD) of
        false ->
            Text = z_string:trim(fetch_text(Es, <<>>)),
            {[{h1, Text} | MD], P};
        true ->
            {MD, P}
    end;
tag({<<"h1">>, _As, _Es}, MD, P) ->
    {MD, P};
tag({<<"nav">>, _As, Es}, MD, P) ->
    {html(Es, MD, P#ps{in_nav=true}), P};
tag({<<"header">>, _As, Es}, MD, P) ->
    {html(Es, MD, P#ps{in_nav=true}), P};
tag({<<"footer">>, _As, Es}, MD, P) ->
    {html(Es, MD, P#ps{in_nav=true}), P};
tag({<<"aside">>, _As, Es}, MD, P) ->
    {html(Es, MD, P#ps{in_nav=true}), P};
tag({_Tag, As, Es}, MD, P) ->
    Cs = split_class(proplists:get_value(<<"class">>, As)),
    Id = proplists:get_value(<<"id">>, As),
    case is_ads(Id, Cs) of
        true ->
            {MD, P};
        false ->
            {html(Es, MD, P#ps{in_nav = P#ps.in_nav orelse has_nav_class(Cs) orelse is_topbar_id(Id)}), P}
    end.

meta_tag(_Name, undefined, MD) -> MD;
meta_tag(_Name, <<>>, MD) -> MD;
meta_tag(<<"og:", _/binary>> = OG, Content, MD) -> [{OG, Content}|MD];
meta_tag(<<"twitter:", _/binary>> = Tw, Content, MD) -> [{Tw, Content}|MD];
meta_tag(<<"al:", _/binary>> = Al, Content, MD) -> [{Al, Content}|MD];
meta_tag(<<"title">>, Content, MD) -> [{mtitle, Content}|MD];
meta_tag(<<"keywords">>, Content, MD) -> [{keywords, Content}|MD];
meta_tag(<<"description">>, Content, MD) -> [{description, Content}|MD];
meta_tag(<<"author">>, Content, MD) -> [{author, Content}|MD];
meta_tag(<<"thumbnail">>, Content, MD) -> [{thumbnail, Content}|MD];
meta_tag(<<"content-type">>, Content, MD) -> [{content_type, Content}|MD];
meta_tag(<<"duration">>, Content, MD) -> [{duration, Content}|MD];
meta_tag(<<"datepublished">>, Content, MD) -> [{date_published, Content}|MD];
meta_tag(<<"uploaddate">>, Content, MD) -> [{date_uploaded, Content}|MD];
meta_tag(<<"embedurl">>, Content, MD) -> [{embed_url, Content}|MD];
meta_tag(<<"contenturl">>, Content, MD) -> [{content_url, Content}|MD];
meta_tag(_Name, _Content, MD) -> MD.

json_ld(Es) ->
    json_ld(Es, []).

json_ld([], Acc) ->
    lists:reverse(Acc);
json_ld([E|Es], Acc) ->
    json_ld(Es, json_ld(E, Acc));
json_ld({comment, _}, Acc) ->
    Acc;
json_ld({pi, _Xml, _Attrs}, Acc) ->
    Acc;
json_ld({_, As, _} = Tag, Acc) ->
    case proplists:is_defined(<<"itemscope">>, As) of
        true ->
            {_Object, Acc1} = json_ld_item(Tag, Acc),
            Acc1;
        false ->
            json_ld_children(Tag, Acc)
    end;
json_ld(_Text, Acc) ->
    Acc.

json_ld_children({_Tag, _As, Es}, Acc) ->
    json_ld(Es, Acc).

json_ld_item({_Tag, As, Es}, Acc) ->
    Object0 = json_ld_item_base(As),
    {Object, Acc1} = json_ld_props(Es, Object0, Acc),
    Acc2 = case proplists:get_value(<<"itemid">>, As) of
        undefined -> Acc1;
        <<>> -> Acc1;
        _ -> [Object | Acc1]
    end,
    {Object, Acc2}.

json_ld_item_base(As) ->
    Object0 = case normalize_itemtype(proplists:get_value(<<"itemtype">>, As)) of
        undefined ->
            #{};
        {schema, Context, Type} ->
            #{ <<"@context">> => Context, <<"@type">> => Type };
        {type, Type} ->
            #{ <<"@type">> => Type }
    end,
    case proplists:get_value(<<"itemid">>, As) of
        undefined -> Object0;
        <<>> -> Object0;
        ItemId -> Object0#{ <<"@id">> => ItemId }
    end.

json_ld_props([], Object, Acc) ->
    {Object, Acc};
json_ld_props([E|Es], Object, Acc) ->
    {Object1, Acc1} = json_ld_prop(E, Object, Acc),
    json_ld_props(Es, Object1, Acc1);
json_ld_props(_Text, Object, Acc) ->
    {Object, Acc}.

json_ld_prop({comment, _}, Object, Acc) ->
    {Object, Acc};
json_ld_prop({pi, _Xml, _Attrs}, Object, Acc) ->
    {Object, Acc};
json_ld_prop({_Tag, As, _Es} = Tag, Object, Acc) ->
    ItemProp = proplists:get_value(<<"itemprop">>, As),
    HasScope = proplists:is_defined(<<"itemscope">>, As),
    case {itemprop_names(ItemProp), HasScope} of
        {[], true} ->
            {_Nested, Acc1} = json_ld_item(Tag, Acc),
            {Object, Acc1};
        {[], false} ->
            json_ld_props_child(Tag, Object, Acc);
        {Props, true} ->
            {Nested, Acc1} = json_ld_item(Tag, Acc),
            {json_ld_add_props(Props, Nested, Object), Acc1};
        {Props, false} ->
            {json_ld_add_props(Props, itemprop_value(Tag), Object), Acc}
    end;
json_ld_prop(_Text, Object, Acc) ->
    {Object, Acc}.

json_ld_props_child({_Tag, _As, Es}, Object, Acc) ->
    json_ld_props(Es, Object, Acc).

itemprop_names(undefined) ->
    [];
itemprop_names(<<>>) ->
    [];
itemprop_names(ItemProp) ->
    [ Prop || Prop <- binary:split(ItemProp, <<" ">>, [global]), Prop =/= <<>> ].

json_ld_add_props([], _Value, Object) ->
    Object;
json_ld_add_props([Prop|Props], Value, Object) ->
    Object1 = case is_empty_itemprop_value(Value) of
        true ->
            Object;
        false ->
            Object#{ Prop => json_ld_append_value(maps:get(Prop, Object, undefined), Value) }
    end,
    json_ld_add_props(Props, Value, Object1).

is_empty_itemprop_value(undefined) -> true;
is_empty_itemprop_value(<<>>) -> true;
is_empty_itemprop_value(_) -> false.

json_ld_append_value(undefined, Value) ->
    Value;
json_ld_append_value(Values, Value) when is_list(Values) ->
    Values ++ [Value];
json_ld_append_value(OldValue, Value) ->
    [OldValue, Value].

itemprop_value({_Tag, As, Es}) ->
    case first([
        proplists:get_value(<<"content">>, As),
        proplists:get_value(<<"href">>, As),
        proplists:get_value(<<"src">>, As),
        proplists:get_value(<<"data">>, As),
        proplists:get_value(<<"datetime">>, As)
    ]) of
        undefined -> z_string:trim(fetch_text(Es, <<>>));
        Value -> Value
    end.

normalize_itemtype(undefined) ->
    undefined;
normalize_itemtype(<<>>) ->
    undefined;
normalize_itemtype(ItemType) ->
    Type = hd(binary:split(ItemType, <<" ">>, [global])),
    normalize_schema_itemtype(Type).

normalize_schema_itemtype(<<"http://schema.org/", Type/binary>>) ->
    {schema, <<"https://schema.org">>, Type};
normalize_schema_itemtype(<<"https://schema.org/", Type/binary>>) ->
    {schema, <<"https://schema.org">>, Type};
normalize_schema_itemtype(Type) ->
    {type, Type}.

meta_link(_Name, undefined, _As, MD) -> MD;
meta_link(_Name, <<>>, _As, MD) -> MD;
meta_link(_Name, <<"undefined">>, _As, MD) -> MD;  % Youtube...
meta_link(<<"canonical">>, Content, _As, MD) -> [{canonical_url, Content}|MD];
meta_link(<<"shortlink">>, Content, _As, MD) -> [{short_url, Content}|MD];
meta_link(<<"shorturl">>, Content, _As, MD) -> [{short_url, Content}|MD];
meta_link(<<"icon">>, Content, As, MD) ->
    case proplists:is_defined(<<"mask">>, As) of
        true -> MD;
        false -> [{icon_fav, Content}|MD]
    end;
meta_link(<<"shortcut icon">>, Content, _As, MD) -> [{icon_shortcut, Content}|MD];
meta_link(<<"apple-touch-icon">>, Content, _As, MD) -> [{icon_touch, Content}|MD];
meta_link(_Name, _Content, _As, MD) -> MD.

links(_Rel, undefined, _As, MD) ->
    MD;
links(_Rel, <<>>, _As, MD) ->
    MD;
links(Rel, Href, As, MD) ->
    As1 = maps:from_list(As),
    As2 = As1#{ <<"href">> => Href },
    [ {link, {Rel, maps:remove(<<"rel">>, As2)}} | MD ].

split_class(undefined) -> [];
split_class(Class) -> binary:split(Class, <<" ">>, [global]).

has_nav_class(Cs) ->
    lists:any(fun is_nav_class/1, Cs).

is_nav_class(<<"nav", _/binary>>) -> true;
is_nav_class(<<"menu", _/binary>>) -> true;
is_nav_class(_) -> false.

is_topbar_id(<<"top">>) -> true;
is_topbar_id(<<"header", _/binary>>) -> true;
is_topbar_id(_) -> false.

is_ads(<<"ad">>, _Cs) -> true;
is_ads(<<"ads">>, _Cs) -> true;
is_ads(_, Cs) -> lists:any(fun is_ad_class/1, Cs).

is_ad_class(<<"ads">>) -> true;
is_ad_class(<<"ad">>) -> true;
is_ad_class(<<"deckad">>) -> true;
is_ad_class(_) -> false.

fetch_text(B, Acc) when is_binary(B) ->
    <<Acc/binary, B/binary>>;
fetch_text({comment, _}, Acc) ->
    Acc;
fetch_text({_Tag, _As, Es}, Acc) ->
    fetch_text(Es, Acc);
fetch_text([], Acc) ->
    Acc;
fetch_text([E|Es], Acc) ->
    fetch_text(Es, fetch_text(E, Acc)).


first([]) -> undefined;
first([undefined|Rest]) -> first(Rest);
first([<<>>|Rest]) -> first(Rest);
first([X|_]) -> X.

is_html(<<"text/html">>) -> true;
is_html(<<"application/xhtml">>) -> true;
is_html(<<"application/xhtml+", _/binary>>) -> true;
is_html(_) -> false.

%% Some servers send us 'gzip', even when we ask for 'identity'
is_text(_CT, <<31, 198, Method, _/binary>>) when Method =< 8 ->
    false;
is_text(CT, _Data) ->
    is_text(CT).

is_text(<<"text/", _/binary>>) -> true;
is_text(<<"application/javascript">>) -> true;
is_text(<<"application/xhtml">>) -> true;
is_text(<<"application/xhtml+", _/binary>>) -> true;
is_text(_) -> false.

% Suppres tracking pixels and small images
is_img_allowed(<<>>, _As) ->
    false;
is_img_allowed(Url, As) ->
    not is_img_small(As)
    andalso binary:match(Url, img_blocklist()) =:= nomatch.

% Images are considered small if their width or height is smaller than 16px
is_img_small(As) ->
    is_small_size(proplists:get_value(<<"width">>, As))
    orelse is_small_size(proplists:get_value(<<"height">>, As)).

is_small_size(undefined) -> false;
is_small_size(<<>>) -> false;
is_small_size(Size) ->
    try
        Sz = z_convert:to_integer(Size),
        Sz =< ?IMG_SMALL_SIZE
    catch
        _:_ -> false
    end.

% Add parts of image URLs to be suppressed
img_blocklist() -> [
    <<"//www.facebook.com/tr?">>,
    <<"//www.googleadservices.com/pagead/">>,
    <<"futuresimple.com/api/v1/">>,
    <<"tracking.cirrusinsight.com">>,
    <<"list-manage.com/track">>,
    <<"mjt.lu/oo">>,
    <<"/1x1/">>
    ].

% Add nowarn because the iconv module is optional.
-dialyzer({[ nowarn_function ], maybe_convert_utf8/4}).
maybe_convert_utf8(true, IsHtml, Charset, Html) ->
    CS1 = z_convert:to_list(
                z_string:to_lower(
                    html_charset(IsHtml, Charset, Html))),
    case is_utf8(CS1) of
        true ->
            Html;
        false ->
            try
                case iconv:open(CS1, "UTF-8") of
                    {ok, C} ->
                        case iconv:conv(C, Html) of
                            {ok, Html1} ->
                                iconv:close(C),
                                Html1;
                            {error, _} ->
                                Html
                        end;
                    {error, _} ->
                        Html
                end
            catch
                _:_ -> Html
            end
    end;
maybe_convert_utf8(false, _IsHtml, _Charset, Data) ->
    Data.

is_utf8("utf-8") -> true;
is_utf8("utf8") -> true;
is_utf8(_) -> false.

html_charset(IsHtml, undefined, Text) ->
    html_charset(IsHtml, <<"iso-8859-1">>, Text);
html_charset(true, Charset, Html) ->
    meta_charset(Charset, Html);
html_charset(false, Charset, _Text) ->
    Charset.

meta_charset(Ch, Html) ->
    case re:run(Html, "<[mM][eE][tT][aA][^>]*[cC][hH][aA][rR][sS][eE][tT]\\s*=\\s*[\"']?([A-Za-z0-9_-]+)", [{capture,all_but_first,binary}]) of
        {match, [CharSet|_]} -> CharSet;
        _ -> Ch
    end.

header_links(Hs, Links) ->
    lists:foldr(
        fun
            ({<<"link">>, LinkHdr}, Acc) ->
                LinkList = split_link_header(LinkHdr),
                lists:foldr(
                    fun(Link, HAcc) ->
                        case parse_header(Link) of
                            {<<>>, _} ->
                                HAcc;
                            {Href, Options} ->
                                case proplists:get_value(<<"rel">>, Options) of
                                    undefined -> HAcc;
                                    <<>> -> HAcc;
                                    Rel ->
                                        Rel1 = z_string:to_lower(z_string:trim(Rel)),
                                        case Rel1 of
                                            <<>> ->
                                                HAcc;
                                            _ ->
                                                Options1 = maps:from_list(Options),
                                                Options2 = Options1#{ <<"href">> => unbracket(Href) },
                                                Options3 = maps:remove(<<"rel">>, Options2),
                                                HAcc#{
                                                    Rel1 => [ Options3 | maps:get(Rel1, HAcc, []) ]
                                                }
                                        end
                                end
                        end
                    end,
                    Acc,
                    LinkList);
            ({_, _}, Acc) ->
                Acc
        end,
        Links,
        Hs).

split_link_header(Bin) ->
    split_link_header(Bin, <<>>, [], false, false).

split_link_header(<<>>, Current, Acc, _InQuotes, _InUri) ->
    lists:reverse([ z_string:trim(Current) | Acc ]);
split_link_header(<<$,, Rest/binary>>, Current, Acc, false, false) ->
    split_link_header(Rest, <<>>, [ z_string:trim(Current) | Acc ], false, false);
split_link_header(<<$", Rest/binary>>, Current, Acc, InQuotes, InUri) ->
    split_link_header(Rest, <<Current/binary, $">>, Acc, not InQuotes, InUri);
split_link_header(<<$<, Rest/binary>>, Current, Acc, InQuotes, false) when not InQuotes ->
    split_link_header(Rest, <<Current/binary, $<>>, Acc, InQuotes, true);
split_link_header(<<$>, Rest/binary>>, Current, Acc, InQuotes, true) when not InQuotes ->
    split_link_header(Rest, <<Current/binary, $>>>, Acc, InQuotes, false);
split_link_header(<<C, Rest/binary>>, Current, Acc, InQuotes, InUri) ->
    split_link_header(Rest, <<Current/binary, C>>, Acc, InQuotes, InUri).

unbracket(<<"<", _/binary>> = Hdr) ->
    case binary:last(Hdr) of
        $> -> binary:part(Hdr, 1, byte_size(Hdr) - 2);
        _ -> Hdr
    end;
unbracket(Hdr) ->
    Hdr.

content_type(Hs) ->
    case proplists:get_value(<<"content-type">>, Hs) of
        undefined ->
            {<<"application/octet-stream">>, []};
        CT ->
            {Type, Params} = parse_header(CT),
            {z_string:to_lower(Type), Params}
    end.

content_length(Hs) ->
    try
        case proplists:get_value(<<"content-range">>, Hs) of
            undefined ->
                case proplists:get_value(<<"content-length">>, Hs) of
                    undefined -> undefined;
                    N -> binary_to_integer(N)
                end;
            <<"bytes ", Range/binary>> ->
                Ts = binary:split(Range, <<"/">>, [global]),
                binary_to_integer(lists:last(Ts))
        end
    catch
        _:_ -> undefined
    end.

%%
%% Tests
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

simple_partial_metadata_test() ->
    Url = "http://example.org",
    Headers = [{"content-type", "text/html"}],
    Data = <<"<html><head><title>Example</title><body></body></html>">>,

    MD = partial_metadata(Url, Headers, Data),

    ?assertEqual(<<"http://example.org">>, MD#url_metadata.final_url),
    ?assertEqual(<<"text/html">>, MD#url_metadata.content_type),
    ?assertEqual([{title, <<"Example">>}], MD#url_metadata.metadata),

    ok.

simple_html_meta_test() ->
    Data = <<"<html><head><title>Example</title><body></body></html>">>,
    ?assertEqual([{title, <<"Example">>}], html_meta(Data)),
    ok.

partial_unbalanced_tags_html_meta_test() ->
    Data = <<"<head><meta name=\"description\" content=\"Example Content\"><title>Example</title>">>,
    ?assertEqual([{description, <<"Example Content">>},
        {title, <<"Example">>}], html_meta(Data)),
    ok.

partial_no_surrounding_tags_html_meta_test() ->
    Data = <<"<meta name=\"description\" content=\"Example Content\"><title>Example</title>">>,
    ?assertEqual([{description, <<"Example Content">>},
        {title, <<"Example">>}], html_meta(Data)),
    ok.

partial_ampersant_in_html_meta_test() ->
    Data = <<"<meta name=\"description\" content=\"Example & Stuff\"><title>Foo &amp; Co</title>">>,
    ?assertEqual([{description, <<"Example & Stuff">>},
        {title, <<"Foo & Co">>}], html_meta(Data)),
    ok.

youtube_itemprop_html_meta_test() ->
    Data = <<"
<div itemscope itemtype=\"http://schema.org/VideoObject\" itemid=\"https://www.youtube.com/watch?v=example\">
<meta itemprop=\"name\" content=\"Example Video\">
<meta itemprop=\"description\" content=\"Example Description\">
<meta itemprop=\"thumbnailUrl\" content=\"https://i.ytimg.com/vi/example/maxresdefault.jpg\">
<span itemprop=\"author\" itemscope itemtype=\"http://schema.org/Person\" itemid=\"http://www.youtube.com/@example\">
    <link itemprop=\"url\" href=\"http://www.youtube.com/@example\">
    <link itemprop=\"name\" content=\"Example Channel\">
</span>
</div>
    ">>,
    MD = html_meta(Data),
    JsonLDs = proplists:get_value(json_ld, MD),
    ?assertEqual(2, length(JsonLDs)),
    Video = json_ld_by_id(<<"https://www.youtube.com/watch?v=example">>, JsonLDs),
    Author = json_ld_by_id(<<"http://www.youtube.com/@example">>, JsonLDs),
    ?assertEqual(<<"https://schema.org">>, maps:get(<<"@context">>, Video)),
    ?assertEqual(<<"VideoObject">>, maps:get(<<"@type">>, Video)),
    ?assertEqual(<<"Example Video">>, maps:get(<<"name">>, Video)),
    ?assertEqual(<<"Example Description">>, maps:get(<<"description">>, Video)),
    ?assertEqual(<<"https://i.ytimg.com/vi/example/maxresdefault.jpg">>,
        maps:get(<<"thumbnailUrl">>, Video)),
    ?assertEqual(Author, maps:get(<<"author">>, Video)),
    ?assertEqual(<<"Person">>, maps:get(<<"@type">>, Author)),
    ?assertEqual(<<"Example Channel">>, maps:get(<<"name">>, Author)),
    ?assertEqual(<<"http://www.youtube.com/@example">>, maps:get(<<"url">>, Author)),
    ?assertEqual(undefined, proplists:get_value(mtitle, MD)),
    ok.

youtube_itemprop_metadata_property_test() ->
    Data = <<"
<div itemscope itemtype=\"https://schema.org/VideoObject\" itemid=\"https://www.youtube.com/watch?v=example\">
<meta itemprop=\"name\" content=\"Example Video\">
</div>
    ">>,
    {ok, MD} = fetch_data([], Data),
    [Video] = p(json_ld, MD),
    ?assertEqual(<<"Example Video">>, maps:get(<<"name">>, Video)),
    ?assertEqual(<<"VideoObject">>, maps:get(<<"@type">>, Video)),
    ok.

youtube_scoped_itemprop_html_meta_test() ->
    Data = <<"
<div id=\"watch7-content\" class=\"watch-main-col\" itemscope itemid=\"https://www.youtube.com/watch?v=6neL1YuX6kQ\" itemtype=\"http://schema.org/VideoObject\">
    <link itemprop=\"url\" href=\"https://www.youtube.com/watch?v=6neL1YuX6kQ\">
    <meta itemprop=\"name\" content=\"Europe&#39;s New Anti-Drone Cannon Is Spreading Across Six NATO Armies\">
    <meta itemprop=\"description\" content=\"See the Skyranger 30 mobile air defense system in action.\">
    <meta itemprop=\"requiresSubscription\" content=\"False\">
    <meta itemprop=\"identifier\" content=\"6neL1YuX6kQ\">
    <meta itemprop=\"duration\" content=\"PT12M58S\">
    <span itemprop=\"author\" itemscope itemtype=\"http://schema.org/Person\">
        <link itemprop=\"url\" href=\"http://www.youtube.com/@WesODonnellX\">
        <link itemprop=\"name\" content=\"Wes O&#39;Donnell\">
    </span>
    <span itemscope itemtype=\"https://schema.org/BreadcrumbList\">
        <span itemprop=\"itemListElement\" itemscope itemtype=\"https://schema.org/ListItem\">
            <meta itemprop=\"position\" content=\"1\"/>
            <span itemprop=\"item\" itemid=\"http://www.youtube.com/@WesODonnellX\" itemscope itemtype=\"https://schema.org/Thing\">
                <meta itemprop=\"name\" content=\"Wes O&#39;Donnell\"/>
            </span>
        </span>
    </span>
    <link itemprop=\"thumbnailUrl\" href=\"https://i.ytimg.com/vi/6neL1YuX6kQ/maxresdefault.jpg\">
    <span itemprop=\"thumbnail\" itemscope itemtype=\"http://schema.org/ImageObject\">
        <link itemprop=\"url\" href=\"https://i.ytimg.com/vi/6neL1YuX6kQ/maxresdefault.jpg\">
        <meta itemprop=\"width\" content=\"1280\">
        <meta itemprop=\"height\" content=\"720\">
    </span>
    <link itemprop=\"embedUrl\" href=\"https://www.youtube.com/embed/6neL1YuX6kQ\">
    <meta itemprop=\"playerType\" content=\"HTML5 Flash\">
    <meta itemprop=\"width\" content=\"1280\">
    <meta itemprop=\"height\" content=\"720\">
    <meta itemprop=\"isFamilyFriendly\" content=\"true\">
    <meta itemprop=\"regionsAllowed\" content=\"AD,AE,AF\">
    <div itemprop=\"interactionStatistic\" itemscope itemtype=\"https://schema.org/InteractionCounter\">
        <meta itemprop=\"interactionType\" content=\"https://schema.org/LikeAction\">
        <meta itemprop=\"userInteractionCount\" content=\"4088\">
    </div>
    <meta itemprop=\"keywords\" content=\"military technology,air defense system\">
    <div itemprop=\"interactionStatistic\" itemscope itemtype=\"https://schema.org/InteractionCounter\">
        <meta itemprop=\"interactionType\" content=\"https://schema.org/WatchAction\">
        <meta itemprop=\"userInteractionCount\" content=\"41418\">
    </div>
    <meta itemprop=\"datePublished\" content=\"2026-06-29T09:00:15-07:00\">
    <meta itemprop=\"uploadDate\" content=\"2026-06-29T09:00:15-07:00\">
    <meta itemprop=\"genre\" content=\"Nonprofits &amp; Activism\">
</div>
    ">>,
    JsonLDs = proplists:get_value(json_ld, html_meta(Data)),
    ?assertEqual(2, length(JsonLDs)),
    Video = json_ld_by_id(<<"https://www.youtube.com/watch?v=6neL1YuX6kQ">>, JsonLDs),
    Channel = json_ld_by_id(<<"http://www.youtube.com/@WesODonnellX">>, JsonLDs),
    Author = maps:get(<<"author">>, Video),
    Thumbnail = maps:get(<<"thumbnail">>, Video),
    InteractionStats = maps:get(<<"interactionStatistic">>, Video),
    ?assertEqual(<<"VideoObject">>, maps:get(<<"@type">>, Video)),
    ?assertEqual(<<"Europe's New Anti-Drone Cannon Is Spreading Across Six NATO Armies">>,
        maps:get(<<"name">>, Video)),
    ?assertEqual(<<"False">>, maps:get(<<"requiresSubscription">>, Video)),
    ?assertEqual(<<"6neL1YuX6kQ">>, maps:get(<<"identifier">>, Video)),
    ?assertEqual(<<"PT12M58S">>, maps:get(<<"duration">>, Video)),
    ?assertEqual(<<"https://i.ytimg.com/vi/6neL1YuX6kQ/maxresdefault.jpg">>,
        maps:get(<<"thumbnailUrl">>, Video)),
    ?assertEqual(<<"https://www.youtube.com/embed/6neL1YuX6kQ">>, maps:get(<<"embedUrl">>, Video)),
    ?assertEqual(<<"Nonprofits & Activism">>, maps:get(<<"genre">>, Video)),
    ?assertEqual(<<"Person">>, maps:get(<<"@type">>, Author)),
    ?assertEqual(<<"Wes O'Donnell">>, maps:get(<<"name">>, Author)),
    ?assertEqual(<<"ImageObject">>, maps:get(<<"@type">>, Thumbnail)),
    ?assertEqual(<<"1280">>, maps:get(<<"width">>, Thumbnail)),
    ?assertMatch([_, _], InteractionStats),
    ?assertEqual(<<"Thing">>, maps:get(<<"@type">>, Channel)),
    ?assertEqual(<<"Wes O'Donnell">>, maps:get(<<"name">>, Channel)),
    ok.

json_ld_by_id(ItemId, JsonLDs) ->
    hd([ JsonLD || JsonLD <- JsonLDs, maps:get(<<"@id">>, JsonLD, undefined) =:= ItemId ]).

links_header_test() ->
    Data = <<"
<head>
<link rel=alternate href=\"/en/html\" hreflang=en type=text/html title=\"English HTML\">
<link rel=alternate href=\"/fr/html\" hreflang=fr type=text/html title=\"French HTML\">
<link rel=alternate href=\"/en/html/print\" hreflang=en type=text/html media=print title=\"English HTML (for printing)\">
<link rel=alternate href=\"/fr/html/print\" hreflang=fr type=text/html media=print title=\"French HTML (for printing)\">
<link rel=alternate href=\"/en/pdf\" hreflang=en type=application/pdf title=\"English PDF\">
<link rel=alternate href=\"/fr/pdf\" hreflang=fr type=application/pdf title=\"French PDF\">
</head>
    ">>,
    Links = #{
        <<"alternate">> => [
            #{ <<"href">> => <<"/en/html">>,
               <<"hreflang">> => <<"en">>,
               <<"type">> => <<"text/html">>,
               <<"title">> => <<"English HTML">> },
            #{ <<"href">> => <<"/fr/html">>,
               <<"hreflang">> => <<"fr">>,
               <<"type">> => <<"text/html">>,
               <<"title">> => <<"French HTML">> },
            #{ <<"href">> => <<"/en/html/print">>,
               <<"hreflang">> => <<"en">>,
               <<"type">> => <<"text/html">>,
               <<"media">> => <<"print">>,
               <<"title">> => <<"English HTML (for printing)">> },
            #{ <<"href">> => <<"/fr/html/print">>,
               <<"hreflang">> => <<"fr">>,
               <<"type">> => <<"text/html">>,
               <<"media">> => <<"print">>,
               <<"title">> => <<"French HTML (for printing)">> },
            #{ <<"href">> => <<"/en/pdf">>,
               <<"hreflang">> => <<"en">>,
               <<"type">> => <<"application/pdf">>,
               <<"title">> => <<"English PDF">> },
            #{ <<"href">> => <<"/fr/pdf">>,
               <<"hreflang">> => <<"fr">>,
               <<"type">> => <<"application/pdf">>,
               <<"title">> => <<"French PDF">>}
        ],
        <<"hub">> => [
            #{ <<"href">> => <<"https://hub.example.com/">> }
        ],
        <<"self">> => [
            #{ <<"href">> => <<"https://example.com/feed">> }
        ]
    },
    Hs1 = [
        {<<"content-type">>, <<"text/html">>},
        {"Link", "<https://hub.example.com/>; rel=\"hub\""},
        {"Link", "<https://example.com/feed>; rel=\"self\""}
    ],
    MD1 = partial_metadata(<<"http://example.com">>, Hs1, Data),
    ?assertEqual(Links, MD1#url_metadata.links),
    Hs2 = [
        {<<"content-type">>, <<"text/html">>},
        {"Link", "<https://hub.example.com/>; rel=\"hub\", <https://example.com/feed>; rel=\"self\""}
    ],
    MD2 = partial_metadata(<<"http://example.com">>, Hs2, Data),
    ?assertEqual(Links, MD2#url_metadata.links),
    ok.

-endif.
