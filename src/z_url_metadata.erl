%% @author Marc Worrell
%% @copyright 2014 Marc Worrell
%% @doc Discover metadata about an url.

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

-module(z_url_metadata).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    fetch/1,
    html_meta/1,
    p/2,
    filename/2
    ]).

-include("../include/z_url_metadata.hrl").

-define(FETCH_LENGTH, 128*1024).

%% @doc Fetch metadata information for the URL
-spec fetch(binary()|string()) -> {ok, #url_metadata{}} | {error, term()}.
fetch(Url) ->
    case z_url_fetch:fetch_partial(Url, [{max_length, ?FETCH_LENGTH}]) of
        {ok, {FinalUrl, Headers, _Size, Data}} ->
            {ok, partial_metadata(FinalUrl, Headers, Data)};
        {error, _} = Error ->
            Error
    end.


%% @doc Fetch properties of the fetched metadata
-spec p(atom(), #url_metadata{}) -> list(binary()) | binary() | undefined.
p(mime, MD) ->
    MD#url_metadata.content_type;
p(final_url, MD) -> 
    MD#url_metadata.final_url;
p(url, MD) ->
    case p1([<<"og:url">>, <<"twitter:url">>, canonical_url, short_url], MD) of
        undefined -> MD#url_metadata.final_url;
        PrefUrl -> z_url:abs_link(PrefUrl, MD#url_metadata.final_url)
    end;
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
                         image_nav, image, icon_touch, icon_shortcut, icon_fav];
                    false ->
                        [<<"twitter:image:src">>, <<"twitter:image">>, <<"og:image">>, 
                         image, icon_touch, image_nav, icon_shortcut, icon_fav]
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

-spec filename(binary()|string(), list()) -> binary() | undefined.
filename(Url, Hs) ->
    case content_disp_filename(proplists:get_value("content-disposition", Hs)) of
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
maybe_abs_link(true, undefined, _FinalUrl) ->
    undefined;
maybe_abs_link(true, Value, FinalUrl) ->
    z_url:abs_link(Value, FinalUrl).

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
    {_Disp, Options} = mochiweb_util:parse_header(Vs),
    case proplists:get_value("filename", Options) of
        undefined -> undefined;
        "" -> undefined;
        FN -> FN
    end.

basename(<<"data:", _/binary>>) ->
    undefined;
basename("data:" ++ _) ->
    undefined;
basename(Url) ->
    {_Protocol, _Host, Path, _Qs, _Hash} = mochiweb_util:urlsplit(z_convert:to_list(Url)),
    case Path of
        [] -> undefined;
        "/" -> undefined;
        _ -> z_convert:to_binary(lists:last(string:tokens(Path, "/")))
    end.

%% -------------------------------------- Analyze fetched data -----------------------------------------

-record(ps, { in_nav = false }).

partial_metadata(Url, Hs, Data) ->
    {CT, CTOpts} = content_type(Hs),
    IsText = is_text(CT, Data),
    IsHTML = IsText andalso is_html(CT),
    Data1 = maybe_convert_utf8(IsText, IsHTML, proplists:get_value("charset", CTOpts), Data),
    #url_metadata{
        final_url = z_convert:to_binary(Url),
        content_type = CT,
        content_type_options = CTOpts,
        content_length = content_length(Hs),
        metadata = html_meta(IsHTML, Data1),
        is_index_page = is_index_page(Url),
        headers = Hs,
        partial_data = Data
    }.

is_index_page(undefined) ->
    true;
is_index_page(Url) ->
    {_Protocol, _Host, Path, Qs, _Hash} = mochiweb_util:urlsplit(z_convert:to_list(Url)),
    case Qs of
        [] ->
            case Path of
                [] -> true;
                "/" -> true;
                "index." ++ _ -> true;
                "default.htm" -> true;
                "Default.htm" -> true;
                _ -> false
            end;
        _ ->
            false
    end.
    
html_meta(Data) ->
    html_meta(true, Data).

html_meta(true, PartialData) ->
    Parsed = parse(PartialData),
    lists:reverse(html(Parsed, [], #ps{}));
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
    Name = z_string:to_lower(proplists:get_value(<<"rel">>, As)),
    Content = proplists:get_value(<<"href">>, As),
    {meta_link(Name, Content, As, MD), P};
tag({<<"img">>, As, _}, MD, P) ->
    case proplists:get_value(<<"src">>, As, <<>>) of
        <<>> -> 
            {MD, P};
        Src ->
            case P#ps.in_nav of
                true -> {[{image_nav, Src} | MD], P};
                false -> {[{image, Src} | MD], P}
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

meta_tag(<<"og:", _/binary>> = OG, Content, MD) -> [{OG, Content}|MD];
meta_tag(<<"twitter:", _/binary>> = Tw, Content, MD) -> [{Tw, Content}|MD];
meta_tag(<<"title">>, Content, MD) -> [{mtitle, Content}|MD];
meta_tag(<<"keywords">>, Content, MD) -> [{keywords, Content}|MD];
meta_tag(<<"description">>, Content, MD) -> [{description, Content}|MD];
meta_tag(<<"author">>, Content, MD) -> [{author, Content}|MD];
meta_tag(<<"thumbnail">>, Content, MD) -> [{thumbnail, Content}|MD];
meta_tag(<<"content-type">>, Content, MD) -> [{content_type, Content}|MD];
meta_tag(_Name, _Content, MD) -> MD.

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

content_type(Hs) ->
    case proplists:get_value("content-type", Hs) of
        undefined ->
            {<<"application/octet-stream">>, []};
        CT ->
            {Mime, Options} = mochiweb_util:parse_header(CT),
            {z_convert:to_binary(Mime), Options}
    end.

content_length(Hs) ->
    try
        case proplists:get_value("content-range", Hs) of
            undefined ->
                case proplists:get_value("content-length", Hs) of
                    undefined -> undefined;
                    N -> list_to_integer(N)
                end;
            "bytes "++Range ->
                Ts = string:tokens(Range, "/"),
                list_to_integer(lists:last(Ts))
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

-endif.