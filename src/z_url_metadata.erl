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
    p/2,
    filename/2
    ]).

-include("../include/z_url_metadata.hrl").

%% @doc Fetch metadata information for the URL
-spec fetch(binary()|string()) -> {ok, #url_metadata{}} | {error, term()}.
fetch(Url) ->
    case z_url_fetch:fetch_partial(Url) of
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
    case p1([<<"og:title">>, <<"twitter:title">>, mtitle, title, h1], MD) of
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
            case p1([<<"twitter:image:src">>, <<"twitter:image">>, <<"og:image">>,
                     image, icon_touch, icon_shortcut, icon_fav], MD) 
            of
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
    case z_string:trim(proplists:get_value(P, MD#url_metadata.metadata,<<>>)) of
        <<>> -> p1(Ps, MD);
        Value -> Value
    end.

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
        _ -> z_convert:to_binary(lists:last(string:tokens(Path, "/")))
    end.

%% -------------------------------------- Analyze fetched data -----------------------------------------

partial_metadata(Url, Hs, Data) ->
    {CT, CTOpts} = content_type(Hs),
    Data1 = maybe_convert_utf8(is_text(CT), is_html(CT), proplists:get_value("charset", CTOpts), Data),
    #url_metadata{
        final_url = z_convert:to_binary(Url),
        content_type = CT,
        content_type_options = CTOpts,
        content_length = content_length(Hs),
        metadata = html_meta(is_html(CT), Data1),
        headers = Hs,
        partial_data = Data
    }.

html_meta(true, PartialData) ->
    Parsed = mochiweb_html:parse(PartialData),
    lists:reverse(html(Parsed, []));
html_meta(false, _PartialData) ->
    [].

html([], MD) ->
    MD;
html([B|Es], MD) when is_binary(B) ->
    html(Es, MD);
html([{comment, _}|Es], MD) ->
    html(Es, MD);
html([Tag|Es], MD) ->
    html(Es, tag(Tag, MD));
html(Tag, MD) when is_tuple(Tag) ->
    tag(Tag, MD).

tag({<<"html">>, As, Es}, MD) ->
    MD1 = case proplists:get_value(<<"lang">>, As) of
              undefined -> MD;
              Lang -> [{language, Lang} | MD]
          end,
    html(Es, MD1);
tag({<<"meta">>, As, _}, MD) ->
    Name = z_string:to_lower(proplists:get_value(<<"name">>, As)),
    Property = proplists:get_value(<<"property">>, As),
    HttpEquiv = proplists:get_value(<<"http-equiv">>, As),
    Content = proplists:get_value(<<"content">>, As),
    case first([Name, Property, HttpEquiv]) of
        undefined ->
            case proplists:get_value(<<"charset">>, As) of
                undefined -> MD;
                Charset -> [{charset,Charset} | MD]
            end;
        Prop ->
            meta_tag(Prop, Content, MD)
    end;
tag({<<"link">>, As, _}, MD) ->
    Name = z_string:to_lower(proplists:get_value(<<"rel">>, As)),
    Content = proplists:get_value(<<"href">>, As),
    meta_link(Name, Content, MD);
tag({<<"img">>, As, _}, MD) ->
    case proplists:get_value(<<"src">>, As, <<>>) of
        <<>> -> MD;
        Src -> [{image, Src} | MD]
    end;
tag({<<"title">>, _As, Es}, MD) ->
    Text = z_string:trim(fetch_text(Es, <<>>)),
    [{title, Text} | MD];
tag({<<"h1">>, _As, Es}, MD) ->
    case proplists:is_defined(h1, MD) of
        false ->
            Text = z_string:trim(fetch_text(Es, <<>>)),
            [{h1, Text} | MD];
        true ->
            MD
    end;
tag({_Tag, _As, Es}, MD) ->
    html(Es, MD).

meta_tag(<<"og:", _/binary>> = OG, Content, MD) -> [{OG, Content}|MD];
meta_tag(<<"twitter:", _/binary>> = Tw, Content, MD) -> [{Tw, Content}|MD];
meta_tag(<<"title">>, Content, MD) -> [{mtitle, Content}|MD];
meta_tag(<<"keywords">>, Content, MD) -> [{keywords, Content}|MD];
meta_tag(<<"description">>, Content, MD) -> [{description, Content}|MD];
meta_tag(<<"author">>, Content, MD) -> [{author, Content}|MD];
meta_tag(<<"thumbnail">>, Content, MD) -> [{thumbnail, Content}|MD];
meta_tag(<<"content-type">>, Content, MD) -> [{content_type, Content}|MD];
meta_tag(_Name, _Content, MD) -> MD.

meta_link(<<"canonical">>, Content, MD) -> [{canonical_url, Content}|MD];
meta_link(<<"shortlink">>, Content, MD) -> [{short_url, Content}|MD];
meta_link(<<"shorturl">>, Content, MD) -> [{short_url, Content}|MD];
meta_link(<<"icon">>, Content, MD) -> [{icon_fav, Content}|MD];
meta_link(<<"shortcut icon">>, Content, MD) -> [{icon_shortcut, Content}|MD];
meta_link(<<"apple-touch-icon">>, Content, MD) -> [{icon_touch, Content}|MD];
meta_link(_Name, _Content, MD) -> MD.

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
                        Html1 = iconv:conv(C, Html),
                        iconv:close(C),
                        Html1;
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
