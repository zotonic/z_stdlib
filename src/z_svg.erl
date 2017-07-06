%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Copyright Channel.Me B.V.
%% @doc Utility functions for svg processing.

%% Copyright 2016 Copyright Channel.Me B.V.
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

-module(z_svg).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    sanitize/1,
    sanitize_element/1
]).

-type svg_element() :: binary()
                    | {binary(), list(), list()}
                    | {comment, binary()}.

%% @doc Sanitize a binary containing a SVG
-spec sanitize(binary()) -> binary().
sanitize(<<>>) ->
    <<>>;
sanitize(<<C, Svg/binary>>) when C =< 32 ->
    sanitize(Svg);
sanitize(<<"<?xml ", _/binary>> = Svg) ->
    Svg1 = sanitize_1(Svg),
    <<"<?xml version=\"1.0\" standalone=\"no\"?>",
      "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
      Svg1/binary>>;
sanitize(Svg) when is_binary(Svg) ->
    sanitize_1(Svg).

sanitize_1(Svg) ->
    Parsed = z_html_parse:parse(Svg),
    Sanitized = sanitize_element(Parsed),
    flatten(Sanitized).

%% @doc Sanitize an element from the SVG parse tree. Also called from z_html.erl
-spec sanitize_element(svg_element()|term()) -> svg_element(). 
sanitize_element(Bin) when is_binary(Bin) -> 
    Bin;
sanitize_element({comment, _Text} = Comment) -> 
    Comment;
sanitize_element({Elt, Attrs, Enclosed}) ->
    case allow_elt(Elt) of
        true ->
            Attrs1 = lists:filter(fun({A,_}) -> allow_attr(A) end, Attrs),
            {Elt, Attrs1, [ sanitize_element(E) || E <- Enclosed ]};
        false ->
            case skip_contents(Elt) of
                false ->
                    {nop, [ sanitize_element(Encl) || Encl <- Enclosed ]};
                true ->
                    {nop, []}
            end
    end;
sanitize_element(_) -> 
    <<>>.

%% @doc Flatten the sanitized svg tree to a binary 
flatten(B) when is_binary(B) ->
    z_html:escape_html_text(B, <<>>);
flatten({nop, Enclosed}) ->
    flatten(Enclosed);
flatten({comment, Text}) ->
    Comment = z_html:escape_html_comment(Text, <<>>),
    <<"<!--", Comment/binary, "-->">>;
flatten({sanitized_html, Html}) ->
    Html;
flatten({Elt, Attrs, Enclosed}) ->
    EncBin = flatten(Enclosed),
    Attrs1 = [ flatten_attr(Attr) || Attr <- Attrs ],
    Attrs2 = iolist_to_binary(prefix(32, Attrs1)),
    case is_selfclosing(Elt) andalso EncBin == <<>> of
        true ->  <<$<, Elt/binary, Attrs2/binary, 32, $/, $>>>;
        false -> <<$<, Elt/binary, Attrs2/binary, $>, EncBin/binary, $<, $/, Elt/binary, $>>>
    end;
flatten(L) when is_list(L) -> 
    iolist_to_binary([ flatten(A) || A <- L ]).

flatten_attr({<<"xmlns">>, Value}) ->
    z_html:flatten_attr({<<"xmlns">>, Value});
flatten_attr({<<"xmlns:", _/binary>> = Attr, Value}) ->
    z_html:flatten_attr({Attr, Value});
flatten_attr({Attr, Value}) ->
    case is_acceptable_svg(Value) of
        true ->
            case binary:split(Attr, <<":">>) of
                [NS, Attr1] ->
                    AV = z_html:flatten_attr({Attr1, Value}),
                    <<NS/binary, $:, AV/binary>>;
                [_] ->
                    z_html:flatten_attr({Attr, Value})
            end;
        false ->
            <<Attr/binary, "=\"\"">>
    end.

%% @doc Don't accept SVG attributes referring to an external resource. 
%%      Refuses anything containing urls and/or "url(...)" not referring to local ids.
is_acceptable_svg(<<C, Rest/binary>>) when C =< 32 ->
    is_acceptable_svg(Rest);
is_acceptable_svg(<<"#", _/binary>>) ->
    true;
is_acceptable_svg(<<"url(#", Rest/binary>>) ->
    case binary:split(Rest, <<")">>) of
        [_, <<>>] -> true;
        _ -> false
    end;
is_acceptable_svg(AttrVal) ->
    case binary:match(AttrVal, <<"://">>) of
        {_,_} -> false;
        nomatch -> true
    end.

is_selfclosing(_) -> false.

prefix(Sep, List) -> prefix(Sep,List,[]).
prefix(_Sep, [], Acc) -> lists:reverse(Acc);
prefix(Sep, [H|T], Acc) -> prefix(Sep, T, [H,Sep|Acc]).


allow_elt(<<"a">>) -> true;
allow_elt(<<"altGlyph">>) -> true;
allow_elt(<<"altGlyphDef">>) -> true;
allow_elt(<<"altGlyphItem">>) -> true;
allow_elt(<<"animate">>) -> true;
allow_elt(<<"animateColor">>) -> true;
allow_elt(<<"animateMotion">>) -> true;
allow_elt(<<"animateTransform">>) -> true;

allow_elt(<<"circle">>) -> true;
allow_elt(<<"clipPath">>) -> true;
allow_elt(<<"color-profile">>) -> true;
allow_elt(<<"cursor">>) -> true;

allow_elt(<<"defs">>) -> true;
allow_elt(<<"desc">>) -> true;

allow_elt(<<"ellipse">>) -> true;

allow_elt(<<"feBlend">>) -> true;
allow_elt(<<"feColorMatrix">>) -> true;
allow_elt(<<"feComponentTransfer">>) -> true;
allow_elt(<<"feComposite">>) -> true;
allow_elt(<<"feConvolveMatrix">>) -> true;
allow_elt(<<"feDiffuseLighting">>) -> true;
allow_elt(<<"feDisplacementMap">>) -> true;
allow_elt(<<"feDistantLight">>) -> true;
allow_elt(<<"feDropShadow">>) -> true;
allow_elt(<<"feFlood">>) -> true;
allow_elt(<<"feFuncA">>) -> true;
allow_elt(<<"feFuncB">>) -> true;
allow_elt(<<"feFuncG">>) -> true;
allow_elt(<<"feFuncR">>) -> true;
allow_elt(<<"feGaussianBlur">>) -> true;
allow_elt(<<"feImage">>) -> true;
allow_elt(<<"feMerge">>) -> true;
allow_elt(<<"feMergeNode">>) -> true;
allow_elt(<<"feMorphology">>) -> true;
allow_elt(<<"feOffset">>) -> true;
allow_elt(<<"fePointLight">>) -> true;
allow_elt(<<"feSpecularLighting">>) -> true;
allow_elt(<<"feSpotlight">>) -> true;
allow_elt(<<"feTile">>) -> true;
allow_elt(<<"feTurbulence">>) -> true;
allow_elt(<<"filter">>) -> true;
allow_elt(<<"font">>) -> true;
allow_elt(<<"font-face">>) -> true;
allow_elt(<<"font-face-format">>) -> true;
allow_elt(<<"font-face-name">>) -> true;
allow_elt(<<"font-face-src">>) -> true;
allow_elt(<<"font-face-uri">>) -> true;
allow_elt(<<"foreignObject">>) -> true;

allow_elt(<<"g">>) -> true;
allow_elt(<<"glyph">>) -> true;
allow_elt(<<"glyphRef">>) -> true;

allow_elt(<<"hkern">>) -> true;

allow_elt(<<"image">>) -> true;

allow_elt(<<"line">>) -> true;
allow_elt(<<"linearGradient">>) -> true;

allow_elt(<<"marker">>) -> true;
allow_elt(<<"mask">>) -> true;
allow_elt(<<"metadata">>) -> true;
allow_elt(<<"missing-glyph">>) -> true;
allow_elt(<<"mpath">>) -> true;

allow_elt(<<"path">>) -> true;
allow_elt(<<"pattern">>) -> true;
allow_elt(<<"polygon">>) -> true;
allow_elt(<<"polyline">>) -> true;

allow_elt(<<"radialGradient">>) -> true;
allow_elt(<<"rect">>) -> true;

allow_elt(<<"set">>) -> true;
allow_elt(<<"stop">>) -> true;
allow_elt(<<"style">>) -> true;
allow_elt(<<"svg">>) -> true;

%% script not allowed of course.
allow_elt(<<"script">>) -> false;
allow_elt(<<"switch">>) -> true;
allow_elt(<<"symbol">>) -> true;

allow_elt(<<"text">>) -> true;
allow_elt(<<"textPath">>) -> true;
allow_elt(<<"title">>) -> true;
allow_elt(<<"tref">>) -> true;
allow_elt(<<"tspan">>) -> true;

allow_elt(<<"use">>) -> true;

allow_elt(<<"view">>) -> true;
allow_elt(<<"vkern">>) -> true;

allow_elt(_) -> false.

allow_attr(<<"accelerate">>) -> true;
allow_attr(<<"accent-height">>) -> true;
allow_attr(<<"accumulate">>) -> true;
allow_attr(<<"additive">>) -> true;
allow_attr(<<"alignment-baseline">>) -> true;
allow_attr(<<"allowReorder">>) -> true;
allow_attr(<<"alphabetic">>) -> true;
allow_attr(<<"amplitude">>) -> true;
allow_attr(<<"arabic-form">>) -> true;
allow_attr(<<"ascent">>) -> true;
allow_attr(<<"attributeName">>) -> true;
allow_attr(<<"attributeType">>) -> true;
allow_attr(<<"autoReverse">>) -> true;
allow_attr(<<"azimuth">>) -> true;

allow_attr(<<"baseFrequency">>) -> true;
allow_attr(<<"baseline-shift">>) -> true;
allow_attr(<<"baseProfile">>) -> true;
allow_attr(<<"bbox">>) -> true;
allow_attr(<<"begin">>) -> true;
allow_attr(<<"bias">>) -> true;
allow_attr(<<"by">>) -> true;

allow_attr(<<"calcMode">>) -> true;
allow_attr(<<"cap-height">>) -> true;
allow_attr(<<"class">>) -> true;
allow_attr(<<"clip">>) -> true;
allow_attr(<<"clipPathUnits">>) -> true;
allow_attr(<<"clip-path">>) -> true;
allow_attr(<<"clip-rule">>) -> true;
allow_attr(<<"color">>) -> true;
allow_attr(<<"color-interpolation">>) -> true;
allow_attr(<<"color-interpolation-filters">>) -> true;
allow_attr(<<"color-profile">>) -> true;
allow_attr(<<"color-rendering">>) -> true;
allow_attr(<<"contentScriptType">>) -> true;
allow_attr(<<"contentStyleType">>) -> true;
allow_attr(<<"cursor">>) -> true;
allow_attr(<<"cx">>) -> true;
allow_attr(<<"cy">>) -> true;

allow_attr(<<"data-", _/binary>>) -> true; %% data attributes, including data-chc-id

allow_attr(<<"d">>) -> true;
allow_attr(<<"decelerate">>) -> true;
allow_attr(<<"descent">>) -> true;
allow_attr(<<"diffuseConstant">>) -> true;
allow_attr(<<"direction">>) -> true;
allow_attr(<<"display">>) -> true;
allow_attr(<<"divisor">>) -> true;
allow_attr(<<"dominant-baseline">>) -> true;
allow_attr(<<"dur">>) -> true;
allow_attr(<<"dx">>) -> true;
allow_attr(<<"dy">>) -> true;

allow_attr(<<"edgeMode">>) -> true;
allow_attr(<<"elevation">>) -> true;
allow_attr(<<"enable-background">>) -> true;
allow_attr(<<"end">>) -> true;
allow_attr(<<"exponent">>) -> true;
allow_attr(<<"externalResourcesRequired">>) -> true;

allow_attr(<<"fill">>) -> true;
allow_attr(<<"fill-opacity">>) -> true;
allow_attr(<<"fill-rule">>) -> true;
allow_attr(<<"filter">>) -> true;
allow_attr(<<"filterRes">>) -> true;
allow_attr(<<"filterUnits">>) -> true;
allow_attr(<<"flood-color">>) -> true;
allow_attr(<<"flood-opacity">>) -> true;
allow_attr(<<"font-family">>) -> true;
allow_attr(<<"font-size">>) -> true;
allow_attr(<<"font-size-adjust">>) -> true;
allow_attr(<<"font-stretch">>) -> true;
allow_attr(<<"font-style">>) -> true;
allow_attr(<<"font-variant">>) -> true;
allow_attr(<<"font-weight">>) -> true;
allow_attr(<<"format">>) -> true;
allow_attr(<<"from">>) -> true;
allow_attr(<<"fx">>) -> true;
allow_attr(<<"fy">>) -> true;

allow_attr(<<"g1">>) -> true;
allow_attr(<<"g2">>) -> true;
allow_attr(<<"glyph-name">>) -> true;
allow_attr(<<"glyph-orientation-horizontal">>) -> true;
allow_attr(<<"glyph-orientation-vertical">>) -> true;
allow_attr(<<"glyphRef">>) -> true;
allow_attr(<<"gradientTransform">>) -> true;
allow_attr(<<"gradientUnits">>) -> true;

allow_attr(<<"hanging">>) -> true;
allow_attr(<<"height">>) -> true;
allow_attr(<<"horiz-adv-x">>) -> true;
allow_attr(<<"horiz-origin-x">>) -> true;

allow_attr(<<"id">>) -> true;
allow_attr(<<"ideographic">>) -> true;

allow_attr(<<"image-rendering">>) -> true;
allow_attr(<<"in">>) -> true;
allow_attr(<<"in2">>) -> true;
allow_attr(<<"intercept">>) -> true;

allow_attr(<<"k">>) -> true;
allow_attr(<<"k1">>) -> true;
allow_attr(<<"k2">>) -> true;
allow_attr(<<"k3">>) -> true;
allow_attr(<<"k4">>) -> true;
allow_attr(<<"kernelMatrix">>) -> true;
allow_attr(<<"kernelUnitLength">>) -> true;
allow_attr(<<"kerning">>) -> true;
allow_attr(<<"keyPoints">>) -> true;
allow_attr(<<"keySplines">>) -> true;
allow_attr(<<"keyTimes">>) -> true;

allow_attr(<<"lang">>) -> true;
allow_attr(<<"lengthAdjust">>) -> true;
allow_attr(<<"letter-spacing">>) -> true;
allow_attr(<<"lighting-color">>) -> true;
allow_attr(<<"limitingConeAngle">>) -> true;
allow_attr(<<"local">>) -> true;

allow_attr(<<"marker-end">>) -> true;
allow_attr(<<"marker-mid">>) -> true;
allow_attr(<<"marker-start">>) -> true;
allow_attr(<<"markerHeight">>) -> true;
allow_attr(<<"markerUnits">>) -> true;
allow_attr(<<"markerWidth">>) -> true;
allow_attr(<<"mask">>) -> true;
allow_attr(<<"maskContentUnits">>) -> true;
allow_attr(<<"maskUnits">>) -> true;
allow_attr(<<"mathematical">>) -> true;
allow_attr(<<"max">>) -> true;
allow_attr(<<"media">>) -> true;
allow_attr(<<"method">>) -> true;
allow_attr(<<"min">>) -> true;
allow_attr(<<"mode">>) -> true;

allow_attr(<<"name">>) -> true;
allow_attr(<<"numOctaves">>) -> true;

allow_attr(<<"offset">>) -> true;

allow_attr(<<"onabort">>) -> false;
allow_attr(<<"onactivate">>) -> false;
allow_attr(<<"onbegin">>) -> false;
allow_attr(<<"onclick">>) -> false;
allow_attr(<<"onend">>) -> false;
allow_attr(<<"onerror">>) -> false;
allow_attr(<<"onfocusin">>) -> false;
allow_attr(<<"onfocusout">>) -> false;
allow_attr(<<"onload">>) -> false;
allow_attr(<<"onmousedown">>) -> false;
allow_attr(<<"onmousemove">>) -> false;
allow_attr(<<"onmouseout">>) -> false;
allow_attr(<<"onmouseover">>) -> false;
allow_attr(<<"onmouseup">>) -> false;
allow_attr(<<"onrepeat">>) -> false;
allow_attr(<<"onresize">>) -> false;
allow_attr(<<"onscroll">>) -> false;
allow_attr(<<"onunload">>) -> false;
allow_attr(<<"onzoom">>) -> false;

allow_attr(<<"opacity">>) -> true;
allow_attr(<<"operator">>) -> true;
allow_attr(<<"order">>) -> true;
allow_attr(<<"orient">>) -> true;
allow_attr(<<"orientation">>) -> true;
allow_attr(<<"origin">>) -> true;
allow_attr(<<"overflow">>) -> true;
allow_attr(<<"overline-position">>) -> true;
allow_attr(<<"overline-thickness">>) -> true;

allow_attr(<<"panose-1">>) -> true;
allow_attr(<<"paint-order">>) -> true;
allow_attr(<<"path">>) -> true;
allow_attr(<<"pathLength">>) -> true;
allow_attr(<<"patternContentUnits">>) -> true;
allow_attr(<<"patternTransform">>) -> true;
allow_attr(<<"patternUnits">>) -> true;
allow_attr(<<"pointer-events">>) -> true;
allow_attr(<<"points">>) -> true;
allow_attr(<<"pointsAtX">>) -> true;
allow_attr(<<"pointsAtY">>) -> true;
allow_attr(<<"pointsAtZ">>) -> true;
allow_attr(<<"preserveAlpha">>) -> true;
allow_attr(<<"preserveAspectRatio">>) -> true;
allow_attr(<<"primitiveUnits">>) -> true;

allow_attr(<<"r">>) -> true;
allow_attr(<<"radius">>) -> true;
allow_attr(<<"refX">>) -> true;
allow_attr(<<"refY">>) -> true;
allow_attr(<<"rendering-intent">>) -> true;
allow_attr(<<"repeatCount">>) -> true;
allow_attr(<<"repeatDur">>) -> true;
allow_attr(<<"requiredExtensions">>) -> true;
allow_attr(<<"requiredFeatures">>) -> true;
allow_attr(<<"restart">>) -> true;
allow_attr(<<"result">>) -> true;
allow_attr(<<"rotate">>) -> true;
allow_attr(<<"rx">>) -> true;
allow_attr(<<"ry">>) -> true;

allow_attr(<<"scale">>) -> true;
allow_attr(<<"seed">>) -> true;
allow_attr(<<"shape-rendering">>) -> true;
allow_attr(<<"slope">>) -> true;
allow_attr(<<"spacing">>) -> true;
allow_attr(<<"specularConstant">>) -> true;
allow_attr(<<"specularExponent">>) -> true;
allow_attr(<<"speed">>) -> true;
allow_attr(<<"spreadMethod">>) -> true;
allow_attr(<<"startOffset">>) -> true;
allow_attr(<<"stdDeviation">>) -> true;
allow_attr(<<"stemh">>) -> true;
allow_attr(<<"stemv">>) -> true;
allow_attr(<<"stitchTiles">>) -> true;
allow_attr(<<"stop-color">>) -> true;
allow_attr(<<"stop-opacity">>) -> true;
allow_attr(<<"strikethrough-position">>) -> true;
allow_attr(<<"strikethrough-thickness">>) -> true;
allow_attr(<<"string">>) -> true;
allow_attr(<<"stroke">>) -> true;
allow_attr(<<"stroke-dasharray">>) -> true;
allow_attr(<<"stroke-dashoffset">>) -> true;
allow_attr(<<"stroke-linecap">>) -> true;
allow_attr(<<"stroke-linejoin">>) -> true;
allow_attr(<<"stroke-miterlimit">>) -> true;
allow_attr(<<"stroke-opacity">>) -> true;
allow_attr(<<"stroke-width">>) -> true;
allow_attr(<<"style">>) -> true;
allow_attr(<<"surfaceScale">>) -> true;
allow_attr(<<"systemLanguage">>) -> true;

allow_attr(<<"tableValues">>) -> true;
allow_attr(<<"target">>) -> true;
allow_attr(<<"targetX">>) -> true;
allow_attr(<<"targetY">>) -> true;
allow_attr(<<"text-anchor">>) -> true;
allow_attr(<<"text-decoration">>) -> true;
allow_attr(<<"text-rendering">>) -> true;
allow_attr(<<"textLength">>) -> true;
allow_attr(<<"to">>) -> true;
allow_attr(<<"transform">>) -> true;
allow_attr(<<"type">>) -> true;

allow_attr(<<"u1">>) -> true;
allow_attr(<<"u2">>) -> true;
allow_attr(<<"underline-position">>) -> true;
allow_attr(<<"underline-thickness">>) -> true;
allow_attr(<<"unicode">>) -> true;
allow_attr(<<"unicode-bidi">>) -> true;
allow_attr(<<"unicode-range">>) -> true;
allow_attr(<<"units-per-em">>) -> true;

allow_attr(<<"v-alphabetic">>) -> true;
allow_attr(<<"v-hanging">>) -> true;
allow_attr(<<"v-ideographic">>) -> true;
allow_attr(<<"v-mathematical">>) -> true;
allow_attr(<<"values">>) -> true;
allow_attr(<<"version">>) -> true;
allow_attr(<<"vert-adv-y">>) -> true;
allow_attr(<<"vert-origin-x">>) -> true;
allow_attr(<<"vert-origin-y">>) -> true;
allow_attr(<<"viewBox">>) -> true;
allow_attr(<<"viewTarget">>) -> true;
allow_attr(<<"visibility">>) -> true;

allow_attr(<<"width">>) -> true;
allow_attr(<<"widths">>) -> true;
allow_attr(<<"word-spacing">>) -> true;
allow_attr(<<"writing-mode">>) -> true;

allow_attr(<<"x">>) -> true;
allow_attr(<<"x-height">>) -> true;
allow_attr(<<"x1">>) -> true;
allow_attr(<<"x2">>) -> true;
allow_attr(<<"xChannelSelector">>) -> true;
allow_attr(<<"xlink:actuate">>) -> true;
allow_attr(<<"xlink:arcrole">>) -> true;
allow_attr(<<"xlink:href">>) -> true;  % Only local references are allowed
allow_attr(<<"xlink:role">>) -> true;
allow_attr(<<"xlink:show">>) -> true;
allow_attr(<<"xlink:title">>) -> true;
allow_attr(<<"xlink:type">>) -> true;
allow_attr(<<"xml:base">>) -> true;
allow_attr(<<"xml:lang">>) -> true;
allow_attr(<<"xml:space">>) -> true;

allow_attr(<<"xmlns">>) -> true;
allow_attr(<<"xmlns:", _/binary>>) -> true;

allow_attr(<<"y">>) -> true;
allow_attr(<<"y1">>) -> true;
allow_attr(<<"y2">>) -> true;
allow_attr(<<"yChannelSelector">>) -> true;

allow_attr(<<"z">>) -> true;
allow_attr(<<"zoomAndPan">>) -> true;

allow_attr(_) -> false.


skip_contents(_) -> true.
