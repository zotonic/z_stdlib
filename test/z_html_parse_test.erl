%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.; copyright 2018-2021 Maas-Maarten Zeeman

-module(z_html_parse_test).

-include_lib("eunit/include/eunit.hrl").

to_html_test() ->
    ?assertEqual(
       <<"<html><head><title>hey!</title></head><body><p class=\"foo\">what's up<br /></p><div>sucka</div>RAW!<!-- comment! --></body></html>">>,
       iolist_to_binary(
         z_html_parse:to_html({html, [],
                  [{<<"head">>, [],
                    [{title, <<"hey!">>}]},
                   {body, [],
                    [{p, [{class, foo}], [<<"what's">>, <<" up">>, {br}]},
                     {'div', <<"sucka">>},
                     {'=', <<"RAW!">>},
                     {comment, <<" comment! ">>}]}]}))),
    ?assertEqual(
       <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">">>,
       iolist_to_binary(
         z_html_parse:to_html({doctype,
                  [<<"html">>, <<"PUBLIC">>,
                   <<"-//W3C//DTD XHTML 1.0 Transitional//EN">>,
                   <<"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">>]}))),
    ?assertEqual(
       <<"<html><?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?></html>">>,
       iolist_to_binary(
         z_html_parse:to_html({<<"html">>,[],
                  [{pi, <<"xml:namespace">>,
                    [{<<"prefix">>,<<"o">>},
                     {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}]}))),
    ok.

escape_test() ->
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       z_html_parse:escape(<<"&quot;\"word ><<up!&quot;">>)),
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       z_html_parse:escape("&quot;\"word ><<up!&quot;")),
    ?assertEqual(
       <<"&amp;quot;\"word &gt;&lt;&lt;up!&amp;quot;">>,
       z_html_parse:escape('&quot;\"word ><<up!&quot;')),
    ?assertEqual(
       <<"pre&nbsp;post">>,
       z_html_parse:escape(<<"pre", 16#c2, 16#a0, "post">>)),
    ok.

escape_attr_test() ->
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       z_html_parse:escape_attr(<<"&quot;\"word ><<up!&quot;">>)),
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       z_html_parse:escape_attr("&quot;\"word ><<up!&quot;")),
    ?assertEqual(
       <<"&amp;quot;&quot;word &gt;&lt;&lt;up!&amp;quot;">>,
       z_html_parse:escape_attr('&quot;\"word ><<up!&quot;')),
    ?assertEqual(
       <<"12345">>,
       z_html_parse:escape_attr(12345)),
    ?assertEqual(
       <<"1.5">>,
       z_html_parse:escape_attr(1.5)),
    ?assertEqual(
       <<"pre&nbsp;post">>,
       z_html_parse:escape_attr(<<"pre", 16#c2, 16#a0, "post">>)),
    ok.

tokens_test() ->
    ?assertEqual(
       [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>},
                                {<<"wibble">>, <<"wibble">>},
                                {<<"alice">>, <<"bob">>}], true}],
       z_html_parse:tokens(<<"<foo bar=baz wibble='wibble' alice=\"bob\"/>">>)),
    ?assertEqual(
       [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>},
                                {<<"wibble">>, <<"wibble">>},
                                {<<"alice">>, <<"bob">>}], true}],
       z_html_parse:tokens(<<"<foo bar=baz wibble='wibble' alice=bob/>">>)),
    ?assertEqual(
       [{comment, <<"[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]">>}],
       z_html_parse:tokens(<<"<!--[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]-->">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       z_html_parse:tokens(<<"<script type=\"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       z_html_parse:tokens(<<"<script type =\"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       z_html_parse:tokens(<<"<script type = \"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},
        {data, <<" A= B <= C ">>, false},
        {end_tag, <<"script">>}],
       z_html_parse:tokens(<<"<script type= \"text/javascript\"> A= B <= C </script>">>)),
    ?assertEqual(
       [{start_tag, <<"textarea">>, [], false},
        {data, <<"<html></body>">>, false},
        {end_tag, <<"textarea">>}],
       z_html_parse:tokens(<<"<textarea><html></body></textarea>">>)),
    ?assertEqual(
       [{start_tag, <<"textarea">>, [], false},
        {data, <<"<html></body></textareaz>">>, false}],
       z_html_parse:tokens(<<"<textarea ><html></body></textareaz>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       z_html_parse:tokens(<<"<?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       z_html_parse:tokens(<<"<?xml:namespace prefix=o ns=urn:schemas-microsoft-com:office:office \n?>">>)),
    ?assertEqual(
       [{pi, <<"xml:namespace">>,
         [{<<"prefix">>,<<"o">>},
          {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}],
       z_html_parse:tokens(<<"<?xml:namespace prefix=o ns=urn:schemas-microsoft-com:office:office">>)),
    ?assertEqual(
       [{data, <<"<">>, false}],
       z_html_parse:tokens(<<"&lt;">>)),
    ?assertEqual(
       [{data, <<"not html ">>, false},
        {data, <<"< at all">>, false}],
       z_html_parse:tokens(<<"not html < at all">>)),

    %% Not a tag because tags can't start with a number.
    ?assertEqual(
       [{data, <<"a ">>, false},
        {data, <<"<100">>, false}],
        z_html_parse:tokens(<<"a <100">>)),
    ok.

parse_test() ->
    D0 = <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
 <head>
   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
   <title>Foo</title>
   <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/rel/dojo/resources/dojo.css\" media=\"screen\">
   <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/foo.css\" media=\"screen\">
   <!--[if lt IE 7]>
   <style type=\"text/css\">
     .no_ie { display: none; }
   </style>
   <![endif]-->
   <link rel=\"icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
   <link rel=\"shortcut icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
 </head>
 <body id=\"home\" class=\"tundra\"><![CDATA[&lt;<this<!-- is -->CDATA>&gt;]]></body>
</html>">>,
    ?assertEqual(
       {ok, {<<"html">>, [],
        [<<"\n ">>,
         {<<"head">>, [],
          [<<"\n   ">>,
           {<<"meta">>,
            [{<<"http-equiv">>,<<"Content-Type">>},
             {<<"content">>,<<"text/html; charset=UTF-8">>}],
            []},
           <<"\n   ">>,
           {<<"title">>,[],[<<"Foo">>]},
           <<"\n   ">>,
           {<<"link">>,
            [{<<"rel">>,<<"stylesheet">>},
             {<<"type">>,<<"text/css">>},
             {<<"href">>,<<"/static/rel/dojo/resources/dojo.css">>},
             {<<"media">>,<<"screen">>}],
            []},
           <<"\n   ">>,
           {<<"link">>,
            [{<<"rel">>,<<"stylesheet">>},
             {<<"type">>,<<"text/css">>},
             {<<"href">>,<<"/static/foo.css">>},
             {<<"media">>,<<"screen">>}],
            []},
           <<"\n   ">>,
           {comment,<<"[if lt IE 7]>\n   <style type=\"text/css\">\n     .no_ie { display: none; }\n   </style>\n   <![endif]">>},
           <<"\n   ">>,
           {<<"link">>,
            [{<<"rel">>,<<"icon">>},
             {<<"href">>,<<"/static/images/favicon.ico">>},
             {<<"type">>,<<"image/x-icon">>}],
            []},
           <<"\n   ">>,
           {<<"link">>,
            [{<<"rel">>,<<"shortcut icon">>},
             {<<"href">>,<<"/static/images/favicon.ico">>},
             {<<"type">>,<<"image/x-icon">>}],
            []},
           <<"\n ">>]},
         <<"\n ">>,
         {<<"body">>,
          [{<<"id">>,<<"home">>},
           {<<"class">>,<<"tundra">>}],
          [<<"&lt;<this<!-- is -->CDATA>&gt;">>]},
         <<"\n">>]}},
       z_html_parse:parse(D0)),
    ?assertEqual(
       {ok, {<<"html">>,[],
        [{pi, <<"xml:namespace">>,
          [{<<"prefix">>,<<"o">>},
           {<<"ns">>,<<"urn:schemas-microsoft-com:office:office">>}]}]}},
       z_html_parse:parse(
         <<"<html><?xml:namespace prefix=\"o\" ns=\"urn:schemas-microsoft-com:office:office\"?></html>">>)),
    ?assertEqual(
       {ok, {<<"html">>, [],
        [{<<"dd">>, [], [<<"foo">>]},
         {<<"dt">>, [], [<<"bar">>]}]}},
       z_html_parse:parse(<<"<html><dd>foo<dt>bar</html>">>)),
    %% Singleton sadness
    ?assertEqual(
       {ok, {<<"html">>, [],
        [{<<"link">>, [], []},
         <<"foo">>,
         {<<"br">>, [], []},
         <<"bar">>]}},
       z_html_parse:parse(<<"<html><link>foo<br>bar</html>">>)),
    ?assertEqual(
       {ok, {<<"html">>, [],
        [{<<"link">>, [], [<<"foo">>,
                           {<<"br">>, [], []},
                           <<"bar">>]}]}},
       z_html_parse:parse(<<"<html><link>foo<br>bar</link></html>">>)),
    %% Case insensitive html tags and attributes.
    ?assertEqual(
       {ok, {<<"html">>, [],
        [{<<"head">>, [], [<<"foo">>,
                           {<<"br">>, [], []},
                           <<"BAR">>]},
         {<<"body">>, [{<<"class">>, <<"">>}, {<<"bgcolor">>, <<"#Aa01fF">>}], []}
        ]}},
       z_html_parse:parse(<<"<html><Head>foo<bR>BAR</head><body Class=\"\" bgcolor=\"#Aa01fF\"></BODY></html>">>)),

    %% Case insensitive html tags and attributes mixed with case sensitive xml data islands.
    ?assertEqual({ok, {<<"body">>,
      [{<<"class">>,<<>>},{<<"bgcolor">>,<<"#Aa01fF">>}],
      [{<<"xml">>,
        [{<<"id">>,<<"data">>}],
        [{<<"Score">>,
          [{<<"Property">>,<<"test">>}],
          [{<<"Player">>,[],[<<"Me">>]}]}]}]}},
       z_html_parse:parse(<<"<body Class=\"\" bgcolor=\"#Aa01fF\"><xml id=\"data\"><Score Property='test'><Player>Me</Player></Score></xml></BODY>">>)),

    %% Case insensitive html tags and attributes mixed with case sensitive xml data islands.
    %% Difference in lowercasing attribute names and tag names.
    ?assertEqual({ok, {<<"body">>,
      [{<<"id">>,<<"x">>},{<<"bgcolor">>,<<"#Aa01fF">>}],
      [{<<"xml">>,
        [{<<"id">>,<<"data">>}],
        [{<<"Score">>,
          [{<<"Property">>,<<"test">>}],
          [{<<"Id">>,[],[<<"Me">>]}]}]}]}},
       z_html_parse:parse(<<"<body Id=\"x\" bgcolor=\"#Aa01fF\"><xml id=\"data\"><Score Property='test'><Id>Me</Id></Score></xml></BODY>">>)),

    ok.

exhaustive_is_singleton_test() ->
    T = z_cover:clause_lookup_table(z_html_parse, is_singleton),
    [?assertEqual(V, z_html_parse:is_singleton(K, #{ mode => html })) || {K, V} <- T].

tokenize_attributes_test() ->
    ?assertEqual(
       {ok, {<<"foo">>,
        [{<<"bar">>, <<"b\"az">>},
         {<<"wibble">>, <<"wibble">>},
         {<<"taco", 16#c2, 16#a9>>, <<"bell">>},
         {<<"quux">>, <<"quux">>}],
        []}},
       z_html_parse:parse(<<"<foo bar=\"b&quot;az\" wibble taco&copy;=bell quux">>)),
    ok.

tokens2_test() ->
    D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org</link><description>Bob's Rants</description></channel>">>,
    ?assertEqual(
       [{start_tag,<<"channel">>,[],false},
        {start_tag,<<"title">>,[],false},
        {data,<<"from __future__ import *">>,false},
        {end_tag,<<"title">>},
        {start_tag,<<"link">>,[],true},
        {data,<<"http://bob.pythonmac.org">>,false},
        {end_tag,<<"link">>},
        {start_tag,<<"description">>,[],false},
        {data,<<"Bob's Rants">>,false},
        {end_tag,<<"description">>},
        {end_tag,<<"channel">>}],
       z_html_parse:tokens(D0)),
    ok.

to_tokens_test() ->
    ?assertEqual(
       [{start_tag, <<"p">>, [{class, 1}], false},
        {end_tag, <<"p">>}],
       z_html_parse:to_tokens({<<"p">>, [{class, 1}], []})),
    ?assertEqual(
       [{start_tag, <<"p">>, [], false},
        {end_tag, <<"p">>}],
       z_html_parse:to_tokens({p})),
    ?assertEqual(
       [{'=', <<"data">>}],
       z_html_parse:to_tokens({'=', <<"data">>})),
    ?assertEqual(
       [{comment, <<"comment">>}],
       z_html_parse:to_tokens({comment, <<"comment">>})),
    %% This is only allowed in sub-tags:
    %% {p, [{"class", "foo"}]} as {p, [{"class", "foo"}], []}
    %% On the outside it's always treated as follows:
    %% {p, [], [{"class", "foo"}]} as {p, [], [{"class", "foo"}]}
    ?assertEqual(
       [{start_tag, <<"html">>, [], false},
        {start_tag, <<"p">>, [{class, 1}], false},
        {end_tag, <<"p">>},
        {end_tag, <<"html">>}],
       z_html_parse:to_tokens({html, [{p, [{class, 1}]}]})),
    ok.

parse2_test() ->
    D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org<br>foo</link><description>Bob's Rants</description></channel>">>,
    ?assertEqual(
       {ok, {<<"channel">>,[],
        [{<<"title">>,[],[<<"from __future__ import *">>]},
         {<<"link">>,[],[
                         <<"http://bob.pythonmac.org">>,
                         {<<"br">>,[],[]},
                         <<"foo">>]},
         {<<"description">>,[],[<<"Bob's Rants">>]}]}},
       z_html_parse:parse(D0)),
    ok.

parse_tokens_test() ->
    D0 = [{doctype,[<<"HTML">>,<<"PUBLIC">>,<<"-//W3C//DTD HTML 4.01 Transitional//EN">>]},
          {data,<<"\n">>,true},
          {start_tag,<<"html">>,[],false}],
    ?assertEqual(
       {ok, {<<"html">>, [], []}},
       z_html_parse:parse_tokens(D0)),
    D1 = D0 ++ [{end_tag, <<"html">>}],
    ?assertEqual(
       {ok, {<<"html">>, [], []}},
       z_html_parse:parse_tokens(D1)),
    D2 = D0 ++ [{start_tag, <<"body">>, [], false}],
    ?assertEqual(
       {ok, {<<"html">>, [], [{<<"body">>, [], []}]}},
       z_html_parse:parse_tokens(D2)),
    D3 = D0 ++ [{start_tag, <<"head">>, [], false},
                {end_tag, <<"head">>},
                {start_tag, <<"body">>, [], false}],
    ?assertEqual(
       {ok, {<<"html">>, [], [{<<"head">>, [], []}, {<<"body">>, [], []}]}},
       z_html_parse:parse_tokens(D3)),
    D4 = D3 ++ [{data,<<"\n">>,true},
                {start_tag,<<"div">>,[{<<"class">>,<<"a">>}],false},
                {start_tag,<<"a">>,[{<<"name">>,<<"#anchor">>}],false},
                {end_tag,<<"a">>},
                {end_tag,<<"div">>},
                {start_tag,<<"div">>,[{<<"class">>,<<"b">>}],false},
                {start_tag,<<"div">>,[{<<"class">>,<<"c">>}],false},
                {end_tag,<<"div">>},
                {end_tag,<<"div">>}],
    ?assertEqual(
       {ok, {<<"html">>, [],
        [{<<"head">>, [], []},
         {<<"body">>, [],
          [{<<"div">>, [{<<"class">>, <<"a">>}], [{<<"a">>, [{<<"name">>, <<"#anchor">>}], []}]},
           {<<"div">>, [{<<"class">>, <<"b">>}], [{<<"div">>, [{<<"class">>, <<"c">>}], []}]}
          ]}]}},
       z_html_parse:parse_tokens(D4)),
    D5 = [{start_tag,<<"html">>,[],false},
          {data,<<"\n">>,true},
          {data,<<"boo">>,false},
          {data,<<"hoo">>,false},
          {data,<<"\n">>,true},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {ok, {<<"html">>, [], [<<"\nboohoo\n">>]}},
       z_html_parse:parse_tokens(D5)),
    D6 = [{start_tag,<<"html">>,[],false},
          {data,<<"\n">>,true},
          {data,<<"\n">>,true},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {ok, {<<"html">>, [], []}},
       z_html_parse:parse_tokens(D6)),
    D7 = [{start_tag,<<"html">>,[],false},
          {start_tag,<<"ul">>,[],false},
          {start_tag,<<"li">>,[],false},
          {data,<<"word">>,false},
          {start_tag,<<"li">>,[],false},
          {data,<<"up">>,false},
          {end_tag,<<"li">>},
          {start_tag,<<"li">>,[],false},
          {data,<<"fdsa">>,false},
          {start_tag,<<"br">>,[],true},
          {data,<<"asdf">>,false},
          {end_tag,<<"ul">>},
          {end_tag,<<"html">>}],
    ?assertEqual(
       {ok, {<<"html">>, [],
        [{<<"ul">>, [],
          [{<<"li">>, [], [<<"word">>]},
           {<<"li">>, [], [<<"up">>]},
           {<<"li">>, [], [<<"fdsa">>,{<<"br">>, [], []}, <<"asdf">>]}]}]}},
       z_html_parse:parse_tokens(D7)),
    ok.

destack_test() ->
    Options = #{
        mode => html,
        escape => true
    },
    {<<"a">>, [], []} =
        z_html_parse:destack([{<<"a">>, [], []}]),
    {<<"a">>, [], [{<<"b">>, [], []}]} =
        z_html_parse:destack([{<<"b">>, [], []}, {<<"a">>, [], []}]),
    {<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]} =
        z_html_parse:destack([{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}]),
    [{<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]}] =
        z_html_parse:destack(<<"b">>,
                [{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}],
                Options),
    [{<<"b">>, [], [{<<"c">>, [], []}]}, {<<"a">>, [], []}] =
        z_html_parse:destack(<<"c">>,
                [{<<"c">>, [], []}, {<<"b">>, [], []},{<<"a">>, [], []}],
                Options),
    ok.

doctype_test() ->
    ?assertEqual(
       {ok, {<<"html">>,[],[{<<"head">>,[],[]}]}},
       z_html_parse:parse("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
                           "<html><head></head></body></html>")),
    %% http://code.google.com/p/mochiweb/issues/detail?id=52
    ?assertEqual(
       {ok, {<<"html">>,[],[{<<"head">>,[],[]}]}},
       z_html_parse:parse("<html>"
                           "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
                           "<head></head></body></html>")),
    %% http://github.com/mochi/mochiweb/pull/13
    ?assertEqual(
       {ok, {<<"html">>,[],[{<<"head">>,[],[]}]}},
       z_html_parse:parse("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"/>"
                           "<html>"
                           "<head></head></body></html>")),
    ok.

dumb_br_test() ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=71
    ?assertEqual(
       {ok, {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]}},
       z_html_parse:parse("<div><br/><br/>z</br/></br/></div>")),
    ?assertEqual(
       {ok, {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]}},
       z_html_parse:parse("<div><br><br>z</br/></br/></div>")),
    ?assertEqual(
       {ok, {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>, {<<"br">>, [], []}, {<<"br">>, [], []}]}},
       z_html_parse:parse("<div><br><br>z<br/><br/></div>")),
    ?assertEqual(
       {ok, {<<"div">>,[],[{<<"br">>, [], []}, {<<"br">>, [], []}, <<"z">>]}},
       z_html_parse:parse("<div><br><br>z</br></br></div>")).


php_test() ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=71
    ?assertEqual(
       [{pi, <<"php\n">>}],
       z_html_parse:tokens(
         "<?php\n?>")),
    ?assertEqual(
       {ok, {<<"div">>, [], [{pi, <<"php\n">>}]}},
       z_html_parse:parse(
         "<div><?php\n?></div>")),
    ok.

parse_unquoted_attr_test() ->
    D0 = <<"<html><img src=/images/icon.png/></html>">>,
    ?assertEqual(
        {ok, {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
        ]}},
        z_html_parse:parse(D0)),
    D1 = <<"<html><img src=/images/icon.png></img></html>">>,
        ?assertEqual(
            {ok, {<<"html">>,[],[
                { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
            ]}},
            z_html_parse:parse(D1)),
    D2 = <<"<html><img src=/images/icon&gt;.png width=100></img></html>">>,
        ?assertEqual(
            {ok, {<<"html">>,[],[
                { <<"img">>, [ { <<"src">>, <<"/images/icon>.png">> }, { <<"width">>, <<"100">> } ], [] }
            ]}},
            z_html_parse:parse(D2)),
    ok.        
    
parse_quoted_attr_test() ->    
    D0 = <<"<html><img src='/images/icon.png'></html>">>,
    ?assertEqual(
        {ok, {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png">> } ], [] }
        ]}},
        z_html_parse:parse(D0)),     

    D1 = <<"<html><img src=\"/images/icon.png'></html>">>,
    ?assertEqual(
        {ok, {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon.png'></html>">> } ], [] }
        ]}},
        z_html_parse:parse(D1)),     

    D2 = <<"<html><img src=\"/images/icon&gt;.png\"></html>">>,
    ?assertEqual(
        {ok, {<<"html">>,[],[
            { <<"img">>, [ { <<"src">>, <<"/images/icon>.png">> } ], [] }
        ]}},
        z_html_parse:parse(D2)),

    %% Quoted attributes can contain whitespace and newlines
    D3 = <<"<html><a href=\"#\" onclick=\"javascript: test(1,\ntrue);\"></html>">>,
    ?assertEqual(
        {ok, {<<"html">>,[],[
            { <<"a">>, [ { <<"href">>, <<"#">> }, {<<"onclick">>, <<"javascript: test(1,\ntrue);">>} ], [] }
        ]}},
        z_html_parse:parse(D3)),     
    ok.

parse_missing_attr_name_test() ->
    D0 = <<"<html =black></html>">>,
    ?assertEqual(
        {ok, {<<"html">>, [ { <<"=">>, <<"=">> }, { <<"black">>, <<"black">> } ], [] }},
        z_html_parse:parse(D0)),
    ok.

parse_amps_attr_test() ->
    D0 = <<"<a href=\"/hello?test=1&amp;that=2\"></a>">>,
    ?assertEqual(
       {ok, {<<"a">>, [ { <<"href">>, <<"/hello?test=1&that=2">> }], [] }},
       z_html_parse:parse(D0)),
    
    D1 = <<"<a href=\"/hello?test=1&that=2\"></a>">>,
    ?assertEqual(
       {ok, {<<"a">>, [ { <<"href">>, <<"/hello?test=1&that=2">> }], [] }},
       z_html_parse:parse(D1)),

    D2 = <<"<a href=\"/hello?test=123&that=2&amp;this=too\"></a>">>,
    ?assertEqual(
       {ok, {<<"a">>, [ { <<"href">>, <<"/hello?test=123&that=2&this=too">> }], [] }},
       z_html_parse:parse(D2)),

    D3 = <<"<a href=\"/product/54?c=hk-machine&id=1008&shop=auto-oko-74-H\"></a>">>,
    ?assertEqual(
       {ok, {<<"a">>, [ { <<"href">>, <<"/product/54?c=hk-machine&id=1008&shop=auto-oko-74-H">> }], [] }},
       z_html_parse:parse(D3)),

    D4 = <<"<a href=\"test?a=1&amp=1008\"></a>">>,
    ?assertEqual(
       {ok, {<<"a">>, [ { <<"href">>, <<"test?a=1&amp=1008">> }], [] }},
       z_html_parse:parse(D4)),
    
    ok.

parse_broken_pi_test() ->
    D0 = <<"<html><?xml:namespace prefix = o ns = \"urn:schemas-microsoft-com:office:office\" /></html>">>,
    ?assertEqual(
        {ok, {<<"html">>, [], [
            { pi, <<"xml:namespace">>, [ { <<"prefix">>, <<"o">> }, 
                                         { <<"ns">>, <<"urn:schemas-microsoft-com:office:office">> } ] }
        ] }},
        z_html_parse:parse(D0)),
    ok.

parse_funny_singletons_test() ->
    D0 = <<"<html><input><input>x</input></input></html>">>,
    ?assertEqual(
        {ok, {<<"html">>, [], [
            { <<"input">>, [], [] },
            { <<"input">>, [], [ <<"x">> ] }
        ] }},
        z_html_parse:parse(D0)),
    ok.

to_html_singleton_test() ->
    D0 = <<"<link />">>,
    T0 = {<<"link">>,[],[]},
    ?assertEqual(D0, iolist_to_binary(z_html_parse:to_html(T0))),

    D1 = <<"<head><link /></head>">>,
    T1 = {<<"head">>,[],[{<<"link">>,[],[]}]},
    ?assertEqual(D1, iolist_to_binary(z_html_parse:to_html(T1))),

    D2 = <<"<head><link /><link /></head>">>,
    T2 = {<<"head">>,[],[{<<"link">>,[],[]}, {<<"link">>,[],[]}]},
    ?assertEqual(D2, iolist_to_binary(z_html_parse:to_html(T2))),

    %% Make sure singletons are converted to singletons.
    D3 = <<"<head><link /></head>">>,
    T3 = {<<"head">>,[],[{<<"link">>,[],[<<"funny">>]}]},
    ?assertEqual(D3, iolist_to_binary(z_html_parse:to_html(T3))),

    D4 = <<"<link />">>,
    T4 = {<<"link">>,[],[<<"funny">>]},
    ?assertEqual(D4, iolist_to_binary(z_html_parse:to_html(T4))),

    D5 = <<"<source srcset=\"logo.svg\" type=\"image/svg+xml\" />">>,
    T5 = {<<"source">>,[{<<"srcset">>, <<"logo.svg">>},
                        {<<"type">>, <<"image/svg+xml">>}],[]},
    ?assertEqual(D5, iolist_to_binary(z_html_parse:to_html(T5))),

    ok.

parse_charref_test() ->
    %% Normal charref
    D0 = <<"<div>&amp;</div>">>,
    ?assertEqual(
       {ok, {<<"div">>, [], [<<"&">>]}},
       z_html_parse:parse(D0)),

    %% Missing semicolon in the middle. 
    D1 = <<"<div>&amp &amp;</div>">>,
    ?assertEqual(
       {ok, {<<"div">>, [], [<<"& &">>]}},
       z_html_parse:parse(D1)),

    %% Missing semicolon on the last enitity
    D2 = <<"<div>&amp &amp</div>">>,
    ?assertEqual(
       {ok, {<<"div">>, [], [<<"& &">>]}},
       z_html_parse:parse(D2)),

    D3 = <<"<div>&amp&amp</div>">>,
    ?assertEqual(
       {ok, {<<"div">>, [], [<<"&&">>]}},
       z_html_parse:parse(D3)),

    D4 = <<"<div>&amp</div>">>,
    ?assertEqual(
       {ok, {<<"div">>, [], [<<"&">>]}},
       z_html_parse:parse(D4)),

    ok.

parse_charref_garbage_in_garbage_out_test() ->
    %% faulty charref is left alone
    D1 = <<"<div>&amp. test</div>">>,
    ?assertEqual(
       {ok, {<<"div">>, [], [<<"&amp. test">>]}},
       z_html_parse:parse(D1)),
    
    ok.

parse_invalid_charref_test() ->
    D1 = <<"<i>&#55357;</i>">>,
    ?assertEqual({ok, {<<"i">>,[],[<<"&#55357;">>]}}, 
                 z_html_parse:parse(D1)),
    ok.

parse_amp_test_() ->
    [?_assertEqual(
       {ok, {<<"html">>,[],
        [{<<"body">>,[{<<"onload">>,<<"javascript:A('1&2')">>}],[]}]}},
       z_html_parse:parse("<html><body onload=\"javascript:A('1&2')\"></body></html>")),
     ?_assertEqual(
        {ok, {<<"html">>,[],
         [{<<"body">>,[{<<"onload">>,<<"javascript:A('1& 2')">>}],[]}]}},
        z_html_parse:parse("<html><body onload=\"javascript:A('1& 2')\"></body></html>")),
     ?_assertEqual(
        {ok, {<<"html">>,[],
         [{<<"body">>,[],[<<"& ">>]}]}},
        z_html_parse:parse("<html><body>& </body></html>")),
     ?_assertEqual(
        {ok, {<<"html">>,[],
         [{<<"body">>,[],[<<"&">>]}]}},
        z_html_parse:parse("<html><body>&</body></html>"))].

parse_unescaped_lt_test() ->
    D1 = <<"<div> < < <a href=\"/\">Back</a></div>">>,
    ?assertEqual(
        {ok, {<<"div">>, [], [<<" < < ">>, {<<"a">>, [{<<"href">>, <<"/">>}], 
                                       [<<"Back">>]}]}},
        z_html_parse:parse(D1)),

    D2 = <<"<div> << <a href=\"/\">Back</a></div>">>,
    ?assertEqual(
        {ok, {<<"div">>, [], [<<" << ">>, {<<"a">>, [{<<"href">>, <<"/">>}], 
                                      [<<"Back">>]}]}},
    z_html_parse:parse(D2)).

parse_unclosed_tbody_test() ->
    D1 = <<"<table><tbody><tr><td>1</td><td>2</td></tr><tbody><tr><td>a</td></tr></tbody></table>">>,
    %% Browsers parse this as two separate tbody's and not a nested one. This should 
    %% be handled in the same way as li's and options are handled.
    ?assertMatch({ok, {<<"table">>, [], [
        {<<"tbody">>, [], _},
        {<<"tbody">>, [], _}
    ]}}, z_html_parse:parse(D1)).

parse_table_with_omitted_tr_and_td_close_test() ->
    D1 = "<table><tr><td>1<td>2<tr><td>3<td>4</table",
    ?assertEqual({ok, {<<"table">>, [], [
        {<<"tr">>, [], [{<<"td">>, [], [<<"1">>]}, {<<"td">>, [], [<<"2">>]}]},
        {<<"tr">>, [], [{<<"td">>,[], [<<"3">>]}, {<<"td">>, [], [<<"4">>]}]} 
    ]}}, z_html_parse:parse(D1)),
    ok.

parse_table_with_omitted_close_tags_test() ->
    %% Close tags inside tables are optional.
    D1 = "<table>"
        "<thead><tr><td>1<td>2"
        "<tfoot><tr><td>1<td>2"
        "<tbody><tr><td>1<td>2<tr><td>2<td>3"
        "<tbody><tr><td>1<td>2<td>3<tr><td>4<td>5<td>6"
    "</table",

    ?assertEqual({ok, {<<"table">>,[],
         [{<<"thead">>,[],
           [{<<"tr">>,[],[{<<"td">>,[],[<<"1">>]},{<<"td">>,[],[<<"2">>]}]}]},
          {<<"tfoot">>,[],
           [{<<"tr">>,[],[{<<"td">>,[],[<<"1">>]},{<<"td">>,[],[<<"2">>]}]}]},
          {<<"tbody">>,[],
           [{<<"tr">>,[], [{<<"td">>,[],[<<"1">>]}, {<<"td">>,[], [<<"2">>]}]},
            {<<"tr">>,[], [{<<"td">>,[],[<<"2">>]},{<<"td">>,[],[<<"3">>]}]}]},
          {<<"tbody">>,[],
           [{<<"tr">>,[], [{<<"td">>,[],[<<"1">>]}, {<<"td">>,[],[<<"2">>]}, {<<"td">>,[], [<<"3">>]}]},
            {<<"tr">>,[], [{<<"td">>,[],[<<"4">>]}, {<<"td">>,[],[<<"5">>]}, {<<"td">>,[],[<<"6">>]}]}]}]}}, z_html_parse:parse(D1)),

    D2 = "<table><tfoot><tr><td>1<td>2</table",
    ?assertEqual({ok, {<<"table">>,[], [{<<"tfoot">>,[], [
        {<<"tr">>,[], [
            {<<"td">>,[],[<<"1">>]},
            {<<"td">>,[],[<<"2">>]}
            ]}
    ]}]}}, z_html_parse:parse(D2)),
    ok.

parse_table_colgroups_test() ->
    D1 = "<table>"
        "<colgroup width=\"20\">"
        "<col span=\"39\">"
        "<col id=\"co1l\">"
        "<colgroup width=\"0*\">"
        "<thead><tr><td>1<td>2<tr><td>2<td>3"
        "<tbody><tr><td>1<td>2<td>3<tr><td>4<td>5<td>6"
    "</table",

    ?assertMatch({ok, {<<"table">>,[],
        [{<<"colgroup">>,
          [{<<"width">>,<<"20">>}],
          [{<<"col">>,[{<<"span">>,<<"39">>}],[]},
           {<<"col">>,[{<<"id">>,<<"co1l">>}],[]}]},
         {<<"colgroup">>,[{<<"width">>,<<"0*">>}],[]},
         {<<"thead">>,[],
          [{<<"tr">>,[],[{<<"td">>,[],[<<"1">>]},{<<"td">>,[],[<<"2">>]}]},
           {<<"tr">>,[],[{<<"td">>,[],[<<"2">>]},{<<"td">>,[],[<<"3">>]}]}]},
         {<<"tbody">>,[],
          [{<<"tr">>,[], [{<<"td">>,[],[<<"1">>]}, {<<"td">>,[],[<<"2">>]}, {<<"td">>,[],[<<"3">>]}]},
           {<<"tr">>,[], [{<<"td">>,[],[<<"4">>]}, {<<"td">>,[],[<<"5">>]}, {<<"td">>,[],[<<"6">>]}]}]}]}}, z_html_parse:parse(D1)),

    ok.

parse_table_elements_without_table_test() ->
    % Table elements are omitted when there is no table tag
    D1 = "<html><tr><td>1<td>2</html>",
    ?assertEqual({ok, {<<"html">>,[],[<<"1">>,<<"2">>]}}, z_html_parse:parse(D1)),
    ok.

nested_table_test() ->
    %% Nested table with tbody's
    D = "<table>"
            "<tbody>"
                "<tr>"
                    "<td>"
                        "<table>"
                           "<tbody>"
                               "<tr><td>1</td><td>2</td></tr>"
                               "<tr><td>3</td><td>4</td></tr>"
                            "</tbody>"
                         "</table>"
                    "</td>"
                    "<td>"
                        "<table>"
                           "<tbody>"
                               "<tr><td>3</td></tr>"
                               "<tr><td>4</td></tr>"
                            "</tbody>"
                        "</table>"
                    "</td>"
               "</tr>"
           "</tbody>"
       "</table>",

    % All optional closing tags removed
    D1 = "<table>"
            "<tbody>"
                "<tr>"
                    "<td>"
                        "<table>"
                           "<tbody>"
                               "<tr><td>1<td>2"
                               "<tr><td>3<td>4"
                         "</table>"
                    "<td>"
                        "<table>"
                           "<tbody>"
                               "<tr><td>3"
                               "<tr><td>4"
                        "</table>"
       "</table>",

    T = {<<"table">>,[],
                  [{<<"tbody">>,[],
                    [{<<"tr">>,[],
                      [{<<"td">>,[],
                        [{<<"table">>,[],
                          [{<<"tbody">>,[],
                            [{<<"tr">>,[], [{<<"td">>,[],[<<"1">>]},{<<"td">>,[],[<<"2">>]}]},
                             {<<"tr">>,[], [{<<"td">>,[],[<<"3">>]},{<<"td">>,[],[<<"4">>]}]}]}]}]},
                       {<<"td">>,[],
                        [{<<"table">>,[],
                          [{<<"tbody">>,[],
                            [{<<"tr">>,[],[{<<"td">>,[],[<<"3">>]}]},
                             {<<"tr">>,[],[{<<"td">>,[],[<<"4">>]}]}]}]}]}
                      ]}
                    ]}
                  ]},

    ?assertEqual({ok, T}, z_html_parse:parse(D)),
    ?assertEqual({ok, T}, z_html_parse:parse(D1)),

    ok.

parse_to_map_test() ->
    D = <<"<A c=1>hal<BR/>lo</A>">>,
    T = #{<<"A">> =>
      [#{<<"@attributes">> => [{<<"c">>,<<"1">>}],
         <<"BR">> => []}]},
    T1 = #{<<"a">> =>
      [#{<<"@attributes">> => [{<<"c">>,<<"1">>}],
         <<"br">> => []}]},

    ?assertEqual({ok, T}, z_html_parse:parse_to_map(D, #{ mode => xml, lowercase => false })),
    ?assertEqual({ok, T1}, z_html_parse:parse_to_map(D, #{ mode => xml, lowercase => true })),
    ok.
