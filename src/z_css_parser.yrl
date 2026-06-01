%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2026 Marc Worrell
%% @doc Grammar for strict CSS parser. Based on http://www.w3.org/TR/CSS21/grammar.html
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

Nonterminals

    Stylesheet
    Charset
    Import
    ImportList
    Location
    MediaList
    MediaQuery
    MediaTerm
    Rules
    RuleSetList
    RuleSet
    Media
    FontFace
    FontFaceBody
    FontFaceToken
    BadAtRule
    BadAtRulePrelude
    BadAtRulePreludeToken
    BadAtRuleBlock
    BadAtRuleBody
    BadAtRuleBodyToken
    Page
    DeclarationList
    Declaration
    Prio
    Expr
    Function
    SelectorList
    Selector
    SimpleSelector
    Term
    Class
    Attrib
    AttrOptVal
    AttrVal
    Pseudo
    PseudoPage
    PseudoVal
    .

Terminals

    badcomment
    includes
    dashmatch
    string
    bad_string
    ident
    hash
    import_sym
    font_face_sym
    page_sym
    media_sym
    charset_sym
    bad_at_rule
    important_sym
    ems
    exs
    length
    angle
    time
    freq
    resolution
    dimension
    percentage
    number
    uri
    bad_uri
    function
    ';'
    '{'
    '}'
    '['
    ']'
    '('
    ')'
    ','
    '.'
    ':'
    '*'
    '/'
    '='
    '>'
    '-'
    '+'
    .

Rootsymbol
    Stylesheet.

%% Expected shift/reduce conflicts
Expect 0.

Stylesheet -> Charset ImportList Rules      : {stylesheet, '$1', '$2', '$3'}.

Charset -> '$empty'                     : no_charset.
Charset -> charset_sym string ';'       : {charset, '$2'}.

Import -> import_sym Location MediaList ';'  : {import, '$2', '$3'}.

ImportList -> '$empty'                       : no_import.
ImportList -> Import ImportList              : no_import.

Location -> string                        : '$1'.
Location -> uri                           : '$1'.

Media -> media_sym MediaList '{' RuleSetList '}' : {media, '$2', '$4'}.

MediaList -> MediaQuery                       : ['$1'].
MediaList -> MediaQuery ',' MediaList         : ['$1' | '$3'].

MediaQuery -> MediaTerm                       : ['$1'].
MediaQuery -> MediaQuery MediaTerm            : '$1' ++ ['$2'].

MediaTerm -> ident                            : '$1'.
MediaTerm -> '(' ident ')'                    : {media_feature, '$2', undefined}.
MediaTerm -> '(' ident ':' Expr ')'           : {media_feature, '$2', '$4'}.

Rules -> '$empty'                         : [].
Rules -> RuleSet Rules                    : ['$1' | '$2'].
Rules -> Media Rules                      : ['$1' | '$2'].
Rules -> FontFace Rules                   : '$2'.
Rules -> BadAtRule Rules                  : ['$1' | '$2'].
Rules -> Page Rules                       : ['$1' | '$2'].

RuleSetList -> '$empty'                   : [].
RuleSetList -> RuleSet RuleSetList        : ['$1' | '$2'].
RuleSetList -> FontFace RuleSetList       : '$2'.
RuleSetList -> BadAtRule RuleSetList      : ['$1' | '$2'].

RuleSet -> SelectorList '{' DeclarationList '}' : {rule, '$1', '$3'}.

FontFace -> font_face_sym '{' FontFaceBody '}' : no_font_face.

FontFaceBody -> '$empty'                   : [].
FontFaceBody -> FontFaceToken FontFaceBody : ['$1' | '$2'].

FontFaceToken -> badcomment                : '$1'.
FontFaceToken -> includes                  : '$1'.
FontFaceToken -> dashmatch                 : '$1'.
FontFaceToken -> string                    : '$1'.
FontFaceToken -> bad_string                : '$1'.
FontFaceToken -> ident                     : '$1'.
FontFaceToken -> hash                      : '$1'.
FontFaceToken -> import_sym                : '$1'.
FontFaceToken -> page_sym                  : '$1'.
FontFaceToken -> media_sym                 : '$1'.
FontFaceToken -> charset_sym               : '$1'.
FontFaceToken -> bad_at_rule               : '$1'.
FontFaceToken -> important_sym             : '$1'.
FontFaceToken -> ems                       : '$1'.
FontFaceToken -> exs                       : '$1'.
FontFaceToken -> length                    : '$1'.
FontFaceToken -> angle                     : '$1'.
FontFaceToken -> time                      : '$1'.
FontFaceToken -> freq                      : '$1'.
FontFaceToken -> resolution                : '$1'.
FontFaceToken -> dimension                 : '$1'.
FontFaceToken -> percentage                : '$1'.
FontFaceToken -> number                    : '$1'.
FontFaceToken -> uri                       : '$1'.
FontFaceToken -> bad_uri                   : '$1'.
FontFaceToken -> function                  : '$1'.
FontFaceToken -> ';'                       : '$1'.
FontFaceToken -> '['                       : '$1'.
FontFaceToken -> ']'                       : '$1'.
FontFaceToken -> '('                       : '$1'.
FontFaceToken -> ')'                       : '$1'.
FontFaceToken -> ','                       : '$1'.
FontFaceToken -> '.'                       : '$1'.
FontFaceToken -> ':'                       : '$1'.
FontFaceToken -> '*'                       : '$1'.
FontFaceToken -> '/'                       : '$1'.
FontFaceToken -> '='                       : '$1'.
FontFaceToken -> '>'                       : '$1'.
FontFaceToken -> '-'                       : '$1'.
FontFaceToken -> '+'                       : '$1'.

BadAtRule -> bad_at_rule BadAtRulePrelude ';'   : bad_at_rule.
BadAtRule -> bad_at_rule BadAtRulePrelude BadAtRuleBlock : bad_at_rule.

BadAtRulePrelude -> '$empty'                         : [].
BadAtRulePrelude -> BadAtRulePreludeToken BadAtRulePrelude : ['$1' | '$2'].

BadAtRuleBlock -> '{' BadAtRuleBody '}'              : [].

BadAtRuleBody -> '$empty'                            : [].
BadAtRuleBody -> BadAtRuleBodyToken BadAtRuleBody    : ['$1' | '$2'].
BadAtRuleBody -> BadAtRuleBlock BadAtRuleBody        : '$2'.

BadAtRulePreludeToken -> badcomment       : '$1'.
BadAtRulePreludeToken -> includes         : '$1'.
BadAtRulePreludeToken -> dashmatch        : '$1'.
BadAtRulePreludeToken -> string           : '$1'.
BadAtRulePreludeToken -> bad_string       : '$1'.
BadAtRulePreludeToken -> ident            : '$1'.
BadAtRulePreludeToken -> hash             : '$1'.
BadAtRulePreludeToken -> import_sym       : '$1'.
BadAtRulePreludeToken -> font_face_sym    : '$1'.
BadAtRulePreludeToken -> page_sym         : '$1'.
BadAtRulePreludeToken -> media_sym        : '$1'.
BadAtRulePreludeToken -> charset_sym      : '$1'.
BadAtRulePreludeToken -> bad_at_rule      : '$1'.
BadAtRulePreludeToken -> important_sym    : '$1'.
BadAtRulePreludeToken -> ems              : '$1'.
BadAtRulePreludeToken -> exs              : '$1'.
BadAtRulePreludeToken -> length           : '$1'.
BadAtRulePreludeToken -> angle            : '$1'.
BadAtRulePreludeToken -> time             : '$1'.
BadAtRulePreludeToken -> freq             : '$1'.
BadAtRulePreludeToken -> resolution       : '$1'.
BadAtRulePreludeToken -> dimension        : '$1'.
BadAtRulePreludeToken -> percentage       : '$1'.
BadAtRulePreludeToken -> number           : '$1'.
BadAtRulePreludeToken -> uri              : '$1'.
BadAtRulePreludeToken -> bad_uri          : '$1'.
BadAtRulePreludeToken -> function         : '$1'.
BadAtRulePreludeToken -> '['              : '$1'.
BadAtRulePreludeToken -> ']'              : '$1'.
BadAtRulePreludeToken -> '('              : '$1'.
BadAtRulePreludeToken -> ')'              : '$1'.
BadAtRulePreludeToken -> ','              : '$1'.
BadAtRulePreludeToken -> '.'              : '$1'.
BadAtRulePreludeToken -> ':'              : '$1'.
BadAtRulePreludeToken -> '*'              : '$1'.
BadAtRulePreludeToken -> '/'              : '$1'.
BadAtRulePreludeToken -> '='              : '$1'.
BadAtRulePreludeToken -> '>'              : '$1'.
BadAtRulePreludeToken -> '-'              : '$1'.
BadAtRulePreludeToken -> '+'              : '$1'.

BadAtRuleBodyToken -> BadAtRulePreludeToken : '$1'.
BadAtRuleBodyToken -> ';'                   : '$1'.

SelectorList -> Selector                    : ['$1'].
SelectorList -> SelectorList ',' Selector   : '$1' ++ ['$3'].

DeclarationList -> Declaration                      : '$1'.
DeclarationList -> DeclarationList ';' Declaration  : '$1' ++ '$3'.

Declaration -> '$empty'                     : [].
Declaration -> ident ':' Expr Prio          : [ {declaration, '$1', '$3', '$4'} ].

Prio -> '$empty'                            : normal.
Prio -> important_sym                       : important.

Expr -> Term                                : '$1'.
Expr -> Expr '/' Term                       : {operator, '/', '$1', '$3'}.
Expr -> Expr ',' Term                       : {operator, ',', '$1', '$3'}.

Term -> '-' Term                            : {operator, '-', '$2'}.
Term -> '+' Term                            : {operator, '+', '$2'}.
Term -> number                              : '$1'.
Term -> percentage                          : '$1'.
Term -> length                              : '$1'.
Term -> ems                                 : '$1'.
Term -> exs                                 : '$1'.
Term -> angle                               : '$1'.
Term -> time                                : '$1'.
Term -> freq                                : '$1'.
Term -> resolution                          : '$1'.
Term -> dimension                           : '$1'.
Term -> string                              : '$1'.
Term -> ident                               : '$1'.
Term -> uri                                 : '$1'.
Term -> hash                                : '$1'.
Term -> Function                            : '$1'.

Function -> function Expr ')'               : {function, '$1', '$2'}.

Selector -> SimpleSelector                  : [{none, '$1'}].
Selector -> Selector SimpleSelector         : '$1' ++ [{none, '$2'}].
Selector -> Selector '+' SimpleSelector     : '$1' ++ [{'+', '$3'}].
Selector -> Selector '>' SimpleSelector     : '$1' ++ [{'>', '$3'}].

SimpleSelector -> '*'                   : '*'.
SimpleSelector -> ident                 : '$1'.
SimpleSelector -> hash                  : '$1'.
SimpleSelector -> Class                 : '$1'.
SimpleSelector -> Attrib                : '$1'.
SimpleSelector -> Pseudo                : '$1'.

Class -> '.' ident                      : {class, '$2'}.

Attrib -> '[' ident AttrOptVal ']'      : {attrib, '$2', '$3'}.

AttrOptVal -> '$empty'                  : undefined.
AttrOptVal -> '=' AttrVal               : {'=', '$2'}.
AttrOptVal -> includes AttrVal          : {includes, '$2'}.
AttrOptVal -> dashmatch AttrVal         : {dashmatch, '$2'}.

AttrVal -> ident                        : '$1'.
AttrVal -> string                       : '$1'.

Pseudo -> ':' PseudoVal                 : {pseudo, '$2'}.

PseudoVal -> ident                      : '$1'.
PseudoVal -> function ident ')'         : {function, '$1', '$2'}.

Page -> page_sym PseudoPage '{' DeclarationList '}' : {page, '$2', '$4'}.

PseudoPage -> '$empty'                  : undefined.
PseudoPage -> ':' ident                 : '$1'.
