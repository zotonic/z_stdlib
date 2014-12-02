%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%%
%% @doc Grammar for strict CSS parser. Based on http://www.w3.org/TR/CSS21/grammar.html

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

Nonterminals

    Stylesheet
    Charset
    Import
    Location
    MediaList
    Rules
    RuleSetList
    RuleSet
    Media
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

    % badcomment
    includes
    dashmatch
    string
    % bad_string
    ident
    hash
    import_sym
    page_sym
    media_sym
    charset_sym
    important_sym
    ems
    exs
    length
    angle
    time
    freq
    dimension
    percentage
    number
    uri
    % bad_uri
    function
    ';'
    '{'
    '}'
    '['
    ']'
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

Stylesheet -> Charset Import Rules      : {stylesheet, '$1', '$2', '$3'}.

Charset -> '$empty'                     : no_charset.
Charset -> charset_sym string ';'       : {charset, '$2'}.

Import -> '$empty'                           : no_import.
Import -> import_sym Location MediaList ';'  : {import, '$2', '$3'}.

Location -> string                        : '$1'.
Location -> uri                           : '$1'.

Media -> media_sym MediaList '{' RuleSetList '}' : {media, '$2', '$4'}.

MediaList -> ident                       : ['$1'].
MediaList -> ident ',' MediaList         : ['$1'] ++ '$3'.

Rules -> '$empty'                         : [].
Rules -> RuleSet Rules                    : ['$1' | '$2'].
Rules -> Media Rules                      : ['$1' | '$2'].
Rules -> Page Rules                       : ['$1' | '$2'].

RuleSetList -> '$empty'                   : [].
RuleSetList -> RuleSet RuleSetList        : ['$1' | '$2'].

RuleSet -> SelectorList '{' DeclarationList '}' : {rule, '$1', '$3'}.

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


