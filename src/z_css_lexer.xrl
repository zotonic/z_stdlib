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

% FLEX definition from http://www.w3.org/TR/CSS21/grammar.html

Definitions.

CSS_h           = ([0-9a-fA-F])
CSS_nonascii    = [\240-\4177777]
CSS_unicode     = \\{CSS_h}+[\s\r\n\f\t]?
CSS_escape      = ({CSS_unicode}|\\[^\r\n\f0-9a-fA-F])
CSS_nmstart     = ([_a-zA-Z]|{CSS_nonascii}|{CSS_escape})
CSS_nmchar      = ([_a-zA-Z0-9-]|{CSS_nonascii}|{CSS_escape})
CSS_string1     = (\"([^\n\r\f\\"]|\\{CSS_nl}|{CSS_escape})*\")
CSS_string2     = (\'([^\n\r\f\\']|\\{CSS_nl}|{CSS_escape})*\')
CSS_badstring1  = (\"([^\n\r\f\\"]|\\{CSS_nl}|{CSS_escape})*\\?)
CSS_badstring2  = (\'([^\n\r\f\\']|\\{CSS_nl}|{CSS_escape})*\\?)
CSS_badcomment1 = (\/\*[^*]*\*+([^/*][^*]*\*+)*)
CSS_badcomment2 = (\/\*[^*]*(\*+[^/*][^*]*)*)
CSS_baduri1     = ([uU][rR][lL]\({CSS_w}([!#$%&*-\[\]-~]|{CSS_nonascii}|{CSS_escape})*{w})
CSS_baduri2     = ([uU][rR][lL]\({CSS_w}{CSS_string}{CSS_w})
CSS_baduri3     = ([uU][rR][lL]\({CSS_w}{CSS_badstring})
CSS_comment     = (\/\*[^*]*\*+([^/*][^*]*\*+)*\/)
CSS_ident       = (-?{CSS_nmstart}{CSS_nmchar}*)
CSS_name        = ({CSS_nmchar}+)
CSS_num         = ([0-9]+|[0-9]*"."[0-9]+)
CSS_string      = ({CSS_string1}|{CSS_string2})
CSS_badstring   = ({CSS_badstring1}|{CSS_badstring2})
CSS_badcomment  = ({CSS_badcomment1}|{CSS_badcomment2})
CSS_baduri      = ({CSS_baduri1}|{CSS_baduri2}|{CSS_baduri3})
CSS_url         = ([!#$%&*-~]|{CSS_nonascii}|{CSS_escape})*
CSS_s           = ([\s\r\n\f\t]+)
CSS_w           = ({CSS_s}?)
CSS_nl          = [\n|\r|\f]

A           = (a|A|\\0{0,4}(41|61)\s?)
C           = (c|C|\\0{0,4}(43|63)\s?)
D           = (d|D|\\0{0,4}(44|64)\s?)
E           = (e|E|\\0{0,4}(45|65)\s?)
G           = (g|G|\\0{0,4}(47|67)\s?|\\g|\\G)
H           = (h|H|\\0{0,4}(48|68)\s?|\\h|\\H)
I           = (i|I|\\0{0,4}(49|69)\s?|\\i|\\I)
K           = (k|K|\\0{0,4}(4b|6b)\s?|\\k|\\K)
L           = (l|L|\\0{0,4}(4c|6c)\s?|\\l|\\L)
M           = (m|M|\\0{0,4}(4d|6d)\s?|\\m|\\M)
N           = (n|N|\\0{0,4}(4e|6e)\s?|\\n|\\N)
O           = (o|O|\\0{0,4}(4f|6f)\s?|\\o|\\O)
P           = (p|P|\\0{0,4}(50|70)\s?|\\p|\\P)
R           = (r|R|\\0{0,4}(52|72)\s?|\\r|\\R)
S           = (s|S|\\0{0,4}(53|73)\s?|\\s|\\S)
T           = (t|T|\\0{0,4}(54|74)\s?|\\t|\\T)
U           = (u|U|\\0{0,4}(55|75)\s?|\\u|\\U)
X           = (x|X|\\0{0,4}(58|78)\s?|\\x|\\X)
Z           = (z|Z|\\0{0,4}(5a|7a)\s?|\\z|\\Z)

%%

Rules.

{CSS_s}                                 : skip_token.

\/\*[^*]*\*+([^/*][^*]*\*+)*\/          : skip_token.                                                         %% /* ignore comments */
{CSS_badcomment}                        : make_token(badcomment, TokenLine, TokenChars).                      %% /* unclosed comment at EOF */

<!--                                    : skip_token.
-->                                     : skip_token.
~=                                      : make_token(includes, TokenLine, TokenChars).
\|=                                     : make_token(dashmatch, TokenLine, TokenChars).

{CSS_string}                            : make_token(string, TokenLine, TokenChars).
{CSS_badstring}                         : make_token(bad_string, TokenLine, TokenChars).

{CSS_ident}                             : make_token(ident, TokenLine, TokenChars).

#{CSS_name}                             : make_token(hash, TokenLine, TokenChars).

@{I}{M}{P}{O}{R}{T}                     : make_token(import_sym, TokenLine, TokenChars).
@{P}{A}{G}{E}                           : make_token(page_sym, TokenLine, TokenChars).
@{M}{E}{D}{I}{A}                        : make_token(media_sym, TokenLine, TokenChars).
@{C}{H}{A}{R}{S}{E}{T}\s                : make_token(charset_sym, TokenLine, TokenChars).

!({w}|{CSS_comment})*{I}{M}{P}{O}{R}{T}{A}{N}{T}  : make_token(important_sym, TokenLine, TokenChars).

{CSS_num}{E}{M}                         : make_token(ems, TokenLine, TokenChars).
{CSS_num}{E}{X}                         : make_token(exs, TokenLine, TokenChars).
{CSS_num}{P}{X}                         : make_token(length, TokenLine, TokenChars).
{CSS_num}{C}{M}                         : make_token(length, TokenLine, TokenChars).
{CSS_num}{M}{M}                         : make_token(length, TokenLine, TokenChars).
{CSS_num}{I}{N}                         : make_token(length, TokenLine, TokenChars).
{CSS_num}{P}{T}                         : make_token(length, TokenLine, TokenChars).
{CSS_num}{P}{C}                         : make_token(length, TokenLine, TokenChars).
{CSS_num}{D}{E}{G}                      : make_token(angle, TokenLine, TokenChars).
{CSS_num}{R}{A}{D}                      : make_token(angle, TokenLine, TokenChars).
{CSS_num}{G}{R}{A}{D}                   : make_token(angle, TokenLine, TokenChars).
{CSS_num}{M}{S}                         : make_token(time, TokenLine, TokenChars).
{CSS_num}{S}                            : make_token(time, TokenLine, TokenChars).
{CSS_num}{H}{Z}                         : make_token(freq, TokenLine, TokenChars).
{CSS_num}{K}{H}{Z}                      : make_token(freq, TokenLine, TokenChars).
{CSS_num}{CSS_ident}                    : make_token(dimension, TokenLine, TokenChars).
{CSS_num}%                              : make_token(percentage, TokenLine, TokenChars).
{CSS_num}                               : make_token(number, TokenLine, TokenChars).
[uU][rR][lL]\({CSS_w}{CSS_string}{CSS_w}\)       : make_token(uri, TokenLine, TokenChars).
[uU][rR][lL]\({CSS_w}{CSS_url}{CSS_w}\)          : make_token(uri, TokenLine, TokenChars).
{CSS_baduri}                            : make_token(bad_uri, TokenLine, TokenChars).
{CSS_ident}\(                           : make_token(function, TokenLine, TokenChars).

.                                       : make_token(literal, TokenLine, TokenChars).    % **yytext


Erlang code.

make_token(literal, Line, ";") -> {token, {';', Line, ";"}};
make_token(literal, Line, "{") -> {token, {'{', Line, "{"}};
make_token(literal, Line, "}") -> {token, {'}', Line, "}"}};
make_token(literal, Line, "[") -> {token, {'[', Line, "["}};
make_token(literal, Line, "]") -> {token, {']', Line, "]"}};
make_token(literal, Line, "(") -> {token, {'(', Line, "("}};
make_token(literal, Line, ")") -> {token, {')', Line, ")"}};
make_token(literal, Line, ",") -> {token, {',', Line, ","}};
make_token(literal, Line, ".") -> {token, {'.', Line, "."}};
make_token(literal, Line, ":") -> {token, {':', Line, ":"}};
make_token(literal, Line, "/") -> {token, {'/', Line, "/"}};
make_token(literal, Line, "-") -> {token, {'-', Line, "-"}};
make_token(literal, Line, "+") -> {token, {'+', Line, "+"}};
make_token(literal, Line, "*") -> {token, {'*', Line, "*"}};
make_token(literal, Line, ">") -> {token, {'>', Line, ">"}};
make_token(literal, Line, "=") -> {token, {'=', Line, "="}};

make_token(literal, Line, [Char] = Chars) ->
    {token, {Char, Line, Chars}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.
