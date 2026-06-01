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
CSS_num         = ([0-9]+|[0-9]*\.[0-9]+)
CSS_string      = ({CSS_string1}|{CSS_string2})
CSS_badstring   = ({CSS_badstring1}|{CSS_badstring2})
CSS_badcomment  = ({CSS_badcomment1}|{CSS_badcomment2})
CSS_baduri      = ({CSS_baduri1}|{CSS_baduri2}|{CSS_baduri3})
CSS_url         = ([!#$%&*-~]|{CSS_nonascii}|{CSS_escape})*
CSS_s           = ([\s\r\n\f\t]+)
CSS_w           = ({CSS_s}?)
CSS_nl          = [\n\r\f]

A           = (a|A|\\0{0,4}(41|61)\s?)
B           = (b|B|\\0{0,4}(42|62)\s?|\\b|\\B)
C           = (c|C|\\0{0,4}(43|63)\s?)
D           = (d|D|\\0{0,4}(44|64)\s?)
E           = (e|E|\\0{0,4}(45|65)\s?)
F           = (f|F|\\0{0,4}(46|66)\s?|\\f|\\F)
G           = (g|G|\\0{0,4}(47|67)\s?|\\g|\\G)
H           = (h|H|\\0{0,4}(48|68)\s?|\\h|\\H)
I           = (i|I|\\0{0,4}(49|69)\s?|\\i|\\I)
K           = (k|K|\\0{0,4}(4b|6b)\s?|\\k|\\K)
L           = (l|L|\\0{0,4}(4c|6c)\s?|\\l|\\L)
M           = (m|M|\\0{0,4}(4d|6d)\s?|\\m|\\M)
N           = (n|N|\\0{0,4}(4e|6e)\s?|\\n|\\N)
O           = (o|O|\\0{0,4}(4f|6f)\s?|\\o|\\O)
P           = (p|P|\\0{0,4}(50|70)\s?|\\p|\\P)
Q           = (q|Q|\\0{0,4}(51|71)\s?|\\q|\\Q)
R           = (r|R|\\0{0,4}(52|72)\s?|\\r|\\R)
S           = (s|S|\\0{0,4}(53|73)\s?|\\s|\\S)
T           = (t|T|\\0{0,4}(54|74)\s?|\\t|\\T)
U           = (u|U|\\0{0,4}(55|75)\s?|\\u|\\U)
V           = (v|V|\\0{0,4}(56|76)\s?|\\v|\\V)
W           = (w|W|\\0{0,4}(57|77)\s?|\\w|\\W)
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
@{F}{O}{N}{T}-{F}{A}{C}{E}              : make_token(font_face_sym, TokenLine, TokenChars).
@{P}{A}{G}{E}                           : make_token(page_sym, TokenLine, TokenChars).
@{M}{E}{D}{I}{A}                        : make_token(media_sym, TokenLine, TokenChars).
@{C}{H}{A}{R}{S}{E}{T}\s                : make_token(charset_sym, TokenLine, TokenChars).

!({w}|{CSS_comment})*{I}{M}{P}{O}{R}{T}{A}{N}{T}  : make_token(important_sym, TokenLine, TokenChars).

{CSS_num}{CSS_ident}                    : make_dimension_token(TokenLine, TokenChars).
{CSS_num}%                              : make_token(percentage, TokenLine, TokenChars).
{CSS_num}                               : make_token(number, TokenLine, TokenChars).
[uU][rR][lL]\({CSS_w}{CSS_string}{CSS_w}\)       : make_token(uri, TokenLine, TokenChars).
[uU][rR][lL]\({CSS_w}{CSS_url}{CSS_w}\)          : make_token(uri, TokenLine, TokenChars).
{CSS_baduri}                            : make_token(bad_uri, TokenLine, TokenChars).
{CSS_ident}\(                           : make_token(function, TokenLine, TokenChars).

.                                       : make_token(literal, TokenLine, TokenChars).    % **yytext


Erlang code.

make_dimension_token(Line, Chars) ->
    {_Number, Unit} = lists:splitwith(fun is_num_char/1, Chars),
    make_token(dimension_unit(string:to_lower(Unit)), Line, Chars).

is_num_char(C) when C >= $0, C =< $9 -> true;
is_num_char($.) -> true;
is_num_char(_) -> false.

dimension_unit("em") -> ems;
dimension_unit("ex") -> exs;
dimension_unit("cap") -> length;
dimension_unit("ch") -> length;
dimension_unit("ic") -> length;
dimension_unit("lh") -> length;
dimension_unit("rem") -> length;
dimension_unit("rex") -> length;
dimension_unit("rcap") -> length;
dimension_unit("rch") -> length;
dimension_unit("ric") -> length;
dimension_unit("rlh") -> length;
dimension_unit("px") -> length;
dimension_unit("cm") -> length;
dimension_unit("mm") -> length;
dimension_unit("q") -> length;
dimension_unit("in") -> length;
dimension_unit("pt") -> length;
dimension_unit("pc") -> length;
dimension_unit("vw") -> length;
dimension_unit("vh") -> length;
dimension_unit("vi") -> length;
dimension_unit("vb") -> length;
dimension_unit("vmin") -> length;
dimension_unit("vmax") -> length;
dimension_unit("svw") -> length;
dimension_unit("svh") -> length;
dimension_unit("svi") -> length;
dimension_unit("svb") -> length;
dimension_unit("svmin") -> length;
dimension_unit("svmax") -> length;
dimension_unit("lvw") -> length;
dimension_unit("lvh") -> length;
dimension_unit("lvi") -> length;
dimension_unit("lvb") -> length;
dimension_unit("lvmin") -> length;
dimension_unit("lvmax") -> length;
dimension_unit("dvw") -> length;
dimension_unit("dvh") -> length;
dimension_unit("dvi") -> length;
dimension_unit("dvb") -> length;
dimension_unit("dvmin") -> length;
dimension_unit("dvmax") -> length;
dimension_unit("cqw") -> length;
dimension_unit("cqh") -> length;
dimension_unit("cqi") -> length;
dimension_unit("cqb") -> length;
dimension_unit("cqmin") -> length;
dimension_unit("cqmax") -> length;
dimension_unit("deg") -> angle;
dimension_unit("rad") -> angle;
dimension_unit("grad") -> angle;
dimension_unit("turn") -> angle;
dimension_unit("ms") -> time;
dimension_unit("s") -> time;
dimension_unit("hz") -> freq;
dimension_unit("khz") -> freq;
dimension_unit("dpi") -> resolution;
dimension_unit("dpcm") -> resolution;
dimension_unit("dppx") -> resolution;
dimension_unit("x") -> resolution;
dimension_unit(_) -> dimension.

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
