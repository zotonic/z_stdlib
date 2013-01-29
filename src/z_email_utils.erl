%% @author Marc Worrell
%% @copyright 2013 Marc Worrell
%% @doc Useful routines for e-mail address handling.

%% Copyright 2013 Marc Worrell
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

-module(z_email_utils).

-author("Marc Worrell <marc@worrell.nl>").

-export([
	is_email/1,
	extract_emails/1
]).


%% @doc Check if the argument is an e-mail address (without any extras like name, comments, etc.)
-spec is_email(iodata()) -> boolean().
is_email(Email) ->
    case re:run(Email, [$^|re()]++"$", [extended]) of
        nomatch   -> false;
        {match,_} -> true
    end.


%% @doc Extract e-mail addresses from a text. All e-mail addresses are lower cased, doubles are removed.
-spec extract_emails(iodata()) -> [ binary() ].
extract_emails(Text) ->
	Text1 = iolist_to_binary([32, Text, 32]), 
	Re = "[^a-zA-Z0-9\\.@\\-]("++re()++")[^a-zA-Z0-9\\.@\\-]",
    case re:run(Text1, Re, [extended, global, {capture, all, binary}]) of
        nomatch -> 
        	[];
        {match, Match} -> 
        	lists:usort([ z_convert:to_binary(z_string:to_lower(iolist_to_binary(hd(tl(M))))) || M <- Match ])
    end.


%% @doc Regular expression to match e-mail addresses.
re() ->
    "(
            (\"[^\"\\f\\n\\r\\t\\v\\b]+\")
        |   ([\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+
                (\\.[\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+)*
            )
    )
    @
    (
        (
            ([A-Za-z0-9\\-])+\\.
        )+
        [A-Za-z\\-]{2,}
    )".
