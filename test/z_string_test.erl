% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% coding: utf-8

%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_string_test).

-include_lib("eunit/include/eunit.hrl").

to_upper_test() ->
    ?assertEqual(<<"ÜÇGEN"/utf8>>, z_string:to_upper(<<"üçgen"/utf8>>)),
    ?assertEqual(<<"HOLA">>, z_string:to_upper("hola")),
    ?assertEqual(<<"NOTIFICAÇÕES"/utf8>>, z_string:to_upper(<<"notificações"/utf8>>)),
    ?assertEqual(<<"NOTIFICAÇÕES"/utf8>>, z_string:to_upper("notificações")).

to_lower_to_upper_test() ->
    ?assertEqual(<<"üçgen"/utf8>>, z_string:to_lower(z_string:to_upper(<<"üçgen"/utf8>>))).

to_lower_test() ->
    ?assertEqual(<<"notificações"/utf8>>, z_string:to_lower(<<"NOTIFICAÇÕES"/utf8>>)),
    ?assertEqual(<<"notificações"/utf8>>, z_string:to_lower("NOTIFICAÇÕES")).

first_char_test() ->
    ?assertEqual($a, z_string:first_char("aap")),
    ?assertEqual($a, z_string:first_char(<<"aap">>)),
    % ?assertEqual(1046, z_string:first_char("ЖЖЖxx")),
    ?assertEqual(1046, z_string:first_char(<<"ЖЖЖxx"/utf8>>)),
    % ?assertEqual(263, z_string:first_char("ćaap")),
    ?assertEqual(263, z_string:first_char(<<"ćaap"/utf8>>)).

last_char_test() ->
    ?assertEqual($p, z_string:last_char("aap")),
    ?assertEqual($p, z_string:last_char(<<"aap">>)),
    % ?assertEqual(1046, z_string:last_char("xxЖЖЖ")),
    ?assertEqual(1046, z_string:last_char(<<"xxЖЖЖ"/utf8>>)),
    % ?assertEqual(263, z_string:last_char("aapć")),
    ?assertEqual(263, z_string:last_char(<<"aapć"/utf8>>)).

to_name_test() ->
    ?assertEqual(<<"ucgen">>, z_string:to_name(<<"üçgen"/utf8>>)),
    ?assertEqual(<<"hola">>, z_string:to_name(hola)),
    ?assertEqual(<<"hola">>, z_string:to_name("hola")),
    ?assertEqual(<<"at">>, z_string:to_name(<<"@">>)),
    ?assertEqual(<<"at">>, z_string:to_name("@")),
    ?assertEqual(<<"foo_at_bar">>, z_string:to_name("foo@bar")),
    ?assertEqual(<<"_">>, z_string:to_name(<<"廣東話"/utf8>>)).

concat_test() ->
    ?assertEqual(<<"abcdef">>, z_string:concat(<<"abc">>, <<"def">>)),
    ?assertEqual(<<"abcdef">>, z_string:concat(<<"abc">>, "def")),
    ?assertEqual("abcdef", z_string:concat("abc", "def")),
    ?assertEqual("abcdef", z_string:concat("abc", <<"def">>)).

contains_test() ->
    ?assert(z_string:contains("", "strange case")),
    ?assert(z_string:contains("", "")),
    ?assert(z_string:contains("a", "a")),
    ?assert(z_string:contains("is", "This is text.")),
    ?assert(z_string:contains("This", "This is text.")),
    ?assert(z_string:contains("t.", "This is text.")),
    ?assertNot(z_string:contains("not", "This is text.")),
    ?assertNot(z_string:contains("n", "")),
    ?assertNot(z_string:contains("n", "b")).

starts_with_test() ->
    ?assert(z_string:starts_with("", "This is text.")),
    ?assert(z_string:starts_with("", "")),
    ?assert(z_string:starts_with("T", "This is text.")),
    ?assertNot(z_string:starts_with("t", "This is text.")),
    ?assert(z_string:starts_with("This", "This is text.")),
    ?assertNot(z_string:starts_with("Bla", "This is text.")),
    ?assert(z_string:starts_with(["This ", "is"], "This is text.")),
    ?assert(z_string:starts_with(["This ", <<"is">>], "This is text.")),
    ?assertNot(z_string:starts_with(["This ", <<"is not">>], "This is text.")).

ends_with_test() ->
    ?assert(z_string:ends_with("", "This is text.")),
    ?assert(z_string:ends_with("", "")),
    ?assert(z_string:ends_with(".", "This is text.")),
    ?assertNot(z_string:ends_with("T", "This is text.")),
    ?assert(z_string:ends_with("ext.", "This is text.")),
    ?assert(z_string:ends_with(["is ", "text."], "This is text.")),
    ?assert(z_string:ends_with(["is ", <<"text.">>], "This is text.")),
    ?assertNot(z_string:ends_with(["is ", <<"jpeg.">>], "This is text.")).

trim_left_test() ->
    ?assertEqual("foo", z_string:trim_left(" foo")),
    ?assertEqual("foo", z_string:trim_left("     foo")),
    ?assertEqual("foo", z_string:trim_left(".foo", $.)),
    ?assertEqual("foo", z_string:trim_left("......foo", $.)),
    ?assertEqual(<<"foo"/utf8>>, z_string:trim_left(<<"​foo"/utf8>>)). %% zero width space (8203)

trim_right_test() ->
    ?assertEqual(<<"foo">>, z_string:trim_right("foo ")),
    ?assertEqual(<<"foo">>, z_string:trim_right("foo         ")),
    ?assertEqual(<<"foo">>, z_string:trim_right("foo.", $.)),
    ?assertEqual(<<"foo">>, z_string:trim_right("foo........", $.)),
    ?assertEqual(<<"foo"/utf8>>, z_string:trim_right(<<"foo​"/utf8>>)). %% zero width space (8203)

trim_utf8_list_test() ->
    ?assertEqual(<<"üçgen"/utf8>>, z_string:trim(<<" üçgen "/utf8>>)),
    ?assertEqual(<<"üçgen"/utf8>>, z_string:trim([<<" üçgen "/utf8>>])).

truncate_test() ->
    ?assertEqual(<<"foox">>, z_string:truncate(<<"foo bar">>, 4, <<"x">>)),
    ?assertEqual(<<"f&amp;ox">>, z_string:truncate(<<"f&amp;o bar">>, 4, <<"x">>)),
    ?assertEqual(<<"f<br>ox">>, z_string:truncate(<<"f<br>o bar">>, 4, <<"x">>)).

truncatechars_test() ->
    ?assertEqual(<<"foo b">>, z_string:truncatechars(<<"foo bar">>, 5)),
    ?assertEqual(<<"foo bx">>, z_string:truncatechars(<<"foo bar">>, 5, <<"x">>)).

truncatewords_test() ->
    ?assertEqual(<<"foo bar x">>, z_string:truncatewords(<<"foo bar bla">>, 2, <<"x">>)).

normalize_test() ->
    % binary()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"ä"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"ë"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"ï"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"ü"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"ö"/utf8>>)),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"Ä"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"Ë"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"Ï"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"Ü"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"Ö"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"é"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"è"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"É"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"È"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"í"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"ì"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"Í"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"Ì"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"ú"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"ù"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"Ú"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"Ù"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"ó"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"ò"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"Ó"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"Ò"/utf8>>)),
    ?assertEqual(<<"ss"/utf8>>, z_string:normalize(<<"ß"/utf8>>)),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize(<<"ç"/utf8>>)),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize(<<"Ç"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"ø"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"Ø"/utf8>>)),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"å"/utf8>>)),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"Å"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"€"/utf8>>)),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize(<<"ÿ"/utf8>>)),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize(<<"Ÿ"/utf8>>)),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"ã"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"ñ"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"õ"/utf8>>)),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"Ã"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"Ñ"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"Õ"/utf8>>)),
    % string()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("ä")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("ë")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("ï")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("ü")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("ö")),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("Ä")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("Ë")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("Ï")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("Ü")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("Ö")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("é")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("è")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("É")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("È")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("í")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("ì")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("Í")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("Ì")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("ú")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("ù")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("Ú")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("Ù")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("ó")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("ò")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("Ó")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("Ò")),
    ?assertEqual(<<"ss"/utf8>>, z_string:normalize("ß")),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize("ç")),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize("Ç")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("ø")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("Ø")),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("å")),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("Å")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("€")),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize("ÿ")),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("ã")),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize("ñ")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("õ")),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("Ã")),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize("Ñ")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("Õ")),
    % atom()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('ä')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('ë')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('ï')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('ü')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('ö')),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('Ä')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('Ë')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('Ï')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('Ü')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('Ö')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('é')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('è')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('É')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('È')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('í')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('ì')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('Í')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('Ì')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('ú')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('ù')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('Ú')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('Ù')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('ó')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('ò')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('Ó')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('Ò')),
    ?assertEqual(<<"ss"/utf8>>, z_string:normalize('ß')),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize('ç')),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize('Ç')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('ø')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('Ø')),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('å')),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('Å')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('€')),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize('ÿ')),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('ã')),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize('ñ')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('õ')),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('Ã')),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize('Ñ')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('Õ')),
    % Cyrillic support
    % binary()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"А"/utf8>>)),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"а"/utf8>>)),
    ?assertEqual(<<"b"/utf8>>, z_string:normalize(<<"Б"/utf8>>)),
    ?assertEqual(<<"b"/utf8>>, z_string:normalize(<<"б"/utf8>>)),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize(<<"В"/utf8>>)),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize(<<"в"/utf8>>)),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"Г"/utf8>>)),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"г"/utf8>>)),
    ?assertEqual(<<"d"/utf8>>, z_string:normalize(<<"Д"/utf8>>)),
    ?assertEqual(<<"d"/utf8>>, z_string:normalize(<<"д"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"Е"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"е"/utf8>>)),
    ?assertEqual(<<"oy"/utf8>>, z_string:normalize(<<"Ё"/utf8>>)),
    ?assertEqual(<<"oy"/utf8>>, z_string:normalize(<<"ё"/utf8>>)),
    ?assertEqual(<<"hz"/utf8>>, z_string:normalize(<<"Ж"/utf8>>)),
    ?assertEqual(<<"hz"/utf8>>, z_string:normalize(<<"ж"/utf8>>)),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"З"/utf8>>)),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"з"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"И"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"и"/utf8>>)),
    ?assertEqual(<<"j"/utf8>>, z_string:normalize(<<"Й"/utf8>>)),
    ?assertEqual(<<"j"/utf8>>, z_string:normalize(<<"й"/utf8>>)),
    ?assertEqual(<<"k"/utf8>>, z_string:normalize(<<"К"/utf8>>)),
    ?assertEqual(<<"k"/utf8>>, z_string:normalize(<<"к"/utf8>>)),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize(<<"Л"/utf8>>)),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize(<<"л"/utf8>>)),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize(<<"М"/utf8>>)),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize(<<"м"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"Н"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"н"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"О"/utf8>>)),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"о"/utf8>>)),
    ?assertEqual(<<"p"/utf8>>, z_string:normalize(<<"П"/utf8>>)),
    ?assertEqual(<<"p"/utf8>>, z_string:normalize(<<"п"/utf8>>)),
    ?assertEqual(<<"r"/utf8>>, z_string:normalize(<<"Р"/utf8>>)),
    ?assertEqual(<<"r"/utf8>>, z_string:normalize(<<"р"/utf8>>)),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"С"/utf8>>)),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"с"/utf8>>)),
    ?assertEqual(<<"t"/utf8>>, z_string:normalize(<<"Т"/utf8>>)),
    ?assertEqual(<<"t"/utf8>>, z_string:normalize(<<"т"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"У"/utf8>>)),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"у"/utf8>>)),
    ?assertEqual(<<"f"/utf8>>, z_string:normalize(<<"Ф"/utf8>>)),
    ?assertEqual(<<"f"/utf8>>, z_string:normalize(<<"ф"/utf8>>)),
    ?assertEqual(<<"h"/utf8>>, z_string:normalize(<<"Х"/utf8>>)),
    ?assertEqual(<<"h"/utf8>>, z_string:normalize(<<"х"/utf8>>)),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize(<<"Ц"/utf8>>)),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize(<<"ц"/utf8>>)),
    ?assertEqual(<<"hc"/utf8>>, z_string:normalize(<<"Ч"/utf8>>)),
    ?assertEqual(<<"hc"/utf8>>, z_string:normalize(<<"ч"/utf8>>)),
    ?assertEqual(<<"hs"/utf8>>, z_string:normalize(<<"Ш"/utf8>>)),
    ?assertEqual(<<"hs"/utf8>>, z_string:normalize(<<"ш"/utf8>>)),
    ?assertEqual(<<"hhs"/utf8>>, z_string:normalize(<<"Щ"/utf8>>)),
    ?assertEqual(<<"hhs"/utf8>>, z_string:normalize(<<"щ"/utf8>>)),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize(<<"Ъ"/utf8>>)),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize(<<"ъ"/utf8>>)),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize(<<"Ы"/utf8>>)),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize(<<"ы"/utf8>>)),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize(<<"Ь"/utf8>>)),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize(<<"ь"/utf8>>)),
    ?assertEqual(<<"he"/utf8>>, z_string:normalize(<<"Э"/utf8>>)),
    ?assertEqual(<<"he"/utf8>>, z_string:normalize(<<"э"/utf8>>)),
    ?assertEqual(<<"uy"/utf8>>, z_string:normalize(<<"Ю"/utf8>>)),
    ?assertEqual(<<"uy"/utf8>>, z_string:normalize(<<"ю"/utf8>>)),
    ?assertEqual(<<"ay"/utf8>>, z_string:normalize(<<"Я"/utf8>>)),
    ?assertEqual(<<"ay"/utf8>>, z_string:normalize(<<"я"/utf8>>)),
    % Cyrillic support
    % string()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("А")),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("а")),
    ?assertEqual(<<"b"/utf8>>, z_string:normalize("Б")),
    ?assertEqual(<<"b"/utf8>>, z_string:normalize("б")),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize("В")),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize("в")),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize("Г")),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize("г")),
    ?assertEqual(<<"d"/utf8>>, z_string:normalize("Д")),
    ?assertEqual(<<"d"/utf8>>, z_string:normalize("д")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("Е")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("е")),
    ?assertEqual(<<"oy"/utf8>>, z_string:normalize("Ё")),
    ?assertEqual(<<"oy"/utf8>>, z_string:normalize("ё")),
    ?assertEqual(<<"hz"/utf8>>, z_string:normalize("Ж")),
    ?assertEqual(<<"hz"/utf8>>, z_string:normalize("ж")),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize("З")),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize("з")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("И")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("и")),
    ?assertEqual(<<"j"/utf8>>, z_string:normalize("Й")),
    ?assertEqual(<<"j"/utf8>>, z_string:normalize("й")),
    ?assertEqual(<<"k"/utf8>>, z_string:normalize("К")),
    ?assertEqual(<<"k"/utf8>>, z_string:normalize("к")),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize("Л")),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize("л")),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize("М")),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize("м")),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize("Н")),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize("н")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("О")),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize("о")),
    ?assertEqual(<<"p"/utf8>>, z_string:normalize("П")),
    ?assertEqual(<<"p"/utf8>>, z_string:normalize("п")),
    ?assertEqual(<<"r"/utf8>>, z_string:normalize("Р")),
    ?assertEqual(<<"r"/utf8>>, z_string:normalize("р")),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize("С")),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize("с")),
    ?assertEqual(<<"t"/utf8>>, z_string:normalize("Т")),
    ?assertEqual(<<"t"/utf8>>, z_string:normalize("т")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("У")),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize("у")),
    ?assertEqual(<<"f"/utf8>>, z_string:normalize("Ф")),
    ?assertEqual(<<"f"/utf8>>, z_string:normalize("ф")),
    ?assertEqual(<<"h"/utf8>>, z_string:normalize("Х")),
    ?assertEqual(<<"h"/utf8>>, z_string:normalize("х")),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize("Ц")),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize("ц")),
    ?assertEqual(<<"hc"/utf8>>, z_string:normalize("Ч")),
    ?assertEqual(<<"hc"/utf8>>, z_string:normalize("ч")),
    ?assertEqual(<<"hs"/utf8>>, z_string:normalize("Ш")),
    ?assertEqual(<<"hs"/utf8>>, z_string:normalize("ш")),
    ?assertEqual(<<"hhs"/utf8>>, z_string:normalize("Щ")),
    ?assertEqual(<<"hhs"/utf8>>, z_string:normalize("щ")),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize("Ъ")),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize("ъ")),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize("Ы")),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize("ы")),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize("Ь")),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize("ь")),
    ?assertEqual(<<"he"/utf8>>, z_string:normalize("Э")),
    ?assertEqual(<<"he"/utf8>>, z_string:normalize("э")),
    ?assertEqual(<<"uy"/utf8>>, z_string:normalize("Ю")),
    ?assertEqual(<<"uy"/utf8>>, z_string:normalize("ю")),
    ?assertEqual(<<"ay"/utf8>>, z_string:normalize("Я")),
    ?assertEqual(<<"ay"/utf8>>, z_string:normalize("я")),
    % Cyrillic support
    % atom()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('А')),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('а')),
    ?assertEqual(<<"b"/utf8>>, z_string:normalize('Б')),
    ?assertEqual(<<"b"/utf8>>, z_string:normalize('б')),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize('В')),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize('в')),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize('Г')),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize('г')),
    ?assertEqual(<<"d"/utf8>>, z_string:normalize('Д')),
    ?assertEqual(<<"d"/utf8>>, z_string:normalize('д')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('Е')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('е')),
    ?assertEqual(<<"oy"/utf8>>, z_string:normalize('Ё')),
    ?assertEqual(<<"oy"/utf8>>, z_string:normalize('ё')),
    ?assertEqual(<<"hz"/utf8>>, z_string:normalize('Ж')),
    ?assertEqual(<<"hz"/utf8>>, z_string:normalize('ж')),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize('З')),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize('з')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('И')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('и')),
    ?assertEqual(<<"j"/utf8>>, z_string:normalize('Й')),
    ?assertEqual(<<"j"/utf8>>, z_string:normalize('й')),
    ?assertEqual(<<"k"/utf8>>, z_string:normalize('К')),
    ?assertEqual(<<"k"/utf8>>, z_string:normalize('к')),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize('Л')),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize('л')),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize('М')),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize('м')),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize('Н')),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize('н')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('О')),
    ?assertEqual(<<"o"/utf8>>, z_string:normalize('о')),
    ?assertEqual(<<"p"/utf8>>, z_string:normalize('П')),
    ?assertEqual(<<"p"/utf8>>, z_string:normalize('п')),
    ?assertEqual(<<"r"/utf8>>, z_string:normalize('Р')),
    ?assertEqual(<<"r"/utf8>>, z_string:normalize('р')),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize('С')),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize('с')),
    ?assertEqual(<<"t"/utf8>>, z_string:normalize('Т')),
    ?assertEqual(<<"t"/utf8>>, z_string:normalize('т')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('У')),
    ?assertEqual(<<"u"/utf8>>, z_string:normalize('у')),
    ?assertEqual(<<"f"/utf8>>, z_string:normalize('Ф')),
    ?assertEqual(<<"f"/utf8>>, z_string:normalize('ф')),
    ?assertEqual(<<"h"/utf8>>, z_string:normalize('Х')),
    ?assertEqual(<<"h"/utf8>>, z_string:normalize('х')),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize('Ц')),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize('ц')),
    ?assertEqual(<<"hc"/utf8>>, z_string:normalize('Ч')),
    ?assertEqual(<<"hc"/utf8>>, z_string:normalize('ч')),
    ?assertEqual(<<"hs"/utf8>>, z_string:normalize('Ш')),
    ?assertEqual(<<"hs"/utf8>>, z_string:normalize('ш')),
    ?assertEqual(<<"hhs"/utf8>>, z_string:normalize('Щ')),
    ?assertEqual(<<"hhs"/utf8>>, z_string:normalize('щ')),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize('Ъ')),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize('ъ')),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize('Ы')),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize('ы')),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize('Ь')),
    ?assertEqual(<<"_"/utf8>>, z_string:normalize('ь')),
    ?assertEqual(<<"he"/utf8>>, z_string:normalize('Э')),
    ?assertEqual(<<"he"/utf8>>, z_string:normalize('э')),
    ?assertEqual(<<"uy"/utf8>>, z_string:normalize('Ю')),
    ?assertEqual(<<"uy"/utf8>>, z_string:normalize('ю')),
    ?assertEqual(<<"ay"/utf8>>, z_string:normalize('Я')),
    ?assertEqual(<<"ay"/utf8>>, z_string:normalize('я')),
    % Ukrainian support
    % binary()
    ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"Ґ"/utf8>>)),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"ґ"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"Ї"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"ї"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"І"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"і"/utf8>>)),
    ?assertEqual(<<"ey"/utf8>>, z_string:normalize(<<"Є"/utf8>>)),
    ?assertEqual(<<"ey"/utf8>>, z_string:normalize(<<"є"/utf8>>)),
    % Ukrainian support
    % string()
    ?assertEqual(<<"g"/utf8>>, z_string:normalize("Ґ")),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize("ґ")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("Ї")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("ї")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("І")),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize("і")),
    ?assertEqual(<<"ey"/utf8>>, z_string:normalize("Є")),
    ?assertEqual(<<"ey"/utf8>>, z_string:normalize("є")),
    % Ukrainian support
    % atom()
    ?assertEqual(<<"g"/utf8>>, z_string:normalize('Ґ')),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize('ґ')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('Ї')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('ї')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('І')),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize('і')),
    ?assertEqual(<<"ey"/utf8>>, z_string:normalize('Є')),
    ?assertEqual(<<"ey"/utf8>>, z_string:normalize('є')),
    % Polish support
    % binary()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"Ą"/utf8>>)),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"ą"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"Ę"/utf8>>)),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"ę"/utf8>>)),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize(<<"Ć"/utf8>>)),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize(<<"ć"/utf8>>)),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize(<<"Ł"/utf8>>)),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize(<<"ł"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"Ń"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"ń"/utf8>>)),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"Ś"/utf8>>)),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"ś"/utf8>>)),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"Ź"/utf8>>)),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"ź"/utf8>>)),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"Ż"/utf8>>)),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"ż"/utf8>>)),
    % Polish support
    % string()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("Ą")),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize("ą")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("Ę")),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize("ę")),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize("Ć")),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize("ć")),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize("Ł")),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize("ł")),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize("Ń")),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize("ń")),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize("Ś")),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize("ś")),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize("Ź")),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize("ź")),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize("Ż")),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize("ż")),
    % Polish support
    % atom()
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('Ą')),
    ?assertEqual(<<"a"/utf8>>, z_string:normalize('ą')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('Ę')),
    ?assertEqual(<<"e"/utf8>>, z_string:normalize('ę')),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize('Ć')),
    ?assertEqual(<<"c"/utf8>>, z_string:normalize('ć')),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize('Ł')),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize('ł')),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize('Ń')),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize('ń')),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize('Ś')),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize('ś')),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize('Ź')),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize('ź')),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize('Ż')),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize('ż')),
    % Turkish support
    ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"Ş"/utf8>>)),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"ş"/utf8>>)),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"Ğ"/utf8>>)),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"ğ"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"İ"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"ı"/utf8>>)),
    % Hebrew support (simplified) https://en.wikipedia.org/wiki/Romanization_of_Hebrew
    % FIXME: commented checks are falling
    % TODO: string() and atom() tests
    ?assertEqual(<<>>, z_string:normalize(<<"א"/utf8>>)),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize(<<"ב"/utf8>>)),
    % ?assertEqual(<<"b"/utf8>>, z_string:normalize(<<"בּ"/utf8>>)),
    ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"ג"/utf8>>)),
    % ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"גּ"/utf8>>)),
    % ?assertEqual(<<"j"/utf8>>, z_string:normalize(<<"ג׳"/utf8>>)),
    ?assertEqual(<<"d"/utf8>>, z_string:normalize(<<"ד"/utf8>>)),
    % ?assertEqual(<<"d"/utf8>>, z_string:normalize(<<"דּ"/utf8>>)),
    % ?assertEqual(<<"dh"/utf8>>, z_string:normalize(<<"ד׳"/utf8>>)),
    ?assertEqual(<<"h"/utf8>>, z_string:normalize(<<"ה"/utf8>>)),
    % ?assertEqual(<<"h"/utf8>>, z_string:normalize(<<"הּ"/utf8>>)),
    ?assertEqual(<<"v"/utf8>>, z_string:normalize(<<"ו"/utf8>>)),
    % ?assertEqual(<<"v"/utf8>>, z_string:normalize(<<"וּ"/utf8>>)),
    ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"ז"/utf8>>)),
    % ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"זּ"/utf8>>)),
    % ?assertEqual(<<"zh"/utf8>>, z_string:normalize(<<"ז׳"/utf8>>)),
    ?assertEqual(<<"ch"/utf8>>, z_string:normalize(<<"ח"/utf8>>)),
    ?assertEqual(<<"t"/utf8>>, z_string:normalize(<<"ט"/utf8>>)),
    % ?assertEqual(<<"t"/utf8>>, z_string:normalize(<<"טּ"/utf8>>)),
    ?assertEqual(<<"y"/utf8>>, z_string:normalize(<<"י"/utf8>>)),
    % ?assertEqual(<<"y"/utf8>>, z_string:normalize(<<"יּ"/utf8>>)),
    ?assertEqual(<<"ch"/utf8>>, z_string:normalize(<<"ךכ"/utf8>>)),
    ?assertEqual(<<"k"/utf8>>, z_string:normalize(<<"ךּ כּ"/utf8>>)),
    ?assertEqual(<<"l"/utf8>>, z_string:normalize(<<"ל"/utf8>>)),
    % ?assertEqual(<<"l"/utf8>>, z_string:normalize(<<"לּ"/utf8>>)),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize(<<"םמ"/utf8>>)),
    ?assertEqual(<<"m"/utf8>>, z_string:normalize(<<"מּ"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"ןנ"/utf8>>)),
    ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"נּ"/utf8>>)),
    ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"ס"/utf8>>)),
    % ?assertEqual(<<"s"/utf8>>, z_string:normalize(<<"סּ"/utf8>>)),
    ?assertEqual(<<>>, z_string:normalize(<<"ע"/utf8>>)),
    ?assertEqual( <<"f"/utf8>>, z_string:normalize(<<"ףפ"/utf8>>)),
    ?assertEqual( <<"p"/utf8>>, z_string:normalize(<<"ףּ פּ"/utf8>>)),
    ?assertEqual( <<"tz"/utf8>>, z_string:normalize(<<"ץצ"/utf8>>)),
    ?assertEqual( <<"tz"/utf8>>, z_string:normalize(<<"צּ"/utf8>>)),
    ?assertEqual( <<"tsh"/utf8>>, z_string:normalize(<<"ץ׳צ׳"/utf8>>)),
    ?assertEqual( <<"k"/utf8>>, z_string:normalize(<<"ק"/utf8>>)),
    % ?assertEqual( <<"k"/utf8>>, z_string:normalize(<<"קּ"/utf8>>)),
    ?assertEqual( <<"r"/utf8>>, z_string:normalize(<<"ר"/utf8>>)),
    % ?assertEqual( <<"r"/utf8>>, z_string:normalize(<<"רּ"/utf8>>)),
    ?assertEqual( <<"sh"/utf8>>, z_string:normalize(<<"ש"/utf8>>)),
    % ?assertEqual( <<"sh"/utf8>>, z_string:normalize(<<"שׁ"/utf8>>)),
    % ?assertEqual( <<"sh"/utf8>>, z_string:normalize(<<"שּׁ"/utf8>>)),
    % ?assertEqual( <<"s"/utf8>>, z_string:normalize(<<"שׂ"/utf8>>)),
    % ?assertEqual( <<"s"/utf8>>, z_string:normalize(<<"שּׂ"/utf8>>)),
    ?assertEqual( <<"t"/utf8>>, z_string:normalize(<<"ת"/utf8>>)),
    % ?assertEqual( <<"t"/utf8>>, z_string:normalize(<<"תּ"/utf8>>)),
    % ?assertEqual( <<"th"/utf8>>, z_string:normalize(<<"ת׳"/utf8>>))
    % Hebrew forms used in translitearion from Arabic
    % FIXME: commented checks are falling
    % TODO: string() and atom() tests
    % ?assertEqual(<<"n"/utf8>>, z_string:normalize(<<"ח׳"/utf8>>)),
    % ?assertEqual(<<"z"/utf8>>, z_string:normalize(<<"ט׳"/utf8>>)),
    % ?assertEqual(<<"g"/utf8>>, z_string:normalize(<<"ע׳ר׳"/utf8>>))
    % Hebrew vowels
    % FIXME: commented checks are falling
    % TODO: string() and atom() tests
    ?assertEqual(<<"aa"/utf8>>, z_string:normalize(<<"צ"/utf8>>)),
    % ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"טְ"/utf8>>)),
    % ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"חֱ"/utf8>>)),
    % ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"חֲ"/utf8>>)),
    % ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"חֳ"/utf8>>)),
    % ?assertEqual(<<"i"/utf8>>, z_string:normalize(<<"טִ"/utf8>>)),
    % ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"טֵ"/utf8>>)),
    % ?assertEqual(<<"e"/utf8>>, z_string:normalize(<<"טֶ"/utf8>>)),
    % ?assertEqual(<<"a"/utf8>>, z_string:normalize(<<"טַ"/utf8>>)),
    % ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"טָ"/utf8>>)),
    % ?assertEqual(<<"o"/utf8>>, z_string:normalize(<<"טֹ"/utf8>>)),
    % ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"טֻ"/utf8>>)),
    % ?assertEqual(<<"u"/utf8>>, z_string:normalize(<<"טוּ"/utf8>>)),
    % ?assertEqual(<<"ei"/utf8>>, z_string:normalize(<<"טֵי"/utf8>>)),
    % ?assertEqual(<<"ei"/utf8>>, z_string:normalize(<<"טֶי"/utf8>>)),
    % ?assertEqual(<<"ai"/utf8>>, z_string:normalize(<<"טַיטַיְ"/utf8>>)),
    % ?assertEqual(<<"ai"/utf8>>, z_string:normalize(<<"טָיטָיְ"/utf8>>)),
    % ?assertEqual(<<"oi"/utf8>>, z_string:normalize(<<"טֹיטֹיְ"/utf8>>)),
    % ?assertEqual(<<"ui"/utf8>>, z_string:normalize(<<"טֻיטֻיְ"/utf8>>)),
    % ?assertEqual(<<"ui"/utf8>>, z_string:normalize(<<"טוּיטוּיְ"/utf8>>))
    % Some entities
    % binary()
    ?assertEqual(<<" "/utf8>>, z_string:normalize(<<"&amp;"/utf8>>)),
    ?assertEqual(<<" "/utf8>>, z_string:normalize(<<"&lt;"/utf8>>)),
    ?assertEqual(<<" "/utf8>>, z_string:normalize(<<"&gt;"/utf8>>)),
    ?assertEqual(<<" "/utf8>>, z_string:normalize(<<"&#39;"/utf8>>)),
    ?assertEqual(<<" "/utf8>>, z_string:normalize(<<"&quot;"/utf8>>)),
    ?assertEqual(<<" "/utf8>>, z_string:normalize(<<"&nbsp;"/utf8>>)),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize(<<"&mdash;"/utf8>>)),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize(<<"&ndash;"/utf8>>)),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize(<<"—"/utf8>>)),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize(<<"–"/utf8>>)),
    ?assertEqual(<<" a"/utf8>>, z_string:normalize(<<" a"/utf8>>)),
    % Some entities
    % string()
    ?assertEqual(<<" "/utf8>>, z_string:normalize("&amp;")),
    ?assertEqual(<<" "/utf8>>, z_string:normalize("&lt;")),
    ?assertEqual(<<" "/utf8>>, z_string:normalize("&gt;")),
    ?assertEqual(<<" "/utf8>>, z_string:normalize("&#39;")),
    ?assertEqual(<<" "/utf8>>, z_string:normalize("&quot;")),
    ?assertEqual(<<" "/utf8>>, z_string:normalize("&nbsp;")),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize("&mdash;")),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize("&ndash;")),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize("—")),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize("–")),
    ?assertEqual(<<" a"/utf8>>, z_string:normalize(" a")),
    % Some entities
    % atom()
    ?assertEqual(<<" "/utf8>>, z_string:normalize('&amp;')),
    ?assertEqual(<<" "/utf8>>, z_string:normalize('&lt;')),
    ?assertEqual(<<" "/utf8>>, z_string:normalize('&gt;')),
    ?assertEqual(<<" "/utf8>>, z_string:normalize('&#39;')),
    ?assertEqual(<<" "/utf8>>, z_string:normalize('&quot;')),
    ?assertEqual(<<" "/utf8>>, z_string:normalize('&nbsp;')),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize('&mdash;')),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize('&ndash;')),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize('—')),
    ?assertEqual(<<"-"/utf8>>, z_string:normalize('–')),
    ?assertEqual(<<" a"/utf8>>, z_string:normalize(' a')),
    % Some words
    ?assertEqual(<<"notificacoes"/utf8>>, z_string:normalize(<<"notificações"/utf8>>)),
    ?assertEqual(<<"notificacoes"/utf8>>, z_string:normalize(<<"NOTIFICAÇÕES"/utf8>>)),
    ?assertEqual(<<"notificacoes"/utf8>>, z_string:normalize("notificações")),
    ?assertEqual(<<"notificacoes"/utf8>>, z_string:normalize("NOTIFICAÇÕES")),
    ?assertEqual(<<"notificacoes"/utf8>>, z_string:normalize('notificações')),
    ?assertEqual(<<"notificacoes"/utf8>>, z_string:normalize('NOTIFICAÇÕES')).
