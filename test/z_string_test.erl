% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% coding: utf-8

%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_string_test).

-include_lib("eunit/include/eunit.hrl").

to_upper_test() ->
    A = <<"üçgen"/utf8>>,
    <<"ÜÇGEN"/utf8>> = z_string:to_upper(A),
    <<"HOLA">> = z_string:to_upper("hola"),
    ok.

to_lower_to_upper_test() ->
    A = <<"üçgen"/utf8>>,
    A = z_string:to_lower(z_string:to_upper(A)),
    ok.

first_char_test() ->
    ?assertEqual($a, z_string:first_char("aap")),
    ?assertEqual($a, z_string:first_char(<<"aap">>)),
    % ?assertEqual(1046, z_string:first_char("ЖЖЖxx")),
    ?assertEqual(1046, z_string:first_char(<<"ЖЖЖxx"/utf8>>)),
    % ?assertEqual(263, z_string:first_char("ćaap")),
    ?assertEqual(263, z_string:first_char(<<"ćaap"/utf8>>)),
    ok.

last_char_test() ->
    ?assertEqual($p, z_string:last_char("aap")),
    ?assertEqual($p, z_string:last_char(<<"aap">>)),
    % ?assertEqual(1046, z_string:last_char("xxЖЖЖ")),
    ?assertEqual(1046, z_string:last_char(<<"xxЖЖЖ"/utf8>>)),
    % ?assertEqual(263, z_string:last_char("aapć")),
    ?assertEqual(263, z_string:last_char(<<"aapć"/utf8>>)),
    ok.

to_name_test() ->
    A = <<"üçgen"/utf8>>,
    <<"ucgen">> = z_string:to_name(A),
    <<"hola">> = z_string:to_name(hola),
    <<"hola">> = z_string:to_name("hola"),
    <<"at">> = z_string:to_name(<<"@">>),
    <<"at">> = z_string:to_name("@"),
    <<"foo_at_bar">> = z_string:to_name("foo@bar"),
    <<"_">> = z_string:to_name(<<"廣東話"/utf8>>),
    ok.

concat_test() ->
    <<"abcdef">> = z_string:concat(<<"abc">>, <<"def">>),
    <<"abcdef">> = z_string:concat(<<"abc">>, "def"),
    "abcdef" = z_string:concat("abc", "def"),
    "abcdef" = z_string:concat("abc", <<"def">>),
    ok.

contains_test() ->
    ?assert(z_string:contains("", "strange case")),
    ?assert(z_string:contains("", "")),
    ?assert(z_string:contains("a", "a")),
    ?assert(z_string:contains("is", "This is text.")),
    ?assert(z_string:contains("This", "This is text.")),
    ?assert(z_string:contains("t.", "This is text.")),
    ?assertNot(z_string:contains("not", "This is text.")),
    ?assertNot(z_string:contains("n", "")),
    ?assertNot(z_string:contains("n", "b")),
    ok.

starts_with_test() ->
    ?assert(z_string:starts_with("", "This is text.")),
    ?assert(z_string:starts_with("", "")),
    ?assert(z_string:starts_with("T", "This is text.")),
    ?assertNot(z_string:starts_with("t", "This is text.")),
    ?assert(z_string:starts_with("This", "This is text.")),
    ?assertNot(z_string:starts_with("Bla", "This is text.")),
    ?assert(z_string:starts_with(["This ", "is"], "This is text.")),
    ?assert(z_string:starts_with(["This ", <<"is">>], "This is text.")),
    ?assertNot(z_string:starts_with(["This ", <<"is not">>], "This is text.")),
    ok.

ends_with_test() ->
    ?assert(z_string:ends_with("", "This is text.")),
    ?assert(z_string:ends_with("", "")),
    ?assert(z_string:ends_with(".", "This is text.")),
    ?assertNot(z_string:ends_with("T", "This is text.")),
    ?assert(z_string:ends_with("ext.", "This is text.")),
    ?assert(z_string:ends_with(["is ", "text."], "This is text.")),
    ?assert(z_string:ends_with(["is ", <<"text.">>], "This is text.")),
    ?assertNot(z_string:ends_with(["is ", <<"jpeg.">>], "This is text.")),
    ok.

trim_left_test() ->
    ?assertEqual("foo", z_string:trim_left(" foo")),
    ?assertEqual("foo", z_string:trim_left("     foo")),
    ?assertEqual("foo", z_string:trim_left(".foo", $.)),
    ?assertEqual("foo", z_string:trim_left("......foo", $.)),
    ?assertEqual(<<"foo"/utf8>>, z_string:trim_left(<<"​foo"/utf8>>)), %% zero width space (8203)
    ok.

trim_right_test() ->
    ?assertEqual(<<"foo">>, z_string:trim_right("foo ")),
    ?assertEqual(<<"foo">>, z_string:trim_right("foo         ")),
    ?assertEqual(<<"foo">>, z_string:trim_right("foo.", $.)),
    ?assertEqual(<<"foo">>, z_string:trim_right("foo........", $.)),
    ?assertEqual(<<"foo"/utf8>>, z_string:trim_right(<<"foo​"/utf8>>)), %% zero width space (8203)
    ok.

trim_utf8_list_test() ->
    A = <<" üçgen "/utf8>>,
    <<"üçgen"/utf8>> = z_string:trim(A),
    <<"üçgen"/utf8>> = z_string:trim([A]),
    ok.

truncate_test() ->
    ?assertEqual(<<"foox">>, z_string:truncate(<<"foo bar">>, 4, <<"x">>)),
    ?assertEqual(<<"f&amp;ox">>, z_string:truncate(<<"f&amp;o bar">>, 4, <<"x">>)),
    ?assertEqual(<<"f<br>ox">>, z_string:truncate(<<"f<br>o bar">>, 4, <<"x">>)).

truncatechars_test() ->
    ?assertEqual(<<"foo b">>, z_string:truncatechars(<<"foo bar">>, 5)),
    ?assertEqual(<<"foo bx">>, z_string:truncatechars(<<"foo bar">>, 5, <<"x">>)).

truncatewords_test() ->
    ?assertEqual(<<"foo bar x">>, z_string:truncatewords(<<"foo bar bla">>, 2, <<"x">>)).
