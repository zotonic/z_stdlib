%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_string_test).

-include_lib("eunit/include/eunit.hrl").


to_lower_to_upper_test() ->
    A = "üçgen",
    A = z_string:to_lower(z_string:to_upper(A)),
    ok.

first_char_test() ->
    ?assertEqual($a, z_string:first_char("aap")),
    ?assertEqual($a, z_string:first_char(<<"aap">>)),
    ?assertEqual(1046, z_string:first_char("ЖЖЖxx")),
    ?assertEqual(1046, z_string:first_char(<<"ЖЖЖxx">>)),
    ?assertEqual(263, z_string:first_char("ćaap")),
    ?assertEqual(263, z_string:first_char(<<"ćaap">>)),
    ok.

last_char_test() ->
    ?assertEqual($p, z_string:last_char("aap")),
    ?assertEqual($p, z_string:last_char(<<"aap">>)),
    ?assertEqual(1046, z_string:last_char("xxЖЖЖ")),
    ?assertEqual(1046, z_string:last_char(<<"xxЖЖЖ">>)),
    ?assertEqual(263, z_string:last_char("aapć")),
    ?assertEqual(263, z_string:last_char(<<"aapć">>)),
    ok.


to_name_test() ->
    A = "üçgen",
    "ucgen" = z_string:to_name(A),
    ok.

concat_test() ->
    <<"abcdef">> = z_string:concat(<<"abc">>, <<"def">>),
    <<"abcdef">> = z_string:concat(<<"abc">>, "def"),
    "abcdef" = z_string:concat("abc", "def"),
    "abcdef" = z_string:concat("abc", <<"def">>),
    "abcdef" = z_string:concat("abc", def),
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
    ok.
    

trim_right_test() ->
    ?assertEqual("foo", z_string:trim_right("foo ")),
    ?assertEqual("foo", z_string:trim_right("foo         ")),
    ?assertEqual("foo", z_string:trim_right("foo.", $.)),
    ?assertEqual("foo", z_string:trim_right("foo........", $.)),
    ok.
    
