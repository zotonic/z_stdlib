%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

-module(z_string_test).

-include_lib("eunit/include/eunit.hrl").


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
    
