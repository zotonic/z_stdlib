%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>

-module(z_jsmin_test).

-include_lib("eunit/include/eunit.hrl").


remove_comment_and_whitespace_test() ->
    ?assertEqual(<<>>, minify(<<>>)),
    ?assertEqual(<<>>, minify(<<"    ">>)),
    ?assertEqual(<<>>, minify(<<"\n   ">>)),
    ?assertEqual(<<>>, minify(<<"\r\n">>)),
    ?assertEqual(<<>>, minify(<<"\r\n\v\f\b">>)),
    ?assertEqual(<<>>, minify(<<"// empty">>)),
    ?assertEqual(<<>>, minify(<<"// one\n// two\n">>)),
    ?assertEqual(<<>>, minify(<<"/* Just comment */">>)),
    ?assertEqual(<<>>, minify(<<"/* Multi line\n * one \n * two \n*/">>)),
    ?assertEqual(<<>>, minify(<<"/* Multi line\n * one \n * two \n*/\n", 
                                "// line comment">>)),
    ok.

simple_expressions_test() ->
    ?assertEqual(<<"s">>, minify(<<"s">>)),
    ?assertEqual(<<"1+2">>, minify(<<"1 + 2">>)),
    ?assertEqual(<<"1+2;">>, minify(<<"1 /* one */ + 2; // two">>)),
    ok.

space_in_regexp_test() ->
    ?assertEqual(<<"/a (a)/.test(\"a\")">>, <<"/a (a)/.test(\"a\")">>).

newline_between_strings_test() ->
    ?assertEqual(<<"\"yolo\"\n\"yolo\"">>, <<"\"yolo\"\n\"yolo\"">>).

comments_between_tokens_test() ->
    ?assertEqual(<<"var a">>, minify(<<"var /* comment */ a">>)).

ends_with_string_test() ->
    ?assertEqual(<<"var s=\"s\"">>, minify(<<"var s = \"s\"">>)).

short_comment_test() ->
    ?assertEqual(<<"a;b">>, minify(<<"a;/**/b">>)).

shorter_comment_test() ->
    ?assertEqual(<<"a;b">>, minify(<<"a;/*/*/b">>)).

block_comment_with_semicolon_test() ->
    ?assertEqual(<<"a;b">>, minify(<<"a;/**/\nb">>)).
    
block_comment_with_implicit_semicolon_test() ->
    ?assertEqual(<<"a\nvar b">>, minify(<<"a/**/\nvar b">>)).

single_comments_test() ->
    Lines = ["",
             "var a = \"hello\" // this is a comment",
             "    a += \" world\"",
             ""],
    
    ?assertEqual(<<"var a=\"hello\"\na+=\" world\"">>, 
                 minify(list_to_binary(string:join(Lines, "\n")))).



%%
%% Helpers
%%

minify(A) ->
    z_jsmin:minify(A).

    

