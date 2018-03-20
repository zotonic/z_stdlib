%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>

-module(z_cssmin_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assertEqual(<<>>, minify(<<>>)),
    ?assertEqual(<<>>, minify(<<"    ">>)),
    ?assertEqual(<<>>, minify(<<"/* comment */">>)),
    ok.

class_selector_test() ->
    ?assertEqual(<<"p :link">>, minify(<<"p :link">>)),

    ?assertEqual(<<"code.html{color: #191970}">>, minify(<<"code.html { color:    #191970; }">>)),
    ?assertEqual(<<"code.html{color: #191970}">>, minify(<<"code.html     { color:    #191970; }">>)),
    %% The comment after the selector causes the insertion of an extra space
    %% it also leaves an extra colon after the last element of a block
    ?assertEqual(<<"code.html { color: #191970; }">>, 
                 minify(<<"/* a */code.html  /* b */  { /* c */ color: /* d */  #191970; /* e */  } /* f */">>)),
    ok.

%%
%% Helpers
%%

minify(Bin) ->
    z_cssmin:minify(Bin).
