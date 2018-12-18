%% @author Linus Schoemaker <linus@driebit.nl>

-module(z_binary_test).

-include_lib("eunit/include/eunit.hrl").

join_test() ->
    ?assertEqual(<<"aap noot mies">>, z_binary:join([<<"aap">>, <<"noot">>, <<"mies">>], <<" ">>)),
    ?assertEqual(<<"foobarbaz">>, z_binary:join([<<"foo">>, <<"bar">>, <<"baz">>], <<>>)),
    ?assertEqual(<<>>, z_binary:join([], <<>>)),
    ?assertEqual(<<>>, z_binary:join([], <<"foo">>)).
