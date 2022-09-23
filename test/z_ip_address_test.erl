%% @author Marc Worrell

-module(z_ip_address_test).

-include_lib("eunit/include/eunit.hrl").

is_local_test() ->
    ?assertEqual(true, z_ip_address:is_local_name(<<"localhost">>)),
    ?assertEqual(true, z_ip_address:is_local_name("localhost")),
    ?assertEqual(true, z_ip_address:is_local_name("127.0.0.1")),
    ?assertEqual(true, z_ip_address:is_local_name("::1")),
    ?assertEqual(true, z_ip_address:is_local_name("192.168.1.10")),
    ?assertEqual(true, z_ip_address:is_local_name(<<"192.168.1.10">>)),
    ?assertEqual(false, z_ip_address:is_local_name(<<"8.8.8.8">>)).
