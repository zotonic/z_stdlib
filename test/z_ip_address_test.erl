%% @author Marc Worrell

-module(z_ip_address_test).

-include_lib("eunit/include/eunit.hrl").

is_local_test() ->
    true = z_ip_address:is_local_name(<<"localhost">>),
    true = z_ip_address:is_local_name("localhost"),
    true = z_ip_address:is_local_name("127.0.0.1"),
    true = z_ip_address:is_local_name("::1"),
    true = z_ip_address:is_local_name("192.168.1.10"),
    true = z_ip_address:is_local_name(<<"192.168.1.10">>),
    false =  z_ip_address:is_local_name(<<"8.8.8.8">>).
