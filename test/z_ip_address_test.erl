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

%% Check both sides of each IPv4 boundary, also through the IPv6 mapped form.
is_local_ipv4_boundaries_test_() ->
    Cases = [
        {"9.255.255.255", false}, {"10.0.0.0", true},
        {"10.255.255.255", true}, {"11.0.0.0", false},
        {"100.63.255.255", false}, {"100.64.0.0", true},
        {"100.64.4.0", true}, {"100.127.255.255", true}, {"100.128.0.0", false},
        {"126.255.255.255", false}, {"127.0.0.0", true},
        {"127.255.255.255", true}, {"128.0.0.0", false},
        {"169.253.255.255", false}, {"169.254.0.0", true},
        {"169.254.255.255", true}, {"169.255.0.0", false},
        {"172.15.255.255", false}, {"172.16.0.0", true},
        {"172.31.255.255", true}, {"172.32.0.0", false},
        {"192.167.255.255", false}, {"192.168.0.0", true},
        {"192.168.255.255", true}, {"192.169.0.0", false},
        {"8.8.8.8", false}
    ],
    [local_address_test(Prefix ++ Address, Expected)
        || {Address, Expected} <- Cases, Prefix <- ["", "::ffff:"]].

is_local_ipv6_boundaries_test_() ->
    [local_address_test(Address, Expected) || {Address, Expected} <- [
        {"::", false}, {"::1", true}, {"::2", false},
        {"fbff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", false},
        {"fc00::", true}, {"fcff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", true},
        {"fd00::", true}, {"fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", true},
        {"fe00::", false}, {"fe7f:ffff:ffff:ffff:ffff:ffff:ffff:ffff", false},
        {"fe80::", true}, {"febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff", true},
        {"fec0::", false}, {"fecf::", false}, {"ff00::", false},
        {"2606:4700::1111", false},
        % Similar-looking prefixes are not IPv4-mapped IPv6 addresses.
        {"::fffe:127.0.0.1", false}, {"::1:ffff:127.0.0.1", false}
    ]].

local_address_test(Address, Expected) ->
    {Address, fun() ->
        {ok, IP} = inet:parse_address(Address),
        ?assertEqual(Expected, z_ip_address:is_local(IP)),
        ?assertEqual(Expected, z_ip_address:ip_match(IP, local))
    end}.

is_public_ipv4_test_() ->
    Cases = [
        {"0.0.0.0", false}, {"0.255.255.255", false}, {"1.0.0.0", true},
        {"10.0.0.1", false}, {"100.64.4.1", false}, {"100.127.255.255", false},
        {"127.0.0.1", false}, {"169.254.1.1", false}, {"172.16.0.1", false},
        {"192.0.0.0", false}, {"192.0.0.255", false}, {"192.0.1.0", true},
        {"192.0.2.0", false}, {"192.0.2.255", false}, {"192.0.3.0", true},
        {"192.88.98.255", true}, {"192.88.99.0", false},
        {"192.88.99.255", false}, {"192.88.100.0", true}, {"192.168.1.1", false},
        {"198.17.255.255", true}, {"198.18.0.0", false},
        {"198.19.255.255", false}, {"198.20.0.0", true},
        {"198.51.99.255", true}, {"198.51.100.0", false},
        {"198.51.100.255", false}, {"198.51.101.0", true},
        {"203.0.112.255", true}, {"203.0.113.0", false},
        {"203.0.113.255", false}, {"203.0.114.0", true},
        {"223.255.255.255", true}, {"224.0.0.0", false},
        {"239.255.255.255", false}, {"240.0.0.0", false},
        {"255.255.255.255", false}, {"8.8.8.8", true}
    ],
    [public_address_test(Prefix ++ Address, Expected)
        || {Address, Expected} <- Cases, Prefix <- ["", "::ffff:"]].

is_public_ipv6_test_() ->
    [public_address_test(Address, Expected) || {Address, Expected} <- [
        {"::", false}, {"::1", false}, {"fc00::1", false}, {"fd00::1", false},
        {"fe80::1", false}, {"fec0::1", false}, {"ff02::1", false},
        {"64:ff9b::808:808", false}, {"64:ff9b:1::1", false}, {"100::1", false},
        {"1fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", false}, {"2000::", true},
        {"2001::", false}, {"2001:1ff:ffff:ffff:ffff:ffff:ffff:ffff", false},
        {"2001:200::", true}, {"2001:db7:ffff:ffff:ffff:ffff:ffff:ffff", true},
        {"2001:db8::", false}, {"2001:db8:ffff:ffff:ffff:ffff:ffff:ffff", false},
        {"2001:db9::", true}, {"2002::", false},
        {"2002:ffff:ffff:ffff:ffff:ffff:ffff:ffff", false}, {"2003::", true},
        {"3ffe:ffff:ffff:ffff:ffff:ffff:ffff:ffff", true}, {"3fff::", false},
        {"3fff:fff:ffff:ffff:ffff:ffff:ffff:ffff", false}, {"3fff:1000::", true},
        {"4000::", false}, {"2606:4700::1111", true}
    ]].

public_address_test(Address, Expected) ->
    {Address, fun() ->
        {ok, IP} = inet:parse_address(Address),
        ?assertEqual(Expected, z_ip_address:is_public(IP)),
        ?assertEqual(Expected, z_ip_address:is_public_name(Address)),
        ?assertEqual(Expected, z_ip_address:is_public_name(list_to_binary(Address)))
    end}.

is_public_invalid_test() ->
    lists:foreach(fun(IP) -> ?assertNot(z_ip_address:is_public(IP)) end, [
        undefined, {}, {8,8,8}, {8,8,8,256}, {8,8,8,-1}, {8,8,8,1.0},
        {16#2000,0,0,0,0,0,0,65536}, {16#2000,0,0,0,0,0,0,bad},
        {0,0,0,0,0,16#ffff,-1,1}, {0,0,0,0,0,16#ffff,65536,1}
    ]),
    ?assertNot(z_ip_address:is_public_name(<<>>)),
    ?assertNot(z_ip_address:is_public_name(<<255>>)).

%% Use the in-memory hosts database, with DNS disabled, for deterministic results.
is_public_name_resolution_test() ->
    Lookup = inet_db:res_option(lookup),
    Public4 = {93,184,215,14},
    Public6 = {16#2606,16#4700,0,0,0,0,0,16#1111},
    Private4 = {10,23,45,67},
    Private6 = {16#fd00,0,0,0,0,0,0,123},
    try
        ok = inet_db:set_lookup([file]),
        ok = inet_db:add_host(Public4, ["public4.ip-test.invalid", "dual.ip-test.invalid", "mixed6.ip-test.invalid", "mixed4.ip-test.invalid"]),
        ok = inet_db:add_host(Public6, ["public6.ip-test.invalid", "dual.ip-test.invalid", "mixed4.ip-test.invalid"]),
        ok = inet_db:add_host(Private4, ["mixed4.ip-test.invalid"]),
        ok = inet_db:add_host(Private6, ["mixed6.ip-test.invalid"]),
        ?assert(z_ip_address:is_public_name("public4.ip-test.invalid")),
        ?assert(z_ip_address:is_public_name("public6.ip-test.invalid")),
        ?assert(z_ip_address:is_public_name(<<"dual.ip-test.invalid">>)),
        ?assertNot(z_ip_address:is_public_name("mixed4.ip-test.invalid")),
        ?assertNot(z_ip_address:is_public_name("mixed6.ip-test.invalid")),
        ?assertNot(z_ip_address:is_public_name("absent.ip-test.invalid"))
    after
        lists:foreach(fun inet_db:del_host/1, [Public4, Public6, Private4, Private6]),
        inet_db:set_lookup(Lookup)
    end.
