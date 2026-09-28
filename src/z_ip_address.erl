%% @author Marc Worrell
%% @copyright 2012-2022 Marc Worrell
%% @doc Misc utility URL functions for zotonic
%% @end

%% Copyright 2012-2022 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_ip_address).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_local_name/1,
    is_local/1,
    is_public_name/1,
    is_public/1,
    ip_match/2
    ]).

%% @doc Check if the host name resolves to a local IP address.
-spec is_local_name( string() | binary() ) -> boolean().
is_local_name(Name) when is_binary(Name) ->
    is_local_name(unicode:characters_to_list(Name, utf8));
is_local_name(Name) ->
    case inet:getaddr(Name, inet) of
        {ok, IP} ->
            is_local(IP);
        {error, nxdomain} ->
            case inet:getaddr(Name, inet6) of
                {ok, IP} ->
                    is_local(IP);
                {error, _} ->
                    false
            end;
        {error, _} ->
            false
    end.

%% @doc Check for loopback, private-use, shared (CGNAT), or link-local addresses.
%% IPv4: 127.0.0.0/8, 10.0.0.0/8, 192.168.0.0/16, 172.16.0.0/12,
%% 169.254.0.0/16, and 100.64.0.0/10. IPv6: ::1, fc00::/7, and fe80::/10.
%% IPv4-mapped IPv6 addresses (::ffff:0:0/96) use the embedded IPv4 classification.
%% This is not a public-address test: false also covers unspecified, multicast,
%% documentation, benchmarking, and other reserved or special-purpose addresses.
-spec is_local( inet:ip_address() ) -> boolean().
is_local({127,_,_,_}) -> true;
is_local({10,_,_,_}) -> true;
is_local({192,168,_,_}) -> true;
is_local({169,254,_,_}) -> true;
is_local({172,X,_,_})  when X >= 16, X =< 31 -> true;
is_local({100,X,_,_}) when X >= 64, X =< 127 -> true;
is_local({0,0,0,0,0,16#ffff,High,Low}) ->
    is_local({High bsr 8, High band 16#ff, Low bsr 8, Low band 16#ff});
is_local({0,0,0,0,0,0,0,1}) -> true;
is_local({X,_,_,_,_,_,_,_}) when X >= 16#fc00, X =< 16#fdff -> true;
is_local({X,_,_,_,_,_,_,_}) when X >= 16#fe80, X =< 16#febf -> true;
is_local(_) -> false.


%% @doc Check whether every IPv4 and IPv6 address of a name is public.
%% Literal addresses are classified directly. Names without addresses, invalid
%% input, and resolver errors return false. An absent address family (nxdomain)
%% is allowed when the other family has public addresses. This is a DNS snapshot,
%% not protection against DNS rebinding between checking and connecting.
-spec is_public_name(Name) -> boolean() when
    Name :: string() | binary().
is_public_name(Name) when is_binary(Name) ->
    is_public_name(unicode:characters_to_list(Name));
is_public_name(Name) when is_list(Name), Name =/= [] ->
    case inet:parse_address(Name) of
        {ok, IP} ->
            is_public(IP);
        {error, _} ->
            IPv4 = inet:getaddrs(Name, inet),
            IPv6 = inet:getaddrs(Name, inet6),
            public_dns_results(IPv4, IPv6)
    end;
is_public_name(_) ->
    false.

public_dns_results({ok, IPv4}, {ok, IPv6}) ->
    public_addresses(IPv4 ++ IPv6);
public_dns_results({ok, IPs}, {error, nxdomain}) ->
    public_addresses(IPs);
public_dns_results({error, nxdomain}, {ok, IPs}) ->
    public_addresses(IPs);
public_dns_results(_, _) ->
    false.

public_addresses(IPs) ->
    IPs =/= [] andalso lists:all(fun is_public/1, IPs).

%% @doc Conservative public-unicast address classification for outbound requests.
%% Excludes local, unspecified, multicast, documentation, benchmarking, reserved,
%% and special protocol/transition ranges. IPv6 is limited to 2000::/3, excluding
%% 2001::/23, 2001:db8::/32, 2002::/16, and 3fff::/20. Special-purpose exceptions
%% inside excluded ranges are deliberately not allowed, even if globally reachable.
%% IPv4-mapped IPv6 addresses inherit the embedded IPv4 classification.
%% This classifies address ranges; it does not prove reachability or trust.
-spec is_public(IP) -> boolean() when
    IP :: inet:ip_address().
is_public({A,B,C,D} = IP) ->
    valid_address_parts([A,B,C,D], 255)
        andalso not is_local(IP)
        andalso not ip_match(IP, [
            "0.0.0.0/8", "192.0.0.0/24", "192.0.2.0/24", "192.88.99.0/24",
            "198.18.0.0/15", "198.51.100.0/24", "203.0.113.0/24", "224.0.0.0/3"
        ]);
is_public({0,0,0,0,0,16#ffff,High,Low}) ->
    valid_address_parts([High,Low], 65535)
        andalso is_public({High bsr 8, High band 16#ff, Low bsr 8, Low band 16#ff});
is_public({A,B,C,D,E,F,G,H} = IP) ->
    valid_address_parts([A,B,C,D,E,F,G,H], 65535)
        andalso ip_match(IP, ["2000::/3"])
        andalso not ip_match(IP, ["2001::/23", "2001:db8::/32", "2002::/16", "3fff::/20"]);
is_public(_) ->
    false.

valid_address_parts(Parts, Max) ->
    lists:all(fun(N) -> is_integer(N) andalso N >= 0 andalso N =< Max end, Parts).


%% @doc Check if an IP address matches a list of addresses and masks like "127.0.0.0/8,10.0.0.0/8,fe80::/10"
-spec ip_match(undefined|string()|binary()|tuple(), local|any|none|list()|string()|binary()) -> boolean().
ip_match(undefined, _IPs) ->
    false;
ip_match(_IP, []) ->
    false;
ip_match(IP, Local) when Local =:= local; Local =:= "local"; Local =:= <<"local">> ->
    {ok, IP1} = parse_address(IP),
    is_local(IP1);
ip_match(_IP, Any) when Any =:= any; Any =:= "any"; Any =:= <<"any">> ->
    true;
ip_match(_IP, None) when None =:= none; None =:= "none"; None =:= <<"none">> ->
    false;
ip_match(IP, [IP|_IPs]) ->
    true;
ip_match(IP, [C|_] = Match) when C >= 32, C =< 127 ->
    Allowlist = string:tokens(Match, ","),
    ip_match(IP, Allowlist);
ip_match(IP, Match) when is_binary(Match) ->
    Allowlist = string:tokens(z_convert:to_list(Match), ","),
    ip_match(IP, Allowlist);
ip_match(Adr, IPs) ->
    {ok, PeerAdr} = parse_address(Adr),
    ip_match_1(PeerAdr, IPs).

ip_match_1(_PeerAdr, []) ->
    false;
ip_match_1(PeerAdr, [PeerAdr|_IPs]) ->
    true;
ip_match_1(PeerAdr, [Local|IPs]) when Local =:= "local"; Local =:= local; Local =:= <<"local">> ->
    case is_local(PeerAdr) of
        true -> true;
        false -> ip_match_1(PeerAdr, IPs)
    end;
ip_match_1(_PeerAdr, [Any|_]) when Any =:= "any"; Any =:= any; Any =:= <<"any">> ->
    true;
ip_match_1(PeerAdr, [None|IPs]) when None =:= "none"; None =:= none; None =:= <<"none">> ->
    ip_match_1(PeerAdr, IPs);
ip_match_1(PeerAdr, [{MatchAdr,BitsNr}|IPs]) when is_tuple(MatchAdr), is_integer(BitsNr) ->
    case ip_match_mask(PeerAdr, MatchAdr, BitsNr) of
        true -> true;
        false -> ip_match(PeerAdr, IPs)
    end;
ip_match_1(PeerAdr, [IP|IPs]) when is_list(IP) ->
    case string:tokens(IP, "/") of
        [IPVal,Bits] ->
            {ok, MatchAdr} = inet:parse_address(IPVal),
            BitsNr = list_to_integer(Bits),
            case ip_match_mask(PeerAdr, MatchAdr, BitsNr) of
                true -> true;
                false -> ip_match(PeerAdr, IPs)
            end;
        [IPVal] ->
            {ok, MatchAdr} = inet:parse_address(IPVal),
            case MatchAdr of
                PeerAdr -> true;
                _ -> ip_match(PeerAdr, IPs)
            end;
        [] ->
            ip_match(PeerAdr, IPs)
    end;
ip_match_1(PeerAdr, [IP|IPs]) when is_binary(IP) ->
    ip_match_1(PeerAdr, [ binary_to_list(IP) | IPs ]).

parse_address({_,_,_,_} = IP) -> {ok, IP};
parse_address({_,_,_,_,_,_,_,_} = IP) -> {ok, IP};
parse_address(Adr) -> inet:parse_address(z_convert:to_list(Adr)).

ip_match_mask({_,_,_,_} = Peer, {_,_,_,_} = Match, Bits) ->
    ip_match_mask_1(Peer, Match, 32-Bits);
ip_match_mask({_,_,_,_,_,_,_,_} = Peer, {_,_,_,_,_,_,_,_} = Match, Bits) ->
    ip_match_mask_1(Peer, Match, 128-Bits);
ip_match_mask(_, _, _) ->
    false.

ip_match_mask_1(PeerAdr, MatchAdr, MaskBits) ->
    NetMask = ((1 bsl 128) - 1) bsl MaskBits,
    (to_ip_number(MatchAdr) band NetMask) =:= (to_ip_number(PeerAdr) band NetMask).

to_ip_number({A,B,C,D}) ->
    ((((A * 256) + B) * 256) + C) * 256 + D;
to_ip_number({A,B,C,D,E,F,G,H}) ->
    (((((((((A * 65536) + B) * 65536) + C) * 65536 + D) * 65536 + E) * 65536 + F) * 65536) + G) * 65536 + H.

