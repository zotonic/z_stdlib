%% @author Marc Worrell
%% @copyright 2012 Marc Worrell
%% @doc Misc utility URL functions for zotonic

%% Copyright 2012 Marc Worrell
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
    is_local/1,
    ip_match/2
    ]).


%% @doc An IP address is local if it matches "127.0.0.0/8,10.0.0.0/8,192.168.0.0/16,172.16.0.0/12,169.254.0.0/16,::1,fd00::/8,fe80::/10"
is_local({127,_,_,_}) -> true;
is_local({10,_,_,_}) -> true;
is_local({192,168,_,_}) -> true;
is_local({169,254,_,_}) -> true;
is_local({172,X,_,_}) when X >= 16, X =< 31 -> true;
is_local({X,_,_,_,_,_,_,_}) when X >= 16#fd00, X =< 16#fdff -> true;
is_local({X,_,_,_,_,_,_,_}) when X >= 16#fe80, X =< 16#fecf -> true;
is_local(_) -> false.


%% @doc Check if an IP address matches a list of addresses and masks like "127.0.0.0/8,10.0.0.0/8,fe80::/10"
-spec ip_match(undefined|string()|binary()|tuple(), local|any|none|list()|string()|binary()) -> boolean().
ip_match(undefined, _IPs) ->
    false;
ip_match(_IP, []) ->
    false;
ip_match(IP, local) ->
    {ok, IP1} = parse_address(IP),
    is_local(IP1);
ip_match(_IP, any) ->
    true;
ip_match(_IP, none) ->
    false;
ip_match(IP, [IP|_IPs]) ->
    true;
ip_match(IP, [C|_] = Match) when C >= 32, C =< 127 ->
    Whitelist = string:tokens(Match, ","),
    ip_match(IP, Whitelist);
ip_match(IP, Match) when is_binary(Match) ->
    Whitelist = string:tokens(z_convert:to_list(Match), ","),
    ip_match(IP, Whitelist);
ip_match(Adr, IPs) ->
    {ok, PeerAdr} = parse_address(Adr),
    ip_match_1(PeerAdr, IPs).

ip_match_1(_PeerAdr, []) ->
    false;
ip_match_1(PeerAdr, [PeerAdr|_IPs]) ->
    true;
ip_match_1(PeerAdr, [Local|IPs]) when Local =:= "local"; Local =:= local ->
    case is_local(PeerAdr) of
        true -> true;
        false -> ip_match_1(PeerAdr, IPs)
    end;
ip_match_1(_PeerAdr, [Any|_]) when Any =:= "any"; Any =:= any ->
    true;
ip_match_1(_PeerAdr, [None|_]) when None =:= "none"; None =:= none ->
    false;
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
    end.

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

