%% @author Marc Worrell
%% @copyright 2015 Marc Worrell
%% @doc Check an IP address against some DNSBL providers (rfc5782)

%% Copyright 2015 Marc Worrell
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

-module(z_email_dnsbl).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_blocked/1,
    is_blocked/2,
    is_blocked/3,
    status/1,
    status/2,
    status/3,
    dnswl_list/0,
    dnsbl_list/0,

    test/0
]).

% Not all ISPs have a subscription to the dnswl.org whitelist, Google has.
-define(WHITELIST_NAMESERVERS, [{{8,8,8,8},53}, {{8,8,4,4},53}]).

-include_lib("kernel/include/inet.hrl").

-spec is_blocked(inet:ip_address()) -> boolean().
is_blocked(IP) ->
    is_blocked(IP, dnsbl_list(), dnswl_list()).

-spec is_blocked(inet:ip_address(), list(string())) -> boolean().
is_blocked(IP, RTBLs) ->
    is_blocked(IP, RTBLs, dnswl_list()).

-spec is_blocked(inet:ip_address(), list(string()), list(string())) -> boolean().
is_blocked(IP, RTBLs, WLs) ->
    case status(IP, RTBLs, WLs) of
        {ok, {blocked, _DNSBL}} -> true;
        _ -> false
    end.

-spec status(inet:ip_address()) -> {ok, {ok, notlisted|whitelisted|{blocked, list(string())}}} | {error, term()}.
status(IP) ->
    status(IP, dnsbl_list(), dnswl_list()).

-spec status(inet:ip_address(), list()) -> {ok, notlisted|whitelisted|{blocked, list(string())}} | {error, term()}.
status(IP, DNSBLs) ->
    status(IP, DNSBLs, []).

-spec status(inet:ip_address(), list(), list()) ->
          {ok, {blocked, list()}} 
        | {ok, notlisted}
        | {ok, whitelisted}
        | {error, term()}.
status(IP, DNSBLs, DNSWLs) ->
    Dotted = reverse(IP),
    case status_1(Dotted, IP, DNSWLs, true) of
        {ok, {blocked, _}} -> {ok, whitelisted};
        _ ->
            case is_whitelisted(IP) of
                true -> {ok, whitelisted};
                false -> status_1(Dotted, IP, DNSBLs, false)
            end
    end.

status_1(_Dotted, _IP, [], _IsWhiteList) ->
    {ok, notlisted};
status_1(Dotted, IP, [DNSBL|Rest], IsWhiteList) ->
    case gethostbyname(Dotted++DNSBL, IsWhiteList) of
        {ok, #hostent{h_addr_list=AddrList}} ->
            case check_addr_list(DNSBL, AddrList) of
                {ok, notlisted} ->
                    status_1(Dotted, IP, Rest, IsWhiteList);
                Other ->
                    Other
            end;
        {error, nxdomain} ->
            status_1(Dotted, IP, Rest, IsWhiteList);
        {error, _} ->
            status_1(Dotted, IP, Rest, IsWhiteList)
    end.

gethostbyname(Name, false) ->
    inet:gethostbyname(Name);
gethostbyname(Name, true) ->
    ResolverOptions = [
        {nameservers, ?WHITELIST_NAMESERVERS}
    ],
    case inet_res:lookup(Name, in, a, ResolverOptions) of
        [] -> {error, enoent};
        IPs when is_list(IPs) ->
            {ok, #hostent{h_addr_list=IPs}}
    end.


%% @todo Use the SPF records of well-known email hosters for whitelist checks
is_whitelisted(IP) ->
    case inet_res:gethostbyaddr(IP) of
        {ok, #hostent{h_name=Hostname}} ->
            case is_whitelisted_hostname(Hostname) of
                true -> true;
                false -> false
            end;
        {error, nxdomain} ->
            false
    end.

is_whitelisted_hostname(Hostname) ->
    Reversed = lists:reverse(string:tokens(Hostname, ".")),
    case Reversed of
        ["com","google","mail-"++_] -> true;
        ["net","msn","ntwk"|_] -> true;   % "ten2-1-111.sn1-6nx-mms-1a.ntwk.msn.net"
        _ -> false
    end.

%% @doc Check the returned address against RFC5782 return values
%%      Here we could check on the addresses returned to check for
%%      the reason an address is blocked
%%      Filter on 127.0.0.x addresses to prevent problems where a
%%      DNS server 'hijacks' queries to resolve to an ISPs own pages.
check_addr_list(_DNSBL, []) ->
    {ok, notlisted};
check_addr_list(DNSBL, List) ->
    List1 = lists:filter(fun
                            ({127,0,_,255}) -> false;
                            ({127,0,_,_}) -> true;
                            (_) -> false
                         end,
                         List),
    case List1 of
        [] -> {ok, notlisted};
        _ -> {ok, {blocked, DNSBL}}
    end.

%% @doc Default list of DNSWL services
dnswl_list() ->
    [
        "list.dnswl.org",       % https://www.dnswl.org/?page_id=15
        "swl.spamhaus.org"      % http://www.spamhauswhitelist.com/en/usage.html
    ].

%% @doc Default list of DNSBL services
dnsbl_list() ->
    [
        "zen.spamhaus.org",     % http://www.spamhaus.org/zen/
        "dnsbl.sorbs.net"       % http://dnsbl.sorbs.net/general/using.shtml
    ].

%% @doc Reverse the ip address so that it can be prefixed to the DNSBL domain
reverse({_,_,_,_} = IPv4) ->
    lists:flatten([ [ integer_to_list(V), $. ] || V <- lists:reverse(tuple_to_list(IPv4)) ]);
reverse(IPv6) when is_tuple(IPv6) ->
    Nibbles = lists:flatten([ nibbles(P) || P <- tuple_to_list(IPv6) ]),
    lists:flatten([ [ to_hex(Nibble), $. ] || Nibble <- lists:reverse(Nibbles) ]).

nibbles(N) ->
    <<A:4,B:4,C:4,D:4>> = <<N:16/unsigned>>,
    [ A, B, C, D ].

to_hex(C) when C =< 9 -> C + $0;
to_hex(C) -> C - 10 + $a.



test() ->
    "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.8.b.d.0.1.0.0.2." = reverse({16#2001,16#db8,1,2,3,4,16#567,16#89ab}),
    "1.0.0.127." = reverse({127,0,0,1}),
    ok.

