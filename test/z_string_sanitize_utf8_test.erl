%% @author Motiejus Jakštys <desired.mta@gmail.com>

-module(z_string_sanitize_utf8_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

valid_utf8_test_() ->
    [
        ?_assert(v_utf8(<<>>)),
        ?_assert(v_utf8(<<127>>)),
        ?_assert(v_utf8(<<2#11001111, 2#10000000>>)),
        ?_assert(v_utf8(<<2#11011111, 2#10111111>>)),

        ?_assert(v_utf8(<<2#11101000, 2#10000000, 2#10000000>>)),
        ?_assert(v_utf8(<<2#11101111, 2#10111111, 2#10111101>>)),

        ?_assert(v_utf8(<<2#11110100, 2#10000000, 2#10000000, 2#10000000>>)),
        ?_assert(v_utf8(<<2#11110000, 2#10111111, 2#10111111, 2#10111111>>)),

        ?_assertNot(v_utf8(<<128>>)),

        ?_assertNot(v_utf8(<<2#11100000, 2#10000000>>)),
        ?_assertNot(v_utf8(<<2#11000000, 2#11000000>>)),

        ?_assertNot(v_utf8(<<2#11110000, 2#10000000, 2#10000000>>)),
        ?_assertNot(v_utf8(<<2#11100000, 2#11000000, 2#10000000>>)),
        ?_assertNot(v_utf8(<<2#11100000, 2#10000000, 2#11000000>>)),

        ?_assertNot(v_utf8(<<2#11111000, 2#10000000, 2#10000000, 2#10000000>>))
    ].

v_utf8(Bin) ->
    z_string:sanitize_utf8(Bin) =:= Bin.

z_string_test_() ->
    [
        {"z_string:sanitize_utf8 -> unicode.erl",
            proper_utils:qc_(s_utf8a())},
        {"unicode.erl -> z_string:sanitize_utf8",
            proper_utils:qc_(s_utf8b())},
        {"For every utf8 binary unicode.erl and z_string:sanitize_utf8",
            proper_utils:qc_(s_utf8c())}
    ].

%% @doc For every random binary if s_utf8 claims it's utf8,
%% unicode.erl agrees
s_utf8a() ->
    ?FORALL(
        ProbablyUtf8,
        ?SUCHTHAT(B, binary(), v_utf8(B)),
        unicode:characters_to_binary(ProbablyUtf8) =:= ProbablyUtf8
    ).

%% @doc For every random binary if unicode.erl claims it's utf8,
%% s_utf agrees
s_utf8b() ->
    ?FORALL(
        ProbablyUtf8,
        ?SUCHTHAT(
            B,
            binary(),
            unicode:characters_to_binary(B) == B),
        v_utf8(ProbablyUtf8)
    ).

s_utf8c() ->
    ?FORALL(
        Utf8,
        utf8_string(),
        case unicode:characters_to_binary(Utf8) == Utf8 of
            false ->
                io:format("Wrong generator! ~p~n", [Utf8]),
                false;
            true ->
                v_utf8(Utf8)
        end
    ).

%% @doc PropErly generate valid utf8 string of various lengths
utf8_string() ->
    ?LET(
        CodePoints,
        ?LET(Len, binary_len(), vector(Len, utf8_codepoint())),
        iolist_to_binary(CodePoints)
    ).

%% @doc Generate a codepoint of max Len bytes in length
-spec gen_codepoint(pos_integer(), [proper_types:type(), ...]) ->
    proper_types:type().
gen_codepoint(4, Acc) ->
    gen_codepoint(3, [{integer(16#10000, 16#10FFFF), 4}|Acc]);

gen_codepoint(3, Acc) ->
    V1 = integer(16#800, 16#D7FF),
    V2 = integer(16#E000, 16#FFFD),
    gen_codepoint(2, [{union([V1, V2]), 3}|Acc]);

gen_codepoint(2, Acc) ->
    gen_codepoint(1, [{integer(16#80, 16#7FF), 2}|Acc]);

gen_codepoint(1, Acc) ->
    union([{integer(0, 16#7F), 1}|Acc]).

utf8_codepoint() ->
    utf8_codepoint(4).

%% @doc Valid sub-UTF-8 code point binary (1-4 byte length)
-spec utf8_codepoint(pos_integer()) -> proper_types:type().
utf8_codepoint(MaxLen) ->
    ?LET(
        {Codepoint, Octets},
        gen_codepoint(MaxLen, []),
        case Octets of
            1 ->
                <<Codepoint:8>>;
            2 ->
                <<A:5, B:6>> = <<Codepoint:11>>,
                <<2#110:3, A:5, 2#10:2, B:6>>;
            3 ->
                <<A:4, B:6, C:6>> = <<Codepoint:16>>,
                <<2#1110:4, A:4, 2#10:2, B:6, 2#10:2, C:6>>;
            4 ->
                <<A:3, B:6, C:6, D:6>> = <<Codepoint:21>>,
                <<2#11110:5, A:3, 2#10:2, B:6, 2#10:2, C:6, 2#10:2, D:6>>
        end
    ).

%% @doc Create binaries of length 0..2^10 bytes (0 - 1KiB)
%%
%% To generate a binary we first generate a Number := uniform(0, 11).
%% Then Length := uniform(1 << Number, 1 << (Number + 1))
binary_len() ->
    ?LET(
        Len,
        integer(0, 9),
        integer(1 bsl Len, 1 bsl (Len + 1))
    ).

