% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% coding: utf-8

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc String normalization, used for search indices. Takes a string, lowercases
%% it and then transliterates to ASCII.
%% @end

-module(z_string_normalize).

-export([ normalize/1 ]).

-define(WORD_MAPPING_FILE, "normalize-words-mapping.csv").


%% @doc Transliterate an unicode string to an ascii string with lowercase characters.
%% Tries to transliterate some characters to a..z
-spec normalize(string() | binary() | atom() | {trans, list()} | undefined) -> binary().
normalize(undefined) ->
    <<>>;
normalize(A) when is_atom(A) ->
    normalize(atom_to_binary(A, utf8));
normalize(L) when is_list(L) ->
    normalize(unicode:characters_to_binary(L));
normalize(B) when is_binary(B)  ->
    B1 = z_string:sanitize_utf8(B),
    B2 = unicode:characters_to_nfc_binary(B1),
    B3 = string:casefold(B2),
    normalize_words(B3);
normalize({trans, []}) ->
    <<>>;
normalize({trans, [{_, First} | _] = Tr}) ->
    V = proplists:get_value(en, Tr, First),
    normalize(V).

%% Separators in a lowercased string
-define(is_sep(C),
        C < $0
        orelse (C > $9 andalso C < $a)
        orelse (C > $z andalso C < 127)
        orelse C =:= 8023   % non breaking zero width space
        orelse C =:= 8212   % mdash
        orelse C =:= 8211   % ndash
    ).

%% Normalize some common (Ukrainian) strings, that would be different when
%% using the Russian romanization rules.
normalize_words(B) ->
    Ws = normalize_words_word(B, <<>>, []),
    normalize(erlang:iolist_to_binary(Ws), <<>>).

normalize_words_word(<<>>, W, Acc) ->
    lists:reverse([map_word(W)|Acc]);
normalize_words_word(<<C/utf8, T/binary>>, W, Acc) when ?is_sep(C) ->
    normalize_words_sep(T, <<C/utf8>>, [map_word(W)|Acc]);
normalize_words_word(<<C/utf8, T/binary>>, W, Acc) ->
    normalize_words_word(T, <<W/binary, C/utf8>>, Acc).

normalize_words_sep(<<>>, W, Acc) ->
    lists:reverse([W|Acc]);
normalize_words_sep(<<C/utf8, T/binary>>, W, Acc) when not ?is_sep(C) ->
    normalize_words_word(T, <<C/utf8>>, [W|Acc]);
normalize_words_sep(<<C/utf8, T/binary>>, W, Acc) ->
    normalize_words_sep(T, <<W/binary, C/utf8>>, Acc).

%% Specific word normalizations. The mappings are loaded from a CSV file
%% and stored in the persistent term storage for efficiency.
map_word(<<>>) ->
    <<>>;
map_word(W) ->
    try
        Mapping = persistent_term:get(z_string_normalize_word_mapping),
        maps:get(W, Mapping, W)
    catch
        error:badarg ->
            Filename = filename:join(code:priv_dir(zotonic_stdlib), ?WORD_MAPPING_FILE),
            {ok, Data} = file:read_file(Filename),
            Lines = binary:split(Data, [ <<"\n">>, <<"\r">> ], [ global, trim_all ]),
            NewMapping = lists:foldl(
                fun
                    (<<"#", _/binary>>, Acc) ->
                        Acc;
                    (Line, Acc) ->
                        case binary:split(Line, [ <<",">>, <<";">>, <<"\t">> ]) of
                            [From, To] ->
                                From1 = string:casefold(z_string:trim(From)),
                                To1 = string:casefold(z_string:trim(To)),
                                Acc#{ From1 => To1 };
                            _ ->
                                Acc
                        end
                end,
                #{},
                Lines),
            persistent_term:put(z_string_normalize_word_mapping, NewMapping),
            maps:get(W, NewMapping, W)
end.

normalize(<<>>, Acc) ->
    Acc;
normalize(<<C,T/binary>>, Acc)
    when       (C >= $a andalso C =< $z)
        orelse (C >= $0 andalso C =< $9)
        orelse C =:= 32
        orelse C =:= $\n
        orelse C =:= $\t
        orelse C =:= $-
        orelse C =:= $_ ->
    % Usual ascii characters - just for efficiency at the top
    normalize(T, <<Acc/binary,C>>);
normalize(<<"ä"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"ë"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ï"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ü"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"ö"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"é"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"è"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"í"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ì"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"ú"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"ù"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"ó"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"ò"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"ô"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"ß"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$s>>);
normalize(<<"ç"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"ø"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"å"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"€"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ÿ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"ã"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"ñ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"õ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
% Cyrillic support (from http://en.wikipedia.org/wiki/Romanization_of_Russian)
% See also the rules for Russian passports (2013, ICAO).
normalize(<<"а"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"б"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$b>>);
normalize(<<"в"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"г"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"д"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d>>);
normalize(<<"е"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ё"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ж"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z,$h>>);
normalize(<<"з"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"и"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"й"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$j>>);
normalize(<<"к"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"л"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"м"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$m>>);
normalize(<<"н"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"о"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"п"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$p>>);
normalize(<<"р"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$r>>);
normalize(<<"с"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"т"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"у"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"ф"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$f>>);
normalize(<<"х"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k,$h>>);
normalize(<<"ц"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"ч"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c,$h>>);
normalize(<<"ш"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h>>);
normalize(<<"щ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h,$c,$h>>);
normalize(<<"ъ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$_>>);
normalize(<<"ы"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"ь"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$_>>);
normalize(<<"э"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ю"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i,$u>>);
normalize(<<"я"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i,$a>>);
% Ukrainian support
normalize(<<"ґ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"ї"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"і"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"є"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e,$y>>);
% Polish support
normalize(<<"ą"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"ę"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"ć"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c>>);
normalize(<<"Ł"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"ł"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"ń"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"ś"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ź"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"ż"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
% Turkish support
normalize(<<"ş"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ğ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"ı"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);

% Hebrew support (simplified) https://en.wikipedia.org/wiki/Romanization_of_Hebrew
% TODO: check this, as it seems quite broken/incomplete
normalize(<<"א"/utf8,T/binary>>, Acc) -> normalize(T, Acc);
normalize(<<"ב"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"בּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$b>>);
normalize(<<"ג"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"גּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
normalize(<<"ג׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$j>>);
normalize(<<"ד"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d>>);
normalize(<<"דּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d>>);
normalize(<<"ד׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$d,$h>>);
normalize(<<"ה"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h>>);
normalize(<<"הּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$h>>);
normalize(<<"ו"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"וּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$v>>);
normalize(<<"ז"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"זּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"ז׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z,$h>>);
normalize(<<"ח"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c,$h>>);
normalize(<<"ט"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"טּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"י"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"יּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$y>>);
normalize(<<"ךכ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$c,$h>>);
normalize(<<"ךּ כּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"ל"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"לּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$l>>);
normalize(<<"םמ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$m>>);
normalize(<<"מּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$m>>);
normalize(<<"ןנ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"נּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"ס"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"סּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ע"/utf8,T/binary>>, Acc) -> normalize(T, Acc);
normalize(<<"ףפ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$f>>);
normalize(<<"ףּ פּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$p>>);
normalize(<<"ץצ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$z>>);
normalize(<<"צּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$z>>);
normalize(<<"ץ׳צ׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$s,$h>>);
normalize(<<"ק"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"קּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$k>>);
normalize(<<"ר"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$r>>);
normalize(<<"רּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$r>>);
normalize(<<"ש"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h>>);
normalize(<<"שׁ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h>>);
normalize(<<"שּׁ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s,$h>>);
normalize(<<"שׂ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"שּׂ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$s>>);
normalize(<<"ת"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"תּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t>>);
normalize(<<"ת׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$t,$h>>);
% Hebrew forms used in translitearion from Arabic
normalize(<<"ח׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$n>>);
normalize(<<"ט׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$z>>);
normalize(<<"ע׳ר׳"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$g>>);
% Hebrew vowels
normalize(<<"צ"/utf8, T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$a>>);
normalize(<<"טְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"חֱ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"חֲ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"חֳ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"טִ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$i>>);
normalize(<<"טֵ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"טֶ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e>>);
normalize(<<"טַ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a>>);
normalize(<<"טָ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"טֹ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o>>);
normalize(<<"טֻ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"טוּ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u>>);
normalize(<<"טֵי"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e,$i>>);
normalize(<<"טֶי"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$e,$i>>);
normalize(<<"טַיטַיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$i>>);
normalize(<<"טָיטָיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$a,$i>>);
normalize(<<"טֹיטֹיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$o,$i>>);
normalize(<<"טֻיטֻיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u,$i>>);
normalize(<<"טוּיטוּיְ"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,$u,$i>>);

% Some entities - we might want to add generic code here, depends
% on where normalize/1 is used (can we assume that the input is always html?)
normalize(<<"&amp;", T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&lt;",  T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&gt;",  T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&#39;", T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&quot;",T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&nbsp;",T/binary>>, Acc) -> normalize(T, <<Acc/binary," ">>);
normalize(<<"&mdash;",T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<"&ndash;",T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<"—"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<"–"/utf8,T/binary>>, Acc) -> normalize(T, <<Acc/binary,"-">>);
normalize(<<C/utf8,T/binary>>, Acc) when C >= 32, C =< 126 ->
    % ASCII characters
    normalize(T, <<Acc/binary, C/utf8>>);
normalize(<<C, T/binary>>, Acc) when C =:= $\n; C =:= $\t ->
    % Keep newlines and tabs
    normalize(T, <<Acc/binary, " ">>);
normalize(<<C/utf8,T/binary>>, Acc) when C < 32 ->
    % Replace control characters with spaces
    normalize(T, <<Acc/binary, " ">>);
normalize(<<C/utf8,T/binary>>, Acc) when C =:= 8023 ->
    % Zero width space
    normalize(T, Acc);
normalize(<<C/utf8,T/binary>>, Acc) ->
    % Try to remove any accents.
    C1 = z_string:unaccent(<<C/utf8>>),
    normalize(T, <<Acc/binary, C1/binary>>);
normalize(<<_C,T/binary>>, Acc) ->
    % Drop non-utf8
    normalize(T, Acc).
