%% @author Marc Worrell <marc@worrell.nl>

-module(z_filelib_test).

-include_lib("eunit/include/eunit.hrl").

os_filename_unix_test() ->
    ?assertEqual(
        "''",
        z_filelib:os_filename(unix, "")),
    ?assertEqual(
        "'simple file.txt'",
        z_filelib:os_filename(unix, "simple file.txt")),
    ?assertEqual(
        lists:flatten([$', "foo", $', $\\, $', $', "bar.jpg", $']),
        z_filelib:os_filename(unix, "foo'bar.jpg")),
    ?assertEqual(
        lists:flatten([$', "foo", $\\, $', $\\, $', $', "bar.jpg", $']),
        z_filelib:os_filename(unix, "foo\\'bar.jpg")),
    ?assertEqual(
        lists:flatten([$', $', $\\, $', $', $', $\\, $', $', $']),
        z_filelib:os_filename(unix, "''")),
    ?assertEqual(
        "'$HOME; rm -rf /; `date`'",
        z_filelib:os_filename(unix, "$HOME; rm -rf /; `date`")).

os_filename_win32_test() ->
    ?assertEqual(
        "\"\"",
        z_filelib:os_filename(win32, "")),
    ?assertEqual(
        "\"simple file.txt\"",
        z_filelib:os_filename(win32, "simple file.txt")),
    ?assertEqual(
        lists:flatten([$", "foo", $\\, $", "bar.jpg", $"]),
        z_filelib:os_filename(win32, "foo\"bar.jpg")),
    ?assertEqual(
        lists:flatten([$", $\\, $", $"]),
        z_filelib:os_filename(win32, "\"")),
    ?assertEqual(
        lists:flatten([$", "c:\\path", "\\\\\\", $", "bar.jpg", $"]),
        z_filelib:os_filename(win32, "c:\\path\\\"bar.jpg")),
    ?assertEqual(
        lists:flatten([$", "c:\\path", "\\\\", $"]),
        z_filelib:os_filename(win32, "c:\\path\\")),
    ?assertEqual(
        lists:flatten([$", "c:\\path", "\\\\\\\\", $"]),
        z_filelib:os_filename(win32, "c:\\path\\\\")).
