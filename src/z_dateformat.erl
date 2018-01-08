%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2009-2014 Marc Worrell
%%%
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>

-module(z_dateformat).
-export([format/1, format/2, format/3]).

-define(TAG_SUPPORTED(C),
    C =:= $a orelse
    C =:= $A orelse
    C =:= $b orelse
    C =:= $B orelse
    C =:= $c orelse
    C =:= $d orelse
    C =:= $D orelse
    C =:= $f orelse
    C =:= $F orelse
    C =:= $g orelse
    C =:= $G orelse
    C =:= $h orelse
    C =:= $H orelse
    C =:= $i orelse
    C =:= $I orelse
    C =:= $j orelse
    C =:= $l orelse
    C =:= $L orelse
    C =:= $m orelse
    C =:= $M orelse
    C =:= $n orelse
    C =:= $N orelse
    C =:= $O orelse
    C =:= $P orelse
    C =:= $r orelse
    C =:= $s orelse
    C =:= $S orelse
    C =:= $t orelse
    C =:= $T orelse
    C =:= $U orelse
    C =:= $w orelse
    C =:= $W orelse
    C =:= $y orelse
    C =:= $Y orelse
    C =:= $z orelse
    C =:= $Z
).

%
% Format the current date/time
%
format(FormatString) ->
    format(FormatString, []).

format(FormatString, Options) ->
    {Date, Time} = erlang:localtime(),
    iolist_to_binary(replace_tags(Date, Time, FormatString, Options)).

%
% Format a tuple of the form {{Y,M,D},{H,M,S}}
% This is the format returned by erlang:localtime()
% and other standard date/time BIFs
%
format({{9999,_,_},_}, _FormatString, _Options) ->
    undefined;
format({{_,_,_} = Date,{_,_,_} = Time}, FormatString, Options) ->
    iolist_to_binary(replace_tags(Date, Time, FormatString, Options));

%
% Format a tuple of the form {Y,M,D}
%
format({_,_,_} = Date, FormatString, Options) ->
    iolist_to_binary(replace_tags(Date, {0,0,0}, FormatString, Options));
format(DateTime, FormatString, _Options) ->
    error_logger:warning_msg("z_dateformat: Unrecognized date parameter : ~p~n", [DateTime]),
    FormatString.

replace_tags(Date, Time, Input, Options) when is_binary(Input) ->
    replace_tags(Date, Time, binary_to_list(Input), Options);
replace_tags(Date, Time, Input, Options) ->
    replace_tags(Date, Time, Input, [], noslash, Options).
replace_tags(_Date, _Time, [], Out, _State, _Options) ->
    lists:reverse(Out);
replace_tags(Date, Time, [C|Rest], Out, noslash, Options) when ?TAG_SUPPORTED(C) ->
    replace_tags(Date, Time, Rest, [tag_to_value(C, Date, Time, Options)|Out], noslash, Options);
replace_tags(Date, Time, [$\\|Rest], Out, noslash, Options) ->
    replace_tags(Date, Time, Rest, Out, slash, Options);
replace_tags(Date, Time, [C|Rest], Out, slash, Options) ->
    replace_tags(Date, Time, Rest, [C|Out], noslash, Options);
replace_tags(Date, Time, [C|Rest], Out, _State, Options) ->
    replace_tags(Date, Time, Rest, [C|Out], noslash, Options).


%-----------------------------------------------------------
% Time formatting
%-----------------------------------------------------------

% 'a.m.' or 'p.m.'
tag_to_value($a, _, {H, _, _}, _Options) when H > 11 -> "p.m.";
tag_to_value($a, _, _, _Options) -> "a.m.";

% 'AM' or 'PM'
tag_to_value($A, _, {H, _, _}, _Options) when H > 11 -> "PM";
tag_to_value($A, _, _, _Options) -> "AM";

% Swatch Internet time
tag_to_value($B, _, _, _Options) ->
   ""; % NotImplementedError

%
% Time, in 12-hour hours and minutes, with minutes
% left off if they're zero.
%
% Examples: '1', '1:30', '2:05', '2'
%
% Proprietary extension.
%
tag_to_value($f, Date, {H, 0, S}, Options) ->
   % If min is zero then return the hour only
   tag_to_value($g, Date, {H, 0, S}, Options);
tag_to_value($f, Date, Time, Options) ->
   % Otherwise return hours and mins
   tag_to_value($g, Date, Time, Options)
      ++ ":" ++ tag_to_value($i, Date, Time, Options);

% Hour, 12-hour format without leading zeros; i.e. '1' to '12'
tag_to_value($g, _, {H,_,_}, _Options) ->
   integer_to_list(hour_24to12(H));

% Hour, 24-hour format without leading zeros; i.e. '0' to '23'
tag_to_value($G, _, {H,_,_}, _Options) ->
   integer_to_list(H);

% Hour, 12-hour format; i.e. '01' to '12'
tag_to_value($h, _, {H,_,_}, _Options) ->
   integer_to_list_zerofill(hour_24to12(H));

% Hour, 24-hour format; i.e. '00' to '23'
tag_to_value($H, _, {H,_,_}, _Options) ->
   integer_to_list_zerofill(H);

% Minutes; i.e. '00' to '59'
tag_to_value($i, _, {_,M,_}, _Options) ->
   integer_to_list_zerofill(M);

% Time, in 12-hour hours, minutes and 'a.m.'/'p.m.', with minutes left off
% if they're zero and the strings 'midnight' and 'noon' if appropriate.
% Examples: '1 a.m.', '1:30 p.m.', 'midnight', 'noon', '12:30 p.m.'
% Proprietary extension.
tag_to_value($P, _, {0,  0, _}, Options) -> tr(label, midnight, Options);
tag_to_value($P, _, {12, 0, _}, Options) -> tr(label, noon, Options);
tag_to_value($P, Date, Time, Options) ->
   tag_to_value($f, Date, Time, Options)
      ++ " " ++ tag_to_value($a, Date, Time, Options);

% Seconds; i.e. '00' to '59'
tag_to_value($s, _, {_,_,S}, _Options) ->
   integer_to_list_zerofill(S);

%-----------------------------------------------------------
% Date formatting
%-----------------------------------------------------------

% Month, textual, 3 letters, lowercase; e.g. 'jan'
tag_to_value($b, {_,M,_}, _, Options) ->
   z_string:to_lower(z_string:truncate(tr(monthname, M, Options), 3, <<>>));

% ISO 8601 date format - 2004-02-12T15:19:21+00:00
tag_to_value($c, Date, Time, Options) ->
    DiffMins = tzoffset({Date, Time}, Options),
    {Mins, Sign} = case DiffMins < 0 of
        true -> {0-DiffMins, $-};
        false -> {DiffMins, $+}
    end,    
    Hours   = Mins div 60,
    Minutes = Mins rem 60,
    replace_tags(Date, Time, "Y-m-d", Options) 
        ++ [$T | replace_tags(Date, Time, "H:i:s", Options)]
        ++ [Sign|integer_to_list_zerofill(Hours)]
        ++ [$:|integer_to_list_zerofill(Minutes)];

% Day of the month, 2 digits with leading zeros; i.e. '01' to '31'
tag_to_value($d, {_, _, D}, _, _Options) ->
   integer_to_list_zerofill(D);

% Day of the week, textual, 3 letters; e.g. 'Fri'
tag_to_value($D, Date, _, Options) ->
   Dow = calendar:day_of_the_week(Date),
   z_string:truncate(tr(dayname, Dow, Options), 3, []);

% Month, textual, long; e.g. 'January'
tag_to_value($F, {_,M,_}, _, Options) ->
   tr(monthname, M, Options);

% '1' if Daylight Savings Time, '0' otherwise.
tag_to_value($I, Date, Time, Options) ->
    try
        case localtime_dst:check({Date,Time}, proplists:get_value(tz, Options, "GMT")) of
            is_in_dst       -> "1";
            is_not_in_dst   -> "0";
            ambiguous_time  -> "0"  %"?"
        end
    catch
        throw:{error, wrong_week_day} ->
            "0"
    end;

% Day of the month without leading zeros; i.e. '1' to '31'
tag_to_value($j, {_, _, D}, _, _Options) ->
   integer_to_list(D);

% Day of the week, textual, long; e.g. 'Friday'
tag_to_value($l, Date, _, Options) ->
   tr(dayname, calendar:day_of_the_week(Date), Options);

% Boolean for whether it is a leap year; i.e. True or False
tag_to_value($L, {Y,_,_}, _, _Options) ->
   case calendar:is_leap_year(Y) of
   true -> "True";
   _ -> "False"
   end;

% Month; i.e. '01' to '12'
tag_to_value($m, {_, M, _}, _, _Options) ->
   integer_to_list_zerofill(M);

% Month, textual, 3 letters; e.g. 'Jan'
tag_to_value($M, {_,M,_}, _, Options) ->
    case tr(monthname_short, M, Options) of
        [] -> z_string:truncate(tr(monthname, M, Options), 3, []);
        S -> S
    end;

% Month without leading zeros; i.e. '1' to '12'
tag_to_value($n, {_, M, _}, _, _Options) ->
   integer_to_list(M);

% Month abbreviation in Associated Press style. Proprietary extension.
tag_to_value($N, {_,M,_}, _, Options) when M =:= 9 ->
   % Special case - "Sept."
   z_string:truncate(tr(monthname, M, Options), 4, ".");
tag_to_value($N, {_,M,_}, _, Options) when M < 3 orelse M > 7 ->
   % Jan, Feb, Aug, Oct, Nov, Dec are all
   % abbreviated with a full-stop appended.
   z_string:truncate(tr(monthname, M, Options), 3, ".");
tag_to_value($N, {_,M,_}, _, Options) ->
   % The rest are the fullname.
   tr(monthname, M, Options);

% Difference to Greenwich time in hours; e.g. '+0200'
tag_to_value($O, Date, Time, Options) ->
   Diff = utc_diff(Date, Time, Options),
   Offset = if
      Diff < 0 ->
          io_lib:format("-~4..0w", [abs(Diff)]);
      true ->
          io_lib:format("+~4..0w", [Diff])
   end,
   lists:flatten(Offset);

% RFC 2822 formatted date; e.g. 'Thu, 21 Dec 2000 16:01:07 +0200'
tag_to_value($r, Date, Time, Options) ->
   replace_tags(Date, Time, "D, j M Y H:i:s O", Options);

% English ordinal suffix for the day of the month, 2 characters;
% i.e. 'st', 'nd', 'rd' or 'th'
tag_to_value($S, {_, _, D}, _, _Options) when
   D rem 100 =:= 11 orelse
   D rem 100 =:= 12 orelse
   D rem 100 =:= 13 -> "th";
tag_to_value($S, {_, _, D}, _, _Options) when D rem 10 =:= 1 -> "st";
tag_to_value($S, {_, _, D}, _, _Options) when D rem 10 =:= 2 -> "nd";
tag_to_value($S, {_, _, D}, _, _Options) when D rem 10 =:= 3 -> "rd";
tag_to_value($S, _, _, _Options) -> "th";

% Number of days in the given month; i.e. '28' to '31'
tag_to_value($t, {Y,M,_}, _, _Options) ->
   integer_to_list(calendar:last_day_of_the_month(Y,M));

% Time zone of this machine; e.g. 'EST' or 'MDT'
tag_to_value($T, Date, Time, Options) ->
    case proplists:get_value(tz, Options) of
        undefined -> "";
        <<>> -> "";
        [] -> "";
        TZ -> tz_name({Date,Time}, prefer_standard, TZ)
    end;

% Seconds since the Unix epoch (January 1 1970 00:00:00 GMT)
tag_to_value($U, Date, Time, Options) ->
    UtcTime = to_utc({Date, Time}, Options), 
    EpochSecs = calendar:datetime_to_gregorian_seconds(UtcTime)
                - 62167219200, % calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    integer_to_list(EpochSecs);

% Day of the week, numeric, i.e. '0' (Sunday) to '6' (Saturday)
tag_to_value($w, Date, _, _Options) ->
   % Note: calendar:day_of_the_week returns
   %   1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7
   integer_to_list(calendar:day_of_the_week(Date) rem 7);

% ISO-8601 week number of year, weeks starting on Monday
tag_to_value($W, {Y,M,D}, _, _Options) ->
   integer_to_list(year_weeknum(Y,M,D));

% Year, 2 digits; e.g. '99'
tag_to_value($y, {Y, _, _}, _, _Options) ->
   Y1 = Y rem 100,
   [ Y1 div 10 + $0, Y1 rem 10 + $0];

% Year, 4 digits; e.g. '1999'
tag_to_value($Y, {Y, _, _}, _, _Options) ->
   integer_to_list(Y);

% Day of the year; i.e. '0' to '365'
tag_to_value($z, {Y,M,D}, _, _Options) ->
    integer_to_list(day_of_year(Y,M,D));

% Time zone offset in seconds (i.e. '-43200' to '43200'). The offset for
% timezones west of UTC is always negative, and for those east of UTC is
% always positive.
tag_to_value($Z, Date, Time, Options) ->
    DiffMins = tzoffset({Date,Time}, Options),
    integer_to_list(DiffMins*60);

tag_to_value(C, Date, Time, _Options) ->
    error_logger:warning_msg("z_dateformat: Unimplemented tag : ~p [Date : ~p] [Time : ~p]", [C, Date, Time]),
    "".

% Date helper functions
day_of_year(Y,M,D) ->
   day_of_year(Y,M,D,0).
day_of_year(_Y,M,D,Count) when M =< 1 ->
   D + Count;
day_of_year(Y,M,D,Count) when M =< 12 ->
   day_of_year(Y, M - 1, D, Count + calendar:last_day_of_the_month(Y,M));
day_of_year(Y,_M,D,_Count) ->
   day_of_year(Y, 12, D, 0).

hour_24to12(0) -> 12;
hour_24to12(H) when H < 13 -> H;
hour_24to12(H) when H < 24 -> H - 12;
hour_24to12(H) -> H.

year_weeknum(Y,M,D) -> 
    First = (calendar:day_of_the_week(Y, 1, 1) rem 7) - 1,
    Wk = ((((calendar:date_to_gregorian_days(Y, M, D) -
            calendar:date_to_gregorian_days(Y, 1, 1)) + First) div 7)
           + (case First < 4 of true -> 1; _ -> 0 end)),
    case Wk of
       0 -> weeks_in_year(Y - 1);
       _ -> case weeks_in_year(Y) of
              WksInThisYear when Wk > WksInThisYear -> 1;
              _ -> Wk
            end
    end.
   
weeks_in_year(Y) ->
    D1 = calendar:day_of_the_week(Y, 1, 1),
    D2 = calendar:day_of_the_week(Y, 12, 31),
    if (D1 =:= 4 orelse D2 =:= 4) -> 53; true -> 52 end.

utc_diff(Date, Time, Options) ->
    DiffSecs = tzoffset({Date, Time}, Options) * 60,
    trunc((DiffSecs / 3600) * 100).

to_utc(LTime, Options) ->
    case proplists:get_value(utc, Options) of
        undefined ->
            TzOffset = tzoffset(LTime, Options),
            calendar:gregorian_seconds_to_datetime(
                calendar:datetime_to_gregorian_seconds(LTime) + TzOffset * 60);
        UTC ->
            UTC
    end.

tzoffset(LTime, Options) ->
    case proplists:get_value(utc, Options) of
        undefined ->
            tzoffset_1(LTime, erlang:localtime_to_universaltime(LTime));
        UTime ->
            tzoffset_1(LTime, UTime)
    end.

tzoffset_1(LTime, LTime) ->
    0;
tzoffset_1(LTime, UTime) ->
    DiffSecs = calendar:datetime_to_gregorian_seconds(LTime) - 
       calendar:datetime_to_gregorian_seconds(UTime),
    DiffSecs div 60.

tz_name(Date, Disambiguate, ToTZ) ->
    case localtime:tz_name(Date, ToTZ) of
        {ShortName, _} when is_list(ShortName) ->
            ShortName;
        {{ShortStandard,_},{ShortDST,_}} -> 
            case Disambiguate of
                prefer_standard -> ShortStandard;
                prefer_daylight -> ShortDST;
                both            -> {ambiguous, ShortStandard, ShortDST}
            end
    end.

% Utility functions
integer_to_list_zerofill(N) when is_float(N) ->
    integer_to_list_zerofill(round(N));
integer_to_list_zerofill(N) when N < 100 ->
    [ N div 10 + $0, N rem 10 + $0];
integer_to_list_zerofill(N) ->
    integer_to_list(N).


tr(What, Label, Options) ->
    case proplists:get_value(tr, Options) of
        undefined -> tr(What, Label);
        F when is_function(F,1) -> F(What, Label);
        A when is_atom(A) -> erlang:apply(A, What, [Label]);
        {M,A} when is_atom(M), is_list(A) -> erlang:apply(M, What, [Label|A]);
        {M,F,A} -> erlang:apply(M, F, [What, Label|A])
    end.


%% @doc Provide some english date strings
tr(label, midnight) -> "midnight";
tr(label, noon) -> "noon";

%% @doc Provide english versions of the day of the week.
tr(dayname, 1) -> "Monday";
tr(dayname, 2) -> "Tuesday";
tr(dayname, 3) -> "Wednesday";
tr(dayname, 4) -> "Thursday";
tr(dayname, 5) -> "Friday";
tr(dayname, 6) -> "Saturday";
tr(dayname, 7) -> "Sunday";
tr(dayname, _) -> "???";

%% @doc Provide english versions of month names.
tr(monthname, 1) ->  "January";
tr(monthname, 2) ->  "February";
tr(monthname, 3) ->  "March";
tr(monthname, 4) ->  "April";
tr(monthname, 5) ->  "May";
tr(monthname, 6) ->  "June";
tr(monthname, 7) ->  "July";
tr(monthname, 8) ->  "August";
tr(monthname, 9) ->  "September";
tr(monthname, 10) -> "October";
tr(monthname, 11) -> "November";
tr(monthname, 12) -> "December";
tr(monthname, _) -> "???";

%% @doc Provide english short versions of month names.
tr(monthname_short, 1) ->  "Jan";
tr(monthname_short, 2) ->  "Feb";
tr(monthname_short, 3) ->  "Mar";
tr(monthname_short, 4) ->  "Apr";
tr(monthname_short, 5) ->  "May";
tr(monthname_short, 6) ->  "Jun";
tr(monthname_short, 7) ->  "Jul";
tr(monthname_short, 8) ->  "Aug";
tr(monthname_short, 9) ->  "Sep";
tr(monthname_short, 10) -> "Oct";
tr(monthname_short, 11) -> "Nov";
tr(monthname_short, 12) -> "Dec";
tr(monthname_short, _) -> "???".

