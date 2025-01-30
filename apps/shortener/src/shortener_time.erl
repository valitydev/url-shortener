-module(shortener_time).

-export([now/0]).
-export([to_rfc3339/1]).
-export([from_rfc3339/1]).
-export([machinery_now/0]).

-type timestamp_ms() :: integer().

-export_type([timestamp_ms/0]).

-spec now() -> timestamp_ms().
now() ->
    erlang:system_time(millisecond).

-spec to_rfc3339(timestamp_ms()) -> binary().
to_rfc3339(Timestamp) ->
    genlib_rfc3339:format_relaxed(Timestamp, millisecond).

-spec from_rfc3339(binary()) -> timestamp_ms().
from_rfc3339(BTimestamp) ->
    genlib_rfc3339:parse(BTimestamp, millisecond).

-spec machinery_now() -> machinery:timestamp().
machinery_now() ->
    Now = {_, _, USec} = os:timestamp(),
    {calendar:now_to_universal_time(Now), USec}.
