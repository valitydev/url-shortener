-module(shortener_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Constants

-define(CURRENT_EVENT_FORMAT_VERSION, undefined).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().
-type context() :: machinery_mg_schema:context().

-type event() :: shortener_machine:timestamped_event(shortener_machine:event()).
-type aux_state() :: term().
-type call_args() :: term().
-type call_response() :: term().

-type data() ::
    aux_state()
    | event()
    | call_args()
    | call_response().

%% machinery_mg_schema callbacks

-spec get_version(value_type()) -> machinery_mg_schema:version().
get_version(event) ->
    ?CURRENT_EVENT_FORMAT_VERSION;
get_version(aux_state) ->
    undefined.

-spec marshal(type(), value(data()), context()) -> {machinery_msgpack:t(), context()}.
marshal({event, FormatVersion}, TimestampedChange, Context) ->
    marshal_event(FormatVersion, TimestampedChange, Context);
marshal(T, V, C) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {aux_state, undefined} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:marshal(T, V, C).

-spec unmarshal(type(), machinery_msgpack:t(), context()) -> {data(), context()}.
unmarshal({event, FormatVersion}, EncodedChange, Context) ->
    unmarshal_event(FormatVersion, EncodedChange, Context);
unmarshal(T, V, C) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {aux_state, undefined} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:unmarshal(T, V, C).

%% Internals

-spec marshal_event(machinery_mg_schema:version(), event(), context()) -> {machinery_msgpack:t(), context()}.
marshal_event(
    undefined,
    {ev, _EvTs,
        {created, #{
            source := Source,
            expires_at := ExpiresAt,
            owner := Owner
        }}},
    Context
) ->
    %% FIXME Shouldn't event marshalling preserve occurrence timestamp?
    {{arr, [{i, 2}, {str, Source}, {str, ExpiresAt}, {str, Owner}]}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(undefined, {arr, [{i, 2}, {str, Source}, {str, ExpiresAt}, {str, Owner}]}, Context) ->
    Ts = shortener_time:machinery_now(),
    Change = {ev, Ts, {created, #{source => Source, expires_at => ExpiresAt, owner => Owner}}},
    {Change, Context}.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec marshal_unmarshal_created_test() -> _.

marshal_unmarshal_created_test() ->
    ID = <<"id">>,
    Created =
        {created, #{
            owner => <<"owner">>,
            source => <<"source">>,
            expires_at => <<"2000-01-01T00:00:00Z">>
        }},
    Context = #{machine_id => ID, machine_ns => config},
    Event = {ev, shortener_time:machinery_now(), Created},
    {Marshaled, _} = marshal_event(undefined, Event, Context),
    {Unmarshaled, _} = unmarshal_event(undefined, Marshaled, Context),
    ?assertMatch({ev, _, Created}, Unmarshaled).

-endif.
