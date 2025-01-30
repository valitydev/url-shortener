-module(shortener_machine).

-export([create/4]).
-export([get/2]).
-export([remove/2]).

-type woody_context() :: woody_context:ctx().

% RFC 3339
-type timestamp() :: binary().

-type id() :: binary().
-type source() :: binary().
-type owner() :: binary().
-type expiration() :: timestamp().

-type slug() :: #{
    id => id(),
    source => source(),
    owner => owner() | undefined,
    expires_at => expiration()
}.

-export_type([slug/0]).

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_notification/4]).

-type timestamped_event(T) ::
    {ev, machinery:timestamp(), T}.

-type event() ::
    {created, slug()}.

-type args(T) :: machinery:args(T).
-type machine() :: machinery:machine(event(), _).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result() :: machinery:result(timestamped_event(event()), none()).
-type repair_response() :: ok.

-export_type([timestamped_event/1]).
-export_type([event/0]).

-define(NS, 'url-shortener').

%%

-spec create(source(), expiration(), owner(), woody_context()) -> {ok, slug()}.
create(Source, ExpiresAt, Owner, WoodyCtx) ->
    create(Source, ExpiresAt, Owner, 0, WoodyCtx).

create(Source, ExpiresAt, Owner, Attempt, WoodyCtx) ->
    ID = construct_id(Source, ExpiresAt, Attempt),
    Slug = #{source => Source, expires_at => ExpiresAt, owner => Owner},
    case machinery:start(?NS, ID, [{created, Slug}], get_backend(WoodyCtx)) of
        ok ->
            {ok, Slug#{id => ID}};
        {error, exists} ->
            create(Source, ExpiresAt, Owner, Attempt + 1, WoodyCtx)
    end.

-spec get(id(), woody_context()) -> {ok, slug()} | {error, notfound}.
get(ID, WoodyCtx) ->
    case machinery:get(?NS, ID, get_backend(WoodyCtx)) of
        {ok, History} ->
            Slug = collapse(History),
            {ok, Slug#{id => ID}};
        {error, notfound} ->
            {error, notfound}
    end.

-spec remove(id(), woody_context()) -> ok | {error, notfound}.
remove(ID, WoodyCtx) ->
    machinery:remove(?NS, ID, get_backend(WoodyCtx)).

%%

construct_id(Source, ExpiresAt, Attempt) ->
    SpaceSize = get_space_size(),
    Message = <<Source/binary, ExpiresAt/binary, Attempt:SpaceSize/integer-unit:8>>,
    <<Hash:SpaceSize/integer-unit:8, _/binary>> = crypto:hash(get_hash_algorithm(), Message),
    format_id(Hash).

format_id(ID) ->
    genlib_format:format_int_base(ID, 62).

get_hash_algorithm() ->
    {ok, V} = application:get_env(shortener, hash_algorithm),
    V.

get_space_size() ->
    {ok, V} = application:get_env(shortener, space_size),
    V.

%%% Machinery callbacks

-spec init(args([event()]), machine(), handler_args(), handler_opts()) -> result().
init([{created, #{expires_at := ExpiresAt}}] = Events, _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        events => emit_events(Events),
        action => [{set_timer, {deadline, parse_timestamp(ExpiresAt)}}]
    }.

-spec process_call(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_call(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(call).

-spec process_timeout(machine(), handler_args(), handler_opts()) -> result().
process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        action => [remove]
    }.

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) -> {ok, {repair_response(), result()}}.
process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    {ok, {ok, #{}}}.

-spec process_notification(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_notification(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(notification).

%%% Internal functions

emit_events(Events) ->
    emit_timestamped_events(Events, shortener_time:machinery_now()).

emit_timestamped_events(Events, Ts) ->
    [{ev, Ts, Body} || Body <- Events].

collapse(#{history := History}) ->
    lists:foldl(fun(Ev, St) -> apply_event(Ev, St) end, undefined, History).

-spec get_backend(woody_context()) -> machinery_mg_backend:backend().
get_backend(WoodyCtx) ->
    NS = ?NS,
    Backend = maps:get(NS, genlib_app:env(shortener, backends, #{})),
    {Mod, Opts} = machinery_utils:get_backend(Backend),
    {Mod, Opts#{
        woody_ctx => WoodyCtx
    }}.

-spec not_implemented(any()) -> no_return().
not_implemented(What) ->
    erlang:error({not_implemented, What}).

%%

-spec apply_event(machinery:event(timestamped_event(event())), slug() | undefined) -> slug().
apply_event({_ID, _Ts, {ev, _EvTs, Event}}, Slug) ->
    apply_event_(Event, Slug).

-spec apply_event_(event(), slug() | undefined) -> slug().
apply_event_({created, Slug}, undefined) ->
    Slug.

-spec parse_timestamp(binary()) -> machinery:timestamp().
parse_timestamp(Bin) ->
    try
        MicroSeconds = genlib_rfc3339:parse(Bin, microsecond),
        case genlib_rfc3339:is_utc(Bin) of
            false ->
                erlang:error({bad_timestamp, not_utc}, [Bin]);
            true ->
                USec = MicroSeconds rem 1000000,
                DateTime = calendar:system_time_to_universal_time(MicroSeconds, microsecond),
                {DateTime, USec}
        end
    catch
        error:Error:St ->
            erlang:raise(error, {bad_timestamp, Bin, Error}, St)
    end.
