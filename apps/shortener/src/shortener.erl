-module(shortener).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-type backend_mode() :: machinegun | progressor.

-define(PROCESSOR_OPT_PATTERN(NS, Handler, Schema), #{
    processor := #{
        client := machinery_prg_backend,
        options := #{
            namespace := NS,
            handler := Handler,
            schema := Schema
        }
    }
}).

%%

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    ?MODULE:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% API

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    HealthRoutes = get_health_routes(genlib_app:env(?MODULE, health_check, #{})),
    PrometeusRout = get_prometheus_route(),
    {Backends, MachineHandlers, ModernizerHandlers} = lists:unzip3([
        contruct_backend_childspec(B, N, H, S)
     || {B, N, H, S} <- get_namespaces_params(genlib_app:env(shortener, machinery_backend, machinegun))
    ]),
    ok = application:set_env(?MODULE, backends, maps:from_list(Backends)),
    RouteOptsEnv = genlib_app:env(?MODULE, route_opts, #{}),
    EventHandlerOpts = genlib_app:env(?MODULE, scoper_event_handler_options, #{}),
    RouteOpts = RouteOptsEnv#{event_handler => {scoper_woody_event_handler, EventHandlerOpts}},

    %% FIXME Healthcheck handles are provided to both 8022 and 8080 endpoints!
    AdditionalRoutes =
        machinery_mg_backend:get_routes(MachineHandlers, RouteOpts) ++
            machinery_modernizer_mg_backend:get_routes(ModernizerHandlers, RouteOpts) ++
            [PrometeusRout | HealthRoutes],
    {ok, {
        {one_for_all, 0, 1},
        % TODO
        get_processor_childspecs(genlib_app:env(?MODULE, processor), AdditionalRoutes) ++
            get_api_childspecs(genlib_app:env(?MODULE, api), HealthRoutes)
    }}.

get_health_routes(Check) ->
    [erl_health_handle:get_route(enable_health_logging(Check))].

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.

enable_health_logging(Check = #{}) ->
    maps:map(
        fun(_, V = {_, _, _}) ->
            #{runner => V, event_handler => {erl_health_event_handler, []}}
        end,
        Check
    ).

get_processor_childspecs(Opts, AdditionalRoutes) ->
    {ok, IP} = inet:parse_address(maps:get(ip, Opts, "::")),
    [
        woody_server:child_spec(
            ?MODULE,
            #{
                ip => IP,
                port => maps:get(port, Opts, 8022),
                protocol_opts => maps:get(protocol_opts, Opts, #{}),
                transport_opts => maps:get(transport_opts, Opts, #{}),
                event_handler => scoper_woody_event_handler,
                handlers => [],
                additional_routes => AdditionalRoutes
            }
        )
    ].

get_api_childspecs(Opts, HealthRoutes) ->
    HealthRouter = [{'_', HealthRoutes}],
    SwaggerServerSpec = shortener_swagger_server:child_spec(shortener_handler, Opts, HealthRouter),
    [SwaggerServerSpec].

-spec get_namespaces_params(backend_mode()) ->
    [{backend_mode(), machinery:namespace(), Handler :: module(), Schema :: module()}].
get_namespaces_params(machinegun = BackendMode) ->
    [
        {BackendMode, 'url-shortener', shortener_machine, shortener_machinery_schema}
    ];
get_namespaces_params(progressor = BackendMode) ->
    {ok, Namespaces} = application:get_env(progressor, namespaces),
    lists:map(
        fun({_, ?PROCESSOR_OPT_PATTERN(NS, Handler, Schema)}) ->
            {BackendMode, NS, Handler, Schema}
        end,
        maps:to_list(Namespaces)
    ).

contruct_backend_childspec(BackendMode, NS, Handler, Schema) ->
    {
        construct_machinery_backend_spec(BackendMode, NS, Handler, Schema),
        construct_machinery_handler_spec(NS, Handler, Schema),
        construct_machinery_modernizer_spec(NS, Schema)
    }.

construct_machinery_backend_spec(machinegun, NS, _Handler, Schema) ->
    {NS,
        {machinery_mg_backend, #{
            schema => Schema,
            client => get_service_client(automaton)
        }}};
construct_machinery_backend_spec(progressor, NS, Handler, Schema) ->
    {NS,
        {machinery_prg_backend, #{
            namespace => NS,
            handler => {shortener_machine, #{handler => Handler}},
            schema => Schema
        }}}.

construct_machinery_handler_spec(_NS, Handler, Schema) ->
    {Handler, #{
        path => "/v1/stateproc",
        backend_config => #{schema => Schema}
    }}.

construct_machinery_modernizer_spec(_NS, Schema) ->
    #{
        path => "/v1/modernizer",
        backend_config => #{schema => Schema}
    }.

get_service_client(ServiceName) ->
    case maps:get(url, get_service_client_config(ServiceName), undefined) of
        undefined ->
            error({unknown_service, ServiceName});
        Url ->
            genlib_map:compact(#{
                url => Url,
                event_handler => genlib_app:env(shortener, woody_event_handlers, [{scoper_woody_event_handler, #{}}])
            })
    end.

get_service_client_config(ServiceName) ->
    ServiceClients = genlib_app:env(shortener, service_clients, #{}),
    maps:get(ServiceName, ServiceClients, #{}).
