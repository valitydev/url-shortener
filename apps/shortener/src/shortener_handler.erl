-module(shortener_handler).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%% Swagger handler

-behaviour(swag_server_ushort_logic_handler).

-export([authorize_api_key/4]).
-export([handle_request/4]).
-export([map_error/2]).

%% Cowboy http handler

-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

%%

%% TODO refactor in case of different classes of users using this API
-define(REALM, <<"external">>).

-type operation_id() :: swag_server_ushort:operation_id().
-type request_ctx() :: swag_server_ushort:request_context().
-type request_data() :: #{atom() | binary() => term()}.
-type subject_id() :: woody_user_identity:id().
-type validation_error() :: swag_server_ushort_validation:error().
-type error_type() :: validation_error.
-type error_message() :: swag_server_ushort:error_reason().

-spec authorize_api_key(
    operation_id(),
    swag_server_ushort:api_key(),
    swag_server_ushort:request_context(),
    swag_server_ushort:handler_opts(_)
) ->
    Result :: false | {true, shortener_auth:preauth_context()}.
authorize_api_key(OperationID, ApiKey, Context, _HandlerOpts) ->
    ok = set_otel_context(Context),
    case shortener_auth:preauthorize_api_key(ApiKey) of
        {ok, Context1} ->
            {true, Context1};
        {error, Error} ->
            _ = logger:info("API Key preauthorization failed for ~p due to ~p", [OperationID, Error]),
            false
    end.

-spec handle_request(operation_id(), request_data(), request_ctx(), any()) ->
    {ok | error, swag_server_ushort:response()}.
handle_request(OperationID, Req, SwagContext, Opts) ->
    SpanName = <<"server ", (atom_to_binary(OperationID))/binary>>,
    ?with_span(SpanName, #{kind => ?SPAN_KIND_SERVER}, fun(_SpanCtx) ->
        scoper:scope('swag.server', #{operation => OperationID}, fun() ->
            handle_request_(OperationID, Req, SwagContext, Opts)
        end)
    end).

handle_request_(OperationID, Req, SwagContext0, _Opts) ->
    try
        WoodyContext0 = create_woody_ctx(Req),
        SwagContext1 = authenticate_api_key(SwagContext0, WoodyContext0),
        AuthContext = get_auth_context(SwagContext1),
        WoodyContext1 = put_user_identity(WoodyContext0, AuthContext),
        ok = set_context_meta(AuthContext),
        Slug = prefetch_slug(Req, WoodyContext1),
        case shortener_auth:authorize_operation(make_prototypes(OperationID, Slug), SwagContext1, WoodyContext1) of
            allowed ->
                SubjectID = shortener_auth:get_subject_id(AuthContext),
                process_request(OperationID, Req, Slug, SubjectID, WoodyContext1);
            forbidden ->
                {ok, {403, #{}, undefined}}
        end
    catch
        throw:{token_auth_failed, Reason} ->
            _ = logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Reason]),
            {error, {401, #{}, undefined}};
        error:{woody_error, {Source, Class, Details}} ->
            {error, handle_woody_error(Source, Class, Details)}
    end.

-spec map_error(error_type(), validation_error()) -> error_message().
map_error(validation_error, Error) ->
    Type = genlib:to_binary(maps:get(type, Error)),
    Name = genlib:to_binary(maps:get(param_name, Error)),
    Message =
        case maps:get(description, Error, undefined) of
            undefined ->
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary>>;
            Description ->
                DescriptionBin = genlib:to_binary(Description),
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary, ", description: ",
                    DescriptionBin/binary>>
        end,
    jsx:encode(#{
        <<"code">> => <<"invalidRequest">>,
        <<"message">> => Message
    }).

-spec prefetch_slug(request_data(), woody_context:ctx()) -> shortener_machine:slug() | no_slug.
prefetch_slug(#{'shortenedUrlID' := ID}, WoodyCtx) ->
    case shortener_machine:get(ID, WoodyCtx) of
        {ok, Slug} ->
            Slug;
        {error, notfound} ->
            no_slug
    end;
prefetch_slug(_Req, _WoodyCtx) ->
    no_slug.

make_prototypes(OperationID, no_slug) ->
    [{operation, #{id => OperationID}}];
make_prototypes(OperationID, Slug) ->
    [
        {operation, #{
            id => OperationID,
            slug => Slug
        }}
    ].

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

create_woody_ctx(#{'X-Request-ID' := RequestID}) ->
    RpcID = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    woody_context:new(RpcID).

put_user_identity(WoodyContext, AuthContext) ->
    woody_user_identity:put(collect_user_identity(AuthContext), WoodyContext).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id => shortener_auth:get_subject_id(AuthContext),
        realm => ?REALM,
        email => shortener_auth:get_user_email(AuthContext)
    }).

set_context_meta(AuthContext) ->
    Meta = #{
        metadata => #{
            'user-identity' => collect_user_identity(AuthContext)
        }
    },
    scoper:add_meta(Meta).

handle_woody_error(_Source, result_unexpected, _Details) ->
    {500, #{}, <<>>};
handle_woody_error(_Source, resource_unavailable, _Details) ->
    {503, #{}, <<>>};
handle_woody_error(_Source, result_unknown, _Details) ->
    {504, #{}, <<>>}.

authenticate_api_key(SwagContext = #{auth_context := PreAuthContext}, WoodyContext) ->
    case shortener_auth:authenticate_api_key(PreAuthContext, make_token_context(SwagContext), WoodyContext) of
        {ok, AuthContext} ->
            SwagContext#{auth_context => AuthContext};
        {error, Error} ->
            throw({token_auth_failed, Error})
    end.

make_token_context(#{cowboy_req := CowboyReq}) ->
    case cowboy_req:header(<<"origin">>, CowboyReq) of
        Origin when is_binary(Origin) ->
            #{request_origin => Origin};
        undefined ->
            #{}
    end.

%%

-spec process_request(operation_id(), request_data(), shortener_machine:slug(), subject_id(), woody_context:ctx()) ->
    {ok | error, swag_server_ushort:response()}.
process_request(
    'ShortenUrl',
    #{
        'ShortenedUrlParams' := #{
            <<"sourceUrl">> := SourceUrl,
            <<"expiresAt">> := ExpiresAt
        }
    },
    no_slug,
    SubjectID,
    WoodyCtx
) ->
    case validate_source_url(SourceUrl) of
        true ->
            {ok, Slug} = shortener_machine:create(SourceUrl, parse_timestamp(ExpiresAt), SubjectID, WoodyCtx),
            {ok, {201, #{}, construct_shortened_url(Slug)}};
        false ->
            {ok,
                {400, #{}, #{
                    <<"code">> => <<"forbidden_source_url">>,
                    <<"message">> => <<"Source URL is forbidden">>
                }}}
    end;
process_request(
    'GetShortenedUrl',
    _Req,
    no_slug,
    _SubjectID,
    _WoodyCtx
) ->
    {error, {404, #{}, undefined}};
process_request(
    'GetShortenedUrl',
    _Req,
    Slug,
    _SubjectID,
    _WoodyCtx
) ->
    {ok, {200, #{}, construct_shortened_url(Slug)}};
process_request(
    'DeleteShortenedUrl',
    #{'shortenedUrlID' := ID},
    _Slug,
    _SubjectID,
    WoodyCtx
) ->
    case shortener_machine:remove(ID, WoodyCtx) of
        ok ->
            {ok, {204, #{}, undefined}};
        {error, notfound} ->
            {error, {404, #{}, undefined}}
    end.

validate_source_url(SourceUrl) ->
    lists:any(
        fun(Pattern) -> is_source_url_valid(SourceUrl, Pattern) end,
        get_source_url_whitelist()
    ).

is_source_url_valid(SourceUrl, Pattern) ->
    genlib_wildcard:match(SourceUrl, genlib:to_binary(Pattern)).

construct_shortened_url(
    #{
        id := ID,
        source := Source,
        expires_at := ExpiresAt
    }
) ->
    #{
        <<"id">> => ID,
        <<"shortenedUrl">> => render_short_url(ID, get_short_url_template()),
        <<"sourceUrl">> => Source,
        <<"expiresAt">> => ExpiresAt
    }.

render_short_url(ID, Template) ->
    iolist_to_binary([
        genlib:to_binary(maps:get(scheme, Template)),
        <<"://">>,
        genlib:to_binary(maps:get(netloc, Template)),
        genlib:to_binary(maps:get(path, Template)),
        ID
    ]).

parse_timestamp(Timestamp) ->
    Microseconds = genlib_rfc3339:parse(Timestamp, microsecond),
    genlib_rfc3339:format_relaxed(Microseconds, microsecond).

get_short_url_template() ->
    % TODO
    % Teach the swagger-codegen bastard to behave and accept handler options
    % upon initialization
    maps:get(short_url_template, genlib_app:env(shortener, api)).

get_source_url_whitelist() ->
    % TODO
    % Teach the swagger-codegen bastard to behave and accept handler options
    % upon initialization
    maps:get(source_url_whitelist, genlib_app:env(shortener, api), []).

set_otel_context(#{cowboy_req := Req}) ->
    Headers = cowboy_req:headers(Req),
    %% Implicitly puts OTEL context into process dictionary.
    %% Since cowboy does not reuse process for other requests, we don't care
    %% about cleaning it up.
    _OtelCtx = otel_propagator_text_map:extract(maps:to_list(Headers)),
    ok.

%%

-type state() :: undefined.
-type request() :: cowboy_req:req().
-type terminate_reason() :: {normal, shutdown} | {error, atom()}.

-spec init(request(), _) -> {ok, request(), state()}.
init(Req, Opts) ->
    ID = cowboy_req:binding('shortenedUrlID', Req),
    Req1 =
        case shortener_machine:get(ID, woody_context:new()) of
            {ok, #{source := Source, expires_at := ExpiresAt}} ->
                Seconds = genlib_rfc3339:parse(ExpiresAt, second),
                {Date, Time} = calendar:system_time_to_universal_time(Seconds, second),
                Headers = #{
                    <<"location">> => Source,
                    <<"expires">> => cowboy_clock:rfc1123({Date, Time}),
                    <<"cache-control">> => <<"must-revalidate">>
                },
                cowboy_req:reply(301, Headers, Req);
            {error, notfound} ->
                cowboy_req:reply(404, Req)
        end,
    {ok, Req1, Opts}.

-spec terminate(terminate_reason(), request(), state()) -> ok.
terminate(_Reason, _Req, _St) ->
    ok.
