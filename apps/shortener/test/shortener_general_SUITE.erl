-module(shortener_general_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([init/1]).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([successful_redirect/1]).
-export([successful_delete/1]).
-export([fordidden_source_url/1]).
-export([url_expired/1]).
-export([always_unique_url/1]).

-export([health_check_passing/1]).

-export([woody_timeout_test/1]).

-export([unsupported_cors_method/1]).
-export([supported_cors_method/1]).
-export([unsupported_cors_header/1]).
-export([supported_cors_header/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().
-type group_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).
-define(AUTH_TOKEN, <<"LETMEIN">>).

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, general},
        {group, cors},
        {group, misc}
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].
groups() ->
    [
        {general, [], [
            successful_redirect,
            successful_delete,
            fordidden_source_url,
            url_expired,
            always_unique_url
        ]},
        {cors, [], [
            unsupported_cors_method,
            supported_cors_method,
            unsupported_cors_header,
            supported_cors_header
        ]},
        {misc, [], [
            woody_timeout_test,
            health_check_passing
        ]}
    ].

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({shortener_swagger_server, '_', '_'}, x),
    Host = "url-shortener",
    Port = 8080,
    Netloc = Host ++ ":" ++ genlib:to_list(Port),
    Apps =
        genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_logger}
        ]) ++ start_progressor_backend(),
    [
        {suite_apps, Apps},
        {api_endpoint, "http://" ++ Netloc},
        {host, Host},
        {port, Port},
        {netloc, Netloc}
    ] ++ C.

-spec init_per_group(atom(), config()) -> config().
init_per_group(misc, C) ->
    ShortenerApp =
        genlib_app:start_application_with(
            shortener,
            shortener_ct_helper:get_app_config(
                ?config(port, C),
                ?config(netloc, C),
                <<"http://invalid_url:8022/v1/automaton">>,
                progressor
            )
        ),
    [{shortener_app, ShortenerApp}] ++ C;
init_per_group(_Group, C) ->
    ShortenerApp =
        genlib_app:start_application_with(
            shortener,
            shortener_ct_helper:get_app_config(
                ?config(port, C),
                ?config(netloc, C),
                <<"http://machinegun:8022/v1/automaton">>,
                progressor
            )
        ),
    [{shortener_app, ShortenerApp}] ++ C.

-spec end_per_group(atom(), config()) -> _.
end_per_group(_Group, C) ->
    genlib_app:stop_unload_applications(?config(shortener_app, C)).

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    _ = genlib_app:stop_unload_applications(?config(suite_apps, C)),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    SupPid = shortener_ct_helper:start_mocked_service_sup(?MODULE),
    _ = shortener_ct_helper_bouncer:mock_client(SupPid),
    [{test_sup, SupPid} | C].

-spec end_per_testcase(test_case_name(), config()) -> ok.
end_per_testcase(_Name, C) ->
    _ = shortener_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.
%%

-spec successful_redirect(config()) -> _.
-spec successful_delete(config()) -> _.
-spec fordidden_source_url(config()) -> _.
-spec url_expired(config()) -> _.
-spec always_unique_url(config()) -> _.

successful_redirect(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    C1 = set_api_auth_token(C),
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"sourceUrl">> := SourceUrl, <<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C1),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

successful_delete(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    C1 = set_api_auth_token(C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 204, _, _} = delete_shortened_url(ID, C1),
    {ok, 404, _, _} = get_shortened_url(ID, C1),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

fordidden_source_url(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    C1 = set_api_auth_token(C),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"http://localhost/hack?id=42">>), C1),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"https://localhost/hack?id=42">>), C1),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://example.io/">>), C1),
    {ok, 400, _, #{}} = shorten_url(construct_params(<<"http://local.domain/phpmyadmin">>), C1),
    {ok, 201, _, #{}} = shorten_url(construct_params(<<"ftp://ftp.hp.com/pub/hpcp/newsletter_july2003">>), C1).

url_expired(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    C1 = set_api_auth_token(C),
    Params = construct_params(<<"https://oops.io/">>, 1),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"shortenedUrl">> := ShortUrl}} = get_shortened_url(ID, C1),
    ok = timer:sleep(2 * 1000),
    {ok, 404, _, _} = get_shortened_url(ID, C1),
    {ok, 404, _, _} = hackney:request(get, ShortUrl).

always_unique_url(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    C1 = set_api_auth_token(C),
    N = 42,
    Params = construct_params(<<"https://oops.io/">>, 3600),
    {IDs, ShortUrls} = lists:unzip([
        {ID, ShortUrl}
     || _ <- lists:seq(1, N),
        {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} <- [shorten_url(Params, C1)]
    ]),
    N = length(lists:usort(IDs)),
    N = length(lists:usort(ShortUrls)).

%% cors
-spec unsupported_cors_method(config()) -> _.
-spec supported_cors_method(config()) -> _.
-spec unsupported_cors_header(config()) -> _.
-spec supported_cors_header(config()) -> _.

unsupported_cors_method(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"PATCH">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    false = lists:member(<<"access-control-allow-methods">>, Headers).

supported_cors_method(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [{<<"origin">>, <<"localhost">>}, {<<"access-control-request-method">>, <<"GET">>}],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    {Allowed, _} = shortener_cors_policy:allowed_methods(undefined, undefined),
    {_, Returned} = lists:keyfind(<<"access-control-allow-methods">>, 1, Headers),
    Allowed = binary:split(Returned, <<",">>, [global]).

supported_cors_header(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [
        {<<"origin">>, <<"localhost">>},
        {<<"access-control-request-method">>, <<"GET">>},
        {<<"access-control-request-headers">>, <<"content-type,authorization">>}
    ],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    {Allowed, _} = shortener_cors_policy:allowed_headers(undefined, undefined),
    {_, Returned} = lists:keyfind(<<"access-control-allow-headers">>, 1, Headers),
    % truncate origin
    [_ | Allowed] = binary:split(Returned, <<",">>, [global]).

unsupported_cors_header(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(C),
    {ok, 201, _, #{<<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    ReqHeaders = [
        {<<"origin">>, <<"localhost">>},
        {<<"access-control-request-method">>, <<"GET">>},
        {<<"access-control-request-headers">>, <<"content-type,42">>}
    ],
    {ok, 200, Headers, _} = hackney:request(options, ShortUrl, ReqHeaders),
    false = lists:member(<<"access-control-allow-headers">>, Headers),
    false = lists:member(<<"access-control-allow-credentials">>, Headers),
    false = lists:member(<<"access-control-allow-methods">>, Headers),
    false = lists:member(<<"access-control-allow-origin">>, Headers).

construct_params(SourceUrl) ->
    construct_params(SourceUrl, 3600).

construct_params(SourceUrl, Lifetime) ->
    #{
        <<"sourceUrl">> => SourceUrl,
        <<"expiresAt">> => format_ts(genlib_time:unow() + Lifetime)
    }.

%%
-spec woody_timeout_test(config()) -> _.
woody_timeout_test(C) ->
    _ = mock_invalid_progressor(),
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_allowed(),
        ?config(test_sup, C)
    ),
    C2 = set_api_auth_token(C),
    SourceUrl = <<"https://example.com/">>,
    Params = construct_params(SourceUrl),
    % different Reason for machinegun and progressor
    % progressor: Reason = timeout (because internal library)
    % machinegun: Reason = {invalid_response_code, 503} (because external service)
    {Time, {error, _Reason}} =
        timer:tc(fun() ->
            shorten_url(Params, 3 * 1000, C2)
        end),
    _ = unmock_progressor(),
    ?assertMatch(V when V >= 3000, Time).

%%
-spec health_check_passing(config()) -> _.
health_check_passing(C) ->
    Path = ?config(api_endpoint, C) ++ "/health",
    {ok, 200, _, Payload} = hackney:request(get, Path, [], <<>>, [with_body]),
    #{<<"service">> := <<"shortener">>} = jsx:decode(Payload, [return_maps]).

%%

set_api_auth_token(C) ->
    set_api_auth_token(?AUTH_TOKEN, C).

set_api_auth_token(Token, C) ->
    lists:keystore(api_auth_token, 1, C, {api_auth_token, Token}).

%%

shorten_url(ShortenedUrlParams, C) ->
    shorten_url(ShortenedUrlParams, 5000, C).

shorten_url(ShortenedUrlParams, RecvTimeout, C) ->
    swag_client_ushort_shortener_api:shorten_url(
        ?config(api_endpoint, C),
        append_common_params(#{body => ShortenedUrlParams}, C),
        [{recv_timeout, RecvTimeout}]
    ).

delete_shortened_url(ID, C) ->
    swag_client_ushort_shortener_api:delete_shortened_url(
        ?config(api_endpoint, C),
        append_common_params(#{binding => #{<<"shortenedUrlID">> => ID}}, C)
    ).

get_shortened_url(ID, C) ->
    swag_client_ushort_shortener_api:get_shortened_url(
        ?config(api_endpoint, C),
        append_common_params(#{binding => #{<<"shortenedUrlID">> => ID}}, C)
    ).

append_common_params(Params, C) ->
    append_media_type(
        append_auth(
            append_request_id(
                maps:merge(#{binding => #{}, qs_val => #{}, header => #{}, body => #{}}, Params)
            ),
            C
        )
    ).

append_media_type(Params = #{header := Headers}) ->
    Params#{
        header => Headers#{
            <<"Accept">> => <<"application/json">>,
            <<"Content-Type">> => <<"application/json; charset=utf-8">>
        }
    }.

append_auth(Params = #{header := Headers}, C) ->
    case lists:keyfind(api_auth_token, 1, C) of
        {api_auth_token, AuthToken} ->
            Params#{header => Headers#{<<"Authorization">> => <<"Bearer ", AuthToken/binary>>}};
        _ ->
            Params
    end.

append_request_id(Params = #{header := Headers}) ->
    Params#{header => Headers#{<<"X-Request-ID">> => woody_context:new_req_id()}}.

format_ts(Ts) ->
    genlib_rfc3339:format(Ts, second).

start_progressor_backend() ->
    EpgApps = start_epg_connector(),
    PrgApps = start_progressor(),
    EpgApps ++ PrgApps.

start_epg_connector() ->
    Config = [
        {databases, #{
            default_db => #{
                host => "postgres",
                port => 5432,
                database => "progressor_db",
                username => "progressor",
                password => "progressor"
            }
        }},
        {pools, #{
            default_pool => #{
                database => default_db,
                size => 50
            }
        }}
    ],
    genlib_app:start_application_with(epg_connector, Config).

start_progressor() ->
    Config = [
        {defaults, #{
            storage => #{
                client => prg_pg_backend,
                options => #{
                    pool => default_pool
                }
            },
            retry_policy => #{
                initial_timeout => 5,
                backoff_coefficient => 1.0,
                max_timeout => 180,
                max_attempts => 3,
                non_retryable_errors => []
            },
            task_scan_timeout => 1,
            worker_pool_size => 1,
            process_step_timeout => 10
        }},
        {namespaces, #{
            'url-shortener' => #{
                processor => #{
                    client => machinery_prg_backend,
                    options => #{
                        namespace => 'url-shortener',
                        handler => shortener_machine,
                        schema => shortener_machinery_schema
                    }
                }
            }
        }}
    ],
    genlib_app:start_application_with(progressor, Config).

mock_invalid_progressor() ->
    meck:new(progressor, [passthrough]),
    meck:expect(
        progressor,
        init,
        fun(Args) ->
            timer:sleep(5000),
            meck:passthrough([Args])
        end
    ).

unmock_progressor() ->
    meck:unload(progressor).
