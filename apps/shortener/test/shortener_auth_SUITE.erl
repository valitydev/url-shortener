-module(shortener_auth_SUITE).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_base_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-include_lib("shortener_token_keeper_data.hrl").
-include_lib("shortener_bouncer_data.hrl").

-export([init/1]).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([failed_authorization/1]).
-export([insufficient_permissions/1]).
-export([readonly_permissions/1]).
-export([other_subject_delete/1]).
-export([other_subject_read/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-define(AUTH_TOKEN, <<"LETMEIN">>).
-define(USER_EMAIL, <<"bla@bla.ru">>).

-spec all() -> [{atom(), test_case_name()} | test_case_name()].
all() ->
    [
        {group, general}
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].
groups() ->
    [
        {general, [], [
            failed_authorization,
            insufficient_permissions,
            readonly_permissions,
            other_subject_delete,
            other_subject_read
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
        ]),
    [
        {suite_apps, Apps},
        {api_endpoint, "http://" ++ Netloc},
        {host, Host},
        {port, Port},
        {netloc, Netloc}
    ] ++ C.

-spec init_per_group(atom(), config()) -> config().
init_per_group(_Group, C) ->
    ShortenerApp =
        genlib_app:start_application_with(
            shortener,
            shortener_ct_helper:get_app_config(
                ?config(port, C),
                ?config(netloc, C)
            )
        ),
    [
        {shortener_app, ShortenerApp}
    ] ++ C.

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

-spec failed_authorization(config()) -> _.
-spec insufficient_permissions(config()) -> _.
-spec readonly_permissions(config()) -> _.
-spec other_subject_delete(config()) -> _.
-spec other_subject_read(config()) -> _.

failed_authorization(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    C1 = clean_api_auth_token(C),
    {ok, 401, _, _} = shorten_url(Params, C1),
    {ok, 401, _, _} = delete_shortened_url(<<"42">>, C1),
    {ok, 401, _, _} = get_shortened_url(<<"42">>, C1).

insufficient_permissions(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        shortener_ct_helper_bouncer:judge_always_forbidden(),
        ?config(test_sup, C)
    ),
    C1 = set_api_auth_token(C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 403, _, _} = shorten_url(Params, C1),
    {ok, 403, _, _} = delete_shortened_url(<<"42">>, C1),
    {ok, 403, _, _} = get_shortened_url(<<"42">>, C1).

readonly_permissions(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(?config(test_sup, C)),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        fun(ContextFragment) ->
            case get_operation_id(ContextFragment) of
                <<"ShortenUrl">> -> {ok, ?JUDGEMENT(?ALLOWED)};
                <<"GetShortenedUrl">> -> {ok, ?JUDGEMENT(?ALLOWED)};
                <<"DeleteShortenedUrl">> -> {ok, ?JUDGEMENT(?FORBIDDEN)}
            end
        end,
        ?config(test_sup, C)
    ),
    C1 = set_api_auth_token(C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"id">> := ID}} = get_shortened_url(ID, C1),
    {ok, 403, _, _} = delete_shortened_url(ID, C1).

other_subject_delete(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(
        fun
            (<<"other_subject_delete_first">>) ->
                {<<"USER1">>, ?USER_EMAIL};
            (<<"other_subject_delete_second">>) ->
                {<<"USER2">>, ?USER_EMAIL}
        end,
        ?config(test_sup, C)
    ),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        fun(ContextFragment) ->
            case get_operation_id(ContextFragment) of
                <<"ShortenUrl">> ->
                    {ok, ?JUDGEMENT(?ALLOWED)};
                <<"GetShortenedUrl">> ->
                    case get_owner_info(ContextFragment) of
                        {ID, ID} -> {ok, ?JUDGEMENT(?ALLOWED)};
                        _ -> {ok, ?JUDGEMENT(?FORBIDDEN)}
                    end;
                <<"DeleteShortenedUrl">> ->
                    case get_owner_info(ContextFragment) of
                        {ID, ID} -> {ok, ?JUDGEMENT(?ALLOWED)};
                        _ -> {ok, ?JUDGEMENT(?FORBIDDEN)}
                    end
            end
        end,
        ?config(test_sup, C)
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(<<"other_subject_delete_first">>, C),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(<<"other_subject_delete_second">>, C1),
    {ok, 403, _, _} = delete_shortened_url(ID, C2),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

other_subject_read(C) ->
    _ = shortener_ct_helper_token_keeper:mock_dumb_token(
        fun
            (<<"other_subject_read_first">>) ->
                {<<"USER1">>, ?USER_EMAIL};
            (<<"other_subject_read_second">>) ->
                {<<"USER2">>, ?USER_EMAIL}
        end,
        ?config(test_sup, C)
    ),
    _ = shortener_ct_helper_bouncer:mock_arbiter(
        fun(ContextFragment) ->
            case get_operation_id(ContextFragment) of
                <<"ShortenUrl">> ->
                    {ok, ?JUDGEMENT(?ALLOWED)};
                <<"GetShortenedUrl">> ->
                    case get_owner_info(ContextFragment) of
                        {ID, ID} -> {ok, ?JUDGEMENT(?ALLOWED)};
                        _ -> {ok, ?JUDGEMENT(?FORBIDDEN)}
                    end;
                <<"DeleteShortenedUrl">> ->
                    case get_owner_info(ContextFragment) of
                        {ID, ID} -> {ok, ?JUDGEMENT(?ALLOWED)};
                        _ -> {ok, ?JUDGEMENT(?FORBIDDEN)}
                    end
            end
        end,
        ?config(test_sup, C)
    ),
    Params = construct_params(<<"https://oops.io/">>),
    C1 = set_api_auth_token(<<"other_subject_read_first">>, C),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(<<"other_subject_read_second">>, C1),
    {ok, 403, _, _} = get_shortened_url(ID, C2).

%%

construct_params(SourceUrl) ->
    construct_params(SourceUrl, 3600).

construct_params(SourceUrl, Lifetime) ->
    #{
        <<"sourceUrl">> => SourceUrl,
        <<"expiresAt">> => format_ts(genlib_time:unow() + Lifetime)
    }.

set_api_auth_token(C) ->
    set_api_auth_token(?AUTH_TOKEN, C).

set_api_auth_token(Token, C) ->
    lists:keystore(api_auth_token, 1, C, {api_auth_token, Token}).

clean_api_auth_token(C) ->
    lists:keydelete(api_auth_token, 1, C).

%%

shorten_url(ShortenedUrlParams, C) ->
    swag_client_ushort_shortener_api:shorten_url(
        ?config(api_endpoint, C),
        append_common_params(#{body => ShortenedUrlParams}, C)
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

get_operation_id(#bctx_v1_ContextFragment{
    shortener = #bctx_v1_ContextUrlShortener{op = #bctx_v1_UrlShortenerOperation{id = OperationID}}
}) ->
    OperationID.

get_owner_info(Context) ->
    {get_owner_id(Context), get_user_id(Context)}.

get_owner_id(#bctx_v1_ContextFragment{
    shortener = #bctx_v1_ContextUrlShortener{op = #bctx_v1_UrlShortenerOperation{shortened_url = Url}}
}) ->
    #bctx_v1_ShortenedUrl{owner = #bouncer_base_Entity{id = OwnerID}} = Url,
    OwnerID.

get_user_id(#bctx_v1_ContextFragment{user = #bctx_v1_User{id = UserID}}) ->
    UserID.
