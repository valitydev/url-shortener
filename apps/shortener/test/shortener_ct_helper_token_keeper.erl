-module(shortener_ct_helper_token_keeper).

-include_lib("token_keeper_proto/include/tk_token_keeper_thrift.hrl").
-include_lib("token_keeper_proto/include/tk_context_thrift.hrl").
-include_lib("shortener_token_keeper_data.hrl").

-define(TOKEN_ID, <<"LETMEIN">>).
-define(TEST_USER_REALM, <<"external">>).
-define(TOKEN_LIFETIME, 259200).

-type sup_or_config() :: shortener_ct_helper:sup_or_config().
-type app_name() :: shortener_ct_helper:app_name().
-type token_handler() :: fun(('Authenticate' | 'Create', tuple()) -> term() | no_return()).

-export([mock_token/2]).
-export([mock_dumb_token/1]).
-export([mock_dumb_token/2]).

-spec mock_token(token_handler(), sup_or_config()) -> list(app_name()).
mock_token(HandlerFun, SupOrConfig) ->
    start_client(
        shortener_ct_helper:mock_services_(
            [
                {
                    token_authenticator,
                    {tk_token_keeper_thrift, 'TokenAuthenticator'},
                    HandlerFun
                }
            ],
            SupOrConfig
        )
    ).

start_client(ServiceURLs) ->
    shortener_ct_helper:start_app(token_keeper_client, [
        {service_clients, #{
            authenticator => #{
                url => maps:get(token_authenticator, ServiceURLs)
            },
            authorities => #{
                ephemeral => #{},
                offline => #{}
            }
        }}
    ]).

%%

-spec mock_dumb_token(sup_or_config()) -> list(app_name()).
mock_dumb_token(SupOrConfig) ->
    mock_dumb_token(fun(_) -> {<<"UserID">>, <<"UserEmail">>} end, SupOrConfig).

-spec mock_dumb_token(function(), sup_or_config()) -> list(app_name()).
mock_dumb_token(UserInfoFun, SupOrConfig) ->
    Handler = make_authenticator_handler(fun(Token) ->
        {UserID, UserEmail} = UserInfoFun(Token),
        UserParams = #{
            id => UserID,
            realm => #{id => ?TEST_USER_REALM},
            email => UserEmail
        },
        AuthParams = #{
            method => <<"DumbToken">>,
            expiration => posix_to_rfc3339(lifetime_to_expiration(?TOKEN_LIFETIME)),
            token => #{id => ?TOKEN_ID}
        },
        {create_bouncer_context(AuthParams, UserParams), make_metadata(UserID, UserEmail)}
    end),
    mock_token(Handler, SupOrConfig).

%%

-spec make_authenticator_handler(function()) -> token_handler().
make_authenticator_handler(Handler) ->
    fun('Authenticate', {Token, _}) ->
        {ContextFragment, Metadata} = Handler(Token),
        AuthData = #token_keeper_AuthData{
            token = Token,
            status = active,
            context = ContextFragment,
            metadata = Metadata
        },
        {ok, AuthData}
    end.

%%

make_metadata(UserID, UserEmail) ->
    genlib_map:compact(#{
        ?TK_META_USER_ID => UserID,
        ?TK_META_USER_EMAIL => UserEmail
    }).

create_bouncer_context(AuthParams, UserParams) ->
    Fragment0 = bouncer_context_helpers:make_auth_fragment(AuthParams),
    Fragment1 = bouncer_context_helpers:add_user(UserParams, Fragment0),
    encode_context(Fragment1).

%%

encode_context(Context) ->
    #bctx_ContextFragment{
        type = v1_thrift_binary,
        content = encode_context_content(Context)
    }.

encode_context_content(Context) ->
    Type = {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}},
    Codec = thrift_strict_binary_codec:new(),
    case thrift_strict_binary_codec:write(Codec, Type, Context) of
        {ok, Codec1} ->
            thrift_strict_binary_codec:close(Codec1)
    end.

%%

lifetime_to_expiration(Lt) when is_integer(Lt) ->
    genlib_time:unow() + Lt.

posix_to_rfc3339(Timestamp) when is_integer(Timestamp) ->
    genlib_rfc3339:format(Timestamp, second).
