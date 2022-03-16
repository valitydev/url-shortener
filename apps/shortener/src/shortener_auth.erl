-module(shortener_auth).

-define(APP, shortener).

-export([get_subject_id/1]).
-export([get_party_id/1]).
-export([get_user_id/1]).
-export([get_user_email/1]).

-export([preauthorize_api_key/1]).
-export([authorize_api_key/3]).
-export([authorize_operation/3]).

-export_type([resolution/0]).
-export_type([preauth_context/0]).
-export_type([auth_context/0]).

%%

-type token_type() :: bearer.
-type auth_context() :: {authorized, token_keeper_client:auth_data()}.
-type preauth_context() :: {unauthorized, {token_type(), token_keeper_client:token()}}.

-type resolution() ::
    allowed
    | forbidden
    | {forbidden, _Reason}.

-define(AUTHORIZED(Ctx), {authorized, Ctx}).
-define(UNAUTHORIZED(Ctx), {unauthorized, Ctx}).

%%

-spec get_subject_id(auth_context()) -> binary() | undefined.
get_subject_id(AuthContext) ->
    case get_party_id(AuthContext) of
        PartyId when is_binary(PartyId) ->
            PartyId;
        undefined ->
            get_user_id(AuthContext)
    end.

-spec get_party_id(auth_context()) -> binary() | undefined.
get_party_id(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(party_id), Metadata).

-spec get_user_id(auth_context()) -> binary() | undefined.
get_user_id(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(user_id), Metadata).

-spec get_user_email(auth_context()) -> binary() | undefined.
get_user_email(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(user_email), Metadata).

%%

-spec preauthorize_api_key(swag_server_ushort:api_key()) -> {ok, preauth_context()} | {error, _Reason}.
preauthorize_api_key(ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, Token} ->
            {ok, ?UNAUTHORIZED(Token)};
        {error, Error} ->
            {error, Error}
    end.

-spec authorize_api_key(preauth_context(), token_keeper_client:token_context(), woody_context:ctx()) ->
    {ok, auth_context()} | {error, _Reason}.
authorize_api_key(?UNAUTHORIZED({TokenType, Token}), TokenContext, WoodyContext) ->
    authorize_token_by_type(TokenType, Token, TokenContext, WoodyContext).

authorize_token_by_type(bearer, Token, TokenContext, WoodyContext) ->
    Authenticator = token_keeper_client:authenticator(WoodyContext),
    case token_keeper_authenticator:authenticate(Token, TokenContext, Authenticator) of
        {ok, AuthData} ->
            {ok, ?AUTHORIZED(AuthData)};
        {error, TokenKeeperError} ->
            _ = logger:warning("Token keeper authorization failed: ~p", [TokenKeeperError]),
            {error, {auth_failed, TokenKeeperError}}
    end.

-spec authorize_operation(Prototypes, ReqContext, WoodyCtx) -> resolution() when
    Prototypes :: shortener_bouncer_context:prototypes(),
    ReqContext :: swag_server_ushort:request_context(),
    WoodyCtx :: woody_context:ctx().
authorize_operation(Prototypes, SwagContext, WoodyContext) ->
    AuthContext = get_auth_context(SwagContext),
    Fragments = shortener_bouncer:gather_context_fragments(
        get_token_keeper_fragment(AuthContext),
        get_user_id(AuthContext),
        SwagContext,
        WoodyContext
    ),
    Fragments1 = shortener_bouncer_context:build(Prototypes, Fragments),
    shortener_bouncer:judge(Fragments1, WoodyContext).

%%

get_token_keeper_fragment(?AUTHORIZED(#{context := Context})) ->
    Context.

%%

parse_api_key(<<"Bearer ", Token/binary>>) ->
    {ok, {bearer, Token}};
parse_api_key(_) ->
    {error, unsupported_auth_scheme}.

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

%%

get_metadata(Key, Metadata) ->
    maps:get(Key, Metadata, undefined).

get_metadata_mapped_key(Key) ->
    maps:get(Key, get_meta_mappings()).

get_meta_mappings() ->
    AuthConfig = genlib_app:env(?APP, auth_config),
    maps:get(metadata_mappings, AuthConfig).
