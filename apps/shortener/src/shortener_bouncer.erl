-module(shortener_bouncer).

-include_lib("bouncer_proto/include/bouncer_context_thrift.hrl").

-export([gather_context_fragments/4]).
-export([judge/2]).

%%

-spec gather_context_fragments(
    TokenContextFragment :: token_keeper_client:context_fragment(),
    UserID :: binary() | undefined,
    RequestContext :: swag_server_ushort:request_context(),
    WoodyContext :: woody_context:ctx()
) -> shortener_bouncer_context:fragments().
gather_context_fragments(TokenContextFragment, UserID, ReqCtx, WoodyCtx) ->
    {Base, External0} = shortener_bouncer_context:new(),
    External1 = add_token_keeper_fragment(External0, TokenContextFragment),
    External2 = maybe_add_userorg_fragment(UserID, External1, WoodyCtx),
    {add_requester_context(ReqCtx, Base), External2}.

-spec judge(shortener_bouncer_context:fragments(), woody_context:ctx()) -> shortener_auth:resolution().
judge({Acc, External}, WoodyCtx) ->
    {ok, RulesetID} = application:get_env(shortener, bouncer_ruleset_id),
    JudgeContext = #{fragments => External#{<<"shortener">> => Acc}},
    bouncer_client:judge(RulesetID, JudgeContext, WoodyCtx).

%%

add_token_keeper_fragment(External, TokenContextFragment) ->
    External#{<<"token-keeper">> => {encoded_fragment, TokenContextFragment}}.

maybe_add_userorg_fragment(undefined, External, _WoodyCtx) ->
    External;
maybe_add_userorg_fragment(UserID, External, WoodyCtx) ->
    case bouncer_context_helpers:get_user_orgs_fragment(UserID, WoodyCtx) of
        {ok, UserOrgsFragment} ->
            External#{<<"userorg">> => UserOrgsFragment};
        {error, {user, notfound}} ->
            External
    end.

-spec add_requester_context(swag_server_ushort:request_context(), shortener_bouncer_context:acc()) ->
    shortener_bouncer_context:acc().
add_requester_context(ReqCtx, FragmentAcc) ->
    ClientPeer = maps:get(peer, ReqCtx, #{}),
    bouncer_context_helpers:add_requester(
        #{ip => maps:get(ip_address, ClientPeer, undefined)},
        FragmentAcc
    ).
