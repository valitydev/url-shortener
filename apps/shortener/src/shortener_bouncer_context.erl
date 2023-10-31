-module(shortener_bouncer_context).

-include_lib("bouncer_proto/include/bouncer_base_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").

-type fragment() :: bouncer_client:context_fragment().
-type acc() :: bouncer_context_helpers:context_fragment().

-type fragments() :: {acc(), _ExternalFragments :: #{_ID => fragment()}}.

-export_type([fragment/0]).
-export_type([acc/0]).
-export_type([fragments/0]).

-type prototypes() :: [
    {operation, prototype_operation()}
].

-type prototype_operation() :: #{
    id := swag_server_ushort:operation_id(),
    slug => shortener_slug:slug()
}.

-export_type([prototypes/0]).
-export_type([prototype_operation/0]).

-export([new/0]).
-export([build/2]).

%%

-spec new() -> fragments().
new() ->
    {mk_base_fragment(), #{}}.

mk_base_fragment() ->
    bouncer_context_helpers:make_env_fragment(#{
        now => genlib_rfc3339:format(genlib_time:unow(), second),
        deployment => #{id => genlib_app:env(shortener, deployment, undefined)}
    }).

-spec build(prototypes(), fragments()) -> fragments().
build(Prototypes, {Acc0, External}) ->
    Acc1 = lists:foldl(fun({T, Params}, Acc) -> build(T, Params, Acc) end, Acc0, Prototypes),
    {Acc1, External}.

build(operation, #{id := OperationID} = Params, Acc) ->
    Slug = maps:get(slug, Params, #{}),
    Acc#ctx_v1_ContextFragment{
        shortener = #ctx_v1_ContextUrlShortener{
            op = #ctx_v1_UrlShortenerOperation{
                id = operation_id_to_binary(OperationID),
                shortened_url = #ctx_v1_ShortenedUrl{
                    id = maps:get(id, Slug, undefined),
                    owner = #base_Entity{
                        id = maps:get(owner, Slug, undefined)
                    }
                }
            }
        }
    }.

%%

operation_id_to_binary(V) ->
    erlang:atom_to_binary(V, utf8).
