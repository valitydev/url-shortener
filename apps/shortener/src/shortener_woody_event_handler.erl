-module(shortener_woody_event_handler).

-behaviour(woody_event_handler).

-export([handle_event/4]).

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta :: woody_event_handler:event_meta(),
    Opts :: woody:options().
handle_event(Event, RpcID, Meta, Opts) ->
    ok = woody_event_handler_otel:handle_event(Event, RpcID, Meta, Opts),
    scoper_woody_event_handler:handle_event(Event, RpcID, Meta, Opts).
