-module(vertex_tracer).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(vertex_tracer_state, {
  ref,
  id
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Id) ->
  gen_server:start_link(?MODULE, [Ref, Id], []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Ref, Id]) ->
  {ok, #vertex_tracer_state{ ref = Ref, id = Id }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({trace, _Worker, send, Message, VertexLinker}, State = #vertex_tracer_state{ ref = StateRef, id = StateId }) ->
  vertex_linker:send(VertexLinker, StateRef, StateId, Message),
  {noreply, State}.
