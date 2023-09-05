-module(vertex_worker).

-behaviour(gen_server).

-export([start_link/2, start/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(vertex_worker_state, {
  ref,
  vertex_tracer
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, VertexTracer) ->
  gen_server:start_link(?MODULE, [Ref, VertexTracer], []).

start(VertexWorker, Ref, Module, Fun, Args) ->
  gen_server:cast(VertexWorker, {Ref, {start, {Module, Fun, Args}}}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Ref, VertexTracer]) ->
  {ok, #vertex_worker_state{ ref = Ref, vertex_tracer = VertexTracer }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({Ref, Request}, State = #vertex_worker_state{ ref = StateRef }) when Ref =:= StateRef  ->
  handle_request(Request, State);
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

handle_request({start, Request}, State) ->
  start_request(Request, State).

start_request({Module, Fun, Args}, State = #vertex_worker_state{ vertex_tracer = StateVertexTracer }) ->
  erlang:trace(self(), true, [{tracer, StateVertexTracer}, send, set_on_spawn]),
  Result = apply(Module, Fun, Args),
  {stop, {shutdown, {result, Result}}, State}.
