-module(vertex).

-behaviour(gen_server).

-export([start_link/2, get/2, start/4, stop/1, blacklist/2, deblacklist/2, read_history/1]).

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(vertex_state, {
  ref,
  id,
  status :: status(),
  vertex_tracer,
  vertex_worker,
  vertex_linker
}).

-type status() :: running | idle.

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Id) ->
  gen_server:start_link(?MODULE, [Ref, Id], []).

get(Vertex, Field) ->
  gen_server:call(Vertex, {get, Field}).

start(Vertex, Module, Fun, Args) ->
  gen_server:call(Vertex, {start, {Module, Fun, Args}}).

stop(Vertex) ->
  gen_server:call(Vertex, stop).

blacklist(Vertex, Id) ->
  gen_server:call(Vertex, {blacklist, Id}).

deblacklist(Vertex, Id) ->
  gen_server:call(Vertex, {deblacklist, Id}).

read_history(Vertex) ->
  gen_server:call(Vertex, read_history, infinity).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Ref, Id]) ->
  process_flag(trap_exit, true),
  {ok, #vertex_state{ ref = Ref, id = Id, status = idle }, {continue, process_init}}.

handle_continue(process_init, State = #vertex_state{ ref = StateRef, id = StateId }) ->
  {ok, VertexTracer} = start_vertex_tracer(StateRef, StateId),
  {ok, VertexWorker} = start_vertex_worker(StateRef, StateId, VertexTracer),
  {ok, VertexLinker} = start_vertex_linker(StateRef, StateId, VertexWorker),
  lists:foreach(fun(Process) -> monitor(process, Process) end, [VertexTracer, VertexWorker, VertexLinker]),
  {noreply, State#vertex_state{ vertex_tracer = VertexTracer, vertex_worker = VertexWorker, vertex_linker = VertexLinker }}.

handle_call({get, Request}, _From, State) ->
  get_request(Request, State);
handle_call({start, Request}, _From, State) ->
  start_request(Request, State);
handle_call(stop, _From, State) ->
  stop_request(State);
handle_call({blacklist, Request}, _From, State) ->
  blacklist_request(Request, State);
handle_call({deblacklist, Request}, _From, State) ->
  deblacklist_request(Request, State);
handle_call(read_history, From, State) ->
  read_history_request(From, State);
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State) ->
  process_down(Pid, Reason, State);
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #vertex_state{ vertex_tracer = StateVertexTracer, vertex_worker = StateVertexWorker, vertex_linker = StateVertexLinker }) ->
  stop_vertex_linker(StateVertexLinker),
  stop_vertex_worker(StateVertexWorker),
  stop_vertex_tracer(StateVertexTracer),
  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

start_vertex_tracer(Ref, Id) ->
  vertex_tracer_supervisor:start_vertex_tracer(Ref, Id).

start_vertex_worker(Ref, Id, VertexTracer) ->
  vertex_worker_supervisor:start_vertex_worker(Ref, Id, VertexTracer).

start_vertex_linker(Ref, Id, VertexWorker) ->
  vertex_linker_supervisor:start_vertex_linker(Ref, Id, VertexWorker).

stop_vertex_tracer(VertexTracer) ->
  vertex_tracer_supervisor:stop_vertex_tracer(VertexTracer).

stop_vertex_worker(VertexWorker) ->
  vertex_worker_supervisor:stop_vertex_worker(VertexWorker).

stop_vertex_linker(VertexLinker) ->
  vertex_linker_supervisor:stop_vertex_linker(VertexLinker).

get_request(linker, State = #vertex_state{ vertex_linker = StateVertexLinker }) -> 
  {reply, {ok, StateVertexLinker}, State}.

start_request(_, State = #vertex_state{ status = running }) ->
  {reply, ok, State};
start_request({Module, Fun, Args}, State = #vertex_state{ ref = StateRef, vertex_worker = StateVertexWorker }) ->
  vertex_worker:start(StateVertexWorker, StateRef, Module, Fun, Args),
  {reply, ok, State#vertex_state{ status = running }}.

stop_request(State = #vertex_state{ vertex_worker = StateVertexWorker }) ->
  stop_vertex_worker(StateVertexWorker),
  {reply, ok, State}.

blacklist_request(Id, State = #vertex_state{ ref = StateRef, vertex_linker = StateVertexLinker }) ->
  vertex_linker:blacklist(StateVertexLinker, StateRef, Id),
  {reply, ok, State}.

deblacklist_request(Id, State = #vertex_state{ ref = StateRef, vertex_linker = StateVertexLinker }) ->
  vertex_linker:deblacklist(StateVertexLinker, StateRef, Id),
  {reply, ok, State}.

read_history_request(From, State = #vertex_state{ ref = StateRef, vertex_linker = StateVertexLinker }) ->
  vertex_linker:read_history(StateVertexLinker, StateRef, From),
  {noreply, State}.

process_down(Pid, Reason, State = #vertex_state{ vertex_tracer = StateVertexTracer }) when Pid =:= StateVertexTracer ->
  {stop, Reason, State};
process_down(Pid, Reason, State = #vertex_state{ 
  ref = StateRef, 
  id = StateId,
  vertex_tracer = StateVertexTracer,
  vertex_worker = StateVertexWorker,
  vertex_linker = StateVertexLinker
}) when Pid =:= StateVertexWorker ->
  {ok, VertexWorker} = start_vertex_worker(StateRef, StateId, StateVertexTracer),
  monitor(process, VertexWorker),
  vertex_linker:set_vertex_worker(StateVertexLinker, StateRef, VertexWorker),
  case Reason of
    {shutdown, {result, Result}} ->
      Result;
    Result ->
      Result
  end,
  vertex_linker:add_history(StateVertexLinker, StateRef, {result, StateId, Result}),
  {noreply, State#vertex_state{ status = idle, vertex_worker = VertexWorker }};
process_down(Pid, Reason, State = #vertex_state{ vertex_linker = StateVertexLinker }) when Pid =:= StateVertexLinker ->
  {stop, Reason, State}.
