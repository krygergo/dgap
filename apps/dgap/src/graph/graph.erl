-module(graph).

-behaviour(gen_server).

-export([start_link/2, ref/1, add_topology/2, start/2, stop/1, blacklist/3, deblacklist/3, read_history/2]).

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(graph_state, {
  ref :: reference(),
  id :: id(),
  module :: atom(),
  vertices = #{} :: #{Key :: id() => Value :: vertex()},
  graph_history :: pid()
}).

-record(vertex, {
  pid,
  vertex_linker,
  edges
}).

-type vertex() :: #vertex{}.

-type id() :: string() | integer().

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id, Module) ->
  gen_server:start_link(?MODULE, [Id, Module], []).

ref(Graph) ->
  gen_server:call(Graph, ref).

add_topology(Graph, Topology) ->
  gen_server:call(Graph, {add_topology, Topology}).

start(Graph, Fun) ->
  gen_server:call(Graph, {start, Fun}).

stop(Graph) ->
  gen_server:call(Graph, stop).

blacklist(Graph, Id1, Id2) ->
  gen_server:call(Graph, {blacklist, {Id1, Id2}}).

deblacklist(Graph, Id1, Id2) ->
  gen_server:call(Graph, {deblacklist, {Id1, Id2}}).

read_history(Graph, From) ->
  gen_server:call(Graph, {read_history, From}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Id, Module]) ->
  process_flag(trap_exit, true),
  {ok, #graph_state{ ref = make_ref(), id = Id, module = Module, vertices = #{} }, {continue, process_init}}.

handle_continue(process_init, State) ->
  {ok, GraphHistory} = graph_history_supervisor:start_graph_history(),
  monitor(process, GraphHistory),
  {noreply, State#graph_state{ graph_history = GraphHistory }}.

handle_call({add_topology, Request}, _From, State) ->
  add_topology_request(Request, State);
handle_call(ref, _From, State) ->
  ref_request(State);
handle_call({start, Request}, _From, State) ->
  start_request(Request, State);
handle_call(stop, _From, State) ->
  stop_request(State);
handle_call({blacklist, Request}, _From, State) ->
  blacklist_request(Request, State);
handle_call({deblacklist, Request}, _From, State) ->
  deblacklist_request(Request, State);
handle_call({read_history, Request}, _From, State) ->
  read_history_request(Request, State);
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State = #graph_state{ graph_history = StateGraphHistory }) when Pid =:= StateGraphHistory ->
  {stop, Reason, State};
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State = #graph_state{ vertices = StateVertices }) ->
  Vertices = maps:filter(
    fun
      (_, #vertex{ pid = VertexPid }) when Pid =:= VertexPid -> 
        false;
      (_, _) -> 
        true 
    end, StateVertices),
  {noreply, State#graph_state{ vertices = Vertices }};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #graph_state{ graph_history = StateGraphHistory }) ->
  graph_history_supervisor:stop_graph_history(StateGraphHistory).

%%%===================================================================
%%% Internals
%%%===================================================================

start_vertex(Ref, Id) ->
  vertex_supervisor:start_vertex(Ref, Id).

add_topology_request(Topology, State = #graph_state{ ref = StateRef, vertices = StateVertices, graph_history = GraphHistory }) ->
  Vertices = maps:map(
    fun(Id, Edges) -> 
      {ok, Vertex} = start_vertex(StateRef, Id),
      monitor(process, Vertex),
      graph_history:listen(GraphHistory, Vertex),
      {ok, VertexLinker} = vertex:get(Vertex, linker),
      #vertex{ pid = Vertex, vertex_linker = VertexLinker, edges = Edges }
    end, maps:without(maps:keys(StateVertices), maps:from_list(Topology))),
  {reply, ok, State#graph_state{ vertices = maps:merge(StateVertices, Vertices) }}.

ref_request(State = #graph_state{ ref = StateRef }) ->
  {reply, {ok, StateRef}, State}.

start_request(Fun, State = #graph_state{ module = StateModule, vertices = StateVertices }) ->
  case lists:keymember(Fun, 1, StateModule:module_info(exports)) of
    false ->
      {reply, {error, unknown_function}, State};
    true ->
      maps:foreach(
        fun(Id, #vertex{ pid = Pid, vertex_linker = VertexLinker, edges = Edges }) ->
          VertexArgs = {Id, VertexLinker},
          EdgeArgs = [
            begin
              #{Edge := #vertex{ vertex_linker = EdgeVertexLinker }} = StateVertices,
              {Edge, EdgeVertexLinker}
            end || Edge <- Edges, maps:is_key(Edge, StateVertices)],
          vertex:start(Pid, StateModule, Fun, [{VertexArgs, EdgeArgs}])
        end, StateVertices),
      {reply, ok, State}
  end.

stop_request(State = #graph_state{ vertices = StateVertices }) ->
  maps:foreach(fun(_, #vertex{ pid = Pid }) -> vertex:stop(Pid) end, StateVertices),
  {reply, ok, State}.

blacklist_request({Id1, Id2}, State = #graph_state{ vertices = StateVertices }) ->
  case StateVertices of
    #{Id1 := #vertex{ pid = Pid1 }, Id2 := #vertex{ pid = Pid2 }} ->
      vertex:blacklist(Pid1, Id2),
      vertex:blacklist(Pid2, Id1),
      {reply, ok, State};
    #{} ->
      {reply, error, State}
  end.

deblacklist_request({Id1, Id2}, State = #graph_state{ vertices = StateVertices }) ->
  case StateVertices of
    #{Id1 := #vertex{ pid = Pid1 }, Id2 := #vertex{ pid = Pid2 }} ->
      vertex:deblacklist(Pid1, Id2),
      vertex:deblacklist(Pid2, Id1),
      {reply, ok, State};
    #{} ->
      {reply, error, State}
  end.

read_history_request(From, State = #graph_state{ graph_history = GraphHistory }) ->
  graph_history:read(GraphHistory, From),
  {reply, ok, State}.
