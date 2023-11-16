-module(graph).

-behaviour(gen_server).

-export([start_link/2, topology/2, start/3, stop/1, kill/1, blacklist/3, deblacklist/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
  ref :: reference(),
  id :: integer(),
  vertices = #{} :: #{Id :: integer() => Vertex :: vertex()}
}).

-record(vertex, {
  pid,
  vertex_linker,
  edges
}).

-type vertex() :: #vertex{}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Id) ->
  gen_server:start_link(?MODULE, [Ref, Id], []).

topology(Graph, Topology) ->
  gen_server:call(Graph, {topology, Topology}).

start(Graph, Module, Fun) ->
  gen_server:call(Graph, {start, {Module, Fun}}).

stop(Graph) ->
  gen_server:call(Graph, stop).

kill(Graph) ->
  gen_server:cast(Graph, kill).

blacklist(Graph, Id1, Id2) ->
  gen_server:cast(Graph, {blacklist, {Id1, Id2}}).

deblacklist(Graph, Id1, Id2) ->
  gen_server:cast(Graph, {deblacklist, {Id1, Id2}}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Ref, Id]) ->
  process_flag(trap_exit, true),
  {ok, #state{ ref = Ref, id = Id }}.

handle_call(Request, _From, State) ->
  handle_call_request(Request, State).

handle_cast(Request, State) ->
  handle_cast_request(Request, State).

handle_info({'EXIT', _From, Reason}, State) ->
  {stop, Reason, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

handle_call_request({topology, Request}, State) ->
  topology_request(Request, State);
handle_call_request({start, Request}, State) ->
  start_request(Request, State);
handle_call_request(stop, State) ->
  stop_request(State).

topology_request(Topology, State = #state{ ref = Ref, vertices = Vertices }) ->
  NewVertices = maps:map(
    fun(Id, Edges) -> 
      {ok, Pid} = vertex:start_link(Ref, Id),
      {ok, VertexLinker} = vertex:vertex_linker(Pid),
      #vertex{ pid = Pid, vertex_linker = VertexLinker, edges = Edges }
    end, maps:without(maps:keys(Vertices), maps:from_list(Topology))),
  {reply, ok, State#state{ vertices = maps:merge(Vertices, NewVertices) }}.

start_request({Module, Fun}, State = #state{ vertices = Vertices }) ->
  maps:foreach(
    fun(Id, #vertex{ pid = Pid, vertex_linker = VertexLinker, edges = Edges }) ->
      VertexArgs = {Id, VertexLinker},
      EdgeArgs = [
        begin
          #{Edge := #vertex{ vertex_linker = EdgeVertexLinker }} = Vertices,
          {Edge, EdgeVertexLinker}
        end || Edge <- Edges, maps:is_key(Edge, Vertices)],
      vertex:prepare(Pid, Module, Fun, [{VertexArgs, EdgeArgs}])
    end, Vertices),
  maps:foreach(fun(_Id, #vertex{ pid = Pid }) -> vertex:start(Pid) end, Vertices),
  {reply, ok, State}.

stop_request(State = #state{ vertices = Vertices }) ->
  maps:foreach(fun(_, #vertex{ pid = Pid }) -> vertex:stop(Pid) end, Vertices),
  {reply, ok, State}.

handle_cast_request(kill, State) ->
  kill_request(State);
handle_cast_request({blacklist, Request}, State) ->
  blacklist_request(Request, State);
handle_cast_request({deblacklist, Request}, State) ->
  deblacklist_request(Request, State).

kill_request(State = #state{ vertices = Vertices }) ->
  maps:foreach(fun(_, #vertex{ pid = Pid }) -> vertex:kill(Pid) end, Vertices),
  {stop, normal, State}.

blacklist_request({Id1, Id2}, State = #state{ vertices = Vertices }) ->
  case Vertices of
    #{Id1 := #vertex{ pid = Pid1 }, Id2 := #vertex{ pid = Pid2 }} ->
      vertex:blacklist(Pid1, Id2),
      vertex:blacklist(Pid2, Id1),
      {noreply, State};
    #{} ->
      {noreply, State}
  end.

deblacklist_request({Id1, Id2}, State = #state{ vertices = Vertices }) ->
  case Vertices of
    #{Id1 := #vertex{ pid = Pid1 }, Id2 := #vertex{ pid = Pid2 }} ->
      vertex:deblacklist(Pid1, Id2),
      vertex:deblacklist(Pid2, Id1),
      {noreply, State};
    #{} ->
      {noreply, State}
  end.
