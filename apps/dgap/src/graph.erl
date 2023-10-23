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

-type topology() :: topology:topology().

-type state() :: #state{}.

-type vertex() :: #vertex{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Ref :: reference(), Id :: integer()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
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

-spec topology_request(topology(), state()) -> {reply, ok, state()}.
topology_request(Topology, State = #state{ ref = Ref, vertices = Vertices }) ->
  NewVertices = maps:map(
    fun(Id, Edges) -> 
      {ok, Pid} = vertex:start_link(Ref, Id),
      {ok, VertexLinker} = vertex:vertex_linker(Pid),
      #vertex{ pid = Pid, vertex_linker = VertexLinker, edges = Edges }
    end, maps:without(maps:keys(Vertices), maps:from_list(Topology))),
  {reply, ok, State#state{ vertices = maps:merge(Vertices, NewVertices) }}.

-spec start_request({atom(), atom()}, state()) -> {reply, ok, state()}.
start_request({Module, Fun}, State = #state{ vertices = Vertices }) ->
  maps:foreach(
    fun(Id, #vertex{ pid = Pid, vertex_linker = VertexLinker, edges = Edges }) ->
      VertexArgs = {Id, VertexLinker},
      EdgeArgs = [
        begin
          #{Edge := #vertex{ vertex_linker = EdgeVertexLinker }} = Vertices,
          {Edge, EdgeVertexLinker}
        end || Edge <- Edges, maps:is_key(Edge, Vertices)],
      vertex:start(Pid, Module, Fun, [{VertexArgs, EdgeArgs}])
    end, Vertices),
  {reply, ok, State}.

-spec stop_request(state()) -> {reply, ok, state()}.
stop_request(State = #state{ vertices = Vertices }) ->
  maps:foreach(fun(_, #vertex{ pid = Pid }) -> vertex:stop(Pid) end, Vertices),
  {reply, ok, State}.

handle_cast_request(kill, State) ->
  kill_request(State);
handle_cast_request({blacklist, Request}, State) ->
  blacklist_request(Request, State);
handle_cast_request({deblacklist, Request}, State) ->
  deblacklist_request(Request, State).

-spec kill_request(state()) -> {stop, killed, state()}.
kill_request(State = #state{ vertices = Vertices }) ->
  maps:foreach(fun(_, #vertex{ pid = Pid }) -> vertex:kill(Pid) end, Vertices),
  {stop, killed, State}.

-spec blacklist_request({integer(), integer()}, state()) -> {noreply, state()}.
blacklist_request({Id1, Id2}, State = #state{ vertices = Vertices }) ->
  case Vertices of
    #{Id1 := #vertex{ pid = Pid1 }, Id2 := #vertex{ pid = Pid2 }} ->
      vertex:blacklist(Pid1, Id2),
      vertex:blacklist(Pid2, Id1),
      {noreply, State};
    #{} ->
      {noreply, State}
  end.

-spec deblacklist_request({integer(), integer()}, state()) -> {noreply, state()}.
deblacklist_request({Id1, Id2}, State = #state{ vertices = Vertices }) ->
  case Vertices of
    #{Id1 := #vertex{ pid = Pid1 }, Id2 := #vertex{ pid = Pid2 }} ->
      vertex:deblacklist(Pid1, Id2),
      vertex:deblacklist(Pid2, Id1),
      {noreply, State};
    #{} ->
      {noreply, State}
  end.
