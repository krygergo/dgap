-module(simulation).

-behaviour(gen_server).

-export([start_link/0, add/3, kill/1, add_topology/2, start/1, stop/1, remove_link/3, reinsert_link/3, read_history/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(simulation_state, {
  graphs = #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Id, Module, Fun) ->
  gen_server:call(?MODULE, {add, {Id, Module, Fun}}).

kill(Id) ->
  gen_server:call(?MODULE, {Id, kill}).

add_topology(Id, Topology) ->
  gen_server:call(?MODULE, {Id, {add_topology, Topology}}).

start(Id) ->
  gen_server:call(?MODULE, {Id, start}).

stop(Id) ->
  gen_server:call(?MODULE, {Id, stop}).

remove_link(Id, Id1, Id2) ->
  gen_server:call(?MODULE, {Id, {remove_link, {Id1, Id2}}}).

reinsert_link(Id, Id1, Id2) ->
  gen_server:call(?MODULE, {Id, {reinsert_link, {Id1, Id2}}}).

read_history(Id) ->
  gen_server:call(?MODULE, {Id, read_history}, infinity).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  {ok, #simulation_state{}}.

handle_call({add, Request}, _From, State) ->
  add_request(Request, State);
handle_call({Id, Request}, From, State = #simulation_state{ graphs = StateGraphs }) ->
  case StateGraphs of
    #{Id := Graph} ->
      handle_request(Graph, Request, From, State);
    #{} ->
      {reply, ok, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

handle_request(Graph, kill, _From, State) ->
  kill_request(Graph, State);
handle_request(Graph, {add_topology, Request}, _From, State) ->
  add_topology_request(Graph, Request, State);
handle_request(Graph, start, _From, State) ->
  start_request(Graph, State);
handle_request(Graph, stop, _From, State) ->
  stop_request(Graph, State);
handle_request(Graph, {remove_link, Request}, _From, State) ->
  remove_link_request(Graph, Request, State);
handle_request(Graph, {reinsert_link, Request}, _From, State) ->
  reinsert_link_request(Graph, Request, State);
handle_request(Graph, read_history, From, State) ->
  read_history_request(Graph, From, State).

add_request({Id, Module, Fun}, State = #simulation_state{ graphs = StateGraphs }) ->
  case StateGraphs of
    #{Id := _Graph} ->
      {reply, {error, found}, State};
    #{} ->    
      {ok, Graph} = graph_supervisor:start_graph(Id, Module, Fun),
      {ok, Ref} = graph:ref(Graph),
      simulation_logger:add(Ref, Id),
      {reply, ok, State#simulation_state{ graphs = StateGraphs#{Id => Graph} }}
  end.

kill_request(Graph, State) ->
  {ok, Ref} = graph:ref(Graph),
  Reply = graph_supervisor:stop_graph(Graph),
  simulation_logger:remove(Ref),
  {reply, Reply, State}.

add_topology_request(Graph, Topology, State) ->
  graph:add_topology(Graph, Topology),
  {reply, ok, State}.

start_request(Graph, State) ->
  graph:start(Graph),
  {reply, ok, State}.

stop_request(Graph, State) ->
  graph:stop(Graph),
  {reply, ok, State}.

remove_link_request(Graph, {Id1, Id2}, State) ->
  graph:blacklist(Graph, Id1, Id2),
  {reply, ok, State}.

reinsert_link_request(Graph, {Id1, Id2}, State) ->
  graph:deblacklist(Graph, Id1, Id2),
  {reply, ok, State}.

read_history_request(Graph, From, State) ->
  graph:read_history(Graph, From),
  {noreply, State}.
