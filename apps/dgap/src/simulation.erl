-module(simulation).

-behaviour(gen_server).

-export([start_link/0, add/1, topology/2, start/2, start/3, stop/1, kill/1, remove_link/3, reinsert_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
  graphs = #{} :: #{Id :: integer() => Graph :: graph()}
}).

-record(graph, {
  pid :: pid(),
  ref :: reference()
}).

-type graph() :: #graph{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Id) ->
  gen_server:call(?MODULE, {add, Id}).

topology(Id, Topology) ->
  gen_server:call(?MODULE, {Id, {topology, Topology}}).

start(Id, Module) ->
  gen_server:call(?MODULE, {Id, {start, Module}}).

start(Id, Module, Fun) ->
  gen_server:call(?MODULE, {Id, {start, {Module, Fun}}}).

stop(Id) ->
  gen_server:call(?MODULE, {Id, stop}).

kill(Id) ->
  gen_server:cast(?MODULE, {kill, Id}).

remove_link(Id, Id1, Id2) ->
  gen_server:cast(?MODULE, {Id, {remove_link, {Id1, Id2}}}).

reinsert_link(Id, Id1, Id2) ->
  gen_server:cast(?MODULE, {Id, {reinsert_link, {Id1, Id2}}}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call(Request, _From, State) ->
  handle_call_request(Request, State).

handle_cast(Request, State) ->
  handle_cast_request(Request, State).

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{}) ->
  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

handle_call_request({add, Id}, State) ->
  add_request(Id, State);
handle_call_request({Id, Request}, State = #state{ graphs = Graphs }) ->
  case Graphs of
    #{Id := Graph} ->
      handle_call_request(Graph, Request, State);
    #{} ->
      {reply, ok, State}
  end.

handle_call_request(Graph, {topology, Topology}, State) ->
  topology_request(Graph, Topology, State);
handle_call_request(Graph, {start, Request}, State) ->
  start_request(Graph, Request, State);
handle_call_request(Graph, stop, State) ->
  stop_request(Graph, State).

add_request(Id, State = #state{ graphs = Graphs }) ->
  case Graphs of
    #{Id := _Graph} ->
      {reply, error, State};
    #{} ->
      Ref = make_ref(),
      {ok, Pid} = graph:start_link(Ref, Id),
      event_handler:add(Ref, Id),
      {reply, ok, State#state{ graphs = Graphs#{Id => #graph{ pid = Pid, ref = Ref }} }}
  end.

topology_request(#graph{ pid = Pid }, Topology, State) ->
  Reply = graph:topology(Pid, Topology),
  {reply, Reply, State}.

start_request(#graph{ pid = Pid }, Module, State) when is_atom(Module) ->
  case [Fun || {Fun, Arity} <- Module:module_info(exports), Fun =/= module_info, Arity =:= 1] of
    [Fun | _] ->
      Reply = graph:start(Pid, Module, Fun),
      {reply, Reply, State};
    [] ->
      {reply, {error, function_match}, State}
  end;
start_request(#graph{ pid = Pid }, {Module, Fun}, State) ->
  Reply = graph:start(Pid, Module, Fun),
  {reply, Reply, State}.

stop_request(#graph{ pid = Pid }, State) ->
  Reply = graph:stop(Pid),
  {reply, Reply, State}.

handle_cast_request({kill, Id}, State) ->
  kill_request(Id, State);
handle_cast_request({Id, Request}, State = #state{ graphs = Graphs }) ->
  case Graphs of
    #{Id := Graph} ->
      handle_cast_request(Graph, Request, State);
    #{} ->
      {noreply, State}
  end.

handle_cast_request(Graph, {remove_link, Request}, State) ->
  remove_link_request(Graph, Request, State);
handle_cast_request(Graph, {reinsert_link, Request}, State) ->
  reinsert_link_request(Graph, Request, State).

kill_request(Id, State = #state{ graphs = Graphs }) ->
  case Graphs of
    #{Id := #graph{ pid = Pid, ref = Ref }} ->
      graph:kill(Pid),
      event_handler:remove(Ref),
      {noreply, State#state{ graphs = maps:remove(Id, Graphs) }};
    #{} ->
      {noreply, State}
  end.

remove_link_request(#graph{ pid = Pid }, {Id1, Id2}, State) ->
  graph:blacklist(Pid, Id1, Id2),
  {noreply, State}.

reinsert_link_request(#graph{ pid = Pid }, {Id1, Id2}, State) ->
  graph:deblacklist(Pid, Id1, Id2),
  {noreply, State}.
