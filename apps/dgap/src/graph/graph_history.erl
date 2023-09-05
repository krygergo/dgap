-module(graph_history).

-behaviour(gen_server).

-export([start_link/0, listen/2, read/2, add_history/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(graph_history_state, {
  history = queue:new(),
  listeners = #{},
  listener
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

listen(GraphHistory, Vertex) ->
  gen_server:call(GraphHistory, {listen, Vertex}).

read(GraphHistory, From) ->
  gen_server:call(GraphHistory, {read, From}).

add_history(GraphHistory, History) ->
  gen_server:cast(GraphHistory, {add_history, History}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #graph_history_state{}}.

handle_call({listen, Request}, _From, State) ->
  listen_request(Request, State);
handle_call({read, Request}, _From, State) ->
  read_request(Request, State);
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({add_history, Request}, State) ->
  add_history_request(Request, State);
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'EXIT', Listener, _Reason}, State = #graph_history_state{ listeners = StateListeners }) ->
  #{Listener := Vertex} = StateListeners,
  case is_process_alive(Vertex) of
    true ->
      Self = self(),
      NewListener = spawn_link(fun() -> listen_loop(Self, Vertex) end),
      {noreply, State#graph_history_state{ listeners = maps:remove(Listener, StateListeners#{NewListener => Vertex}) }};
    false ->
      {noreply, State#graph_history_state{ listeners = maps:remove(Listener, StateListeners) }}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State = #graph_history_state{ listener = undefined }) ->
  ok;
terminate(Reason, State = #graph_history_state{ listener = StateListener }) ->
  gen_server:reply(StateListener, Reason),
  ok.

%%%===================================================================
%%% Internals
%%%===================================================================

listen_request(Vertex, State = #graph_history_state{ listeners = StateListeners }) ->
  Self = self(),
  Listener = spawn_link(fun() -> listen_loop(Self, Vertex) end),
  {reply, ok, State#graph_history_state{ listeners = StateListeners#{Listener => Vertex} }}.

read_request(From, State = #graph_history_state{ history = StateHistory, listener = undefined }) ->
  case queue:out(StateHistory) of
    {{value, Value}, Rest} ->
      gen_server:reply(From, {ok, Value}),
      {reply, ok, State#graph_history_state{ history = Rest }};
    {empty, _Rest} ->
      {reply, ok, State#graph_history_state{ listener = From }}
  end;
read_request(From, State = #graph_history_state{ listener = StateListener }) ->
  gen_server:reply(StateListener, {error, superseded}),
  {reply, ok, State#graph_history_state{ listener = From }}.

add_history_request(History, State = #graph_history_state{ history = StateHistory, listener = undefined }) ->
  {noreply, State#graph_history_state{ history = queue:in(History, StateHistory) }};
add_history_request(History, State = #graph_history_state{ listener = Listener }) ->
  gen_server:reply(Listener, History),
  {noreply, State#graph_history_state{ listener = undefined }}.

listen_loop(GraphHistory, Vertex) ->
  {ok, History} = vertex:read_history(Vertex),
  add_history(GraphHistory, History),
  listen_loop(GraphHistory, Vertex).  
