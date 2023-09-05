-module(vertex_linker).

-behaviour(gen_server).

-export([start_link/3, set_vertex_worker/3, blacklist/3, deblacklist/3, add_history/3, read_history/3, send/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(vertex_linker_state, {
  ref,
  id,
  vertex_worker,
  blacklist = sets:new(),
  history = queue:new(),
  listeners = #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Id, VertexWorker) ->
  gen_server:start_link(?MODULE, [Ref, Id, VertexWorker], []).

set_vertex_worker(VertexLinker, Ref, VertexWorker) ->
  gen_server:call(VertexLinker, {Ref, {set_vertex_worker, VertexWorker}}).

blacklist(VertexLinker, Ref, Id) ->
  gen_server:call(VertexLinker, {Ref, {blacklist, Id}}).

deblacklist(VertexLinker, Ref, Id) ->
  gen_server:call(VertexLinker, {Ref, {deblacklist, Id}}).

read_history(VertexLinker, Ref, From) ->
  gen_server:call(VertexLinker, {Ref, {read_history, From}}).

add_history(VertexLinker, Ref, History) ->
  gen_server:cast(VertexLinker, {Ref, {add_history, History}}).

send(VertexLinker, Ref, SenderId, Message) ->
  gen_server:cast(VertexLinker, {Ref, {send, {SenderId, Message}}}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Ref, Id, VertexWorker]) ->
  process_flag(trap_exit, true),
  {ok, #vertex_linker_state{ ref = Ref, id = Id, vertex_worker = VertexWorker }}.

handle_call({Ref, Request}, From, State = #vertex_linker_state{ ref = StateRef }) when Ref =:= StateRef ->
  handle_request(Request, From, State);
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({Ref, Request}, State = #vertex_linker_state{ ref = StateRef }) when Ref =:= StateRef ->
  handle_request(Request, State);
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, _State = #vertex_linker_state{ listeners = StateListeners }) ->
  maps:foreach(fun(_Key, Value) -> gen_server:reply(Value, {terminate, Reason}) end, StateListeners).

%%%===================================================================
%%% Internals
%%%===================================================================

handle_request({set_vertex_worker, VertexWorker}, _From, State) ->
  set_vertex_worker_request(VertexWorker, State);
handle_request({blacklist, Request}, _From, State) ->
  blacklist_request(Request, State);
handle_request({deblacklist, Request}, _From, State) ->
  deblacklist_request(Request, State);
handle_request({read_history, Request}, _From, State) ->
  read_history_request(Request, State).

handle_request({add_history, Request}, State) ->
  add_history_request(Request, State);
handle_request({send, Request}, State) ->
  send_request(Request, State).

set_vertex_worker_request(VertexWorker, State) ->
  {reply, ok, State#vertex_linker_state{ vertex_worker = VertexWorker }}.

blacklist_request(Id, State = #vertex_linker_state{ blacklist = StateBlacklist }) ->
  {reply, ok, State#vertex_linker_state{ blacklist = sets:add_element(Id, StateBlacklist) }}.

deblacklist_request(Id, State = #vertex_linker_state{ blacklist = StateBlacklist }) ->
  {reply, ok, State#vertex_linker_state{ blacklist = sets:del_element(Id, StateBlacklist) }}.

read_history_request(From, State = #vertex_linker_state{ history = StateHistory, listeners = StateListeners }) ->
  case queue:out(StateHistory) of
    {{value, Value}, Rest} ->
      gen_server:reply(From, {ok, Value}),
      {reply, ok, State#vertex_linker_state{ history = Rest }};
    {empty, _Rest} ->
      case StateListeners of
        #{history_listener := HistoryListener} ->
          gen_server:reply(HistoryListener, {error, superseded}),
          {reply, ok, State#vertex_linker_state{ listeners = StateListeners#{history_listener := From} }};
        #{} ->
          {reply, ok, State#vertex_linker_state{ listeners = StateListeners#{history_listener => From} }}
      end
  end.

add_history_request(History, State = #vertex_linker_state{ history = StateHistory, listeners = StateListeners }) ->
  case StateListeners of
    #{history_listener := HistoryListener} ->
      gen_server:reply(HistoryListener, {ok, History}),
      {noreply, State#vertex_linker_state{ listeners = maps:remove(history_listener, StateListeners) }};
    #{} ->
      {noreply, State#vertex_linker_state{ history = queue:in(History, StateHistory) }}
  end.

send_request({SenderId, Message}, State = #vertex_linker_state{ id = StateId, vertex_worker = StateVertexWorker, blacklist = StateBlacklist }) ->
  case sets:is_element(SenderId, StateBlacklist) of
    true ->
      {noreply, State};
    false ->
      StateVertexWorker ! Message,
      add_history_request({SenderId, send, StateId, Message}, State)
  end.
