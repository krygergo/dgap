-module(simulation_logger).

-behaviour(gen_server).

-export([start_link/0, add/2, remove/1, read/0, write/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(simulation_logger_state, {
  graphs = #{},
  log_queue = queue:new(),
  listener
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Ref, Id) ->
  gen_server:call(?MODULE, {add, {Ref, Id}}).

remove(Ref) ->
  gen_server:call(?MODULE, {remove, Ref}).

read() ->
  gen_server:call(?MODULE, read, infinity).

write(Ref, Id, Message) ->
  gen_server:cast(?MODULE, {write, {Ref, Id, Message}}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  {ok, #simulation_logger_state{}}.

handle_call({add, Request}, _From, State) ->
  add_request(Request, State);
handle_call({remove, Request}, _From, State) ->
  remove_request(Request, State);
handle_call(read, From, State) ->
  read_request(From, State).
  
handle_cast({write, Request}, State) ->
  write_request(Request, State).

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

add_request({Ref, Id}, State = #simulation_logger_state{ graphs = StateGraphs }) ->
  {reply, ok, State#simulation_logger_state{ graphs = StateGraphs#{Ref => Id} }}.

remove_request(Ref, State = #simulation_logger_state{ graphs = StateGraphs }) ->
  {reply, ok, State#simulation_logger_state{ graphs = maps:remove(Ref, StateGraphs) }}.

read_request(From, State = #simulation_logger_state{ log_queue = StateLogQueue, listener = undefined }) ->
  case queue:out(StateLogQueue) of
    {{value, Log}, Rest} ->
      {reply, {ok, Log}, State#simulation_logger_state{ log_queue = Rest }};
    {empty, _} ->
      {noreply, State#simulation_logger_state{ listener = From }}
  end;
read_request(From, State = #simulation_logger_state{ listener = StateListener }) ->
  gen_server:reply(StateListener, {error, superseded}),
  read_request(From, State#simulation_logger_state{ listener = undefined }).

write_request({Ref, Id, Message}, State = #simulation_logger_state{ graphs = StateGraphs, log_queue = StateLogQueue, listener = undefined }) ->
  case StateGraphs of
    #{Ref := GraphId} ->
      {noreply, State#simulation_logger_state{ log_queue = queue:in({GraphId, Id, Message}, StateLogQueue) }};
    #{} ->
      {noreply, State}
  end;
write_request({Ref, Id, Message}, State = #simulation_logger_state{ graphs = StateGraphs, listener = StateListener }) ->
  case StateGraphs of
    #{Ref := GraphId} ->
      gen_server:reply(StateListener, {GraphId, Id, Message}),
      {noreply, State#simulation_logger_state{ listener = undefined }};
    #{} ->
      {noreply, State}
  end.
