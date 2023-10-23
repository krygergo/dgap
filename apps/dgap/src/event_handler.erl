-module(event_handler).

-behaviour(gen_server).

-export([start_link/0, add/2, remove/1, read_log/1, read_message/1, read_result/1, log/2, message/2, result/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
  events = []
}).

-record(event, {
  log_listeners = [],
  log_history = queue:new(),
  message_listeners = [],
  message_history = queue:new(),
  result_listeners = [],
  result_history = queue:new()
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

read_log(Id) ->
  gen_server:call(?MODULE, {read_log, Id}, infinity).
  
read_message(Id) ->
  gen_server:call(?MODULE, {read_message, Id}, infinity).
  
read_result(Id) ->
  gen_server:call(?MODULE, {read_result, Id}, infinity).

log(Ref, Log) ->
  gen_server:cast(?MODULE, {log, {Ref, Log}}).

message(Ref, Message) ->
  gen_server:cast(?MODULE, {message, {Ref, Message}}).

result(Ref, Result) ->
  gen_server:cast(?MODULE, {result, {Ref, Result}}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call(Request, From, State) ->
  handle_call_request(Request, From, State).

handle_cast(Request, State) ->
  handle_cast_request(Request, State).

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

handle_call_request({add, Request}, _From, State) ->
  add_request(Request, State);
handle_call_request({remove, Request}, _From, State) ->
  remove_request(Request, State);
handle_call_request({read_log, Request}, From, State) ->
  read_log_request(Request, From, State);
handle_call_request({read_message, Request}, From, State) ->
  read_message_request(Request, From, State);
handle_call_request({read_result, Request}, From, State) ->
  read_result_request(Request, From, State).

add_request({Ref, Id}, State = #state{ events = Events }) ->
  {reply, ok, State#state{ events = [{Ref, Id, #event{}} | Events] }}.

remove_request(Ref, State = #state{ events = Events }) ->
  case lists:keytake(Ref, 1, Events) of
    false ->
      {reply, ok, State};
    {value, {_Ref, _Id, #event{ 
      log_listeners = LogListeners, 
      message_listeners = MessageListeners, 
      result_listeners = ResultListeners }}, Rest} ->
      lists:foreach(
        fun(Listeners) -> 
          lists:foreach(
            fun(Listener) -> 
              gen_server:reply(Listener, ok) 
            end, Listeners) 
        end, [LogListeners, MessageListeners, ResultListeners]),
      {reply, ok, State#state{ events = Rest }}
  end.

read_log_request(Id, From, State) ->
  read_request(
    fun(Event = #event{ log_listeners = LogListeners, log_history = LogHistory }) ->
      case queue:out(LogHistory) of
        {empty, _LogHistory} ->
          Event#event{ log_listeners = [From | LogListeners] };
        {{value, History}, Rest} ->
          gen_server:reply(From, {ok, History}),
          Event#event{ log_history = Rest }
      end
    end, Id, State
  ).

read_message_request(Id, From, State) ->
  read_request(
    fun(Event = #event{ message_listeners = MessageListeners, message_history = MessageHistory }) ->
      case queue:out(MessageHistory) of
        {empty, _MessageHistory} ->
          Event#event{ message_listeners = [From | MessageListeners] };
        {{value, History}, Rest} ->
          gen_server:reply(From, {ok, History}),
          Event#event{ message_history = Rest }
      end
    end, Id, State
  ).

read_result_request(Id, From, State) ->
  read_request(
    fun(Event = #event{ result_listeners = ResultListeners, result_history = ResultHistory }) ->
      case queue:out(ResultHistory) of
        {empty, _ResultHistory} ->
          Event#event{ result_listeners = [From | ResultListeners] };
        {{value, History}, Rest} ->
          gen_server:reply(From, {ok, History}),
          Event#event{ result_history = Rest }
      end
    end, Id, State
  ).

read_request(Read, Id, State = #state{ events = Events }) ->
  case lists:keytake(Id, 2, Events) of
    false ->
      {reply, ok, State};
    {value, {Ref, _Id, Event}, Rest} ->
      {noreply, State#state{ events = [{Ref, Id, Read(Event)} | Rest] }}
  end.

handle_cast_request({log, Request}, State) ->
  log_request(Request, State);
handle_cast_request({message, Request}, State) ->
  message_request(Request, State);
handle_cast_request({result, Request}, State) ->
  result_request(Request, State).

log_request({Ref, Log}, State) ->
  write_request(
    fun(Event = #event{ log_listeners = LogListeners, log_history = LogHistory }) ->
      case LogListeners of
        [] ->
          Event#event{ log_history = queue:in(Log, LogHistory) };
        Listeners ->
          lists:foreach(fun(Listener) -> gen_server:reply(Listener, {ok, Log}) end, Listeners),
          Event#event{ log_listeners = [] }
      end
    end, Ref, State
  ).

message_request({Ref, Message}, State) ->
  write_request(
    fun(Event = #event{ message_listeners = MessageListeners, message_history = MessageHistory }) ->
      case MessageListeners of
        [] ->
          Event#event{ message_history = queue:in(Message, MessageHistory) };
        Listeners ->
          lists:foreach(fun(Listener) -> gen_server:reply(Listener, {ok, Message}) end, Listeners),
          Event#event{ message_listeners = [] }
      end
    end, Ref, State
  ).

result_request({Ref, Result}, State) ->
  write_request(
    fun(Event = #event{ result_listeners = ResultListeners, result_history = ResultHistory }) ->
      case ResultListeners of
        [] ->
          Event#event{ result_history = queue:in(Result, ResultHistory) };
        Listeners ->
          lists:foreach(fun(Listener) -> gen_server:reply(Listener, {ok, Result}) end, Listeners),
          Event#event{ result_listeners = [] }
      end
    end, Ref, State
  ).

write_request(Write, Ref, State = #state{ events = Events }) ->
  case lists:keytake(Ref, 1, Events) of
    false ->
      {reply, ok, State};
    {value, {_Ref, Id, Event}, Rest} ->
      {noreply, State#state{ events = [{Ref, Id, Write(Event)} | Rest] }}
  end.
