-module(session).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(session_state, {
  socket
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Socket]) ->
  process_flag(trap_exit, true),
  {ok, #session_state{ socket = Socket }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp, _Port, Data}, State = #session_state{ socket = Socket }) ->
  session_handler_supervisor:start_session_handler(Socket, Data),
  {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {noreply, State}.
