-module(session_handler).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(session_handler_state, {
  socket,
  data
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Data) ->
  gen_server:start_link(?MODULE, [Socket, Data], []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Socket, Data]) ->
  process_flag(trap_exit, true),
  {ok, #session_handler_state{ socket = Socket, data = Data }, {continue, handle_request}}.

handle_continue(handle_request, State = #session_handler_state{ socket = Socket, data = Data }) ->
  Request = {Ref, MFA, Args} = binary_to_term(Data),
  io:format("~p~n", [Request]),
  Response = erlang:apply(MFA, Args),
  gen_tcp:send(Socket, term_to_binary({Ref, Response})),
  {stop, normal, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #session_handler_state{}) ->
  ok.
