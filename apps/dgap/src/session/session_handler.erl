-module(session_handler).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2]).

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
  ResponsePacket = handle_request(Data),
  gen_tcp:send(Socket, ResponsePacket),
  {stop, normal, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

read(<<Length:32, Data/binary>>) ->
  <<Payload:Length/binary, Rest/binary>> = Data,
  {Payload, Length, Rest}.

handle_request(<<0/integer, Data/binary>>) ->
  simulation_request(Data);
handle_request(Data) ->
  {Ref, RefLength, Rest} = read(Data),
  ResponsePacket = handle_request(Rest),
  <<RefLength:32, Ref/binary, ResponsePacket/binary>>.

%%% Simulation requests

simulation_request(<<0/integer, Data/binary>>) ->
  Data.
