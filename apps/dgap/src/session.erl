-module(session).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
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
  {ok, #state{ socket = Socket }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({tcp, _Port, Data}, State = #state{ socket = Socket }) ->
  spawn_link_session_handler(Socket, Data),
  {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

spawn_link_session_handler(Socket, Data) ->
  spawn_link(
    fun() ->
      {Ref, MFA, Args} = binary_to_term(Data),
      Response = erlang:apply(MFA, Args),
      gen_tcp:send(Socket, term_to_binary({Ref, Response}))
    end
  ).
