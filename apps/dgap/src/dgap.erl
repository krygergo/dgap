-module(dgap).

-behaviour(gen_server).

-define(MB, 1048576).

-export([start_link/0, port/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
  listen_socket
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

port() ->
  gen_server:call(?MODULE, port).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  case gen_tcp:listen(0, [binary, {buffer, ?MB}, {packet, 4}]) of
    {ok, ListenSocket} ->
      spawn_link(fun() -> listener(ListenSocket) end),
      {ok, #state{ listen_socket = ListenSocket }};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call(port, _From, State = #state{ listen_socket = ListenSocket }) ->
  {reply, inet:port(ListenSocket), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

listener(ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      case session_supervisor:start_session(Socket) of
        {ok, Session} ->
          gen_tcp:controlling_process(Socket, Session),
          listener(ListenSocket);
        {error, _Reason} ->
          listener(ListenSocket)
      end;
    {error, _Reason} ->
      listener(ListenSocket)
  end.