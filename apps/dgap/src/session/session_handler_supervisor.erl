-module(session_handler_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_session_handler/2]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session_handler(Socket, Data) ->
  supervisor:start_child(?MODULE, [Socket, Data]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  SessionHandler = #{id => session_handler,
    start => {session_handler, start_link, []}},
  {ok, {SupFlags, [SessionHandler]}}.