-module(session_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_session/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session(Socket) ->
  supervisor:start_child(?MODULE, [Socket]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  Session = #{id => session,
    start => {session, start_link, []}},
  {ok, {SupFlags, [Session]}}.