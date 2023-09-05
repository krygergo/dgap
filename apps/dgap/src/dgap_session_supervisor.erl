-module(dgap_session_supervisor).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_one},
  SessionSupervisor = #{id => session_supervisor,
    start => {session_supervisor, start_link, []},
    type => supervisor},
  SessionHandlerSupervisor = #{id => session_handler_supervisor,
    start => {session_handler_supervisor, start_link, []},
    type => supervisor},
  {ok, {SupFlags, [SessionSupervisor, SessionHandlerSupervisor]}}.
