-module(dgap_supervisor).

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
  Dgap = #{id => dgap,
    start => {dgap, start_link, []}},
  Simulation = #{id => simulation,
    start => {simulation, start_link, []}},
  EventHandler = #{id => event_handler,
    start => {event_handler, start_link, []}},
  SessionSupervisor = #{id => session_supervisor,
    start => {session_supervisor, start_link, []},
    type => supervisor},
  {ok, {SupFlags, [Dgap, Simulation, EventHandler, SessionSupervisor]}}.
