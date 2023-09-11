-module(dgap_simulation_supervisor).

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
  Simulation = #{id => simulation,
    start => {simulation, start_link, []}},
  SimulationLogger = #{id => simulation_logger,
    start => {simulation_logger, start_link, []}},
  {ok, {SupFlags, [Simulation, SimulationLogger]}}.