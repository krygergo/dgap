-module(dgap_graph_supervisor).

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
  GraphSupervisor = #{id => graph_supervisor,
    start => {graph_supervisor, start_link, []},
    type => supervisor},
  GraphHistorySupervisor = #{id => graph_history_supervisor,
    start => {graph_history_supervisor, start_link, []},
    type => supervisor},
  {ok, {SupFlags, [GraphSupervisor, GraphHistorySupervisor]}}.
