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
  Compilation = #{id => compilation,
    start => {compilation, start_link, []}},
  DgapSimulationSupervisor = #{id => dgap_simulation_supervisor,
    start => {dgap_simulation_supervisor, start_link, []},
    type => supervisor},
  DgapGraphSupervisor = #{id => dgap_graph_supervisor,
    start => {dgap_graph_supervisor, start_link, []},
    type => supervisor},
  DgapVertexSupervisor = #{id => dgap_vertex_supervisor,
    start => {dgap_vertex_supervisor, start_link, []},
    type => supervisor},
  DgapSessionSupervisor = #{id => dgap_session_supervisor,
    start => {dgap_session_supervisor, start_link, []},
    type => supervisor},
  {ok, {SupFlags, [Dgap, Compilation, DgapSimulationSupervisor, DgapGraphSupervisor, DgapVertexSupervisor, DgapSessionSupervisor]}}.
