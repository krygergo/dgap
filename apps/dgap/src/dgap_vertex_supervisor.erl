-module(dgap_vertex_supervisor).

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
  VertexSupervisor = #{id => vertex_supervisor,
    start => {vertex_supervisor, start_link, []},
    type => supervisor},
  VertexTracerSupervisor = #{id => vertex_tracer_supervisor,
    start => {vertex_tracer_supervisor, start_link, []},
    type => supervisor},
  VertexWorkerSupervisor = #{id => vertex_worker_supervisor,
    start => {vertex_worker_supervisor, start_link, []},
    type => supervisor},
  VertexLinkerSupervisor = #{id => vertex_linker_supervisor,
    start => {vertex_linker_supervisor, start_link, []},
    type => supervisor},
  {ok, {SupFlags, [VertexSupervisor, VertexTracerSupervisor, VertexWorkerSupervisor, VertexLinkerSupervisor]}}.
