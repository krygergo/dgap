-module(vertex_worker_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_vertex_worker/2, stop_vertex_worker/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_vertex_worker(Ref, VertexTracer) ->
  supervisor:start_child(?MODULE, [Ref, VertexTracer]).

stop_vertex_worker(VertexWorker) ->
  supervisor:terminate_child(?MODULE, VertexWorker).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  VertexWorker = #{id => vertex_worker,
    start => {vertex_worker, start_link, []},
    restart => temporary},
  {ok, {SupFlags, [VertexWorker]}}.