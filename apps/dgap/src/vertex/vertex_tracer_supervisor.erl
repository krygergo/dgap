-module(vertex_tracer_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_vertex_tracer/2, stop_vertex_tracer/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_vertex_tracer(Ref, Id) ->
  supervisor:start_child(?MODULE, [Ref, Id]).

stop_vertex_tracer(VertexTracer) ->
  supervisor:terminate_child(?MODULE, VertexTracer).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  VertexTracer = #{id => vertex_tracer,
    start => {vertex_tracer, start_link, []},
    restart => temporary},
  {ok, {SupFlags, [VertexTracer]}}.