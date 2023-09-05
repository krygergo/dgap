-module(vertex_linker_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_vertex_linker/3, stop_vertex_linker/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_vertex_linker(Ref, Id, VertexWorker) ->
  supervisor:start_child(?MODULE, [Ref, Id, VertexWorker]).

stop_vertex_linker(VertexLinker) ->
  supervisor:terminate_child(?MODULE, VertexLinker).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  VertexLinker = #{id => vertex_linker,
    start => {vertex_linker, start_link, []},
    restart => temporary},
  {ok, {SupFlags, [VertexLinker]}}.