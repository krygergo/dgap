-module(vertex_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_vertex/2, stop_vertex/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_vertex(Ref, Id) ->
  supervisor:start_child(?MODULE, [Ref, Id]).

stop_vertex(Vertex) ->
  supervisor:terminate_child(?MODULE, Vertex).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  Vertex = #{id => vertex,
    start => {vertex, start_link, []},
    restart => temporary},
  {ok, {SupFlags, [Vertex]}}.