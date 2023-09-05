-module(graph_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_graph/3, stop_graph/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_graph(Id, Module, Fun) ->
  supervisor:start_child(?MODULE, [Id, Module, Fun]).

stop_graph(Graph) ->
  supervisor:terminate_child(?MODULE, Graph).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  Graph = #{id => graph,
    start => {graph, start_link, []},
    restart => temporary},
  {ok, {SupFlags, [Graph]}}.