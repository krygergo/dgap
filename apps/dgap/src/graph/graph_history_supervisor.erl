-module(graph_history_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_graph_history/0, stop_graph_history/1]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_graph_history() ->
  supervisor:start_child(?MODULE, []).

stop_graph_history(GraphHistory) ->
  supervisor:terminate_child(?MODULE, GraphHistory).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => simple_one_for_one},
  GraphHistory = #{id => graph_history,
    start => {graph_history, start_link, []},
    restart => temporary},
  {ok, {SupFlags, [GraphHistory]}}.