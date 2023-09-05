-module(compilation).

-behaviour(gen_server).

-export([start_link/0, compile/1, algorithm/1, algorithms/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(compilation_state, {
  algorithms
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

compile(File) ->
  gen_server:call(?MODULE, {compile, File}).

algorithm(Module) ->
  gen_server:call(?MODULE, {algorithm, Module}).

algorithms() ->
  gen_server:call(?MODULE, algorithms).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  {ok, #compilation_state{ algorithms = #{} }}.

handle_call({compile, File}, _From, State = #compilation_state{ algorithms = Algorithms }) ->
  case c:c(File) of
    {ok, Module} ->
      case Module:module_info(exports) of
        [{Start, _Arity}] ->
          {reply, {ok, Module}, State#compilation_state{ algorithms = Algorithms#{Module => Start} }};
        _ ->
          code:delete(Module),
          code:purge(Module),
          {reply, error, State}
      end;
    error ->
      {reply, error, State}
  end;
handle_call({algorithm, Module}, _From, State = #compilation_state{ algorithms = Algorithms }) ->
  case Algorithms of
    #{Module := Algorithm} ->
      {reply, {ok, Algorithm}, State};
    #{} ->
      {reply, error, State}
  end;
handle_call(algorithms, _From, State = #compilation_state{ algorithms = Algorithms }) ->
  {reply, {ok, maps:keys(Algorithms)}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.