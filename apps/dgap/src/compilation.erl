-module(compilation).

-behaviour(gen_server).

-export([start_link/0, compile/1, algorithms/0]).

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

algorithms() ->
  gen_server:call(?MODULE, algorithms).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
  {ok, #compilation_state{ algorithms = #{} }}.

handle_call({compile, Request}, _From, State) ->
  compile_request(Request, State);
handle_call(algorithms, _From, State = #compilation_state{ algorithms = Algorithms }) ->
  {reply, {ok, [algorithm_info(Module) || Module <- maps:values(Algorithms)]}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

compile_request(File, State) ->
  case epp:parse_file(File, []) of
    {ok, Forms} ->
      case verify_forms(Forms) of
        false ->
          {reply, {error, export_function}, State};
        true ->        
          [{eof, Line} | Rest] = lists:reverse(Forms),
          ExtendedForms = lists:reverse([{eof, Line + 1}, log_bif(Line) | Rest]),
          compile(ExtendedForms, File, State)
      end;
    Error = {error, _} ->
      {reply, Error, State}
  end.

compile(Forms, File, State) ->
  case compile:forms(Forms) of
    {ok, Module, Binary} ->
      load(Module, File, Binary, State);
    error ->
      {reply, {error, compile_error}, State}
  end.

load(Module, File, Binary, State = #compilation_state{ algorithms = Algorithms }) ->
  case code:load_binary(Module, File, Binary) of
    {module, Module} ->
      {reply, ok, State#compilation_state{ algorithms = Algorithms#{Module => Module} }};
    {error, What} ->
      {reply, {error, What}, State}
  end.

log_bif(Line) ->
  {function, Line, log, 1,
    [{clause, Line,
      [{var, Line, 'Message'}],
      [],
      [{op, Line, '!',
        {atom, Line, simulation_logger},
        {var, Line, 'Message'}},
        {atom, Line, ok}]}]}.

verify_forms(Forms) ->
  lists:foldl(fun(Fun, Acc) -> Fun(Forms) and Acc end, true, [fun verify_export_forms/1]).

verify_export_forms(Forms) ->
  lists:foldl(
    fun
      ({attribute, _, export, Funs}, Acc) ->
        lists:keymember(1, 2, Funs) or Acc;
      (_, Acc) ->
        Acc
    end, false, Forms).

algorithm_info(Module) ->
  FunInfo = [Fun || Fun = {Name, Arity} <- Module:module_info(exports), Name =/= module_info, Arity =:= 1],
  {Module, FunInfo}.
