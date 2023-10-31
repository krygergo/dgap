-module(algorithm).

-export([compile/1]).

-spec compile(File :: string()) -> ok | {error, Reason :: term()}.
compile(File) ->
  case epp:parse_file(File, []) of
    {ok, Forms} ->
      case verify_forms(Forms) of
        false ->
          {error, export_function};
        true ->        
          [{eof, Line} | Rest] = lists:reverse(Forms),
          ExtendedForms = lists:reverse([{eof, Line + 1}, log_bif(Line) | Rest]),
          compile(ExtendedForms, File)
      end;
    Error -> 
      Error
  end.

compile(Forms, File) ->
  case compile:forms(Forms) of
    {ok, Module, Binary} ->
      load(Module, File, Binary);
    error ->
      {error, compile}
  end.

load(Module, File, Binary) ->
  case code:load_binary(Module, File, Binary) of
    {module, Module} ->
      ok;
    Error ->
      Error
  end.

log_bif(Line) ->
  {function, Line, log, 1,
    [{clause, Line,
      [{var, Line, 'Message'}],
      [],
      [{op, Line, '!',
        {atom, Line, event_handler},
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