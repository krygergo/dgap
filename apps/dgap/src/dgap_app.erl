-module(dgap_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Start = dgap_supervisor:start_link(),
  init(),
  Start.

stop(_State) ->
  ok.

init() -> 
  case init:get_plain_arguments() of
    ["vsc-ext", VscPort] ->
      case gen_tcp:connect("localhost", list_to_integer(VscPort), []) of
        {ok, Socket} ->
          {ok, Port} = dgap:port(),
          gen_tcp:send(Socket, list_to_binary(integer_to_list(Port)));
        {error, Reason} ->
          io:format("~p~n", [Reason]),
          ok
        end,
      ok;
    _ ->
      ok
  end.
