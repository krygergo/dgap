-module(algorithm_util).

-export([hello_world/1, leader_election/1, ping_pong/1]).

hello_world(_Args) ->
  event_handler ! "Hello World!",
  ok.

leader_election({{Id, _Pid}, [{_LeftId, _LeftPid}, {_RightId, RightPid}]}) ->
  RightPid ! {probe, Id},
  leader_election(Id, RightPid, none).
leader_election(Id, RightPid, Leader) ->
  receive
    {probe, ProbeId} ->
      if
        Id > ProbeId ->
          leader_election(Id, RightPid, none);
        Id < ProbeId ->
          RightPid ! {probe, ProbeId},
          leader_election(Id, RightPid, none);
        true ->
          RightPid ! {selected, Id},
          leader_election(Id, RightPid, none)
      end;
    {selected, SelectedId} ->
      case SelectedId =:= Id of
        true ->
          RightPid ! agreement,
          {id, Id, leader, Id};
        false ->
          RightPid ! {selected, SelectedId},
          leader_election(Id, RightPid, SelectedId)
      end;
    agreement ->
      RightPid ! agreement,
      {id, Id, leader, Leader}
  end.

ping_pong(Args = {{Id1, _Pid1}, [{Id2, _Pid2}]}) when Id1 < Id2 ->
  ping(Args);
ping_pong(Args) ->
  pong(Args).
  
ping(Args = {_, [{_, Pid2}]}) ->
  timer:sleep(1000),
  Pid2 ! ping,
  receive
    pong ->
      ping(Args)
  after 
    2000 ->
      ping(Args)
  end.
  
pong(Args = {_, [{_, Pid2}]}) ->
  receive
    ping ->
      timer:sleep(1000), 
      Pid2 ! pong,
      pong(Args)
  end.