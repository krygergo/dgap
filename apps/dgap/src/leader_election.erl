-module(leader_election).

-export([leader_election/1]).

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
