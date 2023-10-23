-module(topology).

-export([random/2, ring/1]).

-export_type([topology/0]).

-opaque topology() :: [{Id :: integer(), Edges :: [integer()]}].

%%%===================================================================
%%% API
%%%===================================================================

random({From, To}, Alpha) ->
  random(lists:seq(From, To), Alpha);
random(List, Alpha) when -1 =< Alpha, Alpha =< 1 ->
  random(List, Alpha, #{}).

random([], _Alpha, Map) ->
  maps:to_list(maps:fold(
    fun(Vertex, Edges, Acc) ->
      lists:foldl(
        fun(Vertex1, Acc1) ->
          #{Vertex1 := Edges1} = Acc1,
          Acc1#{Vertex1 := [Vertex | Edges1]}
        end, Acc, Edges)
    end, Map, Map));
random([Vertex | []], Alpha, Map) ->
  random([], Alpha, Map#{Vertex => []});
random([Vertex | Rest], Alpha, Map) ->
  Length = length(Rest),
  case rand:uniform(Length) + (Length * Alpha) of
    Random when Random < 1 ->
      random(Rest, Alpha, Map#{Vertex => []});
    Random when Random > Length ->
      random(Rest, Alpha, Map#{Vertex => Rest});
    Random ->
      random(Rest, Alpha, Map#{Vertex => lists:sublist(shuffle(Rest), round(Random))})
  end.

ring({From, To}) ->
  ring(lists:seq(From, To));
ring([A | []]) ->
  [{A, [A]}];
ring([A, B | []]) ->
  [{A, [B]}, {B, [A]}];
ring([A, B | Rest]) ->
  [{Z, [Y]} | Vertices] = ring(A, B, Rest, []),
  [{A, [Z, B]} | lists:reverse([{Z, [Y, A]} | Vertices])].

ring(Prev, Curr, [Next | []], Vertices) ->
  [{Next, [Curr]} , {Curr, [Prev, Next]} | Vertices];
ring(Prev, Curr, [Next | Rest], Vertices) ->
  ring(Curr, Next, Rest, [{Curr, [Prev, Next]} | Vertices]).

%%%===================================================================
%%% Internals
%%%===================================================================

shuffle(List) when is_list(List) ->
  Map = maps:from_list(lists:enumerate(List)),
  lists:map(fun({_Index, Element}) -> Element end, maps:to_list(shuffle(Map, length(List)))).

shuffle(Map, 1) ->
  Map;
shuffle(Map, Count) ->
  Random = rand:uniform(Count),
  shuffle(Map#{Random := maps:get(Count, Map), Count := maps:get(Random, Map)}, Count - 1).
