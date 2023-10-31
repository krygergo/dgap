-module(vertex).

-behaviour(gen_server).

-export([start_link/2, vertex_linker/1, start/4, stop/1, kill/1, blacklist/2, deblacklist/2]).

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
  ref :: reference(),
  id :: integer(),
  vertex_tracer :: pid(),
  vertex_worker :: pid(),
  vertex_linker :: pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Ref :: reference(), Id :: integer()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Ref, Id) ->
  gen_server:start_link(?MODULE, [Ref, Id], []).

vertex_linker(Vertex) ->
  gen_server:call(Vertex, vertex_linker).

start(Vertex, Module, Fun, Args) ->
  gen_server:call(Vertex, {start, {Module, Fun, Args}}).

stop(Vertex) ->
  gen_server:call(Vertex, stop).

kill(Vertex) ->
  gen_server:cast(Vertex, kill).

blacklist(Vertex, Id) ->
  gen_server:cast(Vertex, {blacklist, Id}).

deblacklist(Vertex, Id) ->
  gen_server:cast(Vertex, {deblacklist, Id}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Ref, Id]) ->
  process_flag(trap_exit, true),
  {ok, #state{ ref = Ref, id = Id }, {continue, process_init}}.

handle_continue(process_init, State = #state{ ref = Ref, id = Id }) ->
  VertexTracer = spawn_link_vertex_tracer(Ref, Id),
  VertexLinker = spawn_link_vertex_linker(Ref, Id),
  {noreply, State#state{ vertex_tracer = VertexTracer, vertex_linker = VertexLinker }}.

handle_call(Request, _From, State) ->
  handle_call_request(Request, State).

handle_cast(Request, State) ->
  handle_cast_request(Request, State).

handle_info({'EXIT', From, Reason}, State) ->
  handle_info_exit(From, Reason, State);
handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internals
%%%===================================================================

handle_call_request(vertex_linker, State) ->
  vertex_linker_request(State);
handle_call_request({start, Request}, State) ->
  start_request(Request, State);
handle_call_request(stop, State) ->
  stop_request(State).

vertex_linker_request(State = #state{ vertex_linker = undefined }) ->
  {reply, {error, undefined}, State};
vertex_linker_request(State = #state{ vertex_linker = VertexLinker }) ->
  {reply, {ok, VertexLinker}, State}.

start_request({Module, Fun, Args}, State = #state{ ref = Ref, vertex_tracer = VertexTracer, vertex_worker = undefined, vertex_linker = VertexLinker }) ->
  VertexWorker = spawn_link_vertex_worker(Ref),
  VertexLinker ! {Ref, {self(), vertex_worker, VertexWorker}},
  receive
    ok ->
      VertexWorker ! {start, VertexTracer, Module, Fun, Args},
      {reply, ok, State#state{ vertex_worker = VertexWorker }}
  after
    1000 ->
      {reply, {error, start}}
  end;
start_request(_Request, State) ->
  {reply, ok, State}.

stop_request(State = #state{ vertex_worker = undefined }) ->
  {reply, ok, State};
stop_request(State = #state{ vertex_worker = VertexWorker }) ->
  exit(VertexWorker, kill),
  {reply, ok, State}.

handle_cast_request(kill, State) ->
  kill_request(State);
handle_cast_request({blacklist, Id}, State) ->
  blacklist_request(Id, State);
handle_cast_request({deblacklist, Id}, State) ->
  deblacklist_request(Id, State).

kill_request(State = #state{ vertex_worker = undefined }) ->
  {stop, normal, State};
kill_request(State = #state{ vertex_worker = VertexWorker }) ->
  exit(VertexWorker, kill),
  kill_request(State#state{ vertex_worker = undefined }).

blacklist_request(Id, State = #state{ ref = Ref, vertex_linker = VertexLinker }) ->
  VertexLinker ! {Ref, {blacklist, Id}},
  {noreply, State}.

deblacklist_request(Id, State = #state{ ref = Ref, vertex_linker = VertexLinker }) ->
  VertexLinker ! {Ref, {deblacklist, Id}},
  {noreply, State}.

handle_info_exit(From, {ExitRef, result, Result}, State = #state{ ref = Ref, id = Id, vertex_worker = VertexWorker }) when ExitRef =:= Ref, From =:= VertexWorker ->
  event_handler:result(Ref, {Id, Result}),
  {noreply, State#state{ vertex_worker = undefined }};
handle_info_exit(From, Reason, State = #state{ vertex_tracer = VertexTracer }) when From =:= VertexTracer ->
  {stop, Reason, State};
handle_info_exit(From, Reason, State = #state{ vertex_linker = VertexLinker }) when From =:= VertexLinker ->
  {stop, Reason, State};
handle_info_exit(_From, {Reason, _Stack}, State = #state{ ref = Ref, id = Id }) ->
  event_handler:result(Ref, {Id, {error, Reason}}),
  {noreply, State}.

spawn_link_vertex_tracer(Ref, Id) ->
  spawn_link(
    fun Fun() ->
      receive
        {trace, _Worker, send, Log, event_handler} ->
          event_handler:log(Ref, {Id, Log}),
          Fun();
        {trace, _Worker, send, Message, VertexLinker} ->
          VertexLinker ! {Ref, {Id, send, Message}},
          Fun()
      end
    end
  ).

spawn_link_vertex_worker(Ref) ->
  spawn_link(
    fun() ->
      receive
        {start, VertexTracer, Module, Fun, Args} ->
          erlang:trace(self(), true, [{tracer, VertexTracer}, send, set_on_spawn]),
          Result = apply(Module, Fun, Args),
          exit({Ref, result, Result})
      end
    end
  ).

spawn_link_vertex_linker(Ref, Id) ->
  Fun = 
    fun VertexLinker(StateMap) ->
      #{blacklist := Blacklist, vertex_worker := VertexWorker } = StateMap,
      receive
        {SenderRef, Request} when SenderRef =:= Ref ->
          case Request of
            {SenderId, send, Message} ->
              case sets:is_element(SenderId, Blacklist) of
                true ->
                  VertexLinker(StateMap);
                false ->
                  event_handler:message(Ref, {SenderId, Id, Message}),
                  VertexWorker ! Message,
                  VertexLinker(StateMap)
              end;
            {SenderPid, vertex_worker, NewVertexWorker} ->
              SenderPid ! ok,
              VertexLinker(StateMap#{vertex_worker => NewVertexWorker});
            {blacklist, BId} ->
              VertexLinker(StateMap#{blacklist => sets:add_element(BId, Blacklist)});
            {deblacklist, DId} ->
              VertexLinker(StateMap#{blacklist => sets:del_element(DId, Blacklist)})
            end;
        _ -> VertexLinker(StateMap)
      end
    end,
  spawn_link(fun() -> Fun(#{ blacklist => sets:new(), vertex_worker => undefined }) end).
