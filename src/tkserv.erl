-module(tkserv).

-compile(export_all).

-record(state, {tasks=orddict:new(),   %% list of task records, (Name, task) pairs
                clients=orddict:new()  %% list of client pids, (Ref, Pid) pairs
               }).

-record(task, {name="", desc="", pid}).


start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.


start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.


init() ->
  loop(#state{}).


loop(State=#state{tasks=Tasks, clients=Clients}) ->
  receive
    {Pid, Ref, {subscribe, Client}} ->
      MonRef = erlang:monitor(process, Client),
      NewC = orddict:store(MonRef, Client, Clients),

      Pid ! {Ref, ok},
      loop(State#state{clients=NewC});

    {Pid, Ref, {add, Name, Desc, Exp}} ->
      case valid_datetime(Exp) of
        true ->
          Task = task:start_link(Name, Exp),
          NewT = orddict:store(Name,
                               #task{name=Name, desc=Desc, pid=Task},
                               Tasks),

          Pid ! {Ref, ok},
          loop(State#state{tasks=NewT});

        false ->
          Pid ! {Ref, {error, bad_expire}},
          loop(State)
      end;

    {Pid, Ref, {cancel, Name}} ->
      NewT =
        case orddict:find(Name, Tasks) of
          {ok, T} ->
            ok = task:cancel(T#task.pid),
            orddict:erase(Name, Tasks);
          error ->
            Tasks
        end,

      Pid ! {Ref, ok},
      loop(State#state{tasks=NewT});

    {done, Name} ->
      NewT =
        case orddict:find(Name, Tasks) of
          {ok, T} ->
            notify_clients({done, Name, T#task.desc}, Clients),
            orddict:erase(Name, Tasks);
          error ->
            Tasks
        end,

      loop(State#state{tasks=NewT});

    {'DOWN', Ref, process, _Pid, _Reason} ->
      NewC = orddict:erase(Ref, Clients),
      loop(State#state{clients=NewC});

    code_change ->
      ?MODULE:loop(State);  %% WTF

    shutdown ->
      exit(shutdown);

    Unknown ->
      io:format("unknown message: ~p~n", [Unknown]),
      loop(State)
  end.


upgrade() ->
  ?MODULE ! code_change,
  ok.


terminate() ->
  ?MODULE ! shutdown,
  ok.


subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),

  ?MODULE ! {self(), Ref, {subscribe, Pid}},

  receive
    {Ref, ok} -> ok;
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.


add_task(Name, Desc, Exp) ->
  Ref = make_ref(),

  ?MODULE ! {self(), Ref, {add, Name, Desc, Exp}},

  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.


cancel(Name) ->
  Ref = make_ref(),

  ?MODULE ! {self(), Ref, {cancel, Name}},

  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.


valid_datetime({Date={_, _, _}, Time={_, _, _}}) ->
  calendar:valid_date(Date) andalso valid_time(Time);

valid_datetime(_) ->
  false.


valid_time({H, M, S})
  when H >= 0, H < 24,
       M >= 0, M < 60,
       S >= 0, S < 60 -> true;

valid_time(_) -> false.


notify_clients(Msg, Clients) ->
  orddict:map(
    fun(_, C) -> C ! Msg end,
    Clients).
