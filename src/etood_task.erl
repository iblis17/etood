-module(etood_task).

-compile(export_all).

-record(state, {server, name="", expire=[0]}).


start(Name, Exp) ->
  spawn(?MODULE, init, [self(), Name, Exp]).


start_link(Name, Exp) ->
  spawn_link(?MODULE, init, [self(), Name, Exp]).


init(Server, Name, Exp) ->
  loop(#state{server=Server, name=Name, expire=datetime_expire(Exp)}).


loop(State=#state{server=Server, name=N, expire=[Exp|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after Exp * 1000 ->
    case Next of
      [] -> Server ! {done, N};
      _ -> loop(State#state{expire=Next})
    end
  end.


%% erlang has a limitation of time out seconds: it should =< 50 days
split_expire(N) when is_number(N) ->
  Limit = 49 * 86400,  %% 49 days
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

datetime_expire(DateTime={{_, _, _}, {_, _, _}}) ->
  Now = calendar:local_time(),
  Delta =
    calendar:datetime_to_gregorian_seconds(DateTime) -
    calendar:datetime_to_gregorian_seconds(Now),
  Secs =
    if
      Delta > 0 -> Delta;
      Delta =< 0 -> 0
    end,
  split_expire(Secs).


cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} -> ok
  end.
