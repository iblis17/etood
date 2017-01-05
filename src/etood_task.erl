-module(etood_task).

-behavior(gen_server).

-compile(export_all).

-record(state, {server, name="", expire=[0]}).


%% public APIs

start(Name, Exp) ->
  gen_server:start(?MODULE, {self(), Name, Exp}, []).


start_link(Name, Exp) ->
  gen_server:start_link(?MODULE, {self(), Name, Exp}, []).


cancel(Pid) ->
  gen_server:call(Pid, cacnel).


renew(Pid, Exp) ->
  gen_server:call(Pid, {renew, Exp}).


%% gen_server functions

init({Server, Name, Exp}) ->
  [Timeout|NextExp] = datetime_expire(Exp),
  {ok, #state{server=Server, name=Name, expire=NextExp}, Timeout}.


handle_call(cacnel, _From, State) ->
  {stop, normal, ok, State};

handle_call({renew, NewExp}, _From, State) ->
  NewState = State#state{expire=datetime_expire(NewExp)},
  {reply, ok, NewState}.


handle_info(timeout, State=#state{server=Srv, name=N, expire=Exp}) ->
  if
    Exp =/= [] ->
      {noreply, State#state{expire=tl(Exp)}, hd(Exp)};
    Exp =:= [] ->
      Srv ! {done, N},
      {stop, normal, State}
  end;

handle_info(Msg, State) ->
  io:format("unknown message: ~p~n", [Msg]),
  {noreply, State}.


handle_cast(Msg, State) ->
  io:format("unknown cast operation: ~p~n", [Msg]),
  {noreply, State}.


code_change(_OldVer, State, _Extra) ->
  %% current no upgrade plan
  {ok, State}.


terminate(_Reason, _State) ->
  %% TODO: maybe write log to db
  ok.


%% private functions

%% erlang has a limitation of time out seconds: it should =< 50 days
split_expire(N) when is_number(N) ->
  Limit = 49 * 86400 * 1000,  %% 49 days
  [(N rem Limit) * 1000 | lists:duplicate(N div Limit, Limit)].


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
