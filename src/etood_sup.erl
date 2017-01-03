-module(etood_sup).

-compile(export_all).


start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.


start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.


init() ->
  process_flag(trap_exit, true),
  loop().


loop() ->
  Pid = etood_tkserv:start_link(),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown);
    {'EXIT', Pid, Reason} ->
      io:format("task server ~p crashed: ~p~nrestart it ~n", [Pid, Reason]),
      loop()
  end.


terminate() ->
  true = exit(whereis(?MODULE), shutdown),
  ok.
