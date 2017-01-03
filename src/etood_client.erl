-module(etood_client).

-compile(export_all).


%% a simple client to receive all message
listen(Timeout) ->
  receive
    Msg = {done, _Name, _Desc} ->
      [Msg|listen(0)]  %% no last call optimization
  after Timeout * 1000 ->
    []
  end.
