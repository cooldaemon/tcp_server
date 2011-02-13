-module({{appid}}_SUITE).

-compile(export_all).

-include("ct.hrl").
-include("../deps/tcp_server/include/tcp_server.hrl").

sequences() -> [{seq, [echo]}].

all() -> [{sequence, seq}].

init_per_testcase(_TestCase, Config) ->
  {{appid}}:start_link(),
  Config.

end_per_testcase(_TestCase, _Config) ->
  {{appid}}:stop().

echo(_Config) ->
  {ok, Socket} = connect_to_echo_server(),
  gen_tcp:send(Socket, <<"hello\\r\\n">>),
  case gen_tcp:recv(Socket, 0) of
    {ok, <<"hello\\r\\n">>} -> ok;
    _HelloError           -> ct:fail(bad_echo_value)
  end,
  gen_tcp:send(Socket, <<"bye\\r\\n">>),
  case gen_tcp:recv(Socket, 0) of
    {ok, <<"cya\\r\\n">>} -> ok;
    _ByeError           -> ct:fail(bad_return_value)
  end,
  gen_tcp:close(Socket).

connect_to_echo_server() ->
  gen_tcp:connect(
    {127,0,0,1}, 10000, [binary, {packet, line}, {active, false}]
  ).

