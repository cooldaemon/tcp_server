%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2011

-module(tcp_server_SUITE).
-author('cooldaemon@gmail.com').

-compile(export_all).
-export([init/1, handle_call/3]). %% For echo server

-include("ct.hrl").
-include("../include/tcp_server.hrl").

sequences() ->
  [{seq, [
    single_connection,
    error_and_reboot,
    multiple_connections,
    connection_counter
  ]}].

all() -> [{sequence, seq}].

init_per_testcase(connection_counter, Config) ->
    start_server(10),
    Config;
init_per_testcase(_TestCase, Config) ->
    start_server(1),
    Config.

start_server(MaxProcesses) ->
  tcp_server:start_link(
    ?MODULE, [], #tcp_server_option{max_processes=MaxProcesses}
  ).

end_per_testcase(_TestCase, _Config) ->
  tcp_server:stop().

single_connection(_Conf) ->
  normal_procedure().

normal_procedure() ->
  {ok, Socket} = connect_to_echo_server(),
  gen_tcp:send(Socket, <<"hello\r\n">>),
  case gen_tcp:recv(Socket, 0) of
    {ok, <<"hello\r\n">>} -> ok;
    _HelloError           -> ct:fail(bad_echo_value)
  end,
  gen_tcp:send(Socket, <<"bye\r\n">>),
  case gen_tcp:recv(Socket, 0) of
    {ok, <<"cya\r\n">>} -> ok;
    _ByeError           -> ct:fail(bad_return_value)
  end,
  gen_tcp:close(Socket).

error_and_reboot(_Conf) ->
  {ok, Socket} = connect_to_echo_server(),
  gen_tcp:send(Socket, <<"error\r\n">>),
  {error, closed} = gen_tcp:recv(Socket, 0),
  gen_tcp:close(Socket),
  normal_procedure(). %% Check whether the echo server has rebooted

multiple_connections(_Conf) ->
  lists:foreach(fun (_N) ->
    {ok, Socket} = connect_to_echo_server(),
    gen_tcp:close(Socket)
  end, lists:seq(1, 1024)).

connection_counter(_Conf) ->
  Sockets = lists:map(fun (_N) ->
    {ok, Socket} = connect_to_echo_server(),
    Socket
  end, lists:seq(1, 5)),
  timer:sleep(100), %% Wait for increment
  case tcp_server:info(curr_connections) of
    5 -> ok;
    Error ->
      ct:comment(io:format("bad_info:~p", [Error])),
      ct:fail(bad_info)
  end,
  lists:foreach(fun (Socket) -> gen_tcp:close(Socket) end, Sockets).

connect_to_echo_server() ->
  gen_tcp:connect(
    {127,0,0,1}, 11211, [binary, {packet, line}, {active, false}]
  ).

%% Echo server
init(_Args) -> {ok, {}}.

handle_call(_Socket, <<"bye\r\n">>, State) ->
  {close, <<"cya\r\n">>, State};
handle_call(_Socket, <<"error\r\n">>, State) ->
  (fun(X) -> 1 / X end)(0), %% Always throws a bad arithmetic exception
  {close, <<"error\r\n">>, State};
handle_call(_Socket, Data, State) ->
  {reply, Data, State}.
