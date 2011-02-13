%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2011

-module(tcp_server_acceptor).
-author('cooldaemon@gmail.com').

-export([start_link/6]).
-export([init/6]).

-include("tcp_server.hrl").

%% External APIs
-spec start_link(Name::server_name(), ListenSocket::port(), State::any(), MonitorName::atom(), Mod::module_name(), Option::#tcp_server_option{}) -> {ok, pid()}.
start_link({Dest, Name}, ListenSocket, State, MonitorName, Mod, Option) ->
  {ok, Pid} = proc_lib:start_link(
    ?MODULE, init,
    [self(), ListenSocket, State, MonitorName, Mod, Option]
  ),
  case Dest of
    local   -> register(Name, Pid);
    _Global -> global:register_name(Name, Pid)
  end,
  {ok, Pid}.

%% Callbacks
-spec init(Parent::pid(), ListenSocket::port(), State::any(), MonitorName::atom(), Mod::module_name(), Option::#tcp_server_option{}) -> no_return().
init(Parent, ListenSocket, State, MonitorName, Mod, Option) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  tcp_server_monitor:register(MonitorName, self()),
  accept(ListenSocket, State, MonitorName, Mod, Option).

%% Internal Functions
-spec accept(ListenSocket::port(), State::any(), MonitorName::atom(), Mod::module_name(), Option::#tcp_server_option{}) -> no_return().
accept(ListenSocket, State, MonitorName, Mod, Option) ->
  case gen_tcp:accept(
    ListenSocket, Option#tcp_server_option.accept_timeout
  ) of 
    {ok, Socket} ->
      try
        tcp_server_monitor:increment(MonitorName, self()),
        recv(
          proplists:get_value(
            active, Option#tcp_server_option.listen
          ),
          Socket, State, Mod, Option
        )
      catch
        Type:Reason ->
          error_logger:warning_msg("accept(~p) ~p", [Mod, {Type, Reason}])
      after
        tcp_server_monitor:decrement(MonitorName, self()),
        gen_tcp:close(Socket)
      end;
    {error, Reason} ->
      error_logger:warning_msg("accept(~p) ~p", [Mod, {error, Reason}]),
      timer:sleep(Option#tcp_server_option.accept_error_sleep_time)
  end,
  accept(ListenSocket, State, MonitorName, Mod, Option).

-spec recv(Active::boolean(), Socket::port(), State::any(), Mod::module_name(), Option::#tcp_server_option{}) -> ok.

recv(false, Socket, State, Mod, Option) ->
  case gen_tcp:recv(
    Socket,
    Option#tcp_server_option.recv_length,
    Option#tcp_server_option.recv_timeout
  ) of
    {ok, Data} ->
      call_mod(false, Socket, Data, State, Mod, Option);
    {error, closed} ->
      ok;
    {error, Reason} ->
      error_logger:warning_msg("recv(~p) ~p", [Mod, {error, Reason}]),
      ok
  end;

recv(true, _DummySocket, State, Mod, Option) ->
  receive
    {tcp, Socket, Data} ->
      call_mod(true, Socket, Data, State, Mod, Option);
    {tcp_closed, _Socket} ->
      ok;
    Error ->
      error_logger:warning_msg("recv(~p) ~p", [Mod, {error, Error}]),
      ok
  after Option#tcp_server_option.recv_timeout ->
    error_logger:warning_msg("recv(~p) ~p", [Mod, {error, timeout}]),
    ok
  end.

-type(packet()::[char()] | binary()).

-spec call_mod(Active::boolean(), Socket::port(), Data::packet(), State::any(), Mod::module_name(), Option::#tcp_server_option{}) -> ok.
call_mod(Active, Socket, Data, State, Mod, Option) ->
  case Mod:handle_call(Socket, Data, State) of
    {reply, DataToSend, State} ->
      send(Socket, DataToSend, Mod),
      recv(Active, Socket, State, Mod, Option);
    {noreply, State} ->
      recv(Active, Socket, State, Mod, Option);
    {close, State} ->
      ok;
    {close, DataToSend, State} ->
      send(Socket, DataToSend, Mod);
    Other ->
      error_logger:warning_msg("call_mod(~p) ~p", [Mod, {unexpected_result, Other}]),
      ok
  end.

-spec send(Socket::port(), Data::packet(), Mod::module_name()) -> ok.
send(Socket, Data, Mod) ->
  case gen_tcp:send(Socket, Data) of
    ok -> 
      ok;
    {error, Reason} ->
      error_logger:warning_msg("send(~p) ~p", [Mod, {error, Reason}]),
      ok
  end.

