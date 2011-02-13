%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2011

-module(tcp_server_sup).
-author('cooldaemon@gmail.com').

-behaviour(supervisor).

-export([start_link/4, stop/1]).
-export([init/1]).
-export([build_monitor_name/1]).

-include("tcp_server.hrl").

%% External APIs
-spec start_link(Name::server_name(), Mod::module_name(), Args::args(), Option::#tcp_server_option{}) -> result_start_link().
start_link(Name, Mod, Args, Option) ->
  supervisor:start_link(Name, ?MODULE, {Name, Mod, Args, Option}).

-spec stop(Name::atom()) -> result().
stop(Name) ->
  case whereis(Name) of
    Pid when is_pid(Pid) ->
      exit(Pid, normal),
      ok;
    _ ->
      {error, not_started}
  end.

%% Callbacks
-type(worker_spec()::{term(), {atom(), atom(), [term(), ...]}, permanent, brutal_kill | integer(), worker, [atom(), ...] | []}).
-type(supervisor_spec()::{ok, {{one_for_one, integer(), integer()},[worker_spec(), ...]}} | ignore).

-spec init({Name::server_name(), Mod::module_name(), Args::args(), Option::#tcp_server_option{}}) -> supervisor_spec().
init({Name, Mod, Args, Option}) ->
  case Mod:init(Args) of 
    {ok, State} ->
      listen(State, Name, Mod, Option);
    Other ->
      error_logger:warning_msg("init(~p) ~p", [Mod, Other]),
      ignore
  end.

%% Internal Functions
-spec listen(State::any(), Name::server_name(), Mod::module_name(), Option::#tcp_server_option{}) -> supervisor_spec().
listen(State, Name, Mod, Option) ->
  case gen_tcp:listen(
    Option#tcp_server_option.port,
    Option#tcp_server_option.listen
  ) of
    {ok, ListenSocket} ->
      build_result(ListenSocket, State, Name, Mod, Option);
    {error, Reason} ->
      error_logger:warning_msg("listen(~p) ~p", [Mod, {error, Reason}]),
      ignore
  end.

-spec build_result(ListenSocket::port(), State::any(), Name::server_name(), Mod::module_name(), Option::#tcp_server_option{}) -> supervisor_spec().
build_result(ListenSocket, State, {Dest, Name}, Mod, Option) ->
  #tcp_server_option{
    max_restarts = MaxRestarts,
    time         = Time
  } = Option,
  MonitorName = build_monitor_name(Name),
  {ok, {
    {one_for_one, MaxRestarts, Time},
    [
      monitor_spec({Dest, MonitorName}) |
      acceptor_specs(
        ListenSocket, State, {Dest, Name}, MonitorName, Mod, Option
      )
    ]
  }}.

-spec monitor_spec(Name::server_name()) -> worker_spec().
monitor_spec({Dest, MonitorName}) ->
  {
    MonitorName,
    {
      tcp_server_monitor,
      start_link,
      [{Dest, MonitorName}]
    },
    permanent,
    brutal_kill,
    worker,
    []
  }.

-spec acceptor_specs(ListenSocket::port(), State::any(), Name::server_name(), MonitorName::atom(), Mod::module_name(), Option::#tcp_server_option{}) -> [worker_spec(), ...].
acceptor_specs(
  ListenSocket, State, {Dest, Name}, MonitorBaseName, Mod, Option
) ->
  #tcp_server_option{
    max_processes = MaxProcesses,
    shutdown      = Shutdown
  } = Option,
  MonitorName = case Dest of
    local   -> MonitorBaseName;
    _Global -> {Dest, MonitorBaseName}
  end,
  lists:map(
    fun (N) ->
      AcceptorName = build_acceptor_name(Name, N),
      {
        AcceptorName,
        {
          tcp_server_acceptor,
          start_link,
          [
            {Dest, AcceptorName},
            ListenSocket,
            State,
            MonitorName,
            Mod,
            Option
          ]
        },
        permanent,
        Shutdown,
        worker,
        []
      }
    end,
    lists:seq(1, MaxProcesses)
  ).

-spec build_monitor_name(atom()) -> atom().
build_monitor_name(Prefix) ->
  list_to_atom(atom_to_list(Prefix) ++ "_monitor").

-spec build_acceptor_name(atom(), integer()) -> atom().
build_acceptor_name(Prefix, Number) ->
  list_to_atom(
    atom_to_list(Prefix) ++ "_acceptor_" ++ integer_to_list(Number)
  ).
 
