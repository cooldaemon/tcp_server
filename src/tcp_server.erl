%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2011
%% @doc Befaviour for the tcp_server.

-module(tcp_server).
-author('cooldaemon@gmail.com').

-export([behaviour_info/1]).
-export([start_link/1, start_link/2, start_link/3, start_link/4]).
-export([stop/0, stop/1]).
-export([info/1, info/2]).

%% @headerfile "tcp_server.hrl"
-include("tcp_server.hrl").

behaviour_info(callbacks) -> [{init, 1}, {handle_call, 3}];
behaviour_info(_Other)    -> undefined.

%% @doc start tcp_server.
%% @spec start_link(Mod::module_name()) -> result_start_link()
-spec start_link(Mod::module_name()) -> result_start_link().
start_link(Mod) -> start_link(Mod, []).

%% @doc start tcp_server.
%% @spec start_link(Mod::module_name(), Args::args()) -> result_start_link()
-spec start_link(Mod::module_name(), Args::args()) -> result_start_link().
start_link(Mod, Args) ->
  start_link(Mod, Args, #tcp_server_option{}).

%% @doc start tcp_server.
%% @spec start_link(Mod::module_name(), Args::args(), Option::tcp_server_option()) -> result_start_link()
-spec start_link(Mod::module_name(), Args::args(), Option::#tcp_server_option{}) -> result_start_link().
start_link(Mod, Args, Option) ->
  start_link({local, ?MODULE}, Mod, Args, Option).

%% @doc start tcp_server.
%% @spec start_link(Name::server_name(), Mod::module_name(), Args::args(), Option::tcp_server_option()) -> result_start_link()
-spec start_link(Name::server_name(), Mod::module_name(), Args::args(), Option::#tcp_server_option{}) -> result_start_link().
start_link(Name, Mod, Args, Option) ->
  tcp_server_sup:start_link(Name, Mod, Args, Option).

%% @doc stop tcp_server.
%% @spec stop() -> result()
-spec stop() -> result().
stop() -> stop(?MODULE).

%% @doc stop tcp_server.
%% @spec stop(Name::atom()) -> result()
-spec stop(Name::atom()) -> result().
stop(Name) -> tcp_server_sup:stop(Name).

%% @doc stop tcp_server.
%% @spec info(Key::info_key()) -> result_info()
-spec info(Key::info_key()) -> result_info().
info(Key) -> info(?MODULE, Key).

%% @doc stop tcp_server.
%% @spec info(Name::server_name(), Key::info_key()) -> result_info()
-spec info(Name::atom(), Key::info_key()) -> result_info().
info(Name, Key) ->
  tcp_server_monitor:info(
    tcp_server_sup:build_monitor_name(Name), Key
  ).
