%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2011

-module(tcp_server_app).
-author('cooldaemon@gmail.com').

-behaviour(application).
-export([start/2, stop/1]).

-include("tcp_server.hrl").

-spec start(Type::normal | {takeover, node()} | {failover, node()}, [any(), ...]) -> ignore | error() | {ok, pid()}.
start(_Type, [Mod]) ->
  tcp_server:start_link(Mod);
start(_Type, [Mod, Args]) ->
  tcp_server:start_link(Mod, Args);
start(_Type, [Mod, Args, Option]) ->
  tcp_server:start_link(Mod, Args, Option);
start(_Type, [Name, Mod, Args, Option]) ->
  tcp_server:start_link(Name, Mod, Args, Option);
start(_Type, _StartArgs) ->
  {error, "Application StartArgs is Invalid."}.

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
