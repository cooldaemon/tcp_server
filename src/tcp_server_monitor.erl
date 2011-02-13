%% @author Masahito Ikuta <cooldaemon@gmail.com> [http://d.hatena.ne.jp/cooldaemon/]
%% @copyright Masahito Ikuta 2011

-module(tcp_server_monitor).
-author('cooldaemon@gmail.com').

-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([
    register/2,
    increment/2, decrement/2,
    info/2
]).
-export([
    init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-include("tcp_server.hrl").

%% External APIs
-spec start_link(Name::server_name()) -> result_start_link().
start_link(Name) ->
  gen_server:start_link(Name, ?MODULE, [], []).

-spec stop(ServerRef::server_ref()) -> stopped.
stop(ServerRef) ->
  gen_server:call(ServerRef, stop).

-spec register(ServerRef::server_ref(), pid()) -> ok.
register(ServerRef, Pid) ->
  gen_server:call(ServerRef, {register, Pid}).

-spec increment(ServerRef::server_ref(), pid()) -> ok.
increment(ServerRef, Pid) ->
  gen_server:cast(ServerRef, {increment, Pid}).

-spec decrement(ServerRef::server_ref(), pid()) -> ok.
decrement(ServerRef, Pid) ->
  gen_server:cast(ServerRef, {decrement, Pid}).

-spec info(ServerRef::server_ref(), Key::info_key()) -> result_info().
info(ServerRef, Key) ->
  gen_server:call(ServerRef, {info, Key}).

%% Callbacks
-type(monitor_state()::{[reference(), ...] | [], [pid(), ...] | []}).

-spec init(any()) -> {ok, monitor_state()}.
init(_Args) ->
  {ok, {_MonitorRefs = [], _Pids = []}}.

-spec handle_call(Message::stop | {register, pid()} | {info, Key::info_key()} | any(), any(), State::monitor_state()) -> {stop, normal, stopped, monitor_state()} | {reply, ok | result_info(), monitor_state()}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call({register, Pid}, _From, {MonitorRefs, Pids}) ->
  {reply, ok, {
    [erlang:monitor(process, Pid) | MonitorRefs],
    Pids
  }};
      
handle_call({info, Key}, _From, State) ->
  {reply, state_to_info(State, Key), State};

handle_call(_Message, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(Message::{increment, pid()} | {decrement, pid()} | any(), State::monitor_state()) -> {noreply, monitor_state()}.

handle_cast({increment, Pid}, {MonitorRefs, Pids}) ->
  {noreply, {MonitorRefs, [Pid | Pids]}};

handle_cast({decrement, Pid}, {MonitorRefs, Pids}) ->
  {noreply, {MonitorRefs, lists:delete(Pid, Pids)}};

handle_cast(_Message, State) ->
  {noreply, State}.

-spec handle_info({'DOWN', MonitorRef::reference(), any(), Pid::pid(), any()} | any(), State::monitor_state()) -> {noreply, monitor_state()}.

handle_info({'DOWN', MonitorRef, _Type, Pid, _Info}, {MonitorRefs, Pids}) ->
  erlang:demonitor(MonitorRef),
  {noreply, {
    lists:delete(MonitorRef, MonitorRefs),
    lists:delete(Pid, Pids)
  }};

handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(Reason::any(), State::monitor_state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(any(), State::monitor_state(), any()) -> {ok, State::monitor_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Functions
-spec state_to_info(State::monitor_state(), Key::info_key()) -> result_info().

state_to_info({_MonitorRefs, Pids}, curr_connections) ->
  length(Pids); 

state_to_info(_State, _Key) ->
  undefined.

