-module({{appid}}).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3]).

-include("tcp_server.hrl").

%% I/F
-spec start_link() -> result_start_link().
start_link() ->
  tcp_server:start_link(
    ?MODULE, [], #tcp_server_option{port = 10000, max_processes = 256}
  ).

-spec stop() -> result().
stop() ->
  tcp_server:stop().

%% Callbacks
-type({{appid}}_state()::any()).

-spec init(Args::any()) -> {ok, {{appid}}_state()}.
init(_Args) ->
  {ok, {}}.

-spec handle_call(Socket::port(), Data::packet(), State::{{appid}}_state()) -> {close | reply, packet(), {{appid}}_state()}.
handle_call(_Socket, <<"bye\\r\\n">>, State) ->
  {close, <<"cya\\r\\n">>, State};
handle_call(_Socket, Data, State) ->
  {reply, Data, State}.

