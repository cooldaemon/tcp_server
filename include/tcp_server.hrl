%% @type error() = {error, any()}
-type(error()::{error, any()}).

%% @type result() = ok | error()
-type(result()::ok | error()).

%% @type result_start_link() = {ok, pid()} | ignore | error()
-type(result_start_link()::{ok, pid()} | ignore | error()).

-type(server_ref()::atom() | {atom(), node()} | {global, any()} | pid()).

%% @type server_name() = {local, atom()} | {global, atom()}. See supervisor:start_link/3 or gen_server:start_link/4
-type(server_name()::{local, atom()} | {global, atom()}).

%% @type module_name() = atom(). Module name.
-type(module_name()::atom()).

%% @type args() = any(). Arguments for Module:init/1
-type(args()::any()).

%% @type info_key() = curr_connections | any()
-type(info_key()::curr_connections | any()).

%% @type result_info() = integer() | undefined
-type(result_info()::integer() | undefined).

-type(packet()::[char()] | binary()).

%% @type listen_option() = [any()]. See gen_tcp:listen/2
-type(listen_option()  ::[any()]).

%% @type tcp_server_option() = #tcp_server_option{
%%         listen                  = listen_option(),
%%         port                    = integer(),
%%         max_processes           = integer(),
%%         max_restarts            = integer(),
%%         time                    = timeout(),
%%         shutdown                = brutal_kill | integer() | infinity,
%%         accept_timeout          = timeout(),
%%         accept_error_sleep_time = integer(),
%%         recv_length             = integer(),
%%         recv_timeout            = timeout()
%%       } 
-record(tcp_server_option, {
    listen = [{active, false}, binary, {packet, line}, {reuseaddr, true}] ::listen_option(),
    port                    = 11211    ::integer(),
    max_processes           = 8        ::integer(),
    max_restarts            = 3        ::integer(),
    time                    = 60       ::timeout(),
    shutdown                = 2000     ::brutal_kill | integer() | infinity,
    accept_timeout          = infinity ::timeout(),
    accept_error_sleep_time = 3000     ::integer(),
    recv_length             = 0        ::integer(),
    recv_timeout            = infinity ::timeout()
}).
