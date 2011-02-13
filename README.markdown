## tcp_server
A simple module for implementing concurrent TCP servers in Erlang.

* Makes it easy to implement concurrent TCP servers,
* Provides typical TCP server behaviours, listen, accept, and so forth,
* Handles multiple requests concurrently by maintaining process pool,
* Supports active as well as passive mode of gen_tcp,
* Based on OTP principles.

### Building from Source
    % cd /path/to
    % git clone git://github.com/cooldaemon/tcp_server.git
    % cd ./tcp_server
    % make

### How to Use
    % cd /path/to/tcp_server
    % PROJECT=echo_server make app
    % cd /path/to/echo_server
    % make && make ct

