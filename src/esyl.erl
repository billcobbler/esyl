% A basic Erlang Syslog Server

-module(esyl).
-behavior(application).
-export([
    start/0,
    start/2, 
    stop/1
]).

-define(UDP_OPTIONS, [binary, {active, false}]).

start() ->
    application:start(?MODULE).

start(_Type, _Args) ->
    io:format("Starting application~n"),
    esyl_sup:start_link().

stop(_State) ->
    ok.



