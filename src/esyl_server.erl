% A simple Erlang Syslog Server

-module(esyl_server).
-behaviour(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/1]).

-define(UDP_OPTIONS, [binary, {active, true}]).

start_link(Port) ->
    io:format("Starting server~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    io:format("Opening socket at port ~p~n", [Port]),
    try gen_udp:open(Port, ?UDP_OPTIONS) of
        {ok, Socket} ->
            {ok, Socket}
    catch
        error:Error ->
            io:format("Error: ~p~n", [Error]),
            {ok, Port}
    end.

handle_cast(Cast, State) ->
    io:format("~p catchall: ~p, ~p~n", [?MODULE, Cast, State]),
    {noreply, State}.

handle_call({logged}, _Caller, State) ->
    {ok, State}.

handle_info(Info, State) ->
    io:format("~p catchall: ~p, ~p~n", [?MODULE, Info, State]),
    {noreply, State}.

terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

