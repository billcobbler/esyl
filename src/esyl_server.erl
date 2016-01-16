% A simple Erlang Syslog Server

-module(esyl_server).
-behaviour(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/1]).

-define(UDP_OPTIONS, [binary, {active, once}, {recbuf, 3728270}, {read_packets, 5}]).
-define(MAX_PROCESS_THRESHOLD, 0.95).

start_link(Port) ->
    io:format("Starting server~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    io:format("Opening socket at port ~p~n", [Port]),
    try gen_udp:open(Port, ?UDP_OPTIONS) of
        {ok, _Socket} ->
            {ok, []}
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

handle_info({udp, Socket, _IP, _RemotePortNo, _Packet} = Req, State) ->
    inet:setopts(Socket, [{active, once}]),
    case check_overload() of
        allow ->
            spawn(fun() -> handle_udp_packet(Req, State) end),
            {noreply, State};
        deny ->
            {noreply, State}
    end;
handle_info(Info, State) ->
    io:format("~p catchall: ~p, ~p~n", [?MODULE, Info, State]),
    {noreply, State}.

terminate(_Reason, _Library) -> ok.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

handle_udp_packet({udp, _Socket, _IP, _RemotePortNo, _Packet} = _Req, _State) ->
    io:format("~p~n",[self()]).

check_overload() ->
    N = erlang:system_info(process_count),
    T = erlang:system_info(process_limit),
    check_overload(N,T).
check_overload(N,T) when (N / T) < ?MAX_PROCESS_THRESHOLD ->
    allow;
check_overload(_,_) ->
    deny.

