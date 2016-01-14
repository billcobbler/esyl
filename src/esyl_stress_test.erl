-module(esyl_stress_test).
-compile(export_all).

-define(LIMIT, 10).

start() ->
    {ok, Socket} = gen_udp:open(5145),
    spawn_many(?LIMIT, Socket),
    Socket.

stop(Socket) ->
    gen_udp:close(Socket).

spawn_many(0,_) -> ok;
spawn_many(N, Socket) -> 
    spawn(?MODULE,process,[Socket, 0]),
    spawn_many(N - 1, Socket).

process(Socket, N) when N < 100 ->
    gen_udp:send(Socket, {127,0,0,1}, 5144, "test"),
    process(Socket, N+1);
process(_,_) -> ok.
