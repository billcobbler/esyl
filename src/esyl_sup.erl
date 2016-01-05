-module(esyl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    io:format("Starting supervisor~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, Port} = application:get_env(esyl, port),
    
    Policy = {one_for_one, 1, 60},
    EventManager = {esyl_event_manager, 
                        {esyl_event_manager, start_link, []},
                        permanent, brutal_kill, worker, dynamic},
    Server = {esyl_server, 
                {esyl_server, start_link, [Port]},
                permanent, brutal_kill, worker, [esyl_server]},

    {ok, {Policy, [EventManager, Server]}}.
