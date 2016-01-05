-module(esyl_event_manager).
-export([start_link/0]).

start_link() ->
%%    {ok, Config} = esyl_config:parse("etc/syslog.conf"),
    io:format("Starting event manager~n"),
    {ok, Pid} = gen_event:start_link({local, esyl_logger}),
    
    gen_event:add_handler(esyl_logger, esyl_console_logger, []),
%%    gen_event:add_handler(esyl_logger, esyl_standard_logger, Config),

    {ok, Pid}.

