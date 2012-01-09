#!/usr/bin/env escript

nagios(Header, Message, Code) ->
    io:format("~s: ~s~n", [Header, Message]),
    halt(Code).
    
critical(Message) -> nagios("CRITICAL", Message, 2).
warning(Message) -> nagios("WARNING", Message, 1).
okay(Message) -> nagios("OKAY", Message, 0).

main(_) ->
    Status = string:tokens(os:cmd("riak ping"), "\n"),
    io:format("~s~n", [Status]),
    case lists:any(fun(X) -> X == "pong" end, Status) of
        true -> okay("It's up!");
        "whatever\n" -> warning("whatever");
        _ -> critical("It's down!")
    end.
