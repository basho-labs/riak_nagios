#!/usr/bin/env escript

main(_) ->
    Status = string:tokens(os:cmd("riak ping"), "\n"),
    case lists:any(fun(X) -> X == "pong" end, Status) of
        true -> nagios:okay("It's up!");
        _ -> nagios:critical("It's down!")
    end.
