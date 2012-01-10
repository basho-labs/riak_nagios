#!/usr/bin/env escript

main(_) ->
    Status = string:tokens(os:cmd("riak ping"), "\n"),
    % io:format("~s~n", [Status]),
    case lists:any(fun(X) -> X == "pong" end, Status) of
        true -> nagios:okay("It's up!");
        "whatever\n" -> nagios:warning("whatever");
        _ -> nagios:critical("It's down!")
    end.
