-module(check_riak_up).

-export([main/1]).
%%%%%%
%
% Checks if riak is up using riak ping

main(_) ->
    Status = string:tokens(os:cmd("riak ping"), "\n"),
    case lists:any(fun(X) -> X == "pong" end, Status) of
        true -> riak_nagios:okay("It's up!");
        _ -> riak_nagios:critical("It's down!")
    end.
