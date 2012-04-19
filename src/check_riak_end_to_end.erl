-module(check_riak_end_to_end).

-export([main/1]).
%%%%%%
%
% This is an end to end check for riak's http api, which runs the following workflow
%
% 1) HTTP PUT
% 2) Confirm PUT via HTTP GET
% 3) HTTP DELETE
% 4) Confirm DELETE via HTTP GET


main([Host, Port, Path]) ->
    %Concatenate the Path to be tested
    TestPath = "http://" ++ Host ++ ":" ++ Port ++ "/" ++ Path ++ "/tb/tk",
    % io:put_chars(string:concat(string:concat("Testing: ", TestPath), "\n")),

    %Create the two requests that need to be called for testing:
    %  R1 = write, R2 = read and delete
    R1 = {TestPath, [], "text/plain", "Hello World"},
    R2 = {TestPath, []},

    %Test writing, reading, deleting, and reading (to confirm delete)
    inets:start(),

    case test(put, R1, 204) of
        ok -> ok;
        _  -> riak_nagios:critical("Write Failed")
    end,
    
    timer:sleep(500),

    case test(get, R2, 200) of
        ok -> ok;
        _  -> riak_nagios:critical("First Read Failed")
    end,
    
    timer:sleep(500),

    case test(delete, R2, 204) of
        ok -> ok;
        _  -> riak_nagios:critical("Delete Failed")
    end,
    
    timer:sleep(500),

    case test(get, R2, 404) of
        ok -> riak_nagios:okay("Test Passed");
        _  -> riak_nagios:critical("Second Read Failed")
    end,

    inets:stop();

main(_) ->
    usage().

test(M,R,S) -> test(M,R,S,5).
test(M,R,S,N) -> 
    case httpc:request(M, R, [], []) of
        {ok,{{_,S,_},_,_}} -> ok;
        _ ->
            case N of
                5 -> timer:sleep(500) , test(M,R,S,N-1);
                4 -> timer:sleep(1000), test(M,R,S,N-1);
                3 -> timer:sleep(2000), test(M,R,S,N-1);
                2 -> timer:sleep(4000), test(M,R,S,N-1);
                _ -> fail
            end
    end.

usage() ->
    io:format("Usage: check_end_to_end.erl hostname port path~n"),
    io:format("hostname - hostname to be tested (most likely 127.0.0.1)~n"),
    io:format("port     - port to be tested~n"),
    io:format("path     - base path to riak~n"),
    riak_nagios:unknown("improper usage of check script").