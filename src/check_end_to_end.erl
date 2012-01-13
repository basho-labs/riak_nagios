-module(check_end_to_end).

-export([main/1]).
%%%%%%
%
% This is an end to end check for riak's http api, which runs the following workflow
%
% 1) HTTP PUT
% 2) Confirm PUT via HTTP GET
% 3) HTTP DELETE
% 4) Confirm DELETE via HTTP GET

%The _code functions below break down the output from the commands and isolates the HTTP return value (i.e. 200,204,404),
%  if there is no return value (occurs when there's a failure to connect) then they return the value 0.
return_code(StringList) ->
    http_code(StringList, null).
    
http_code([H|T], Code) ->
    case string:str(H, "< HTTP") of
        0 -> http_code(T, Code);
        _ -> http_code(T, string:sub_string(H, 12, 14))
    end;
http_code([], null) ->
    "0";
http_code([], Code) ->
    Code.

%This function takes the path that's being tested, and creates the three commands that need to be called for testing:
%  C1 = write, C2 = read, and C3 = delete
get_commands(Path) ->
    C1 = string:concat(string:concat("curl -v -X PUT -d \'Hello World\' -H \"content-type: text/plain\" ", Path), "/tb/tk"),
    C2 = string:concat(string:concat("curl -v ", Path), "/tb/tk"),
    C3 = string:concat(string:concat("curl -v -X DELETE ", Path), "/tb/tk"),
    {C1, C2, C3}.
    
main([Host, Port, Path]) ->
    %Concatenate the Path to be tested
    TestPath = string:concat(string:concat(string:concat(string:concat(string:concat("http://", Host), ":"), Port), "/"), Path),
    io:put_chars(string:concat(string:concat("Testing: ", TestPath), "\n")),

    %Get the three commands need to be run for writing, reading, and deleting
    {Cmd1, Cmd2, Cmd3} = get_commands(TestPath),

    %Test writing, reading, deleting, and reading (to confirm delete)
    {Stat1, _} = string:to_integer(return_code(string:tokens(os:cmd(Cmd1), "\n"))),
    if 
        Stat1 =/= 204 -> riak_nagios:critical("Write Failed"); 
        true -> true
    end,
    
    {Stat2, _} = string:to_integer(return_code(string:tokens(os:cmd(Cmd2), "\n"))),
    if  
        Stat2 =/= 200 -> riak_nagios:critical("First Read Failed");
        true -> true
    end,
    
    {Stat3, _} = string:to_integer(return_code(string:tokens(os:cmd(Cmd3), "\n"))),
    if
        Stat3 =/= 204 -> riak_nagios:critical("Delete Failed");
        true -> true
    end,
    
    {Stat4, _} = string:to_integer(return_code(string:tokens(os:cmd(Cmd2), "\n"))),
    if
        Stat4 =/= 404 -> riak_nagios:critical("Second Read Failed");
        true -> riak_nagios:okay("Test Passed")
    end;
main(_) ->
    usage().

usage() ->
    io:format("Usage: check_end_to_end.erl hostname port path~n"),
    io:format("hostname - hostname to be tested (most likely 127.0.0.1)~n"),
    io:format("port     - port to be tested~n"),
    io:format("path     - base path to riak~n"),
    riak_nagios:unknown("improper usage of check script").
