%% check_riak_repl.erl
-module(check_riak_repl).

-export([main/1]).

%%%%%%
%
% Checks the status of Riak Repl with the riak-repl status command
%
% Parameters to the script documented in usage/0


%% This is how to handle the "Attempting to restart script through sudo -u riak" line
objects(["Attempting to restart script through sudo -u riak"|T]) ->
    objects(T);
objects(StringList) ->
    object_acc(StringList, {null, null}, []).
    
object_acc([H|T], {Name, Value}, Objects) ->
    case string:chr(H, $:) of
        0 -> object_acc(T, {Name, string:concat(Value, string:strip(H))}, Objects);
        Index -> {NewName, NewValue} = lists:split(Index, H),
            object_acc(T, {string:sub_string(NewName, 1, string:len(NewName)-1), string:strip(NewValue)}, [{Name, riak_nagios:value(Value)}|Objects])
    end;
object_acc([], {Name, Value}, Objects) ->
    [{Name, riak_nagios:value(Value)}|Objects].
    
main([Type, Site]) ->
    Status = objects(string:tokens(os:cmd("riak-repl status"), "\n")),
    Stats = find_stats(Type, Site, Status),
    {state, ConnectionState} = Stats,
    case ConnectionState of
        % client
        disconnected    -> riak_nagios:critical("Disconnected"); 
        connecting      -> riak_nagios:critical("Connecting");
        wait_peerinfo   -> riak_nagios:okay("Wait PeerInfo");
        merkle_recv     -> riak_nagios:okay("Merkle Recv");
        merkle_diff     -> riak_nagios:okay("Merkle Diff");
        merkle_exchange -> riak_nagios:okay("Merkle Exchange");
        
        %server
        send_peerinfo   -> riak_nagios:okay("send_peerinfo");
        %wait_peerinfo   -> riak_nagios:okay("wait_peerinfo");
        merkle_send     -> riak_nagios:okay("merkle_send");
        merkle_build    -> riak_nagios:okay("merkle_build");
        merkle_xfer     -> riak_nagios:okay("merkle_xfer");
        merkle_wait_ack -> riak_nagios:okay("merkle_wait_ack");
        %merkle_diff     -> riak_nagios:okay("merkle_diff");  % <-- client already defines this one.
        connected       -> riak_nagios:okay("connected");
        server_not_found -> riak_nagios:critical("Server Not Found");
        Response -> riak_nagios:unknown(string:concat("I don't know what to make of ", Response))
    end;
main(_) ->
    usage().
    
usage() ->
    io:format("Usage: check_riak_repl.erl [server|client] [sitename]~n"),
    io:format("server|client - role performed by this node~n"),
    io:format("sitename      - the name of the replication site~n"),
    riak_nagios:unknown("improper usage of check script").
    
find_stats("client", Site, Status) ->
    find_stats("client_stats", Site, Status);
find_stats("server", Site, Status) ->
    find_stats("server_stats", Site, Status);
find_stats(StatKey, Site, Status) ->
    {StatKey, Stats} = lists:keyfind(StatKey, 1, Status),
    ListOfStats = [X || {{pid, _}, {message_queue_len, _}, {status, X}} <- Stats],
    Stat = [lists:keyfind(state, 1, S) || S <- ListOfStats, lists:keyfind(Site, 2, S) =/= false],
    case Stat of
        [] -> {state, server_not_found};
        _ -> lists:nth(1, [lists:keyfind(state, 1, S) || S <- ListOfStats, lists:keyfind(Site, 2, S) =/= false])
    end.
