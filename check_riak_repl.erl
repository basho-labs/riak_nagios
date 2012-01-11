#!/usr/bin/env escript

value(null) -> null;
value(String) ->
    %% The code below has a problem parsing pids, so we use the regular expression to wrap all pids in quotes.
    %% Also, we wrapped the pid in a {pid, "<X.Y.Z>"} tuple for easier querying later.
    S = string:concat(re:replace(String, "<[^>]*>", "{pid, \"&\"}", [global, {return, list}]), "."),
    {ok,Tokens,_} = erl_scan:string(S),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

%% This is how to handle the "Attempting to restart script through sudo -u riak" line
objects(["Attempting to restart script through sudo -u riak"|T]) ->
    objects(T);
objects(StringList) ->
    object_acc(StringList, {null, null}, []).
    
object_acc([H|T], {Name, Value}, Objects) ->
    case string:chr(H, $:) of
        0 -> object_acc(T, {Name, string:concat(Value, string:strip(H))}, Objects);
        Index -> {NewName, NewValue} = lists:split(Index, H),
            object_acc(T, {string:sub_string(NewName, 1, string:len(NewName)-1), string:strip(NewValue)}, [{Name, value(Value)}|Objects])
    end;
object_acc([], {Name, Value}, Objects) ->
    [{Name, value(Value)}|Objects].
    
main([Type, Site]) ->
    Status = objects(string:tokens(os:cmd("riak-repl status"), "\n")),
    Stats = find_stats(Type, Site, Status),
    {state, ConnectionState} = Stats,
    case ConnectionState of
        % client
        disconnected    -> nagios:critical("Disconnected"); 
        connecting      -> nagios:critical("Connecting");
        wait_peerinfo   -> nagios:okay("Wait PeerInfo");
        merkle_recv     -> nagios:okay("Merkle Recv");
        merkle_diff     -> nagios:okay("Merkle Diff");
        merkle_exchange -> nagios:okay("Merkle Exchange");
        
        %server
        send_peerinfo   -> nagios:okay("send_peerinfo");
        wait_peerinfo   -> nagios:okay("wait_peerinfo");
        merkle_send     -> nagios:okay("merkle_send");
        merkle_build    -> nagios:okay("merkle_build");
        merkle_xfer     -> nagios:okay("merkle_xfer");
        merkle_wait_ack -> nagios:okay("merkle_wait_ack");
        %merkle_diff     -> nagios:okay("merkle_diff");  % <-- client already defines this one.
        connected       -> nagios:okay("connected");
        server_not_found -> nagios:critical("Server Not Found");
        Response -> nagios:unknown(string:concat("I don't know what to make of ", Response))
    end.
    
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
