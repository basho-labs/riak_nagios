#!/usr/bin/env escript
get_property(Prop, [H|StringList]) ->
    case string:str(H, Prop) of
        1 -> {_, Value} = lists:split(string:len(Prop) + 3, H),
             riak_nagios:value(Value);
        _ -> get_property(Prop, StringList)
    end;
get_property(_Prop, []) -> null.

main([Property, Warn, Critical]) ->
    WarnThreshold = riak_nagios:value(Warn),
    CriticalThreshold = riak_nagios:value(Critical),
    
    Status = string:tokens(os:cmd("riak-admin status"), "\n"),
    case Property of
        "memory" -> nagios:decide(
            "Memory", 
            get_property("mem_allocated", Status) / get_property("mem_total", Status), 
            WarnThreshold, 
            CriticalThreshold);
        "siblings" -> nagios:decide(
            "Siblings", 
            get_property("node_get_fsm_siblings_mean", Status), 
            WarnThreshold, 
            CriticalThreshold)
        %% TODO: Tx/Rx within the cluster (GET/PUT fsms)
    end.