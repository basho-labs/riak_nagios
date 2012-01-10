#!/usr/bin/env escript

value(null) -> null;
value(String) ->
    S = string:concat(String, "."),
    io:format("~s~n", [S]),
    {ok,Tokens,_} = erl_scan:string(S),
    io:format("~w~n", [Tokens]),
    % filter pids : {'<',1},{float,1,7765.625},{'.',1},{integer,1,0},{'>',1},{',',1}
    
    io:format("~w~n", [Tokens]),
    {ok,Term} = erl_parse:parse_term(Tokens),
    io:format("~w~n", [Term]),
    Term.

%% How to handle the "Attempting to restart script through sudo -u riak" line?
objects(["Attempting to restart script through sudo -u riak"|T]) ->
    objects(T);
objects(StringList) ->
    object_acc(StringList, {null, null}, []).
    
object_acc([H|T], {Name, Value}, Objects) ->
    %io:format("~s~n", [H]),
    case string:chr(H, $:) of
        0 -> object_acc(T, {Name, string:concat(Value, string:strip(H))}, Objects);
        Index -> {NewName, NewValue} = lists:split(Index, H),
            object_acc(T, {NewName, string:strip(NewValue)}, [{Name, value(Value)}|Objects])
    end;
object_acc([], Current, Objects) ->
    [Current|Objects].
    
main(_) ->
    Status = objects(string:tokens(os:cmd("riak-repl status"), "\n")),
    io:format("~w~n", [Status]).
    
    %% each line that has a (:), parse value following the colon, until the next line with a colon, or end.
    
