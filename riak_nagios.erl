-module(riak_nagios).

-export([value/1]).

value(null) -> null;
value(String) ->
    %% The code below has a problem parsing pids, so we use the regular expression to wrap all pids in quotes.
    %% Also, we wrapped the pid in a {pid, "<X.Y.Z>"} tuple for easier querying later.
    S = string:concat(re:replace(String, "<[^>]*>", "{pid, \"&\"}", [global, {return, list}]), "."),
    {ok,Tokens,_} = erl_scan:string(S),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.