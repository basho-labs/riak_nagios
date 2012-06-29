-module(check_riak_repl).

-export([run/2]).

run(Options, _NonOptArgs) ->
    Node = proplists:get_value(node, Options),
    Checks = check_repl(Node),
    case lists:member(critical, Checks) of
        true ->
            critical();
        false ->
            case lists:member(warning, Checks) of
                true ->
                    warning();
                false ->
                    okay()
            end
    end.


check_repl(Node) ->
    case is_leader(Node) of
        true ->
            Clients = check_repl(Node, {riak_repl_client_sup, riak_repl_tcp_client}),
            Servers = check_repl(Node, {riak_repl_server_sup, riak_repl_tcp_server}),
            Clients ++ Servers;
        false ->
            []
    end.

check_repl(Node, {Sup, Mod}) ->
    Pids = repl_pids(Node, Sup),
    [check_repl_pid(Node, Mod, Pid) || Pid <- Pids].

repl_pids(Node, Sup) ->
    Nodes = riak_nodes(Node),
    {ResL, _} = rpc:multicall(Nodes, supervisor, which_children, [Sup]),
    Children = lists:flatten([Res || Res <- ResL, filter_badrpc(Res)]),
    [Pid || {_,Pid,_,_} <- Children, Pid /= undefined].

check_repl_pid(Node, Mod, Pid) ->
    case rpc:call(Node, Mod, status, [Pid]) of
        {status, SiteStatus} ->
            {site, _Site} = lists:keyfind(site, 1, SiteStatus),
            case check_repl_socket(Node, Mod, Pid, SiteStatus) of
                ok ->
                    ok;
                {error, _Reason} ->
                    warning
            end;
        _Reply ->
            critical
    end.

check_repl_socket(_Node, Mod, Pid, SiteStatus) ->
    case is_connected(Mod, SiteStatus) of
        true ->
            %% http://www.erlang.org/doc/man/sys.html#get_status-1
            Status = sys:get_status(Pid),
            {status, _Pid, {module, gen_server}, SItem} = Status,
            [_PDict, _SysState, _ParentPid, _Dbg, Misc] = SItem,
            [State] = [State || {data,[{"State", State}]} <- Misc],
            Socket = get_repl_socket_from_state(Mod, State),
            case send_keepalive(Mod, Socket) of
                ok ->
                    ok;
                {error, Reason} ->
                    Pid ! {tcp_error, Socket, Reason},
                    {error, Reason}
            end;
        false ->
            ok
    end.

%% Depends on the internal `state` record
get_repl_socket_from_state(riak_repl_tcp_client, State) ->
    %% riak_repl_tcp_client.erl
    element(5, State);

get_repl_socket_from_state(riak_repl_tcp_server, State) ->
    %% riak_repl_tcp_server.erl
    element(3, State).

send_keepalive(Mod, Socket) ->
    Node = erlang:node(Socket),
    rpc:call(Node, Mod, send, [Socket, keepalive]).

is_leader(Node) ->
    rpc:call(Node, riak_repl_leader, leader_node, []) == Node.

riak_nodes(Node) ->    
    rpc:call(Node, riak_core_node_watcher, nodes, [riak_kv]).

filter_badrpc({badrpc, _}) -> false;
filter_badrpc(_) -> true.

is_connected(riak_repl_tcp_server, _Status) ->
    true;

is_connected(riak_repl_tcp_client, Status) ->
    case lists:keyfind(connected, 1, Status) of
        {connected, _, _} ->
            true;
        _ ->
            false
    end.

critical() ->
    {critical, "Unexpected return from status", []}.

warning() ->
    {warning, "Socket errors were detected on some replication connections. These errors have been logged on the Riak node. No action is required", []}.

okay() ->
    {ok, "Replication links working correctly.", []}.
