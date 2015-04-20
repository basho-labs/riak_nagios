-module(check_port_count).

-export([run/2]).

%% pass check level options after "--"
%% check_node file_handle_count -- --warning_threshold=50
run(Options, NonOptArgs) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, NonOptArgs) of
        {ok, {CmdOptions, _}} ->
            run_cmd(Options, CmdOptions);
        {error, {Reason, Data}} ->
            {unknown, "~s ~p", [Reason, Data]}
    end.

run_cmd(Options, CmdOptions) ->
    Node = proplists:get_value(node, Options),
    Critical = proplists:get_value(critical, CmdOptions),
    Warning = proplists:get_value(warning, CmdOptions),
    case catch rpc:call(Node, erlang, ports, []) of
        L when is_list(L) ->
            Count = length(L),
            Msg = "~B ports in use",
            if
                Count >= Critical -> {critical, Msg, [Count]};
                Count >= Warning -> {warning, Msg, [Count]};
                true -> {ok, Msg, [Count]}
            end;
        Err ->
            {warning,"Error retrieving port count: ~p",Err}
    end.


option_spec_list() ->
    [
    %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
    {warning, undefined, "warning_threshold", {integer, 32768}, "Warning threshold"},
    {critical, undefined, "critical_threshold", {integer, 49152}, "Critical threshold"}
    ].

