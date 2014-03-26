-module(check_connection_pools).

-export([run/2]).

%% pass check level options after "--"
%% check_node file_handle_count -- --request_warning_threshold=64
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
    {_, _, _, Requests} = rpc:call(Node, poolboy, status, [request_pool]),
    {_, _, _, BucketLists} = rpc:call(Node, poolboy, status, [bucket_list_pool]), 
    RequestCritical = proplists:get_value(request_critical, CmdOptions),
    RequestWarning = proplists:get_value(request_warning, CmdOptions),
    BucketCritical = proplists:get_value(bucket_critical, CmdOptions),
    BucketWarning = proplists:get_value(bucket_warning, CmdOptions),
    RequestMsg = "~B request poolboy workers in use",
    BucketMsg = "~B bucket listing poolboy workers in use",
    BothMsg = "~B request workers in use; ~B bucket workers in use",
    if
        Requests >= RequestCritical, BucketLists >= BucketCritical -> {critical, BothMsg, [Requests, BucketLists]};
        Requests >= RequestCritical -> {critical, Msg, [Requests]};
        BucketLists >= BucketCritical -> {critical, Msg, [BucketLists]};
        Requests >= RequestWarning, BucketLists >= BucketWarning -> {warning, BothMsg, [Requests, BucketLists]};
        Requests >= RequestWarning -> {warning, Msg, [Requests]};
        BucketLists >= BucketWarning -> {warning, Msg, [BucketLists]};
        true -> {ok, Msg, [Count]}
    end;


option_spec_list() ->
    [
    %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
    {request_warning, undefined, "request_warning_threshold", {integer, 64}, "Request Warning threshold"},
    {request_critical, undefined, "request_critical_threshold", {integer, 96}, "Request Critical threshold"},
    {bucket_warning, undefined, "bucket_warning_threshold", {integer, 8}, "Bucket Warning threshold"},
    {bucket_critical, undefined, "bucket_critical_threshold", {integer, 12}, "Bucket Critical threshold"}
    ].
