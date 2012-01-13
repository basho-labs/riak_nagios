#!/usr/bin/env escript

main(_) ->
    {ok, _, BeamCode} = compile:file("src/riak_nagios.erl", [binary, debug_info]),
    escriptize("check_riak_repl", BeamCode),
    escriptize("check_riak_admin", BeamCode),
    escriptize("check_end_to_end", BeamCode),
    escriptize("check_riak_up", BeamCode).
        
escriptize(Filename, BeamCode) ->
    {ok, _, ScriptBeamCode} = compile:file("src/" ++ Filename ++ ".erl", [binary, debug_info]),
    escript:create("ebin/" ++ Filename ++ ".escript",
                     [shebang,
                     {archive, [{Filename ++ ".beam", ScriptBeamCode},
                                {"riak_nagios.beam", BeamCode}], []}]).