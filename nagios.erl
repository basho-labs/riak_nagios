-module(nagios).

-export([unknown/1, critical/1, warning/1, okay/1, decide/4]).

nagios(Header, Message, Code) ->
    io:format("~s: ~s~n", [Header, Message]),
    halt(Code).
   
unknown(Message) -> nagios("UNKNOWN", Message, 3).
critical(Message) -> nagios("CRITICAL", Message, 2).
warning(Message) -> nagios("WARNING", Message, 1).
okay(Message) -> nagios("OKAY", Message, 0).

decide(Message, Value, WarnThreshold, CriticalThreshold) ->
    if
        Value >= CriticalThreshold -> critical(Message);
        Value >= WarnThreshold -> warning(Message);
        Value >= 0 -> okay(Message);
        true -> unknown(Message)
    end.