-module(test).
-export([main/0]).


incr_counter(C, I) ->
    if
        I rem 1000 == 0 ->
          io:format("Sent ~w messages~n", [I]);
        true -> ok
    end,
    Counter = counter:counter_new(I),
    % io:format("Send message ~w to Cluster ~w~n", [Counter, C]),
    cluster:weak_cast(C, {incr, Counter}).

main() -> run(3).

run(NodeCount) ->
    random:seed(now()),

    io:format("hello, world!, NodeCount=~w~n", [NodeCount]),
    C = cluster:start(NodeCount),
    Msgs = 2000,

    % send deltas
    Deltas = lists:seq(1,Msgs),
    lists:map(fun(I) -> incr_counter(C, I) end, Deltas),

    % collect answers
    io:format("Expected value is ~w~n", [lists:sum(Deltas)]),

    io:format("Gettting full counters on all nodes...~n"),
    Counters = cluster:call(C, get_raw),
    Resolved = counter:counter_lub(Counters),
    io:format("Value of resolved counters: ~w~n",
        [counter:counter_value(Resolved)]),

    io:format("Print sum according to each node:~n"),
    cluster:weak_cast(C, print_raw),

    io:format("Trigger GC~n"),
    gc_process ! run,

    receive after 1000 -> ok end,
    
    io:format("Print sum according to each node:~n"),
    cluster:weak_cast(C, print_raw),

    bye.

