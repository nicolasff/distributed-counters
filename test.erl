-module(test).
-export([main/0]).

main() ->
    run(3).

incr_counter(C, I) ->
    if  I rem 1000 == 0 -> io:format("Sent ~w messages~n", [I]);
        true -> ok
    end,
    Counter = counter:counter_new(I), % create new counter with value "+I"
    cluster:weak_call(C, {incr, Counter}). % send to all with probabt. of loss

get_summaries(C) ->
    Counters = cluster:call(C, get_raw),
    lists:map(fun counter:counter_value/1, Counters).

run(NodeCount) ->
    random:seed(now()),

    io:format("hello, world!, NodeCount=~w~n", [NodeCount]),
    C = cluster:start(NodeCount),
    Msgs = 10000,

    % send deltas
    Deltas = lists:seq(1,Msgs),
    lists:map(fun(I) -> incr_counter(C, I) end, Deltas),

    % collect answers
    io:format("Expected value is ~w~n", [lists:sum(Deltas)]),

    io:format("Gettting full counters on all nodes...~n"),
    Counters = cluster:call(C, get_raw),
    Resolved = counter:counter_merge_all(Counters),
    io:format("Value of resolved counters: ~w~n",
        [counter:counter_value(Resolved)]),

    io:format("Sum according to each node: ~w~n", [get_summaries(C)]),

    timer:sleep(1100), % wait a bit so that GC can pick up all increments
    io:format("Trigger GC~n"),
    gc:run(),
    timer:sleep(1000), % wait for GC to return
    
    io:format("Sum according to each node: ~w~n", [get_summaries(C)]),

    init:stop(0).
