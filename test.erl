-module(test).
-export([main/0, test_simple_merge/0]).
-define(CTR_MIN, -1000000).
-define(CTR_MAX,  1000000).

main() ->
    NodeCount = 3,
    MsgCount = 10000,
    run(ctr_sum, MsgCount, NodeCount),
    run(ctr_min, MsgCount, NodeCount),
    run(ctr_max, MsgCount, NodeCount),
    run(ctr_avg, MsgCount, NodeCount),

    init:stop(0).

incr_counter(Cluster, CounterModule, I, Delta) ->
    if  I rem 1000 == 0 -> io:format("Sent ~w messages~n", [I]);
        true -> ok
    end,
    Counter = counter:new(CounterModule, Delta), % create new counter
    cluster:weak_call(Cluster, {incr, Counter}). % send to all with probabt. of loss

get_summaries(Cluster) ->
    Counters = cluster:call(Cluster, get_raw),
    lists:map(fun counter:value/1, Counters).

local_reduce(CounterModule, Deltas) ->
    counter:value(merge_all(CounterModule, lists:map(
            fun(Delta) -> counter:new(CounterModule, Delta)
            end, Deltas))).

random_integers(N) ->
    lists:map(fun(_) ->
            ?CTR_MIN + random:uniform(?CTR_MAX - ?CTR_MIN) end,
        lists:seq(1, N)).


% Generate the merged version of several counters
merge_all(CounterModule, [H|T]) ->
    Empty = counter:bottom(CounterModule),
    lists:foldr(fun counter:merge/2, Empty, [H|T]).

run(CounterModule, MsgCount, NodeCount) ->
    random:seed(now()),

    % start cluster
    io:format("~n==============================~n"),
    io:format("Testing ~w on ~w nodes~n", [CounterModule, NodeCount]),
    Cluster = cluster:start(NodeCount, CounterModule),

    % send "Cluster" record to all nodes.
    cluster:call(Cluster, {set_cluster, Cluster}),

    % send deltas
    Deltas = random_integers(MsgCount),
    lists:map(fun({I, Delta}) ->
                incr_counter(Cluster, CounterModule, I, Delta)
        end, lists:zip(lists:seq(1, MsgCount), Deltas)),

    % collect answers
    io:format("Expected value is ~w~n", [local_reduce(CounterModule, Deltas)]),

    io:format("Gettting full counters on all nodes...~n"),
    Counters = cluster:call(Cluster, get_raw),
    Resolved = merge_all(CounterModule, Counters),
    io:format("Value of resolved counters: ~w~n",
        [counter:value(Resolved)]),

    io:format("Counter according to each node: ~w~n", [get_summaries(Cluster)]),
	io:format("Waiting a second before running GC...~n"),
    timer:sleep(1100), % wait a bit so that GC can pick up all increments
    io:format("Trigger GC~n"),
    cluster:gc_run(Cluster),
    timer:sleep(1000), % wait for GC to return
    io:format("Counter according to each node: ~w~n", [get_summaries(Cluster)]),

    done.

test_simple_merge() ->
    A = counter:new(ctr_sum, 12),
    B = counter:new(ctr_sum, 34),
    C = counter:merge(A,B),

    io:format("A=~w~n", [A]),
    io:format("B=~w~n", [B]),
    io:format("C=~w~n", [C]),

    % wait a bit
    timer:sleep(1100),

    GcInfo = lists:map(fun counter:gc_info/1, [A,B]),
    io:format("GcInfo=~w~n", [GcInfo]),

    A1 = counter:gc_merge(A, GcInfo),
    B1 = counter:gc_merge(B, GcInfo),
    C1 = counter:gc_merge(C, GcInfo),

    io:format("A1=~w~n", [A1]),
    io:format("B1=~w~n", [B1]),
    io:format("C1=~w~n", [C1]),

    init:stop(0).
