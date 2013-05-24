-module(node).
-export([start/0, loop/1, start_gc_process/1]).
-record(node, {data,pids}).
-define(GC_CHANCE,  1).
-define(GC_TOTAL,  100).

start_gc_process(Cluster) ->
    Pid = spawn(fun() -> gc_main(Cluster) end),
    register(gc_process, Pid).

maybe_run_gc() ->
    RunGC = random:uniform(?GC_TOTAL),
    if  RunGC =< ?GC_CHANCE ->
            gc_process ! run;
        true -> no_gc
    end.

gc_main(Cluster) ->
    receive
        run ->
            io:format("GC!~n"),
            Counters = cluster:call(Cluster, get_raw),   % take all known deltas
            % io:format("Counters=~w~n", [Counters]),
            Merged = counter:counter_lub(Counters), % merge them into one, with all refs.
            % io:format("Merged=~w~n", [Merged]),

            Equivalent = counter:counter_new(counter:counter_value(Merged)),
            % io:format("Equivalent=~w~n", [Equivalent]),
            cluster:call(Cluster, {replace, Merged, Equivalent}) % replace deltas
        end,
    gc_main(Cluster).


loop(State) ->
    receive
        {cluster_info, Pids} ->
            loop(State#node{pids=Pids});

        print_raw ->
            Value = counter:counter_value(State#node.data),
            io:format("Value in ~w is ~w~n", [self(), Value]);

        {get_raw, Pid, Ref} ->
            Pid ! {Ref, State#node.data};

        {{replace, ToRemove, ToAdd}, Pid, Ref} ->
            Counter = State#node.data,
            NewValue = counter:counter_gc(Counter, ToRemove, ToAdd),
            Pid ! {Ref, ok},
            loop(State#node{data=NewValue});

        {incr, Delta} ->
            Counter = State#node.data, % extract counter
            Merge = counter:merge_fun(Counter), % select merge fun
            NewValue = Merge(Counter,Delta), % merge with delta
            maybe_run_gc(), % trigger GC with a given probability
            loop(State#node{data=NewValue})
    end,
    loop(State).

start() ->
    Counter = counter:counter_new(0),
    Pid = spawn(?MODULE, loop, [#node{data=Counter, pids=[]}]),
    io:format("node:started (pid=~w)~n", [Pid]),
    Pid.

