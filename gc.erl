-module(gc).
-export([start/1, maybe_run/0]).
-define(GC_CHANCE,    1).
-define(GC_TOTAL,  1000).
-define(GC_PROCESS_NAME, gc_process).

start(Cluster) ->
    Pid = spawn(fun() -> gc_loop(Cluster) end),
    register(?GC_PROCESS_NAME, Pid).

% run GC with a given probability
maybe_run() ->
    RunGC = random:uniform(?GC_TOTAL),
    if  RunGC =< ?GC_CHANCE ->
            ?GC_PROCESS_NAME ! run;
        true -> no_gc
    end.

gc_loop(Cluster) ->
    receive
        run ->
            Ref = make_ref(), % run actual GC in a separate process
            spawn(fun() -> gc_run(Cluster, Ref) end),
            gc_wait(Ref), % wait for completion
            ok
        end,
    gc_loop(Cluster).

gc_run(Cluster, Ref) ->
    Counters = cluster:call(Cluster, get_raw_for_gc),   % take all known deltas
    Merged = counter:counter_merge_all(Counters), % merge them into one, keeping all refs.
    Equivalent = counter:counter_new(counter:counter_value(Merged)),
    cluster:call(Cluster, {replace, Merged, Equivalent}), % replace deltas
    ?GC_PROCESS_NAME ! Ref. % signal back

gc_wait(Ref) ->
    receive
        Ref -> done;
        _   -> gc_wait(Ref) % skip messages sent to the GC process whilst it's waiting for completion
    end.
