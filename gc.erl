-module(gc).
-export([entry_point/0]).

entry_point() ->
    receive
        Cluster -> loop(Cluster)
    end.

loop(Cluster) ->
    receive
        run ->
            Ref = make_ref(), % run actual GC in a separate process
            spawn(fun() -> gc_run(Cluster, Ref) end),
            wait(Ref) % wait for completion
        end,
    loop(Cluster).

gc_run(Cluster, Ref) ->
    GcInfo = cluster:call(Cluster, get_raw_for_gc),   % take all known deltas
    UniqId = make_ref(), % FIXME: it's not great to define it here.
    cluster:call(Cluster, {perform_gc, GcInfo, UniqId}), % replace deltas
    cluster:gc_pid(Cluster) ! Ref. % signal back

wait(Ref) ->
    receive
        Ref -> done;
        _   -> wait(Ref) % skip other messages sent to the GC process
    end.
