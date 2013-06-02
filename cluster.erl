-module(cluster).
-export([start/2, weak_call/2, call/2, gc_pid/1, gc_maybe_run/1, gc_run/1]).
-record(cluster, {nodes, gc_pid}).
-define(GC_CHANCE,      0.1).
-define(MSGDROP_CHANCE, 0.01).
-define(MSGDUP_CHANCE,  0.01).

% Create a cluster of N nodes, each running a gen_server
start(N, CounterModule) ->
    Pids = lists:map(fun(_) ->
                element(2, gen_server:start_link(node, [CounterModule], []))
        end, lists:seq(1,N)),
    Cluster = #cluster{nodes=Pids},
    start_gc(Cluster).

start_gc(Cluster) ->
	GcPid = spawn(gc, entry_point, []), % start GC process
    NewCluster = Cluster#cluster{gc_pid=GcPid}, % update cluster record
	GcPid ! NewCluster, % give full cluster info to GC process
	NewCluster.

gc_pid(Cluster) ->
	Cluster#cluster.gc_pid.

% run GC with a given probability
gc_maybe_run(Cluster) ->
    RunGC = random:uniform(),
    if  RunGC =< ?GC_CHANCE -> gc_run(Cluster);
        true -> no_gc
    end.

gc_run(Cluster) ->
	gc_pid(Cluster) ! run.

% send an async message to the cluster, with a given probability
% of message loss for any node.
weak_call(Cluster, Msg) ->
    lists:map(fun(Pid) ->
        Drop = random:uniform(),
        if  Drop =< ?MSGDROP_CHANCE -> fail;
            true -> send_at_least_once(Pid, Msg)
        end
    end, Cluster#cluster.nodes).

send_at_least_once(Pid, Msg) ->
    gen_server:call(Pid, Msg), % send message once
    Dup = random:uniform(),
    if Dup =< ?MSGDUP_CHANCE ->
            send_at_least_once(Pid, Msg); % re-deliver
        true -> done
    end.

% send a synchronous message to all nodes
call(Cluster, Msg) ->
    lists:map(fun(Pid) -> gen_server:call(Pid, Msg, infinity) end,
        Cluster#cluster.nodes).
