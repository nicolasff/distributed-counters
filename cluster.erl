-module(cluster).
-export([new/1, start/1, weak_cast/2, call/2]).
-record(cluster, {pids}).
-define(MSGDROP_CHANCE,   1).
-define(MSGDROP_TOTAL,  100).

start(N) ->
    Pids = lists:map(fun(_) -> node:start() end, lists:seq(1,N)),
    Cluster = cluster:new(Pids),
    lists:map(fun(Pid) -> Pid ! {cluster_info, Pids} end, Pids),
    node:start_gc_process(Cluster),
    Cluster.

new(Pids) ->
    #cluster{pids=Pids}.


weak_cast(Cluster, Msg) ->
    lists:map(fun(Pid) ->
        Drop = random:uniform(?MSGDROP_TOTAL),
        if  Drop =< ?MSGDROP_CHANCE ->
                io:format("Failed delivery of msg ~w to ~w~n", [Msg, Pid]),
                fail;
            true -> 
                Pid ! Msg
        end
    end, Cluster#cluster.pids).

call(Cluster, Msg) ->
    Ref = make_ref(),
    lists:map(fun(Pid) ->
        Pid ! {Msg, self(), Ref},
        receive {Ref, Value} -> Value end
     end, Cluster#cluster.pids).

