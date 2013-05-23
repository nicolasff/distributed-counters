-module(cluster).
-export([start/1, weak_cast/2, call/2]).
-record(cluster, {pids}).
-define(MSGDROP_CHANCE,   1).
-define(MSGDROP_TOTAL,  100).

start(N) ->
    Pids = lists:map(fun(_) -> node:start() end, lists:seq(1,N)),
    lists:map(fun(P) -> P ! {cluster_info, Pids} end, Pids),
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

    lists:map(fun(Pid) ->
        Pid ! {Msg, self()},
        receive Value -> Value end
     end, Cluster#cluster.pids).

