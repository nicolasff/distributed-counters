-module(cluster).
-export([new/1, start/1, weak_cast/2, call/2, cast/2]).
-record(cluster, {pids}).
-define(MSGDROP_CHANCE,   1).
-define(MSGDROP_TOTAL,  100).

% Create a cluster of N nodes, each running a gen_server
start(N) ->
    Pids = lists:map(fun(_) ->
                {ok, Pid} = gen_server:start_link(node, [], []),
                Pid
        end, lists:seq(1,N)),
    Cluster = cluster:new(Pids),
    gc:start(Cluster),
    Cluster.

new(Pids) ->
    #cluster{pids=Pids}.

% send an async message to the cluster, with a given probability
% of message loss for any node.
weak_cast(Cluster, Msg) ->
    lists:map(fun(Pid) ->
        Drop = random:uniform(?MSGDROP_TOTAL),
        if  Drop =< ?MSGDROP_CHANCE ->
                io:format("Failed delivery of msg to ~w~n", [Pid]),
                fail;
            true -> 
                gen_server:cast(Pid, Msg)
        end
    end, Cluster#cluster.pids).

% send a synchronous message to all nodes
call(Cluster, Msg) ->
    lists:map(fun(Pid) ->
        gen_server:call(Pid, Msg)
     end, Cluster#cluster.pids).

% send an asynchronous message to all nodes
cast(Cluster, Msg) ->
    lists:map(fun(Pid) ->
        gen_server:cast(Pid, Msg)
     end, Cluster#cluster.pids).

