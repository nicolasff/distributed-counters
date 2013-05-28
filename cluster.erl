-module(cluster).
-export([start/1, weak_call/2, call/2]).
-record(cluster, {pids}).
-define(MSGDROP_CHANCE,   1).
-define(MSGDROP_TOTAL,  100).
-define(MSGDUP_CHANCE,    1).
-define(MSGDUP_TOTAL,   100).

% Create a cluster of N nodes, each running a gen_server
start(N) ->
    Pids = lists:map(fun(_) ->
                element(2, gen_server:start_link(node, [], []))
        end, lists:seq(1,N)),
    Cluster = #cluster{pids=Pids},
    gc:start(Cluster),
    Cluster.

% send an async message to the cluster, with a given probability
% of message loss for any node.
weak_call(Cluster, Msg) ->
    lists:map(fun(Pid) ->
        Drop = random:uniform(?MSGDROP_TOTAL),
        if  Drop =< ?MSGDROP_CHANCE -> fail;
            true -> send_at_least_once(Pid, Msg)
        end
    end, Cluster#cluster.pids).

send_at_least_once(Pid, Msg) ->
    gen_server:call(Pid, Msg), % send message once
    Dup = random:uniform(?MSGDUP_TOTAL),
    if Dup =< ?MSGDUP_CHANCE ->
            send_at_least_once(Pid, Msg); % re-deliver
        true -> done
    end.

% send a synchronous message to all nodes
call(Cluster, Msg) ->
    lists:map(fun(Pid) -> gen_server:call(Pid, Msg, infinity) end,
        Cluster#cluster.pids).
