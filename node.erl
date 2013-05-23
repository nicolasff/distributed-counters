-module(node).
-export([start/0, loop/1]).
-record(node, {data,pids}).


loop(State) ->
    receive
        {cluster_info, Pids} ->
            loop(State#node{pids=Pids});

        print_raw ->
            Value = counter:counter_value(State#node.data),
            io:format("Value in ~w is ~w~n", [self(), Value]);

        {get_raw, Pid} ->
            Pid ! State#node.data;

        {incr, Delta} ->
            % io:format("got incr ~w~n", [Delta]),
            Counter = State#node.data,
            % io:format("Counter = ~w~n", [Counter]),
            Merge = counter:merge_fun(Counter),
            % io:format("Merge = ~w~n", [Merge]),
            loop(State#node{data=Merge(Counter,Delta)})
    end,
    loop(State).

start() ->
    Counter = counter:counter_new(0),
    Pid = spawn(?MODULE, loop, [#node{data=Counter, pids=[]}]),
    io:format("node:started (pid=~w)~n", [Pid]),
    Pid.

