-module(node).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([start_gc_process/1, maybe_run_gc/0]).
-record(node, {data}).
-define(GC_CHANCE,   1).
-define(GC_TOTAL, 1000).

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
            Merged = counter:counter_lub(Counters), % merge them into one, with all refs.
            Equivalent = counter:counter_new(counter:counter_value(Merged)),
            cluster:cast(Cluster, {replace, Merged, Equivalent}) % replace deltas
        end,
    gc_main(Cluster).


init(_Args) ->
    Counter = counter:counter_new(0),
    State = #node{data=Counter},
    {ok, State}.


handle_call(get_raw, _From, State) ->
    {reply, State#node.data, State}.


handle_cast({replace, ToRemove, ToAdd}, State) ->
    Counter = State#node.data, % extract counter
    NewValue = counter:counter_gc(Counter, ToRemove, ToAdd), % replace some, add some.
    NewState = State#node{data=NewValue},
    {noreply, NewState};

handle_cast({incr, Delta}, State) ->
    Counter = State#node.data, % extract counter
    Merge = counter:merge_fun(Counter), % select merge fun
    NewValue = Merge(Counter,Delta), % merge with delta
    maybe_run_gc(), % trigger GC with a given probability
    NewState = State#node{data=NewValue},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
