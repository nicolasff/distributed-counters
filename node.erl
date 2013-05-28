-module(node).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-record(node, {data}).
-define(GC_COUNT_LIMIT, 10000).

init(_Args) ->
    Counter = counter:counter_new(0),
    State = #node{data=Counter},
    {ok, State}.


handle_call(get_raw_for_gc, _From, State) ->
    Counter = State#node.data, % extract counter
    Reply = counter:counter_remove_recent(Counter, ?GC_COUNT_LIMIT),
    {reply, Reply, State};

% handle increment command
handle_call({incr, Delta}, _From, State) ->
    Counter = State#node.data, % extract counter
    Merge = counter:merge_fun(Counter), % select merge fun
    NewValue = Merge(Counter,Delta), % merge with delta
    gc:maybe_run(), % trigger GC with a given probability
    NewState = State#node{data=NewValue},
    {reply, ok, NewState};

handle_call(get_raw, _From, State) ->
    Counter = State#node.data, % extract counter
    {reply, Counter, State};

% handle replace command (used in GC).
% ToRemove is a Counter, we'll remove all known refs
% ToAdd is a Counter, we'll add it no matter what.
handle_call({replace, ToRemove, ToAdd}, _From, State) ->
    Counter = State#node.data, % extract counter
    NewValue = counter:counter_gc(Counter, ToRemove, ToAdd), % replace some, add some.
    NewState = State#node{data=NewValue},
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
