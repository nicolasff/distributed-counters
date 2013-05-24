-module(node).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-record(node, {data}).

init(_Args) ->
    Counter = counter:counter_new(0),
    State = #node{data=Counter},
    {ok, State}.


handle_call(get_raw, _From, State) ->
    {reply, State#node.data, State}.


% handle replace command (used in GC).
% ToRemove is a Counter, we'll remove all known refs
% ToAdd is a Counter, we'll add it no matter what.
handle_cast({replace, ToRemove, ToAdd}, State) ->
    Counter = State#node.data, % extract counter
    NewValue = counter:counter_gc(Counter, ToRemove, ToAdd), % replace some, add some.
    NewState = State#node{data=NewValue},
    {noreply, NewState};

% handle increment command
handle_cast({incr, Delta}, State) ->
    Counter = State#node.data, % extract counter
    Merge = counter:merge_fun(Counter), % select merge fun
    NewValue = Merge(Counter,Delta), % merge with delta
    gc:maybe_run(), % trigger GC with a given probability
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
