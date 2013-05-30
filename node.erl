-module(node).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).
-record(node, {data, cluster}).

init([CounterModule]) ->
    Counter = counter:bottom(CounterModule),
    State = #node{data=Counter},
    {ok, State}.

% handle metadata update
handle_call({set_cluster, Cluster}, _From, State) ->
    NewState = State#node{cluster=Cluster},
    {reply, ok, NewState};

handle_call(get_raw_for_gc, _From, State) ->
    Counter = State#node.data, % extract counter
    Reply = counter:gc_info(Counter),
    {reply, Reply, State};

% handle increment command
handle_call({incr, Delta}, _From, State) ->
    Counter = State#node.data, % extract counter
    NewValue = counter:merge(Counter, Delta), % merge with delta

	% trigger GC with a given probability
	IsIdempotent = counter:is_idempotent(Counter),
	if  IsIdempotent ->
			do_nothing;
		true -> cluster:gc_maybe_run(State#node.cluster)
	end,

    NewState = State#node{data=NewValue},
    {reply, ok, NewState};

handle_call(get_raw, _From, State) ->
    Counter = State#node.data, % extract counter
    {reply, Counter, State};

% handle perform_gc command. We'll get a data structure
% that the counter knows how to extract data from.
handle_call({perform_gc, GcInfo, UniqId}, _From, State) ->
    Counter = State#node.data, % extract counter
    NewValue = counter:gc_merge(Counter, GcInfo, UniqId), % replace
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
