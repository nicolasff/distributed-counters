-module(counter).
-record(counter, {data, merge}).
-export([create/2,merge_fun/1]).

-export([counter_value/1,counter_new/1,counter_merge/2,counter_merge_all/1,counter_gc/3]).


create(Value, Fun) ->
	#counter {data=Value, merge=Fun}.


% internal access
merge_fun(C) -> C#counter.merge.
value(C)     -> C#counter.data.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Simple counters with replay capability          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A counter has a list of increments.
% Each increment is a tuple {ref, delta}

counter_merge(L,R) -> 
	LV = value(L),
	RV = value(R),
	Fun = merge_fun(L),
	create(LV ++ RV, Fun).

% Creates a new counter with a single value
% This function generates a unique reference for the delta
counter_new(Value) -> 
	UniqueId = make_ref(),
	Timestamp = now(),
	create([{UniqueId, {Timestamp,Value}}], fun counter_merge/2).

% Extracts the total counter value. In this case, we sum up the increments.
counter_value(C) ->
	Increments = value(C),
	Deduped = lists:ukeysort(1, Increments),
	lists:sum([Delta || {_Id,{Timestamp,Delta}} <- Deduped]).

% Generate the merged version of several counters
counter_merge_all(Counters) ->
	lists:foldr(fun counter_merge/2, counter_new(0), Counters).

% Garbage-collect a counter, taking a large counter containing references
% to remove and a small counter containing references to add.
% Returns "(Counter \ ToRemove) U ToAdd"
counter_gc(Counter, ToRemove, ToAdd) ->
	RefsToRemove = sets:from_list([Ref || {Ref,_} <- ToRemove#counter.data]),
	io:format("remove ~w increments, replace with one.~n", [sets:size(RefsToRemove)]),
	Cleaned = lists:foldr(fun(Ref, C) -> 
		NewDeltas = lists:keydelete(Ref, 1, value(C)), % Remove "Ref" from C
		create(NewDeltas, merge_fun(C)) % return a new counter with the ref removed.
		end, Counter, sets:to_list(RefsToRemove)), % do this for all refs in "ToRemove".

	counter_merge(ToAdd, Cleaned). % Add the new counter to the cleaned-up one.
