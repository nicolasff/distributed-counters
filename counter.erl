-module(counter).
-record(counter, {data, merge}).
-export([create/2,merge/2,merge_fun/1]).

-export([counter_value/1,counter_new/1,counter_merge/2,counter_lub/1,counter_gc/3]).


create(Value, Fun) ->
	#counter {data=Value, merge=Fun}.

merge(L, R) ->
	Fun = merge_fun(L),
	Value = Fun(L#counter.data,R#counter.data),
	create(Value, Fun).

% internal access
merge_fun(C) -> C#counter.merge.
value(C)     -> C#counter.data.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Simple counters with replay capability          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A counter has a list of increments.
% Each increment is a tuple {ref, delta}

counter_merge(L,R) -> 
	% io:format("counter_merge: L=~w, R=~w~n", [L, R]),
	LV = value(L),
	RV = value(R),
	Fun = merge_fun(L),
	create(LV ++ RV, Fun).

counter_new(Value) -> 
	UniqueId = make_ref(),
	Timestamp = now(),
	create([{UniqueId, {Timestamp,Value}}], fun counter_merge/2).

counter_value(C) ->
	Increments = value(C),
	% io:format("Increments=~w~n", [Increments]),
	Deduped = lists:ukeysort(1, Increments),
	lists:sum([Delta || {_Id,{Timestamp,Delta}} <- Deduped]).

counter_lub(Counters) ->
	lists:foldr(fun counter_merge/2, counter_new(0), Counters).

counter_gc(Counter, ToRemove, ToAdd) ->
	% io:format("Counter=~w~n", [Counter]),
	% io:format("ToAdd=~w~n", [ToAdd]),

	RefsToRemove = sets:from_list([Ref || {Ref,_} <- ToRemove#counter.data]),
	% io:format("Refs to remove =~w~n", [sets:to_list(RefsToRemove)]),
	Cleaned = lists:foldr(fun(Ref, C) -> 
		Deltas = lists:keydelete(Ref, 1, value(C)),
		create(Deltas, merge_fun(C))
		end, Counter, sets:to_list(RefsToRemove)),

	Ret = counter_merge(ToAdd, Cleaned),
	% io:format("Ret = ~w~n", [Ret]),
	Ret.
