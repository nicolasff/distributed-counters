-module(counter).
-record(counter, {data, merge}).
-export([create/2,merge/2,merge_fun/1]).

-export([counter_value/1,counter_new/1,counter_merge/2]).


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
	create([{make_ref(), Value}], fun counter_merge/2).

counter_value(C) ->
	Increments = value(C),
	% io:format("Increments=~w~n", [Increments]),
	Deduped = lists:ukeysort(1, Increments),
	lists:sum([Delta || {_Id,Delta} <- Deduped]).

