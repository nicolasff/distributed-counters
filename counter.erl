-module(counter).
-record(counter, {data, merge, module}).
-record(ctr, {module, value}).
-export([new/2, merge/2, gc_info/1, gc_merge/2, value/1]).
-export([create/2, merge_fun/1]).
-define(GC_THRESHOLD, 1000000). % 1 second
-export([counter_value/1,
        merge_all/1,counter_gc/3,counter_remove_recent/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{new,1},          % create a new counter from an initial value
    {merge,2},         % merge two counters together and return a new one
    {value,1},         % extract the value from a counter
    {is_idempotent,0}, % true/false - are updates to this counter idempotent?
	{gc_info,1},       % extract data from a counter as required for GC
	{gc_merge,2}       % take a list of GC info and a counter, return a counter
];
behaviour_info(_Other) ->
    undefined.

create(Value, Fun) ->
    #counter {data=Value, merge=Fun}.


new(Mod, Init) ->
	Value = Mod:new(Init),
	#ctr{module=Mod, value=Value}.

merge(L,R) ->
	LV = L#ctr.value,
	RV = R#ctr.value,
	Mod = L#ctr.module, % extract module from L
	Mod = R#ctr.module, % match module in R too
	Out = Mod:merge(LV, RV),
	#ctr{module=Mod, value=Out}.

gc_info(C) ->
	Mod = C#ctr.module,
	Value = C#ctr.value,
	Mod:gc_info(Value).

gc_merge(GcInfo, C) ->
	Mod = C#ctr.module,
	Value = C#ctr.value,
	Mod:gc_merge(GcInfo, Value).

value(C) ->
	Mod = C#ctr.module,
	Value = C#ctr.value,
	Mod:value(Value).

% internal access
merge_fun(C) -> C#counter.merge.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Simple counters with replay capability          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A counter has a list of increments.
% Each increment is a tuple {ref, delta}

% Extracts the total counter value. In this case, we sum up the increments.
counter_value(C) ->
    Increments = value(C),
    Deduped = lists:ukeysort(1, Increments),
    lists:sum([Delta || {_Id,{_Timestamp,Delta}} <- Deduped]).

% Generate the merged version of several counters
merge_all([H|T]) ->
	Mod = H#ctr.module,
	Empty = new(Mod,0),
	lists:foldr(fun(L,R) ->
				io:format("L=~w~n", [L]),
				io:format("R=~w~n", [R]),
				merge(L,R) end, Empty, [H|T]).

% Garbage-collect a counter, taking a large counter containing references
% to remove and a small counter containing references to add.
% Returns "(Counter \ ToRemove) U ToAdd"
counter_gc(Counter, ToRemove, ToAdd) ->
    RefsToRemove = sets:from_list([Ref || {Ref,_} <- ToRemove#counter.data]),
    Cleaned = lists:foldr(fun(Ref, C) -> 
        NewDeltas = lists:keydelete(Ref, 1, value(C)), % Remove "Ref" from C
        create(NewDeltas, merge_fun(C)) % new counter with the ref removed
        end, Counter, sets:to_list(RefsToRemove)), % for all refs in "ToRemove"

	ok.
    % Ret = counter_merge(ToAdd, Cleaned), % Add the new counter to "Cleaned"
    % Ret.

% Returns a counter without the recent increments
counter_remove_recent(Counter, MaxItems) ->
    Increments = lists:sublist(value(Counter), MaxItems), % first N items
    Now = erlang:now(),
    OldIncrements = lists:filter( % filter out increments that are too old
        fun({_Id,{Timestamp,_Delta}}) ->
                timer:now_diff(Now, Timestamp) > ?GC_THRESHOLD
        end, Increments),
    Counter#counter{data=OldIncrements}.
