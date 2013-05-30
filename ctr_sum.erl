-module(ctr_sum).
-behaviour(counter).
-export([bottom/0, is_idempotent/0, merge/2, new/1,
         value/1, gc_info/1, gc_merge/3]).

-define(MAX_GC_ITEMS, 10000).        % max # of increments to merge
-define(GC_TIME_THRESHOLD, 1000000). % 1 sec

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Simple counters with replay capability          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A counter has a list of increments.
% Each increment is a tuple {ref, delta}

is_idempotent() -> false.

dedupe(Increments) ->
    lists:ukeysort(1, Increments).

merge(L,R) ->
    dedupe(lists:keymerge(1, L, R)). % merge increments

bottom() -> new(0).
new(Value) -> % single increment w/ ID & time
	new(Value, make_ref()).

new(Value, UniqId) -> % single increment w/ ID & time
    [{UniqId, {now(), Value}}].

value(C) -> % sum up all deltas
    Deduped = dedupe(C),
    lists:sum([Delta || {_Id,{_Timestamp,Delta}} <- Deduped]).

gc_info(C) -> % extract all "non-recent" increments
    Increments = lists:sublist(C, ?MAX_GC_ITEMS), % first N items
    Now = erlang:now(),
    OldIncrements = lists:filter( % filter out increments that are too old
        fun({_Id,{Timestamp,_Delta}}) ->
                timer:now_diff(Now, Timestamp) > ?GC_TIME_THRESHOLD
        end, Increments),
    OldIncrements.

gc_merge(C, GcData, UniqId) ->
    AllIncrements = lists:concat(GcData),
    ToAdd = new(value(AllIncrements), UniqId),
    RefsToRemove = sets:from_list([Ref || {Ref,_} <- AllIncrements]),
    Cleaned = lists:foldr(fun(Ref, Cur) -> 
            lists:keydelete(Ref, 1, Cur) % Remove "Ref" from C
        end, C, sets:to_list(RefsToRemove)), % for all refs in "ToRemove"

    Ret = merge(ToAdd, Cleaned), % Add the new counter to "Cleaned"
    Ret.
