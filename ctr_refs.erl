-module(ctr_refs).
-export([new/1, merge/2, value/3, gc_info/1, gc_merge/4]).

-define(MAX_GC_ITEMS, 10000).        % max # of increments to merge
-define(GC_TIME_THRESHOLD, 1000000). % 1 sec

% This module is used by non-idempotent counters
keep_left(_K, L, _R) -> L.

new(Value) -> % single increment w/ ID & time
	new(Value, make_ref()).

new(Value, UniqId) -> % single increment w/ ID & time
	orddict:store(UniqId, {now(), Value}, orddict:new()).

merge(L,R) ->
	% keep left value in case of key conflict (this is arbitrary)
	orddict:merge(fun keep_left/3, L, R).

value(C, Fun, Orig) ->
	{_T, V} = orddict:fold(fun(_Key, {TL,L}, {_TR,R}) ->
			Merged = Fun(L,R),
			{TL,Merged} % return a tmestamp+value pair
		end, {0,Orig}, C),
	V.

gc_info(C) -> % extract all "non-recent" increments
	L = orddict:to_list(C),
    Increments = lists:sublist(L, ?MAX_GC_ITEMS), % first N items
    Now = erlang:now(),
    OldIncrements = lists:filter( % filter out increments that are too old
        fun({_Id,{Timestamp,_Delta}}) ->
                timer:now_diff(Now, Timestamp) > ?GC_TIME_THRESHOLD
        end, Increments),
    orddict:from_list(OldIncrements).

gc_merge(CounterModule, C, GcData, UniqId) ->

	% merge all GC info in one dict
	AllIncrements = lists:foldr(
		fun (OL, OR) ->
			orddict:merge(fun keep_left/3, OL, OR)
		end,
		orddict:new(), GcData),

	% remove increments
    RefsToRemove = orddict:fetch_keys(AllIncrements),
    Cleaned = lists:foldr(fun(Ref, Cur) -> 
            orddict:erase(Ref, Cur) % Remove "Ref" from C
        end, C, RefsToRemove), % for all refs in "ToRemove"

	% create new equivalent counter
    ToAdd = new(CounterModule:value(AllIncrements), UniqId),

	% Add the new counter to "Cleaned"
    merge(ToAdd, Cleaned).
