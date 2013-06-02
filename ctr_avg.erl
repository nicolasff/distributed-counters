-module(ctr_avg).
-behaviour(counter).
-export([bottom/0, is_idempotent/0, merge/2, new/1,
         value/1, gc_info/1, gc_merge/3]).

is_idempotent() -> false.

bottom()   -> ctr_refs:new({0, 0}).
new(Value) -> ctr_refs:new({Value, 1}).
merge(L,R) -> ctr_refs:merge(L,R).

value(C) -> ctr_refs:value(C, fun avg/2, {0,0}). % average all deltas
avg({LSum,LCount},{RSum,RCount}) -> {LSum+RSum,LCount+RCount}.

gc_info(C) -> ctr_refs:gc_info(C).
gc_merge(C, GcData, UniqId) -> ctr_refs:gc_merge(?MODULE, C, GcData, UniqId).

