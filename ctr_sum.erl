-module(ctr_sum).
-behaviour(counter).
-export([bottom/0, is_idempotent/0, merge/2, new/1,
         value/1, gc_info/1, gc_merge/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Simple counters with replay capability          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A counter has a list of increments.
% Each increment is a tuple {ref, delta}

is_idempotent() -> false.

merge(L,R) -> ctr_refs:merge(L,R).

bottom() -> ctr_refs:new(0).

new(Value) -> ctr_refs:new(Value).

value(C) -> ctr_refs:value(C, fun lists:sum/1). % sum up all deltas

gc_info(C) -> ctr_refs:gc_info(C).

gc_merge(C, GcData, UniqId) ->
	ctr_refs:gc_merge(ctr_sum, C, GcData, UniqId).

