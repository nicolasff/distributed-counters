-module(ctr_min).
-behaviour(counter).
-export([is_idempotent/0, merge/2, new/1,
         value/1, gc_info/1, gc_merge/2]).

is_idempotent() -> true.

merge(L,R) when L < R -> L;
merge(_,R) -> R.

new(Value) -> Value.

value(C) -> C.

gc_info(_C) -> error.
gc_merge(_GC, _C) -> error.
