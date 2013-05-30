-module(ctr_max).
-behaviour(counter).
-export([bottom/0, is_idempotent/0, merge/2, new/1,
         value/1, gc_info/1, gc_merge/3]).

is_idempotent() -> true.

merge(L,R) when L > R -> L;
merge(_,R) -> R.

bottom() -> new(0).
new(Value) -> Value.

value(C) -> C.

gc_info(_C) -> error.
gc_merge(_C, _GC, _UniqId) -> error.
