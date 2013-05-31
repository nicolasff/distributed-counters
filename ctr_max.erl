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

gc_info(C) -> C. % return the whole counter
gc_merge(C, GC, _UniqId) -> lists:foldr(fun merge/2, C, GC).
