-module(ctr_min).
-behaviour(counter).
-export([is_idempotent/0, merge/2, new/1, value/1]).

is_idempotent() ->
	true.

merge(L,R) when L < R -> L;
merge(_,R) -> R.

new(Value) -> Value.

value(C) -> C.
