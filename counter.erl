-module(counter).
-record(ctr, {module, data}).
-export([bottom/1, new/2, merge/2, gc_info/1, gc_merge/3,
        is_idempotent/1, value/1]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{bottom,0},       % create a new counter with an empty value
    {new,1},           % create a new counter from an initial value
    {merge,2},         % merge two counters together and return a new one
    {value,1},         % extract the value from a counter
    {is_idempotent,0}, % true/false - are updates to this counter idempotent?
    {gc_info,1},       % extract data from a counter as required for GC
    {gc_merge,3}       % take a list of GC info and a counter, return a counter
];
behaviour_info(_Other) ->
    undefined.

wrap(Mod, Value) ->
    #ctr{module=Mod, data=Value}.

bottom(Mod) ->
    wrap(Mod, Mod:bottom()).

new(Mod, Init) ->
    wrap(Mod, Mod:new(Init)).

merge(L,R) ->
    LV = L#ctr.data,
    RV = R#ctr.data,
    Mod = L#ctr.module, % extract module from L
    Mod = R#ctr.module, % match module in R too
    wrap(Mod, Mod:merge(LV, RV)).

gc_info(C) ->
    Mod = C#ctr.module,
    Value = C#ctr.data,
    Mod:gc_info(Value).

gc_merge(C, GcInfo, UniqId) ->
    Mod = C#ctr.module,
    Value = C#ctr.data,
    wrap(Mod, Mod:gc_merge(Value, GcInfo, UniqId)).

value(C) ->
    Mod = C#ctr.module,
    Value = C#ctr.data,
    Mod:value(Value).

is_idempotent(C) ->
    Mod = C#ctr.module,
    Mod:is_idempotent().
