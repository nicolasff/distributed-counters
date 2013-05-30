-module(counter).
-record(ctr, {module, data}).
-export([new/2, merge/2, gc_info/1, gc_merge/2, value/1]).

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


new(Mod, Init) ->
    Value = Mod:new(Init),
    #ctr{module=Mod, data=Value}.

merge(L,R) ->
    LV = L#ctr.data,
    RV = R#ctr.data,
    Mod = L#ctr.module, % extract module from L
    Mod = R#ctr.module, % match module in R too
    Out = Mod:merge(LV, RV),
    #ctr{module=Mod, data=Out}.

gc_info(C) ->
    Mod = C#ctr.module,
    Value = C#ctr.data,
    Mod:gc_info(Value).

gc_merge(C, GcInfo) ->
    Mod = C#ctr.module,
    Value = C#ctr.data,
	#ctr{module=Mod, data=Mod:gc_merge(Value, GcInfo)}.

value(C) ->
    Mod = C#ctr.module,
    Value = C#ctr.data,
    Mod:value(Value).
