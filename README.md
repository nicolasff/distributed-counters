Experiments with distributed counters
=====================================

This project implements simple distributed counters in Erlang.
Counters are distributed values with custom *merge policies*. A merge policy is a function that takes two existing counters and returns a new one.
In this context, *counters* are not just values that are incremented by an arbitrary amount but any value that might be wrapped in a container and merged with an other.

Several functions can be implemented that way, including:

* SUM (implemented in `ctr_sum.erl`)
* MIN (implemented in `ctr_min.erl`)
* MAX (implemented in `ctr_max.erl`)
* AVG (implemented in `ctr_avg.erl`)
* STDDEV
* Count-Min
* (Hyper)LogLog
* …

Counter definition
------------------

An Erlang “behaviour” is used to implement a few functions per counter, namely:

* `bottom/0` returns a new, *empty* counter (think ⊥).
* `new/1` returns a new counter containing a single value.
* `merge/2` takes two counters and returns a new one.
* `value/1` extracts the value from a counter.
* `is_idempotent/0` `true`/`false` depending on the counter; `MIN` is idempotent, `SUM` is not.
* `gc_info/1` return a data structure used in GC.
* `gc_merge/3` gives the GC data structure to an existing counter with a unique identifier and ask the counter to clean up old (and presumably irrelevant) data.

Idempotent counter types have trivial implementations for `gc_info/1` and `gc_merge/3`.

Distribution
------------

The demo program starts 3 nodes as separate Erlang processes and sends them a few thousand counter updates with a high probability of dropped and duplicate messages. Once all the updates have been sent, each node is asked to give its own opinion about the total count and a merged value of these 3 values is displayed. If needed, a GC process is run to update the nodes.


Running the demo
----------------

Run `make clean all demo` to run the demo.
Here is what you should see:

    ==============================
    Testing ctr_sum on 3 nodes
    Sent 1000 messages
    Sent 2000 messages
    Sent 3000 messages
    Sent 4000 messages
    Sent 5000 messages
    Sent 6000 messages
    Sent 7000 messages
    Sent 8000 messages
    Sent 9000 messages
    Sent 10000 messages
    Expected value is -110836610
    Gettting full counters on all nodes...
    Value of resolved counters: -110836610
    Counter according to each node: [-109582605,-110537717,-112440005]
    Waiting a second before running GC...
    Trigger GC
    Counter according to each node: [-110836610,-110836610,-110836610]
    
    ==============================
    Testing ctr_min on 3 nodes
    Sent 1000 messages
    Sent 2000 messages
    Sent 3000 messages
    Sent 4000 messages
    Sent 5000 messages
    Sent 6000 messages
    Sent 7000 messages
    Sent 8000 messages
    Sent 9000 messages
    Sent 10000 messages
    Expected value is -999999
    Gettting full counters on all nodes...
    Value of resolved counters: -999999
    Counter according to each node: [-999999,-999999,-999999]
    Waiting a second before running GC...
    Trigger GC
    Counter according to each node: [-999999,-999999,-999999]
    
    ==============================
    Testing ctr_max on 3 nodes
    Sent 1000 messages
    Sent 2000 messages
    Sent 3000 messages
    Sent 4000 messages
    Sent 5000 messages
    Sent 6000 messages
    Sent 7000 messages
    Sent 8000 messages
    Sent 9000 messages
    Sent 10000 messages
    Expected value is 999650
    Gettting full counters on all nodes...
    Value of resolved counters: 999650
    Counter according to each node: [999650,999650,999650]
    Waiting a second before running GC...
    Trigger GC
    Counter according to each node: [999650,999650,999650]
    
    ==============================
    Testing ctr_avg on 3 nodes
    Sent 1000 messages
    Sent 2000 messages
    Sent 3000 messages
    Sent 4000 messages
    Sent 5000 messages
    Sent 6000 messages
    Sent 7000 messages
    Sent 8000 messages
    Sent 9000 messages
    Sent 10000 messages
    Expected value is {-99168540,10000}
    Gettting full counters on all nodes...
    Value of resolved counters: {-99168540,10000}
    Counter according to each node: [{-100403932,9985},{-97717578,9991},{-101242460,9984}]
    Waiting a second before running GC...
    Trigger GC
    Counter according to each node: [{-99168540,10000},{-99168540,10000},{-99168540,10000}]


Limitations
-----------

This code is quite inefficient and only intended as an experiment.
The `GcInfo` object should use a more compact representation; Merkle Trees would probably help.

License
-------

Released in the Public Domain.
Originally developed as an internal prototype at [Acunu](http://acunu.com/).
