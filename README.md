Experiments with distributed counters
=====================================

This project implements simple distributed counters in Erlang.
Counters are distributed values with custom *merge policies*. A merge policy is a function that takes two existing counters and returns a new one.
In this context, *counters* are not just values that are incremented by an arbitrary amount but any value that might be wrapped in a container and merged with an other.

Several functions can be implemented that way, including:

* SUM (implemented in `ctr_sum.erl`)
* MIN (implemented in `ctr_min.erl`)
* MAX (implemented in `ctr_max.erl`)
* AVG
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

Idempotent counter types do not need to implement `gc_info/1` and `gc_merge/3`.

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
    Expected value is 28408551
    Gettting full counters on all nodes...
    Value of resolved counters: 28408551
    Counter according to each node: [29846104,26378597,26373581]
    Trigger GC
    Counter according to each node: [28408551,28408551,28408551]
    
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
    Expected value is -999902
    Gettting full counters on all nodes...
    Value of resolved counters: -999902
    Counter according to each node: [-999902,-999902,-999902]
    
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
    Expected value is 999768
    Gettting full counters on all nodes...
    Value of resolved counters: 999768
    Counter according to each node: [999768,999768,999768]
    

Limitations
-----------

This code is quite inefficient and only intended as an experiment.

License
-------

Released in the Public Domain.
Originally developed as an internal prototype at [Acunu](http://acunu.com/).
