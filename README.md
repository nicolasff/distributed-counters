Experiments with distributed counters
=====================================

This project implements simple distributed counters in Erlang.
Counters are distributed values with custom *merge policies*. A merge policy is a function that takes two existing counters and returns a new one.
In this context, *counters* are not just values that are incremented by an arbitrary amount but any value that might be wrapped in a container and merged with an other.

Several functions can be implemented that way, including:

* SUM (implemented in `ctr_sum.erl`)
* MIN (implemented in `ctr_min.erl`)
* MAX
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

(TODO)

Limitations
-----------

This code is quite inefficient and only intended as an experiment.

License
-------

Released in the Public Domain.
Originally developed as an internal prototype at [Acunu](http://acunu.com/).
