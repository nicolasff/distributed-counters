EBIN=ebin
OBJS=$(EBIN)/cluster.beam $(EBIN)/counter.beam $(EBIN)/node.beam \
	 $(EBIN)/test.beam $(EBIN)/gc.beam $(EBIN)/ctr_min.beam

all: $(OBJS)

test: $(OBJS)
	erl -pa ebin -noshell -run test main

$(EBIN)/%.beam: %.erl
	erlc -o $(EBIN) $<

$(EBIN)/%.beam: counters/%.erl
	erlc -I .. -o $(EBIN) $<

clean:
	rm -f $(OBJS)
