EBIN=ebin
OBJS=$(EBIN)/cluster.beam $(EBIN)/counter.beam $(EBIN)/node.beam \
	$(EBIN)/test.beam $(EBIN)/gc.beam \
	$(EBIN)/ctr_min.beam $(EBIN)/ctr_max.beam $(EBIN)/ctr_sum.beam 

all: $(OBJS)

demo: $(OBJS)
	erl -pa $(EBIN) -noshell -run test main

$(EBIN)/%.beam: %.erl
	erlc -pa $(EBIN) -o $(EBIN) $<

clean:
	rm -f $(OBJS)
