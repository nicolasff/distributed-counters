EBIN=ebin
OBJS=$(EBIN)/cluster.beam $(EBIN)/counter.beam $(EBIN)/node.beam \
	$(EBIN)/test.beam $(EBIN)/gc.beam $(EBIN)/ctr_refs.beam \
	$(EBIN)/ctr_min.beam $(EBIN)/ctr_max.beam $(EBIN)/ctr_sum.beam \
	$(EBIN)/ctr_avg.beam


all: $(OBJS)

demo: $(OBJS)
	erl -pa $(EBIN) -noshell -run test main

$(EBIN)/%.beam: %.erl $(EBIN)
	erlc -pa $(EBIN) -o $(EBIN) $<

$(EBIN):
	mkdir -p $(EBIN)

clean:
	rm -f $(OBJS)
