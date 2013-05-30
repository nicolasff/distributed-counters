EBIN=ebin
OBJS=$(EBIN)/cluster.beam $(EBIN)/counter.beam $(EBIN)/node.beam $(EBIN)/test.beam $(EBIN)/gc.beam

all: $(OBJS)

test: $(OBJS)
	erl -pa ebin -noshell -run test main

$(EBIN)/%.beam: %.erl
	erlc -o $(EBIN) $<

clean:
	rm -f $(OBJS)
