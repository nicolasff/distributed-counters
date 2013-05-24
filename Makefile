OBJS=cluster.beam counter.beam node.beam test.beam gc.beam

all: $(OBJS)


%.beam: %.erl
	erlc $<

clean:
	rm -f $(OBJS)
