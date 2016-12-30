HC=ghc
HC_FLAGS=-Wall

BIN=babel
SRCS=src/Main.hs
OBJS=$(SRCS:.hs=.o)
INTS=$(SRCS:.hs=.hi)

.PHONY: clean

all: $(BIN)

$(BIN): $(OBJS)
	$(HC) -o $(BIN) $(HC_FLAGS) $(OBJS)

.hs.o: $(SRCS)
	$(HC) -c $(HC_FLAGS) -o $@ $<

debug: $(OBJS)
	$(HC) -debug -o $(BIN) $(HC_FLAGS) $(OBJS)

clean:
	rm $(BIN) $(OBJS) $(INTS)
