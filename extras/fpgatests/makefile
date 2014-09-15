ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.est.txt,$(SRCS))
RES += $(patsubst %.lua,out/%.v,$(SRCS))
TERRA = $(TERRADIR)/terra

all: $(RES)

clean: 
	rm out/*

out/%.est.txt : %.lua
	$(TERRA) $< est

out/%.v : %.lua
	$(TERRA) $< 
