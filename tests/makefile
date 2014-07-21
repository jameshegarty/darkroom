ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.4.bmp,$(SRCS))
TERRA = $(TERRADIR)/terra

all: $(RES)

clean: 
	rm out/*

out/%.lua.bmp : %.lua
	$(TERRA) $< frame_128.bmp

out/%.lua.4.bmp : %.lua
	$(TERRA) $< frame_128.bmp 4

out/coverage.%.txt : out/bug.lua.yml
	$(TERRA) cover.t /Users/research/Documents/orion/src/$*.t > $@