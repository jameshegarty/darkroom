ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.lua.bmp,$(SRCS))

all: $(RES)

clean: 
	rm out/*

out/%.lua.bmp : %.lua
	terra $< frame_128.bmp

out/coverage.%.txt : out/bug.lua.yml
	terra cover.t /Users/research/Documents/orion/src/$*.t > $@