ISRCS = $(wildcard ../../tests/*.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst ../../tests/%.lua,out/%.lua.yml,$(SRCS))

all: $(RES)

clean: 
	rm out/*

out/%.lua.yml : ../../tests/%.lua
	terra $< ../../tests/frame_128.bmp

