ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.4.bmp,$(SRCS))
#RES += $(patsubst %.lua,out/%.correct.txt,$(SRCS))
#RES += $(patsubst %.lua,out/%.4.correct.txt,$(SRCS))
TERRA = $(TERRADIR)/terra

all: $(RES)

clean: 
	rm out/*

out/%.lua.bmp : %.lua
	$(TERRA) $< frame_128.bmp

out/%.correct.txt : out/%.lua.bmp
	diff out/$*.lua.bmp gold/$*.bmp > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/%.lua.4.bmp : %.lua
	$(TERRA) $< frame_128.bmp 4

out/%.4.correct.txt : out/%.lua.4.bmp
	diff out/$*.lua.4.bmp gold/$*.bmp > out/$*.4.diff
	test ! -s out/$*.4.diff && touch $@

out/coverage.%.txt : out/bug.lua.yml
	$(TERRA) cover.t /Users/research/Documents/orion/src/$*.t > $@