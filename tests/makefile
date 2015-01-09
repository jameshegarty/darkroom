ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
BMPSRCS := $(filter-out filter.lua filterColor.lua, $(SRCS))
RES = $(patsubst %.lua,out/%.lua.bmp,$(BMPSRCS))
RES += $(patsubst %.lua,out/%.lua.bmp.correct.txt,$(BMPSRCS))
RES += $(patsubst %.lua,out/%.lua.4.bmp,$(BMPSRCS))
RES += $(patsubst %.lua,out/%.lua.4.bmp.correct.txt,$(BMPSRCS))
CSVSRCS = filter.lua filterColor.lua
RES += $(patsubst %.lua,out/%.lua.csv,$(CSVSRCS))
RES += $(patsubst %.lua,out/%.lua.csv.correct.txt,$(CSVSRCS))
RES += $(patsubst %.lua,out/%.lua.4.csv,$(CSVSRCS))
RES += $(patsubst %.lua,out/%.lua.4.csv.correct.txt,$(CSVSRCS))
TERRA = $(TERRADIR)/terra

all: $(RES)

clean: 
	rm out/*

out/%.lua.bmp : %.lua
	$(TERRA) $< frame_128.bmp

out/%.lua.4.bmp : %.lua
	$(TERRA) $< frame_128.bmp 4

out/%.lua.csv : %.lua
	$(TERRA) $< frame_128.bmp

out/%.lua.4.csv : %.lua
	$(TERRA) $< frame_128.bmp 4

out/%.lua.bmp.correct.txt : out/%.lua.bmp
	diff out/$*.lua.bmp gold/$*.lua.bmp > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/%.lua.4.bmp.correct.txt : out/%.lua.4.bmp
	diff out/$*.lua.4.bmp gold/$*.lua.bmp > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/%.lua.csv.correct.txt : out/%.lua.csv
	diff out/$*.lua.csv gold/$*.lua.csv > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/%.lua.4.csv.correct.txt : out/%.lua.4.csv
	diff out/$*.lua.4.csv gold/$*.lua.csv > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/coverage.%.txt : out/bug.lua.yml
	$(TERRA) cover.t /Users/research/Documents/orion/src/$*.t > $@