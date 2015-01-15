ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
BMPSRCS := $(filter-out filter.lua filterColor.lua, $(SRCS))
RES = $(patsubst %.lua,out/%.lua.bmp,$(BMPSRCS))
RES += $(patsubst %.lua,out/%.lua.bmp.correct.txt,$(BMPSRCS))
RES += $(patsubst %.lua,out/%.lua.c4.bmp,$(BMPSRCS))
RES += $(patsubst %.lua,out/%.lua.c4.bmp.correct.txt,$(BMPSRCS))
CSVSRCS = filter.lua filterColor.lua
RES += $(patsubst %.lua,out/%.lua.csv,$(CSVSRCS))
RES += $(patsubst %.lua,out/%.lua.csv.correct.txt,$(CSVSRCS))
RES += $(patsubst %.lua,out/%.lua.c4.csv,$(CSVSRCS))
RES += $(patsubst %.lua,out/%.lua.c4.csv.correct.txt,$(CSVSRCS))
TERRA = $(TERRADIR)/terra

all: $(RES)

clean: 
	rm out/*

out/%.lua.bmp : %.lua
	$(TERRA) $< frame_128.bmp

out/%.lua.c4.bmp : %.lua
	$(TERRA) $< frame_128.bmp 4

out/%.lua.csv : %.lua
	$(TERRA) $< frame_128.bmp

out/%.lua.c4.csv : %.lua
	$(TERRA) $< frame_128.bmp 4

out/%.lua.bmp.correct.txt : out/%.lua.bmp
	diff out/$*.lua.bmp gold/$*.lua.bmp > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/%.lua.c4.bmp.correct.txt : out/%.lua.c4.bmp
	diff out/$*.lua.c4.bmp gold/$*.lua.bmp > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/%.lua.csv.correct.txt : out/%.lua.csv
	diff out/$*.lua.csv gold/$*.lua.csv > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/%.lua.c4.csv.correct.txt : out/%.lua.c4.csv
	sort out/$*.lua.c4.csv > out/$*.lua.c4.csv.sort
	sort gold/$*.lua.csv > out/$*.lua.c4.csv.gold.sort
	diff out/$*.lua.c4.csv.sort out/$*.lua.c4.csv.gold.sort > out/$*.diff
	test ! -s out/$*.diff && touch $@

out/coverage.%.txt : out/bug.lua.yml
	$(TERRA) cover.t /Users/research/Documents/orion/src/$*.t > $@