ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.lua.est.txt,$(SRCS))
RES += $(patsubst %.lua,out/%.v,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.bit,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.fpga.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.test.txt,$(SRCS))

TERRA = $(TERRADIR)/terra

all: $(RES)

clean: 
	rm out/*
	rm build/*

out/%.lua.est.txt : %.lua
	$(TERRA) $< est

out/%.v : %.lua
	$(TERRA) $< 

out/%.lua.bmp : %.lua
	$(TERRA) $< frame_128.bmp

out/%.lua.fpga.bmp : %.lua out/%.lua.bmp
	$(TERRA) $< frame_128.bmp test

out/%.test.txt : %.lua out/%.lua.bmp out/%.lua.fpga.bmp
	diff out/$*.lua.bmp out/$*.lua.fpga.bmp > out/$*.lua.diff
	test ! -s out/$*.lua.diff && touch $@

out/%.bit : out/%.v
	cp $< build/stage.v
	make -f makefile.xc6slx9
	cp build/stage.bit $@

