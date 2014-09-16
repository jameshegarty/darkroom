ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.est.txt,$(SRCS))
RES += $(patsubst %.lua,out/%.v,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.bit,$(SRCS))
TERRA = $(TERRADIR)/terra

all: $(RES)

clean: 
	rm out/*

out/%.est.txt : %.lua
	$(TERRA) $< est

out/%.v : %.lua
	$(TERRA) $< 

out/%.lua.bmp : %.lua
	$(TERRA) $< frame_128.bmp

out/%.bit : out/%.v
	cp $< out/build/stage.v

