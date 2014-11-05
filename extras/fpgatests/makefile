ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.v,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
#RES += $(patsubst %.lua,out/%.sim.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.bit,$(SRCS))
IMAGES = $(patsubst %.lua,out/%.fpga.bmp,$(SRCS))
RES += $(IMAGES)
RES += $(patsubst %.lua,out/%.correct.txt,$(SRCS))
BITS = $(patsubst %.lua,out/%.bit,$(SRCS))
BITS += $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
BITS += $(patsubst %.lua,out/%.lua.est.lua,$(SRCS))
BITS += $(patsubst %.lua,out/%.lua.actual.lua,$(SRCS))
RES += $(BITS)

TERRA = $(TERRADIR)/terra

DARKROOM_CHIP?=xc6slx9

all: $(RES)

bits: $(BITS)

images: $(IMAGES)

clean: 
	rm -rf out/*
	rm -rf build_*

out/%.lua.est.lua : %.lua
	$(TERRA) $< est

out/%.lua.actual.lua : out/%.bit
	$(TERRA) ../extractStats.t out/$*.par.txt $@

out/%.v : %.lua
	$(TERRA) $< build frame_128.bmp $(DARKROOM_CHIP)

out/%.lua.bmp : %.lua
	$(TERRA) $< cpu frame_128.bmp

out/%.sim.bmp : out/%.v
	bash sim.sh $*

out/%.fpga.bmp : %.lua out/%.lua.bmp out/%.bit
	bash ../$(DARKROOM_CHIP).test.sh $*

out/%.correct.txt : %.lua out/%.lua.bmp out/%.fpga.bmp
	diff out/$*.lua.bmp out/$*.fpga.bmp > out/$*.lua.diff
	test ! -s out/$*.lua.diff && touch $@

out/%.bit : out/%.v
	mkdir -p build_$*
	cp $< build_$*/stage.v
	bash ../$(DARKROOM_CHIP).sh $*
	export LD_LIBRARY_PATH=
	cp build_$*/OUT_xst.txt out/$*.xst.txt
	cp build_$*/OUT_map.txt out/$*.map.txt
	cp build_$*/OUT_par.txt out/$*.par.txt
	cp build_$*/OUT_ngd.txt out/$*.ngd.txt
	cp build_$*/OUT_bitgen.txt out/$*.bitgen.txt
	cp build_$*/stage.bit $@
