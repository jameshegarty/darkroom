ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.lua.est.txt,$(SRCS))
RES += $(patsubst %.lua,out/%.v,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.sim.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.bit,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.fpga.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.correct.txt,$(SRCS))

TERRA = $(TERRADIR)/terra

DARKROOM_CHIP?=xc6slx9

all: $(RES)

clean: 
	rm out/*
	rm -r build/*

out/%.lua.est.txt : %.lua
	$(TERRA) $< est

out/%.v : %.lua
	$(TERRA) $< build frame_128.bmp $(DARKROOM_CHIP)

out/%.lua.bmp : %.lua
	$(TERRA) $< cpu frame_128.bmp

out/%.sim.bmp : out/%.v
	echo "verilog work "$*".v" > out/$*.prj
	cd out; fuse sim -o $*.exe -prj $*.prj
	cd out; ./$*.exe -testplusarg inputFilename=../frame_128.raw -testplusarg outputFilename=$*.sim.bmp -tclbatch ../isim.cmd

out/%.lua.fpga.bmp : %.lua out/%.lua.bmp out/%.bit
	bash $(DARKROOM_CHIP).test.sh $*

out/%.correct.txt : %.lua out/%.lua.bmp out/%.lua.fpga.bmp
	diff out/$*.lua.bmp out/$*.lua.fpga.bmp > out/$*.lua.diff
	test ! -s out/$*.lua.diff && touch $@

out/%.bit : out/%.v
	cp $< build/stage.v
	bash $(DARKROOM_CHIP).sh
	export LD_LIBRARY_PATH=
	cp build/OUT_xst.txt out/$*.xst.txt
	cp build/OUT_map.txt out/$*.map.txt
	cp build/OUT_par.txt out/$*.par.txt
	cp build/OUT_ngd.txt out/$*.ngd.txt
	cp build/OUT_bitgen.txt out/$*.bitgen.txt
	cp build/stage.bit $@
