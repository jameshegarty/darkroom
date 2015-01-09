ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
RES = $(patsubst %.lua,out/%.v,$(SRCS))
LUA = $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
RES += $(LUA)

SIM = $(patsubst %.lua,out/%.sim.bmp,$(SRCS))
SIM += $(patsubst %.lua,out/%.sim.correct.txt,$(SRCS))
SIM += $(patsubst %.lua,out/%.sim.v,$(SRCS))

AXIBITS = $(patsubst %.lua,out/%.axi.bit,$(SRCS))
AXIBITS += $(patsubst %.lua,out/%.axi.v,$(SRCS))

AXI = $(patsubst %.lua,out/%.axi.bmp,$(SRCS))
AXI += $(patsubst %.lua,out/%.axi.correct.txt,$(SRCS))
AXI += $(AXIBITS)
RES += $(AXI)

UART = $(patsubst %.lua,out/%.uart.bit,$(SRCS))
UART += $(patsubst %.lua,out/%.uart.bmp,$(SRCS))
UART += $(patsubst %.lua,out/%.uart.v,$(SRCS))
UART += $(patsubst %.lua,out/%.uart.correct.txt,$(SRCS))
RES += $(UART)

BITS += $(patsubst %.lua,out/%.lua.bmp,$(SRCS))
BITS += $(patsubst %.lua,out/%.lua.est.lua,$(SRCS))
BITS += $(patsubst %.lua,out/%.lua.actual.lua,$(SRCS))
RES += $(BITS)

TERRA = $(TERRADIR)/terra

DARKROOM_CHIP?=xc6slx9

all: $(RES)

bits: $(BITS)

sim: $(SIM)

axibits: $(AXIBITS)

axi: $(AXI)

lua: $(LUA)

clean: 
	rm -rf out/*
	rm -rf build_*

out/%.lua.est.lua : %.lua
	$(TERRA) $< est

out/%.lua.actual.lua : out/%.bit
	$(TERRA) ../extractStats.t out/$*.par.txt $@

out/%.sim.correct.txt : %.lua out/%.lua.bmp out/%.sim.bmp
	diff out/$*.lua.bmp out/$*.sim.bmp > out/$*.sim.diff
	test ! -s out/$*.sim.diff && touch $@

out/%.axi.correct.txt : %.lua out/%.lua.bmp out/%.axi.bmp
	diff out/$*.lua.bmp out/$*.axi.bmp > out/$*.axi.diff
	test ! -s out/$*.axi.diff && touch $@

out/%.sim.v : %.lua ../fpga.t ../fpgamodules.t
	$(TERRA) $< buildsim frame_128.bmp $(DARKROOM_CHIP)

out/%.axi.v : %.lua
	$(TERRA) $< buildaxi frame_128.bmp $(DARKROOM_CHIP)

out/%.uart.v : %.lua
	$(TERRA) $< builduart frame_128.bmp $(DARKROOM_CHIP)

out/%.lua.bmp : %.lua
	$(TERRA) $< cpu frame_128.bmp

out/%.sim.bmp : out/%.sim.v
	bash ../sim.sh $*

out/%.axi.bmp : out/%.axi.bit
	bash -x ../axi.sh $*

out/%.uart.bmp : out/%.uart.bit
	bash ../$(DARKROOM_CHIP).test.sh $*

out/%.uart.correct.txt : %.lua out/%.lua.bmp out/%.uart.bmp
	diff out/$*.lua.bmp out/$*.uart.bmp > out/$*.uart.diff
	test ! -s out/$*.uart.diff && touch $@

out/%.axi.bit : out/%.axi.v
	mkdir -p build_$*_axi
	cp $< build_$*_axi/pipeline.v
	bash ../$(DARKROOM_CHIP)_axi.sh $*
	export LD_LIBRARY_PATH=
	cp build_$*_axi/OUT_xst.txt out/$*.axi.xst.txt
	cp build_$*_axi/OUT_map.txt out/$*.axi.map.txt
	cp build_$*_axi/OUT_par.txt out/$*.axi.par.txt
	cp build_$*_axi/OUT_ngd.txt out/$*.axi.ngd.txt
	cp build_$*_axi/OUT_trce.txt out/$*.axi.trce.txt
	cp build_$*_axi/OUT_bitgen.txt out/$*.axi.bitgen.txt
	cp build_$*_axi/stage.bit $@

out/%.uart.bit : out/%.uart.v
	mkdir -p build_$*_uart
	cp $< build_$*_uart/stage.v
	bash ../$(DARKROOM_CHIP)_uart.sh $*
	export LD_LIBRARY_PATH=
	cp build_$*_uart/OUT_xst.txt out/$*.xst.txt
	cp build_$*_uart/OUT_map.txt out/$*.map.txt
	cp build_$*_uart/OUT_par.txt out/$*.par.txt
	cp build_$*_uart/OUT_ngd.txt out/$*.ngd.txt
	cp build_$*_uart/OUT_bitgen.txt out/$*.bitgen.txt
	cp build_$*_uart/stage.bit $@

