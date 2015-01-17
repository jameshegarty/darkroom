SRCS = $(wildcard *.t)
RES = $(patsubst %.t,out/%.v,$(SRCS))

SIM = $(patsubst %.t,out/%.sim.v,$(SIMSRCS))
SIM += $(patsubst %.t,out/%.sim.bmp,$(SIMSRCS))
SIM += $(patsubst %.t,out/%.sim.correct.txt,$(SIMSRCS))
RES += $(SIM)

UART = $(patsubst %.t,out/%.uart.bit,$(SRCS))
UART += $(patsubst %.t,out/%.uart.bmp,$(SRCS))
UART += $(patsubst %.t,out/%.uart.v,$(SRCS))
UART += $(patsubst %.t,out/%.uart.correct.txt,$(SRCS))
RES += $(UART)

AXIBITS = $(patsubst %.t,out/%.axi.bit,$(SRCS))
AXIBITS += $(patsubst %.t,out/%.axi.v,$(SRCS))

AXI = $(patsubst %.lua,out/%.axi.bmp,$(SRCS))
AXI += $(patsubst %.lua,out/%.axi.correct.txt,$(SRCS))
AXI += $(AXIBITS)
RES += $(AXI)

TERRA = $(TERRADIR)/terra
DARKROOM_CHIP?=xc6slx9

all: $(RES)

sim: $(SIM)

clean: 
	rm -f out/*
	rm -rf build_*

out/%.sim.correct.txt : %.t out/%.sim.bmp
	diff out/$*.bmp out/$*.sim.bmp > out/$*.sim.diff
	test ! -s out/$*.sim.diff && touch $@

out/%.axi.correct.txt : %.t out/%.axi.bmp
	diff out/$*.bmp out/$*.axi.bmp > out/$*.axi.diff
	test ! -s out/$*.axi.diff && touch $@

out/%.vga.v out/%.sim.v out/%.uart.v out/%.axi.v: %.t
	$(TERRA) $< $(DARKROOM_CHIP)

out/%.sim.bmp : out/%.sim.v
	bash -x ../sim.sh $*

out/%.axi.bmp : out/%.axi.bit
	bash -x ../axi.sh $*

out/%.vga.bit : out/%.vga.v
	mkdir -p build_vga_$*
	cp $< build_vga_$*/stage.v
	bash ../$(DARKROOM_CHIP)_vga.sh $*
	export LD_LIBRARY_PATH=
	cp build_vga_$*/OUT_xst.txt out/$*.xst.txt
	cp build_vga_$*/OUT_map.txt out/$*.map.txt
	cp build_vga_$*/OUT_par.txt out/$*.par.txt
	cp build_vga_$*/OUT_ngd.txt out/$*.ngd.txt
	cp build_vga_$*/OUT_trce.txt out/$*.trce.txt
	cp build_vga_$*/OUT_bitgen.txt out/$*.bitgen.txt
	cp build_vga_$*/stage.bit $@

out/%.uart.bit : out/%.uart.v
	mkdir -p build_$*_uart
	cp $< build_$*_uart/stage.v
	bash ../$(DARKROOM_CHIP)_uart.sh $*
	export LD_LIBRARY_PATH=
	cp build_$*_uart/OUT_xst.txt out/$*.xst.txt
	cp build_$*_uart/OUT_map.txt out/$*.map.txt
	cp build_$*_uart/OUT_par.txt out/$*.par.txt
	cp build_$*_uart/OUT_ngd.txt out/$*.ngd.txt
	cp build_$*_uart/OUT_trce.txt out/$*.trce.txt
	cp build_$*_uart/OUT_bitgen.txt out/$*.bitgen.txt
	cp build_$*_uart/stage.bit $@

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

out/%.uart.bmp : out/%.uart.bit
	bash ../$(DARKROOM_CHIP).test.sh $*
