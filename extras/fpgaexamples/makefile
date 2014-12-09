SRCS = $(wildcard *.t)
RES = $(patsubst %.t,out/%.v,$(SRCS))

SIM = $(patsubst %.t,out/%.sim.v,$(SRCS))
SIM += $(patsubst %.t,out/%.sim.bmp,$(SRCS))
SIM += $(patsubst %.t,out/%.sim.correct.txt,$(SRCS))

RES += $(SIM)
RES += $(patsubst %.t,out/%.bit,$(SRCS))
RES += $(patsubst %.t,out/%.fpga.bmp,$(SRCS))

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

out/%.vga.v out/%.sim.v out/%.v : %.t
	$(TERRA) $< $(DARKROOM_CHIP)

out/%.sim.bmp : out/%.sim.v
	bash -x ../sim.sh $*

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

out/%.bit : out/%.v
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


out/%.fpga.bmp : out/%.bit
	bash ../$(DARKROOM_CHIP).test.sh $*
