SRCS = $(wildcard *.t)
RES = $(patsubst %.t,out/%.v,$(SRCS))
RES += $(patsubst %.t,out/%.bit,$(SRCS))
RES += $(patsubst %.t,out/%.fpga.bmp,$(SRCS))

TERRA = $(TERRADIR)/terra
DARKROOM_CHIP?=xc6slx9

all: $(RES)

clean: 
	rm -f out/*
	rm -rf build_*

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
	mkdir -p build_$*
	cp $< build_$*/stage.v
	bash ../$(DARKROOM_CHIP).sh $*
	export LD_LIBRARY_PATH=
	cp build_$*/OUT_xst.txt out/$*.xst.txt
	cp build_$*/OUT_map.txt out/$*.map.txt
	cp build_$*/OUT_par.txt out/$*.par.txt
	cp build_$*/OUT_ngd.txt out/$*.ngd.txt
	cp build_$*/OUT_trce.txt out/$*.trce.txt
	cp build_$*/OUT_bitgen.txt out/$*.bitgen.txt
	cp build_$*/stage.bit $@


out/%.fpga.bmp : out/%.bit
	bash ../$(DARKROOM_CHIP).test.sh $*
