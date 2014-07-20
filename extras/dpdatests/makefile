ISRCS = $(wildcard ../../tests/*.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
SRCS := $(filter-out ../../tests/boolcrop.lua ../../tests/resample.lua, $(SRCS)) # not straight pipes
SRCS := $(filter-out ../../tests/conststencil.lua ../../tests/defaultcrop.lua ../../tests/internalirtest.lua, $(SRCS)) # negative stencils (bug)
SRCS := $(filter-out ../../tests/luaarray.lua ../../tests/mrnonstencil.lua, $(SRCS)) # array values (bug)
SRCS := $(filter-out ../../tests/mapreduce.lua ../../tests/nonpowerof2stencil.lua ../../tests/rgbindex.lua, $(SRCS)) # mapreduce
SRCS := $(filter-out ../../tests/mo.lua ../../tests/multiout.lua ../../tests/operators.lua, $(SRCS)) # multiple outputs
RES = $(patsubst ../../tests/%.lua,out/%.lua.yml,$(SRCS))

all: $(RES)

clean: 
	rm out/*

out/%.lua.yml : ../../tests/%.lua
	terra $< ../../tests/frame_128.bmp

