
ISRCS = $(wildcard *.lua)
SRCS := $(filter-out test.t cover.t, $(ISRCS))
# some tests can't compile with the conv engine
YML_SRCS := $(filter-out array_simple.lua cropGrow.lua cropShrink.lua multiout.lua resample.lua mo.lua position.lua position2.lua scalar_select.lua switch.lua vector_select.lua, $(SRCS))
RES = $(patsubst %.lua,out/%.lua.default.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.mat.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.allmat.bmp,$(SRCS))
RES += $(patsubst %.lua,out/%.lua.yml,$(YML_SRCS))
RES += $(patsubst %.lua,out/%.lua.alllb.bmp,$(SRCS))

#CSRCS = $(wildcard ../src/*.t)
#RES += $(patsubst ../src/%.t,out/coverage.%.txt, $(CSRCS))

all: $(RES)

clean: 
	rm out/*

out/%.lua.allmat.bmp : %.lua
	terra $< allmat frame_128.bmp

out/%.lua.alllb.bmp : %.lua
	terra $< alllb frame_128.bmp

out/%.lua.mat.bmp : %.lua
	terra $< mat frame_128.bmp

out/%.lua.default.bmp : %.lua
	terra $< default frame_128.bmp

out/%.lua.yml : %.lua
	terra $< conv frame_128.bmp

out/coverage.%.txt : out/bug.lua.yml
	terra cover.t /Users/research/Documents/orion/src/$*.t > $@