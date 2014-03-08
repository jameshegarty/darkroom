SRCS = $(wildcard *.lua)

RES = $(patsubst %.lua,out/%.bmp,$(SRCS))

all: $(RES)

out/%.bmp : %.lua
	terra $<
