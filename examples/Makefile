ISRCS = $(wildcard *.t)
SRCS := $(filter-out clibrary.t, $(ISRCS))
RES = $(patsubst %.t,out/%.bmp,$(SRCS))
RES += out/clibrary.o
RES += out/clibrary
RES += out/clibrary1.ppm

all: $(RES)

clean: 
	rm out/*

out/%.bmp : %.t
	terra $<

out/clibrary.o : clibrary.t
	terra clibrary.t

out/clibrary : out/clibrary.o
	gcc -o out/clibrary out/clibrary.o clibrary.c

out/clibrary1.ppm : out/clibrary
	out/clibrary frame_128.raw out/clibrary1.ppm out/clibrary2.ppm
