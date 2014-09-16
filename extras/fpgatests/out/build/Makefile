UNAME := $(shell uname)

all: stage.bit

stage_map.ncd: stage.v
	xst -ifn ./stage.xst
	ngdbuild -nt timestamp -uc constraints.ucf -p xc6slx9-tqg144-3 stage.ngc stage.ngd
	map -p xc6slx9-tqg144-3 -w -logic_opt off -ol high -t 1 -xt 0 -register_duplication off -r 4 -mt off -ir off -pr off -lc off -power off -o stage_map.ncd stage.ngd stage.pcf

stage.ncd: stage_map.ncd
	par -w -ol high -mt off stage_map.ncd stage.ncd stage.pcf

stage.bit: stage.ncd
	bitgen -intstyle ise -f stage.ut stage.ncd
