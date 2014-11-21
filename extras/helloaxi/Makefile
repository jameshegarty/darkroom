
ADDR := 192.168.2.2
LOC := ~
CD = cd build; 
UNAME := $(shell uname)
system.bit.bin: system.v
ifeq ($(UNAME), Darwin)
	vagrant ssh -- "source ~/.profile; make -C /vagrant/darkroom/extras/helloaxi $@"
else
	$(CD) xst -ifn ../system.xst
	$(CD) ngdbuild -nt timestamp -uc ../ps7_constraints.ucf -uc ../system.ucf -p xc7z020-clg484-1 system.ngc system.ngd
	$(CD) map -p xc7z020-clg484-1 -w -logic_opt off -ol high -t 1 -xt 0 -register_duplication off -r 4 -mt off -ir off -pr off -lc off -power off -o system_map.ncd system.ngd system.pcf
	$(CD) par -w -ol high -mt off system_map.ncd system.ncd system.pcf
	$(CD) trce -v 3 -s 1 -n 3 -fastpaths -xml system.twx system.ncd -o system.twr system.pcf -ucf ps7_constraints.ucf -ucf system.ucf
	$(CD) bitgen -w -g Binary:no -g CRC:Enable -g ProgPin:PullUp -g InitPin:Pullup -g TckPin:PullUp -g TdiPin:PullUp \
	  -g TdoPin:PullUp -g TmsPin:PullUp -g Disable_JTAG:No -g UnusedPin:PullDown -g UserID:0xFFFFFFFF -g OverTempPowerDown:Disable \
	  -g USR_ACCESS:None -g JTAG_XADC:Enable -g DCIUpdateMode:AsRequired -g StartUpClk:CClk -g DONE_cycle:4 -g GTS_cycle:5 -g GWE_cycle:6 \
	  -g Match_cycle:Auto -g Security:None -g ICAP_select:Auto -g DonePipe:Yes -g DriveDone:No system.ncd
	bootgen -w -image boot.bif -split bin -o boot.bin
endif


%:  %.c
ifeq ($(UNAME), Darwin)
	vagrant ssh -- "make -C /vagrant/darkroom/extras/helloaxi $@"
else
	arm-linux-gnueabi-gcc -std=c99  $< -o $@
endif

load: system.bit.bin gpio-dev-mem-test memcpy readuio
	rm -f ~/.ssh/known_hosts
	scp system.bit.bin gpio-dev-mem-test memcpy readuio root@$(ADDR):$(LOC)
	ssh root@$(ADDR) "cat $(LOC)/system.bit.bin > /dev/xdevcfg"

clean:
	rm -rf build/* system.bit.bin boot.bin u-boot.elf.bin
	
crash: load
	ssh root@$(ADDR) ./gpio-dev-mem-test -g 0x70000000 -i
	ssh root@$(ADDR)