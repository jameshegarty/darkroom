. /opt/Xilinx/14.7/ISE_DS/settings64.sh
echo "verilog work "$1".v" > out/$1.prj
cd out; 
/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64/fuse sim -o $1.exe -prj $1.prj
./$1.exe -testplusarg inputFilename=../frame_128.raw -testplusarg outputFilename=$1.sim.raw -tclbatch ../isim.cmd
