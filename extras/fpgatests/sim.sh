. /opt/Xilinx/14.7/ISE_DS/common/.settings64.sh /opt/Xilinx/14.7/ISE_DS/common
. /opt/Xilinx/14.7/ISE_DS/EDK/.settings64.sh /opt/Xilinx/14.7/ISE_DS/EDK
. /opt/Xilinx/14.7/ISE_DS/PlanAhead/.settings64.sh /opt/Xilinx/14.7/ISE_DS/PlanAhead
. /opt/Xilinx/14.7/ISE_DS/ISE/.settings64.sh /opt/Xilinx/14.7/ISE_DS/ISE
echo "verilog work "$1".v" > out/$1.prj
cd out; 
/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64/fuse sim -o $1.exe -prj $1.prj
echo $PATH
echo $LD_LIBRARY_PATH
./$1.exe -testplusarg inputFilename=../frame_128.raw -testplusarg outputFilename=$1.sim.raw -tclbatch ../isim.cmd
export LD_LIBRARY_PATH=
terra ../raw2bmp.t $*.sim.raw $*.sim.bmp