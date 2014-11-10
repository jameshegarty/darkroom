. /opt/Xilinx/14.7/ISE_DS/common/.settings64.sh /opt/Xilinx/14.7/ISE_DS/common
. /opt/Xilinx/14.7/ISE_DS/EDK/.settings64.sh /opt/Xilinx/14.7/ISE_DS/EDK
. /opt/Xilinx/14.7/ISE_DS/PlanAhead/.settings64.sh /opt/Xilinx/14.7/ISE_DS/PlanAhead
. /opt/Xilinx/14.7/ISE_DS/ISE/.settings64.sh /opt/Xilinx/14.7/ISE_DS/ISE
echo "verilog work "$1".sim.v" > out/$1.prj
cd out; 
# unisim has all the macros like RAMB16_S9_S9 etc
/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64/fuse sim -lib unisim -o $1.exe -prj $1.prj
echo $PATH
echo $LD_LIBRARY_PATH
export LD_LIBRARY_PATH=
INPUT_FILE=$(terra ../../extractMetadata.t $1.sim.metadata.lua inputFile1)
echo $INPUT_FILE
./$1.exe -testplusarg inputFilename=../$INPUT_FILE -testplusarg outputFilename=$1.sim.raw -tclbatch ../../isim.cmd
export LD_LIBRARY_PATH=
terra ../../raw2bmp.t $*.sim.raw $*.sim.bmp
