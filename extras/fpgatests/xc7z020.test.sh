echo "fpga -f out/$1.bit" > out/$1.xmd
sudo /opt/Xilinx/14.7/ISE_DS/EDK/bin/lin64/xmd -tcl out/$1.xmd
sudo modprobe -r ftdi_sio
sudo modprobe ftdi_sio
sudo LUA_PATH="$LUA_PATH;$DR/?.t;$DR/src/?.t;$DR/extras/?.t;$TERRADIR/tests/lib/?.t" /scratch/jhegarty/terra/terra $1.lua test frame_128.bmp out/$1.maxstencil.lua /dev/serial/by-id/usb-FTDI_FT232R_USB_UART_AJ031DOH-if00-port0