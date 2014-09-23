import "darkroom"
fpga = terralib.require("fpga")
fpgaEstimate = terralib.require("fpgaEstimate")
darkroomSimple = terralib.require("darkroomSimple")
terralib.require("image")

if arg[1]=="cpu" then
  testinput = darkroomSimple.load(arg[2])
else
  testinput = darkroom.input(uint8)
end

BLOCKX = 128
BLOCKY = 4
local UART_DELAY = 300000

local uart = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>//Used for UART
#include <fcntl.h>//Used for UART
#include <termios.h>//Used for UART

int uart0_filestream;

void init(char* device){
  //-------------------------
  //----- SETUP USART 0 -----
  //-------------------------
  //At bootup, pins 8 and 10 are already set to UART0_TXD, UART0_RXD (ie the alt0 function) respectively
  uart0_filestream = -1;
                                                                      
  //OPEN THE UART
  //The flags (defined in fcntl.h):
  //Access modes (use 1 of these):
  //O_RDONLY - Open for reading only.
  //O_RDWR - Open for reading and writing.
  //O_WRONLY - Open for writing only.
  //
  //O_NDELAY / O_NONBLOCK (same function) - Enables nonblocking mode. When set read requests on the file can return immediately with a failure status
  //if there is no input immediately available (instead of blocking). Likewise, write requests can also return
  //immediately with a failure status if the output can't be written immediately.
  //
  //O_NOCTTY - When set and path identifies a terminal device, open() shall not cause the terminal device to become the controlling terminal for the process.
  uart0_filestream = open(device, O_RDWR | O_NOCTTY | O_NDELAY);//Open in non blocking read/write mode
  if (uart0_filestream == -1)
  {
    //ERROR - CAN'T OPEN SERIAL PORT
    printf("Error - Unable to open UART %s.  Ensure it is not in use by another application\n",device);
    exit(1);
  }

//  int ret = fcntl(uart0_filestream, F_SETFL, O_RDWR);
//  if (ret < 0) {
//    perror("fcntl");
//    exit(-1);
//  }

  //CONFIGURE THE UART
  //The flags (defined in /usr/include/termios.h - see http://pubs.opengroup.org/onlinepubs/007908799/xsh/termios.h.html):
  //Baud rate:- B1200, B2400, B4800, B9600, B19200, B38400, B57600, B115200, B230400, B460800, B500000, B576000, B921600, B1000000, B1152000, B1500000, B2000000, B2500000, B3000000, B3500000, B4000000
  //CSIZE:- CS5, CS6, CS7, CS8
  //CLOCAL - Ignore modem status lines
  //CREAD - Enable receiver
  //IGNPAR = Ignore characters with parity errors
  //ICRNL - Map CR to NL on input (Use for ASCII comms where you want to auto correct end of line characters - don't use for bianry comms!)
  //PARENB - Parity enable
  //PARODD - Odd parity (else even)
  struct termios options;
  tcgetattr(uart0_filestream, &options);
  options.c_cflag = B57600 | CS8 | CLOCAL | CREAD;//<Set baud rate
  options.c_iflag = IGNPAR;
  options.c_oflag = 0;
  options.c_lflag = 0;

  cfmakeraw(&options); 
//  cfsetspeed(&options, B230400);
//  cfsetspeed(&options, B115200);
  cfsetspeed(&options, B57600);

  tcflush(uart0_filestream, TCIFLUSH);
  tcsetattr(uart0_filestream, TCSANOW, &options);
}

void transmit(unsigned char* tx_buffer, int size){
  printf("SEND %s\n",tx_buffer);
  if (uart0_filestream != -1){
    int count = write(uart0_filestream, tx_buffer, size); //Filestream, bytes to write, number of bytes to write
    if (count < 0){
      printf("UART TX error\n");
    }
    printf("tx COUNT %d\n",count);
  }
}

int receive(unsigned char* rx_buffer, int expectedSize){
  //----- CHECK FOR ANY RX BYTES -----
  if (uart0_filestream != -1){
    // Read up to 255 characters from the port if they are there
    int rx_length = read(uart0_filestream, rx_buffer, expectedSize); //Filestream, buffer to store in, number of bytes to read (max)

    if (rx_length < 0){
      //An error occured (will occur if there are no bytes)
      printf("RX error, len <0\n");
    }else if (rx_length == 0){
      //No data waiting
      printf("Error, no data\n");
    }else{
      //Bytes received
      rx_buffer[rx_length] = '\0';
      printf("%d bytes read : %s\n", rx_length, rx_buffer);
      printf("%d\n",rx_length);
    }

    return rx_length;
  }
}

void closeuart(){
  close(uart0_filestream);
}
                                      ]]
local terra pad(infile : &int8, outfile : &int8, left:int, right:int, top:int, bottom:int)

  var imgIn : Image
  imgIn:load(infile)

  var w = imgIn.width+(right-left)
  var h = imgIn.height+(top-bottom)
  var d = uart.malloc(w*h*(imgIn.bits/8))

  var id = [&uint8](d)
  for y=bottom,imgIn.height+top do
    for x=left,imgIn.width+right do
      var rx = x-left
      var ry = y-bottom
      if y<0 or x<0 or y>=imgIn.height or x>=imgIn.width then
        id[ry*w+rx] = 0
      else
        id[ry*w+rx] = [&uint8](imgIn.data)[y*imgIn.width+x]
      end
    end
  end

  var imgOut : Image
  imgOut:initSimple(w,h,imgIn.channels, imgIn.bits,imgIn.floating, imgIn.isSigned,imgIn.SOA,d)
  imgOut:save(outfile)

end

function test(inast)
  assert(darkroom.ast.isAST(inast))

  print("TEST",arg[1],arg[2])
  if arg[1]=="est" then
    local est,pl = fpgaEstimate.compile({inast}, 640)
    io.output("out/"..arg[0]..".est.txt")
    io.write(est)
    io.close()
    io.output("out/"..arg[0]..".perlineest.txt")
    io.write(pl)
    io.close()
  elseif arg[1]=="build" then
    local v, maxStencil = fpga.compile( {{testinput,"uart"}}, {{inast,"uart"}}, BLOCKX, BLOCKY)
    local s = string.sub(arg[0],1,#arg[0]-4)
    io.output("out/"..s..".v")
    io.write(v)
    io.close()

    pad(arg[2], "out/"..s..".input.bmp", maxStencil:min(1), maxStencil:max(1), maxStencil:min(2), maxStencil:max(2))
  elseif arg[1]=="test" then
    print("TEST")
    uart.init(arg[3] or "/dev/tty.usbserial-142B")

    local terra procim(filename:&int8)
      var txbuf = [&uint8](uart.malloc(2048));
      var rxbuf = [&uint8](uart.malloc(2048));

      var img : Image

      img:load([arg[2]])

      var bw = img.width/BLOCKX
      var bh = img.height/BLOCKY

      var retries = 0

      for by=0,bh do
        for bx=0,bw do
          ::RESTART::
          uart.printf("BX %d/%d BY %d/%d\n",bx,bw,by,bh)

          for y=0,BLOCKY do
            for x=0,BLOCKX do
              txbuf[y*BLOCKX+x] = [&uint8](img.data)[(by*BLOCKY+y)*img.width+(bx*BLOCKX+x)]
              rxbuf[y*BLOCKX+x] = 0;
            end
          end

          var txcrc : uint8 
          txcrc = 0
          for i=0,BLOCKX*BLOCKY do 
            txcrc = txcrc + txbuf[i] 
--            uart.printf("tx CRC %d %d\n",txbuf[i],txcrc)
          end

          uart.transmit(txbuf,BLOCKX*BLOCKY)
          uart.usleep(UART_DELAY);
          var rsx : int
          rsx = uart.receive(rxbuf,BLOCKX*BLOCKY+2)

--          if rsx <= 0 then
          if rsx < BLOCKX*BLOCKY+2 then
            uart.printf("no data, attempting to restart. press key\n")
--            while uart.getchar()~=32 do end
            while true do
              uart.printf("SENDBYTE\n")
              uart.transmit(txbuf,1)
              uart.usleep(UART_DELAY);
              var rsxx = uart.receive(rxbuf,BLOCKX*BLOCKY+2)
              if rsxx>0 then break end
            end
            uart.printf("DONe\n")

            uart.printf("RESTART\n")
            retries = retries + 1
            goto RESTART
--            uart.exit(1);
          end

          -- check CRC
          var crc : uint8 
          crc = 0
          for i=0,BLOCKX*BLOCKY do 
            crc = crc + rxbuf[i] 
--            uart.printf("CRC %d %d\n",rxbuf[i],crc)
          end
          
          if crc ~= rxbuf[BLOCKX*BLOCKY] then
            uart.printf("CRC ERROR %d %d\n",crc,rxbuf[BLOCKX*BLOCKY])
            retries = retries + 1
            goto RESTART
--            uart.exit(1)
          end

          if txcrc ~= rxbuf[BLOCKX*BLOCKY+1] then
            uart.printf("tx CRC ERROR %d %d\n",txcrc,rxbuf[BLOCKX*BLOCKY+1])
            retries = retries + 1
            goto RESTART
--            uart.exit(1)
          end

          for y=0,BLOCKY do
            for x=0,BLOCKX do
              [&uint8](img.data)[(by*BLOCKY+y)*img.width+(bx*BLOCKX+x)] = rxbuf[y*BLOCKX+x]
            end
          end

        end
      end

      uart.printf("RETRIES: %d\n",retries)
      img:save(filename)

    end

    procim("out/"..arg[0]..".fpga.bmp")

    uart.closeuart()

    
  else
    if darkroom.ast.isAST(inast) then inast = {inast} end

    local terra dosave(img: &Image, filename : &int8)
      img:save(filename)
      img:free()
    end

    local tprog = darkroomSimple.compile(inast,{debug=true, verbose=true, printruntime=true})

    local res = pack(unpacktuple(tprog()))
    for k,v in ipairs(res) do
      print(v)
      local st = ""
      if k>1 then st = "."..k end
      dosave(v,"out/"..arg[0]..st..".bmp")
    end
  end
end