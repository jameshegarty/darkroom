import "darkroom"
fpga = terralib.require("fpga")
fpgaEstimate = terralib.require("fpgaEstimate")
darkroomSimple = terralib.require("darkroomSimple")
terralib.require("image")

if arg[1]~="est" and arg[1]~=nil then
  testinput = darkroomSimple.load(arg[1])
else
  testinput = darkroom.input(uint8)
end

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
    printf("Error - Unable to open UART.  Ensure it is not in use by another application\n");
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
  options.c_cflag = B230400 | CS8 | CLOCAL | CREAD;//<Set baud rate
  options.c_iflag = IGNPAR;
  options.c_oflag = 0;
  options.c_lflag = 0;

  cfmakeraw(&options); 
  cfsetspeed(&options, B230400);

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
    printf("COUNT %d\n",count);
  }
}

void receive(unsigned char* rx_buffer, int expectedSize){
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
      printf("%i bytes read : %s\n", rx_length, rx_buffer);
    }
  }
}

void closeuart(){
  close(uart0_filestream);
}
                                      ]]

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
  elseif arg[1]==nil then
    local v = fpga.compile( {{testinput,"uart"}}, {{inast,"uart"}}, 32,32)
    local s = string.sub(arg[0],1,#arg[0]-4)
    io.output("out/"..s..".v")
    io.write(v)
    io.close()
  elseif arg[2]=="test" then
    print("TEST")
    uart.init("/dev/tty.usbserial-141B")

    local terra uarttest()
      uart.transmit([&uint8]("aaaabbbbccccdddd"),16)

      uart.usleep(1000000);
      var rxbuf = [&uint8](uart.malloc(256));
      uart.receive(rxbuf,16);
      uart.printf("E %s\n",rxbuf);
    end

--    for i=0,9 do
--      uarttest()
--    end

    local terra procim()
      var txbuf = [&uint8](uart.malloc(2048));
      var rxbuf = [&uint8](uart.malloc(2048));



      var img : Image

      img:load([arg[1]])

      var bw = img.width/32
      var bh = img.height/32

      for by=0,bh do
        for bx=0,bw do
          for y=0,32 do
            for x=0,32 do
              txbuf[y*32+x] = [&uint8](img.data)[(by*32+y)*img.width+(bx*32+x)]
            end
          end

          uart.transmit(txbuf,32*32)
          uart.usleep(2000000);
          uart.receive(rxbuf,32*32)

          for y=0,32 do
            for x=0,32 do
              [&uint8](img.data)[(by*32+y)*img.width+(bx*32+x)] = rxbuf[y*32+x]
            end
          end

        end
      end

      img:save("LOL.bmp")

    end

    procim()

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