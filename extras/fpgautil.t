local fpgaUtil = {}
terralib.require("image")



local uart = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>//Used for UART
#include <fcntl.h>//Used for UART
#include <termios.h>//Used for UART
#include <math.h>

int uart0_filestream;

void init(char* device, int uartClock){
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
  if(uartClock==57600){
    options.c_cflag = B57600 | CS8 | CLOCAL | CREAD;//<Set baud rate
  }else if(uartClock==19200){
    options.c_cflag = B19200 | CS8 | CLOCAL | CREAD;//<Set baud rate
  }else if(uartClock==9600){
    options.c_cflag = B9600 | CS8 | CLOCAL | CREAD;//<Set baud rate
printf("9600 B \n");
  }else{
    printf("Unsupported uart clock %d\n",uartClock);
    exit(1);
  } 
  options.c_iflag = IGNPAR;
  options.c_oflag = 0;
  options.c_lflag = 0;

  cfmakeraw(&options); 

  tcflush(uart0_filestream, TCIFLUSH);
  tcsetattr(uart0_filestream, TCSANOW, &options);
}

void transmit(unsigned char* tx_buffer, int size){
//  printf("SEND %s\n",tx_buffer);
  printf("SEND\n");
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
//      printf("%d bytes read : %s\n", rx_length, rx_buffer);
      printf("%d bytes read\n", rx_length);
//      printf("%d\n",rx_length);
    }

    return rx_length;
  }
}

void closeuart(){
  close(uart0_filestream);
}
                                      ]]
local terra pad(infile : &int8, outfile : &int8, left:int, right:int, bottom:int, top:int)

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
  imgOut:initSimple(w,h,imgIn.channels, imgIn.bits,imgIn.floating, imgIn.isSigned,imgIn.SOA,false,d)
  imgOut:save(outfile)

end

local terra padImg(imgIn : &Image, left:int, right:int, bottom:int, top:int)

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
  imgOut:initSimple(w,h,imgIn.channels, imgIn.bits,imgIn.floating, imgIn.isSigned,imgIn.SOA,false,d)
--  imgOut:save(outfile)
  return imgOut

end

function fpgaUtil.deviceToOptions(dev)
  print("DEV TO",dev)
  if dev=="xc7z020" then
    return {clockMhz=100}
  else
    return {}
  end
end

terra fpgaUtil.test(
  uartDevice : &int8,
  inputImage : &Image, 
  BLOCKX : uint, 
  BLOCKY : int, 
  stencilMinX : int, 
  stencilMinY :int, 
  stencilMaxX : int,
  stencilMaxY : int,
  outputShift : int,
  outputChannels : int,
  outputBytes : int,
  uartClock : int)

  var UART_DELAY = 800*(BLOCKX*BLOCKY*outputBytes)*(57600/uartClock)
  cstdio.printf("UART_DELAY %d\n", UART_DELAY)

  uart.init(uartDevice, uartClock)

  var txbuf = [&uint8](uart.malloc(2048));
  var txbufInt16 = [&int16](txbuf)
  var rxbuf = [&uint8](uart.malloc(2048));
  
  var paddedImg = padImg(inputImage, stencilMinX, stencilMaxX, stencilMinY, stencilMaxY)
  
  var imgOut : Image
  imgOut:allocateDarkroomFormat(inputImage.width, inputImage.height, 1, outputChannels, (outputBytes/outputChannels)*8,false,false,false)
  imgOut.SOA=false

  -- each block has an area around its perimeter that's invalid b/c the stencil
  -- is reading invalid stuff. So we have to pad each block by the stencil size.
  var BLOCKX_core = BLOCKX + stencilMinX - stencilMaxX
  var BLOCKY_core = BLOCKY + stencilMinY - stencilMaxY
  
  if BLOCKX_core<=0 or BLOCKY_core<=0 then
    uart.printf("ERROR: block too small for this stencil %d %d\n",BLOCKX_core, BLOCKY_core)
    uart.exit(1)
  end
  
  var bw = [int](uart.ceil([float](inputImage.width)/[float](BLOCKX_core)))
  var bh = [int](uart.ceil([float](inputImage.height)/[float](BLOCKY_core)))
  
  var retries = 0
  
  var metadataBytes = 4
  
  for by=0,bh do
    for bx=0,bw do
      ::RESTART::
      uart.printf("BX %d/%d BY %d/%d\n",bx,bw,by,bh)
      
      for i=0,metadataBytes do
        txbuf[i] = 0
      end
      txbufInt16[0]=[int16](bx*BLOCKX_core+stencilMinX) -- x coord of first input pixel
      txbufInt16[1]=[int16](by*BLOCKY_core+stencilMinY) -- y coord of first input pixel
      uart.printf("SENT XY %d %d\n",txbufInt16[0], txbufInt16[1])
      
      for y=0,BLOCKY do
        for x=0,BLOCKX do
          var px = bx*BLOCKX_core+x
          var py = by*BLOCKY_core+y
          
          if px>=paddedImg.width or py>=paddedImg.height then
            txbuf[y*BLOCKX+x+metadataBytes] = 0
          else
            txbuf[y*BLOCKX+x+metadataBytes] = [&uint8](paddedImg.data)[py*paddedImg.width+px]
          end
          
          rxbuf[y*BLOCKX+x] = 0;
        end
      end
      
      var txcrc : uint8 
      txcrc = 0
      for i=0,BLOCKX*BLOCKY+metadataBytes do 
        txcrc = txcrc + txbuf[i] 
--            uart.printf("tx CRC %d %d\n",txbuf[i],txcrc)
      end
      
      uart.transmit(txbuf,BLOCKX*BLOCKY+metadataBytes)
      uart.usleep(UART_DELAY);
      var rsx : int
      rsx = uart.receive(rxbuf,BLOCKX*BLOCKY*outputBytes+2)
      
--          if rsx <= 0 then
      if rsx <= 0 then
        uart.printf("no data, assuming tx got dropped. attempting to restart. press key\n")
        --            while uart.getchar()~=32 do end
        while true do
          uart.printf("SENDBYTE\n")
          uart.transmit(txbuf,1)
          uart.usleep(UART_DELAY);
          var rsxx = uart.receive(rxbuf,BLOCKX*BLOCKY*outputBytes+2)
          if rsxx>0 then break end
        end
        uart.printf("DONe\n")
        
        uart.printf("RESTART\n")
        retries = retries + 1
        goto RESTART
        --            uart.exit(1);
      elseif rsx < BLOCKX*BLOCKY*outputBytes+2 then
        uart.printf("missing data, retrying\n")
        goto RESTART
      end
      
      -- check CRC
      var crc : uint8 
      crc = 0
      for i=0,BLOCKX*BLOCKY*outputBytes do 
        crc = crc + rxbuf[i] 
--            uart.printf("CRC %d %d\n",rxbuf[i],crc)
      end
      
      if crc ~= rxbuf[BLOCKX*BLOCKY*outputBytes] then
        uart.printf("CRC ERROR %d %d\n",crc,rxbuf[BLOCKX*BLOCKY])
        retries = retries + 1
        goto RESTART
        --            uart.exit(1)
      end
      
      if txcrc ~= rxbuf[BLOCKX*BLOCKY*outputBytes+1] then
        uart.printf("tx CRC ERROR %d %d\n",txcrc,rxbuf[BLOCKX*BLOCKY*outputBytes+1])
        retries = retries + 1
        goto RESTART
        --            uart.exit(1)
      end
      
      
      uart.printf("Write Out\n")
      
      var shiftY = [int](outputShift)/[int](BLOCKX)
      var shiftX = [int](outputShift)-shiftY*[int](BLOCKX)
      var tlX = [int](txbufInt16[0])
      var tlY = [int](txbufInt16[1])
      uart.printf("SHIFT %d %d, %d %d\n", shiftX, shiftY, tlX,tlY)
      
      for y=-stencilMinY+shiftY, BLOCKY_core-stencilMinY+shiftY do
        for x=-stencilMinX+shiftX, BLOCKX_core-stencilMinX+shiftX do
          
          var px = bx*BLOCKX_core+x+stencilMinX-shiftX
          var py = by*BLOCKY_core+y+stencilMinY-shiftY
          
          if px<imgOut.width and py<imgOut.height and px>=0 and py>=0 then
            for c=0, outputChannels do
              [&uint8](imgOut.data)[py*imgOut.width*outputChannels+px*outputChannels+c] = rxbuf[(y)*BLOCKX*outputChannels+(x*outputChannels)+c]
            end
          end
        end
      end
      
    end
  end
  
  uart.printf("RETRIES: %d\n",retries)
  
  uart.closeuart()
  return imgOut
end

function fpgaUtil.writeMetadata(filename, metadata)
    io.output(filename)
    io.write("return {")
    for k,v in pairs(metadata) do
      if type(v)=="number" then io.write(k.."="..v..",") elseif type(v)=="string" then io.write(k.."='"..v.."',") else print("BT",type(v),k);assert(false) end
    end
    io.write("rofl=1}")
    io.close()
end


return fpgaUtil