import "darkroom"
fpga = require("fpga")
require("image")
cstdlib = terralib.includec("stdlib.h")
cstdio = terralib.includec("stdio.h")

local metadata = dofile(arg[2])

local W = metadata.stripWidth+metadata.padMinX-metadata.padMaxX
local H = metadata.stripHeight+metadata.padMinY-metadata.padMaxY


terra dopad(infile : &int8, outfile : &int8, inputBytes:int)
  cstdio.printf("PAD %s to %s\n",infile,outfile)
  cstdio.printf("W %d H %d padMinX %d padMaxX %d padMinY %d padMaxY %d\n",W,H,[metadata.padMinX],[metadata.padMaxX],[metadata.padMinY],[metadata.padMaxY])

  var inp : Image
  var totalSize = W*H*inputBytes
  inp.dataPtr = cstdlib.malloc(totalSize)
  inp.data = inp.dataPtr
  
  var imgIn = cstdio.fopen(infile, "rb");
  cstdio.fread(inp.dataPtr,1,totalSize,imgIn)
  cstdio.fclose(imgIn)
  inp.width = W
  inp.height = H
  inp.stride = W
  inp.channels = 1
  inp.bits = 8
  inp.floating = false
  inp.SOA = false
  inp.sparse = false
  inp.isSigned = false
  
  var padded = fpga.util.padImg(&inp,[metadata.padMinX],[metadata.padMaxX],[metadata.padMinY],[metadata.padMaxY])

  cstdio.printf("RES W %d H %d bits %d\n",padded.width,padded.height,padded.bits)
  var imgOut = cstdio.fopen(outfile, "wb");
  cstdio.fwrite(padded.data,padded.width*padded.height*(padded.bits/8),1,imgOut)
  cstdio.fclose(imgOut)
end


local i=1
while metadata["inputFile"..i] do
  dopad("../"..metadata["inputFile"..i], arg[1].."."..metadata["inputFile"..i],metadata["inputBytes"..i])
  i = i + 1
end