import "darkroom"
fpga = terralib.require("fpga")
cstdlib = terralib.includec("stdlib.h")

local metadata = dofile(arg[1])

local uartDevice = arg[3] or "/dev/tty.usbserial-142B"
local outputFile = arg[2]

local inputImgs = symbol(&Image,"inputImgs")
local loadImgsQuote = {}
local imgCnt = 0
while metadata["inputFile"..(imgCnt+1)] do 
  table.insert(loadImgsQuote, quote inputImgs[imgCnt]:load([metadata["inputFile"..(imgCnt+1)]]) end)
  imgCnt=imgCnt+1
end

local terra test()
  var [inputImgs] = [&Image](cstdlib.malloc([terralib.sizeof(Image)*imgCnt]))
  [loadImgsQuote]

  var outputImg = fpga.util.test(uartDevice, imgCnt, inputImgs, metadata.stripWidth, metadata.stripHeight, metadata.minX, metadata.minY, metadata.maxX, metadata.maxY, metadata.outputShift, metadata.outputChannels, metadata.outputBytes, metadata.uartClock )

  outputImg:save(outputFile)
end
test:printpretty()
test()
