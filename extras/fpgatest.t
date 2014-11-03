import "darkroom"
fpga = terralib.require("fpga")

local metadata = dofile(arg[1])

local uartDevice = arg[3] or "/dev/tty.usbserial-142B"
local outputFile = arg[2]

local terra test()
  var inputImg : Image
  inputImg:load(metadata.inputFile1)

  var outputImg = fpga.util.test(uartDevice, &inputImg, metadata.stripWidth, metadata.stripHeight, metadata.minX, metadata.minY, metadata.maxX, metadata.maxY, metadata.outputShift, metadata.outputChannels, metadata.outputBytes, metadata.uartClock )

  outputImg:save(outputFile)
end

test()
