import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
fpga = terralib.require("fpga")


sensor = darkroomSimple.load("300d.bmp")
im sensorb(x,y) map i=-1,2 j=-1,2 reduce(sum) sensor(x+i,y+j) >> [uint8](4) end end
local SHIFT = 6
campipeline = im(x,y) { (sensorb >> [uint8](SHIFT))*[uint8](2), (sensorb >> [uint8](SHIFT))*[uint8](2), sensorb>>[uint8]([SHIFT])} end

campipeline:save("out/trivial.bmp")

BLOCKX = 24
BLOCKY = 6
print("Build For: "..arg[1])
local v, metadata = fpga.compile({{sensor,"uart",darkroom.type.uint(8)}},{{campipeline,"uart",darkroom.type.array(darkroom.type.uint(8),3)}}, 128,64, BLOCKX, BLOCKY, fpga.util.deviceToOptions(arg[1]))

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".uart.v")
io.write(v)
io.close()

metadata.inputFile = "300d.bmp"
fpga.util.writeMetadata("out/"..s..".metadata.lua", metadata)

local vVGA, metadata = fpga.compile({{sensor,"vga",darkroom.type.uint(8)}},{{campipeline,"vga",darkroom.type.array(darkroom.type.uint(8),3)}}, 650,480, 784, BLOCKY, fpga.util.deviceToOptions(arg[1]))

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".v")
io.write(vVGA)
io.close()
