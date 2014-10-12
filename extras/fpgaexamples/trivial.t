import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
fpga = terralib.require("fpga")


sensor = darkroomSimple.load("300d.bmp")
campipeline = im(x,y) {sensor, sensor+[uint8](10), sensor+[uint8](20)} end

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

local vVGA, metadata = fpga.compile({{sensor,"vga",darkroom.type.uint(8)}},{{campipeline,"vga",darkroom.type.array(darkroom.type.uint(8),3)}}, 128,64, BLOCKX, BLOCKY, fpga.util.deviceToOptions(arg[1]))

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".v")
io.write(vVGA)
io.close()
