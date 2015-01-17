import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
fpga = terralib.require("fpga")

CONV_DEPTH = 3
CONV_WIDTH = 2
sensor = darkroomSimple.load("frame_128.bmp")


local ks = 1
function makeKern(n)
  local k = {}
  local sum = 0
  for i=1,n do table.insert(k,ks); sum = sum + ks; ks = ks + 1; if ks > 12 then ks=1 end end
  return k, sum
end

local res = sensor
for d=1,CONV_DEPTH do
--  local k,sum = makeKern(math.pow(CONV_WIDTH*2+1,2))
  local sum = 0
  for i=-CONV_WIDTH, CONV_WIDTH do
    for j=-CONV_WIDTH, CONV_WIDTH do
      sum = sum + (i+CONV_WIDTH)*(j+CONV_WIDTH)
    end
  end

  im res(x,y) map i=-CONV_WIDTH,CONV_WIDTH j=-CONV_WIDTH,CONV_WIDTH reduce(sum) [uint16](res(x+i,y+j))*[uint16]((CONV_WIDTH+i)*(CONV_WIDTH+j)) end end
  im res(x,y) [uint8](res >> [uint8]([math.floor(math.log(sum)/math.log(2))])) end
end

res:save("out/conv.bmp")

BLOCKX = 55
BLOCKY = 55
print("Build For: "..arg[1])
local o = fpga.util.deviceToOptions(arg[1])
o.uartClock=9600
local v, metadata = fpga.compile({{sensor,"uart","frame_128.bmp"}},{{res,"uart",darkroom.type.uint(8)}}, 128, 64, o)

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".v")
io.write(v)
io.close()

metadata.inputFile = "frame_128.bmp"
fpga.util.writeMetadata("out/"..s..".metadata.lua", metadata)

------------------
local v, metadata = fpga.compile({{sensor,"axi","frame_128.bmp"}},{{res,"axi"}}, 128, 64, o)

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".axi.v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/"..s..".axi.metadata.lua", metadata)

------------------
local opt = fpga.util.deviceToOptions(arg[1])
opt.stripWidth=128
opt.stripHeight=64
local v, metadata = fpga.compile({{sensor,"sim","frame_128.raw"}},{{res,"sim"}}, 128, 64, o)

io.output("out/conv.sim.v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/conv.sim.metadata.lua", metadata)
