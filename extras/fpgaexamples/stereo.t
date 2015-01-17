import "darkroom"
--terralib.require("darkroomDebug")
darkroomSimple = terralib.require("darkroomSimple")
terralib.require("bilinear")
fpga = terralib.require("fpga")

SEARCH_RADIUS = 10
WINDOW_RADIUS = 4
DO_RECTIFY = false

print("Search Radius",SEARCH_RADIUS)

function rectify( img, remapX, remapY )
  local u = im(j,i) [int8](remapX - 128) end
  local v = im(j,i) [int8](remapY - 128) end
  return resampleBilinearInt( true, img, uint8, 8, 8, u, v )
end

function makeOF( searchRadius, windowRadius, frame1, frame2 )
--  frame1 = im(x,y) [int32](frame1) end
--  frame2 = im(x,y) [int32](frame2) end

  local SAD = {}
  local offset = im(x,y)
    map i = 20, 20+searchRadius reduce(argmin)
      map ii=-windowRadius, windowRadius jj=-windowRadius, windowRadius reduce(sum) -- SAD
        [uint16](darkroom.abs([int16](frame1(x+ii,y+jj))-[int16](frame2(x+i+ii,y+jj))))
      end
    end
  end

  return im(x,y) [uint8]((offset[0]-20) * (255.0 >> [math.floor(math.log(searchRadius)/math.log(2))])) end
end

local leftI = darkroomSimple.load("left0224_sm.bmp")
--local leftI = darkroomSimple.load("left0224.bmp")
local leftRx = darkroomSimple.load("right-remap-x.bmp")
local leftRy = darkroomSimple.load("right-remap-y.bmp")
--local rightI = darkroomSimple.load("right0224.bmp")
local rightI = darkroomSimple.load("right0224_sm.bmp")
local rightRx = darkroomSimple.load("left-remap-x.bmp")
local rightRy = darkroomSimple.load("left-remap-y.bmp")

local left
local right

if DO_RECTIFY then
  left = rectify(leftI, leftRx, leftRy)
  right = rectify(rightI, rightRx, rightRy)
else
  left = leftI
  right = rightI
end

local vectors = makeOF(SEARCH_RADIUS,WINDOW_RADIUS,right,left)
vectors:save("out/stereo.bmp")

--------------
fpgaEstimate = terralib.require("fpgaEstimate")
local est, perline = fpgaEstimate.compile({vectors}, 1920)
io.output("out/stereoEstimate.txt")
io.write(est)
io.close()
io.output("out/stereoEstimatePerline.txt")
io.write(perline)
io.close()
------------
BLOCKX = 30
BLOCKY = 9
print("Build For: "..arg[1])
local o = fpga.util.deviceToOptions(arg[1])
o.uartClock=9600
local v, metadata = fpga.compile({{leftI,"uart","left0224_sm.bmp"},{rightI,"uart","right0224_sm.bmp"}},{{vectors,"uart",darkroom.type.uint(8)}}, 300,20, o)

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".uart.v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/"..s..".uart.metadata.lua", metadata)

------------------
local v, metadata = fpga.compile({{leftI,"axi","left0224_sm.bmp"},{rightI,"axi","right0224_sm.bmp"}},{{vectors,"axi"}}, 300,20, o)

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".axi.v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/"..s..".axi.metadata.lua", metadata)

------------------
local opt = fpga.util.deviceToOptions(arg[1])
opt.stripWidth=256
opt.stripHeight=20
local v, metadata = fpga.compile({{leftI,"sim","left0224_sm.raw"},{rightI,"sim","right0224_sm.raw"}},{{vectors,"sim"}}, opt.stripWidth, opt.stripHeight, opt)

io.output("out/stereo.sim.v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/stereo.sim.metadata.lua", metadata)
