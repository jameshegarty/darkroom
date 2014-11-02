import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
terralib.require("bilinear")

SEARCH_RADIUS = 2 -- 60
WINDOW_RADIUS = 4
function rectify( img, remap )
  local u = im(j,i) [int8](remap[0] - 128) end
  local v = im(j,i) [int8](remap[1] - 128) end
  return resampleBilinearInt( true, im(x,y) img[1] end, uint8, 8, 8, u, v )
end

function makeOF( searchRadius, windowRadius, frame1, frame2 )
  frame1 = im(x,y) [int32](frame1) end
  frame2 = im(x,y) [int32](frame2) end

  local SAD = {}
  local offset = im(x,y)
    map i = 20, 20+searchRadius reduce(argmin)
      map ii=-windowRadius, windowRadius jj=-windowRadius, windowRadius reduce(sum) -- SAD
        darkroom.abs(frame1(x+ii,y+jj)-frame2(x+i+ii,y+jj))
      end
    end
  end

  return im(x,y) [uint8]((offset[0]-20) * (255.0 >> [math.floor(math.log(searchRadius)/math.log(2))])) end
end

local leftI = darkroomSimple.load("../../examples/left0224.bmp")
local leftR = darkroomSimple.load("../../examples/right-remap.bmp")
local rightI = darkroomSimple.load("../../examples/right0224.bmp")
local rightR = darkroomSimple.load("../../examples/left-remap.bmp")

local left = rectify(leftI, leftR)
local right = rectify(rightI, rightR)
local vectors = makeOF(SEARCH_RADIUS,WINDOW_RADIUS,right,left)
vectors:save("out/stereo.bmp")

--------------
fpgaEstimate = terralib.require("fpgaEstimate")
local est = fpgaEstimate.compile({vectors}, 1920)
io.output("out/stereoEstimate.txt")
io.write(est)
io.close()
------------
BLOCKX = 20
BLOCKY = 9
print("Build For: "..arg[1])
local o = fpga.util.deviceToOptions(arg[1])
o.uartClock=9600
local v, metadata = fpga.compile({{sensor,"uart",darkroom.type.uint(8)}},{{res,"uart",darkroom.type.uint(8)}}, 128,64, BLOCKX, BLOCKY, o)

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".v")
io.write(v)
io.close()

metadata.inputFile = "frame_128.bmp"
fpga.util.writeMetadata("out/"..s..".metadata.lua", metadata)