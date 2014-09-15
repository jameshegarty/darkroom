import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
terralib.require("bilinear")

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

local left = rectify(darkroomSimple.load("../../examples/left0224.bmp"), darkroomSimple.load("../../examples/right-remap.bmp"))
local right = rectify(darkroomSimple.load("../../examples/right0224.bmp"), darkroomSimple.load("../../examples/left-remap.bmp"))
local vectors = makeOF(60,4,right,left)
vectors:save("out/stereo.bmp")

fpgaEstimate = terralib.require("fpgaEstimate")
local est = fpgaEstimate.compile({vectors}, 1920)
io.output("out/stereoEstimate.txt")
io.write(est)
io.close()