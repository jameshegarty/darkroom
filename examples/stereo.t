import "darkroom"
darkroomSimple = require("darkroomSimple")
require("bilinear")

function rectify( img, remap )
  local u = im(j,i) [int8](remap[0] - 128) end
  local v = im(j,i) [int8](remap[1] - 128) end
  return resampleBilinearInt( true, im(x,y) img[1] end, uint8, 8, 8, u, v )
end

function makeOF( searchRad, windowRad, frame1, frame2 )
  frame1 = im(x,y) [int32](frame1) end
  frame2 = im(x,y) [int32](frame2) end

  local SAD = {}
  local offset = im(x,y)
    map i = 20, 20+searchRad reduce(argmin)
      map ii=-windowRad, windowRad jj=-windowRad, windowRad reduce(sum) -- SAD
        darkroom.abs(frame1(x+ii,y+jj)-frame2(x+i+ii,y+jj))
      end
    end
  end

  return im(x,y) [uint8]((offset[0]-20) * 255.0/(searchRad)) end
end

local left = darkroomSimple.load("left0224.bmp")
local leftOffsets = darkroomSimple.load("right-remap.bmp")
local leftRectified = rectify(left, leftOffsets)

local right = darkroomSimple.load("right0224.bmp")
local rightOffsets = darkroomSimple.load("left-remap.bmp")
local rightRectified = rectify(right, rightOffsets)

local vectors = makeOF(60,4,rightRectified,leftRectified)
vectors:save("out/stereo.bmp")