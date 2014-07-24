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
    map i = 0, searchRadius reduce(argmin)
      map ii=-windowRadius, windowRadius jj=-windowRadius, windowRadius reduce(sum) -- SAD
        darkroom.abs(frame1(x+ii,y+jj)-frame2(x+i+ii,y+jj))
      end
    end
  end

  return im(x,y) [uint8](offset[0] * 255.0/(searchRadius)) end
end

local left = rectify(darkroomSimple.load("left0224.bmp"), darkroomSimple.load("left-remap.bmp"))
local right = rectify(darkroomSimple.load("right0224.bmp"), darkroomSimple.load("right-remap.bmp"))
local vectors = makeOF(80,4,left,right)
vectors:save("out/stereo.bmp")