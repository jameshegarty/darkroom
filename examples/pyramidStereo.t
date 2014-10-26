import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
terralib.require("darkroomDebug")
terralib.require("bilinear")

LEVELS = 2 -- including base level
SEARCH_DIST = 8 -- at each level. Total Search at base level = SEARCH_DIST * 2^(levels-1)
WINDOW_RADIUS = 4 -- SAD window radius

function rectify( img, remap )
  local u = im(j,i) [int8](remap[0] - 128) end
  local v = im(j,i) [int8](remap[1] - 128) end
  return resampleBilinearInt( true, im(x,y) img[1] end, uint8, 8, 8, u, v )
end

function makeOF( searchRadius, windowRadius, frame1, frame2, level, disparity )
  frame1 = im(x,y) [int32](frame1) end
  frame2 = im(x,y) [int32](frame2) end

  local effSearchRadius = math.pow(2,LEVELS-level)*searchRadius
  local SAD = {}
  local offset

  if level<LEVELS then
  offset= im(x,y)
    map i = 0,searchRadius reduce(argmin)
      map ii=-windowRadius, windowRadius jj=-windowRadius, windowRadius reduce(sum) -- SAD
      darkroom.abs(frame1(x+ii,y+jj)-darkroom.gather(frame2,i+ii+disparity(x/2,y/2)*2,jj,[effSearchRadius+windowRadius+searchRadius],windowRadius))
      end
    end
  end
else
  offset= im(x,y)
    map i = 0,searchRadius reduce(argmin)
      map ii=-windowRadius, windowRadius jj=-windowRadius, windowRadius reduce(sum) -- SAD
      darkroom.abs(frame1(x+ii,y+jj)-frame2(x+i+ii,y+jj))
      end
    end
  end

  end

  return im finaloutlol(x,y) [uint8](offset[0]) end
end


local left = {rectify(darkroomSimple.load("left0224.bmp"), darkroomSimple.load("right-remap.bmp"))}
local right = {rectify(darkroomSimple.load("right0224.bmp"), darkroomSimple.load("left-remap.bmp"))}

for l=2,LEVELS do
  left[l] = downsampleGaussianFloat(left[l-1])
  right[l] = downsampleGaussianFloat(right[l-1])
end

local disparity = im(x,y) [uint8](0) end

local l=LEVELS
while l>=1 do
  disparity = makeOF( SEARCH_DIST, WINDOW_RADIUS, right[l], left[l], l, disparity )
  l = l - 1
end

--left[2]:save("out/pyramidStereo.bmp",{verbose=true})

disparity:save("out/pyramidStereo.bmp",{verbose=true})