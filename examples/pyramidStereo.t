import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
--terralib.require("darkroomDebug")
terralib.require("bilinear")

LEVELS = 3 -- including base level
BASE_SEARCH = 40
SEARCH_DIST = 4 -- at each level. Total Search at base level = SEARCH_DIST * 2^(levels-1)
WINDOW_RADIUS = 4 -- SAD window radius
DEBUG = false

function rectify( img, remap )
  local u = im(j,i) [int8](remap[0] - 128) end
  local v = im(j,i) [int8](remap[1] - 128) end
  return resampleBilinearInt( true, im(x,y) img[1] end, uint8, 8, 8, u, v )
end

function boxUpsample(inp)
  return  im(x,y) phase = {x%2,y%2}
     in (if darkroom.arrayAnd(phase=={0,0}) then inp(x/2,y/2) else
     if darkroom.arrayAnd(phase=={1,0}) then (inp(x/2,y/2)/[uint8](2))+(inp((x/2)+1,y/2)/[uint8](2)) else
     if darkroom.arrayAnd(phase=={0,1}) then (inp(x/2,y/2)/[uint8](2))+(inp((x/2),(y/2)+1)/[uint8](2)) else
     (inp(x/2,y/2)/[uint8](4))+(inp((x/2)+1,y/2)/[uint8](4))+(inp(x/2,(y/2)+1)/[uint8](4))+(inp((x/2)+1,(y/2)+1)/[uint8](4))
     end end end) end --*[uint8](0)+[uint8](x%2) end
end

function makeOF( searchRadius, windowRadius, frame1, frame2, level, disparity )
  frame1 = im(x,y) [int32](frame1) end
  frame2 = im(x,y) [int32](frame2) end

  local effSearchRadius = math.pow(2,LEVELS-level-1)*searchRadius
  local SAD = {}
  local offset

  if level<LEVELS then
    local disp = boxUpsample(disparity)
    disp:save("out/pyramidStereo.US."..level..".bmp",{debug=DEBUG})
--    local disp = im(x,y) disparity(x/2,y/2) end
    offset= im(x,y)
      map i = -searchRadius,searchRadius reduce(argmin)
        map ii=-windowRadius, windowRadius jj=-windowRadius, windowRadius reduce(sum) -- SAD
          darkroom.abs(frame1(x+ii,y+jj)-darkroom.gather(frame2,i+ii+disp*2,jj,[BASE_SEARCH+effSearchRadius+windowRadius],windowRadius))
        end
      end
end
return im finaloutlol(x,y) [uint8](offset[0]+disp*2) end
--return im finaloutlol(x,y) [uint8](disparity(x/2,y/2)*2) end
  else
    offset= im(x,y)
      map i = 0,BASE_SEARCH reduce(argmin)
        map ii=-windowRadius, windowRadius jj=-windowRadius, windowRadius reduce(sum) -- SAD
          darkroom.abs(frame1(x+ii,y+jj)-frame2(x+i+ii,y+jj))
        end
      end
end
return im finaloutlol(x,y) [uint8](offset[0]) end
  end


end


local left = {rectify(darkroomSimple.load("left0224.bmp"), darkroomSimple.load("right-remap.bmp"))}
local right = {rectify(darkroomSimple.load("right0224.bmp"), darkroomSimple.load("left-remap.bmp"))}

for l=2,LEVELS do
  left[l] = downsampleGaussianFloat(left[l-1])
  right[l] = downsampleGaussianFloat(right[l-1])
end

local disparity = {}
disparity[LEVELS+1] = im(x,y) [uint8](0) end

local l=LEVELS
while l>=1 do
  disparity[l] = makeOF( SEARCH_DIST, WINDOW_RADIUS, right[l], left[l], l, disparity[l+1] )
  disparity[l]:save("out/pyramidStereo."..l..".bmp",{verbose=true, debug=DEBUG})
  l = l - 1
end

disparity[1]:save("out/pyramidStereo.bmp",{verbose=true, debug=DEBUG})