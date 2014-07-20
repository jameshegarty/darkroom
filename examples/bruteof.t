import "darkroom"
--terralib.require("darkroomDebug")
darkroomSimple = terralib.require "darkroomSimple"

-- brute force optical flow

searchWindowRadius = 2
SADWindowRadius = 2

local frame1 = darkroomSimple.load("frame10.bmp")
frame1 = im(x,y) [int](frame1) end
local frame2 = darkroomSimple.load("frame11.bmp")
frame2 = im(x,y) [int](frame2) end

im bruteofVectorField(x,y)
  map i=-searchWindowRadius, searchWindowRadius j=-searchWindowRadius, searchWindowRadius reduce(argmin)
    -- sum of absolute differences (SAD)
    map ii=-SADWindowRadius, SADWindowRadius jj=-SADWindowRadius, SADWindowRadius reduce(sum)
      darkroom.abs(frame1(x+ii,y+jj)-frame2(x+i+ii,y+j+jj))
    end
  end
end

-- convert this to an RGB image so that the user can view it
im ofRGB(x,y) [uint8[3]]({(bruteofVectorField(x,y)[0])*50+128, (bruteofVectorField(x,y)[1])*50+128, 0}) end
ofRGB:save("out/bruteof.bmp")
