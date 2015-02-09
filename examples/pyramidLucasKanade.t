import "darkroom"
darkroomSimple = require("darkroomSimple")
require "bilinear"

windowRadius = 2
iterations = 1 -- iterations per level
LEVELS = 3 -- number of levels to compute
maxResampleX = 10
maxResampleY = 10
clamp = true


function boxUpsample(inp)
  return  im(x,y) phase = {x%2,y%2}
     in (if darkroom.arrayAnd(phase=={0,0}) then inp(x/2,y/2) else
           if darkroom.arrayAnd(phase=={1,0}) then (inp(x/2,y/2)+inp((x/2)+1,y/2))/((2)) else
             if darkroom.arrayAnd(phase=={0,1}) then (inp(x/2,y/2)+inp((x/2),(y/2)+1))/((2)) else
               (inp(x/2,y/2)+inp((x/2)+1,y/2)+inp(x/2,(y/2)+1)+inp((x/2)+1,(y/2)+1))/((4))
      end end end) end
end


function invert2x2( matrix )
  local im denom(x,y) matrix[0]*matrix[3]-matrix[1]*matrix[2] end
  local im det(x,y) if denom(x,y)~=0 then 1/denom(x,y) else 0 end end
  return im(x,y) {det*matrix[3], -det*matrix[1], -det*matrix[2], det*matrix[0]} end
end

-- see here for ref: http://www.cs.ucf.edu/~mikel/Research/Optical_Flow.htm
function makeLK(frame1, frame2, vectorField)
  -- calculate stuff that we will use every iteration
  -- such as derivatives, matrix A^-1 of image gradients, weights.
  -- were calling frame1 F and frame2 G, as in the original LK paper
    
  -- calculate derivatives
  local im Fdx(x,y) (frame1(x+1,y)-frame1(x-1,y))/2 end
  local im Fdy(x,y) (frame1(x,y+1)-frame1(x,y-1))/2 end

  local im Gdx(x,y) (frame2(x+1,y)-frame2(x-1,y))/2 end
  local im Gdy(x,y) (frame2(x,y+1)-frame2(x,y-1))/2 end
                
  -- calculate A^-1
  local im A(x,y)
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
    {Fdx(x+wx,y+wy)*Fdx(x+wx,y+wy), Fdx(x+wx,y+wy)*Fdy(x+wx,y+wy), Fdx(x+wx,y+wy)*Fdy(x+wx,y+wy), Fdy(x+wx,y+wy)*Fdy(x+wx,y+wy)}
    end 
  end

  local Ainv = invert2x2(A)
  
  -- do LK calculation
  for i=1,iterations do
    local G = im(x,y) [resampleBilinear( clamp, frame2, maxResampleX, maxResampleY, im(x,y) vectorField[0] end, im(x,y) vectorField[1] end)] end
    
    im vectorField(x,y)
      -- loop over search window
      b = map  wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
            F = frame1 (x+wx, y+wy)
            in
            {Fdx(x+wx, y+wy)*(G(x+wx,y+wy)-F),Fdy(x+wx, y+wy)*(G(x+wx,y+wy)-F)}
          end
      in
        {Ainv[0](x,y)*(-b[0])+Ainv[1](x,y)*(-b[1])+vectorField[0],
         Ainv[2](x,y)*(-b[0])+Ainv[3](x,y)*(-b[1])+vectorField[1]}
    end
  end

    return vectorField
end

--local frame1Original = darkroomSimple.load("left0224.bmp")
--local frame2Original = darkroomSimple.load("right0224.bmp")

local frame1Original = darkroomSimple.load("frame10.bmp")
local frame2Original = darkroomSimple.load("frame11.bmp")


local frame1_pyramid = {}
frame1_pyramid[1] = im(x,y) [float](frame1Original(x,y)) end
local frame2_pyramid = {}
frame2_pyramid[1] = im(x,y) [float](frame2Original(x,y)) end

im fr1_pyr(x,y)  [uint8[3]] ({[frame1_pyramid[1]](x,y)*255, [frame1_pyramid[1]](x,y)*255,[frame1_pyramid[1]](x,y)*255}) end
im fr2_pyr(x,y)  [uint8[3]] ({[frame2_pyramid[1]](x,y)*255, [frame2_pyramid[1]](x,y)*255,[frame2_pyramid[1]](x,y)*255}) end

fr1_pyr:save("out/fr1_pyr.bmp", {cores=1})
fr2_pyr:save("out/fr2_pyr.bmp", {cores=1})

  -- initial condition: no offset
vectorField = {}
vectorField[LEVELS+1] = im(x,y) [float[2]]({0,0}) end
for l=2,LEVELS do
   frame1_pyramid[l] = downsampleGaussianFloat(frame1_pyramid[l-1])
   frame2_pyramid[l] = downsampleGaussianFloat(frame2_pyramid[l-1])
end


for l=LEVELS,1,-1 do
   im frame1(x,y) [float]([frame1_pyramid[l]](x,y)) end
   im frame2(x,y) [float]([frame2_pyramid[l]](x,y)) end
   local intermediateResult = makeLK( frame1, frame2, vectorField[l+1] )
   vectorField[l] = im(x,y) [boxUpsample(intermediateResult)](x,y) end
end

im outputResults(x,y) [uint8[3]]({([vectorField[1]](x,y)[0])*20+128, ([vectorField[1]](x,y)[1])*20+128, 0}) end

outputResults:save("out/pyramidlucaskanade.bmp", {cores=1})
