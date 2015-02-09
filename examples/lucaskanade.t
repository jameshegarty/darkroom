import "darkroom"
darkroomSimple = require("darkroomSimple")
require "bilinear"

windowRadius = 2
iterations = 1 -- iterations per level
maxResampleX = 10
maxResampleY = 10
clamp = true

function invert2x2( matrix )
  local im denom(x,y) matrix[0]*matrix[3]-matrix[1]*matrix[2] end
  local im det(x,y) if denom(x,y)~=0 then 1/denom(x,y) else 0 end end
  return im(x,y) {det*matrix[3], -det*matrix[1], -det*matrix[2], det*matrix[0]} end
end

-- see here for ref: http://www.cs.ucf.edu/~mikel/Research/Optical_Flow.htm
function makeLK(frame1, frame2)
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
  
  -- initial condition: no offset
  local im vectorField(x,y) [float[2]]({0,0}) end

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

  return im(x,y) [uint8[3]]({(vectorField(x,y)[0])*50+128, (vectorField(x,y)[1])*50+128, 0}) end
end

local frame1 = darkroomSimple.load("frame10.bmp")
im frame1(x,y) [float](frame1) end
local frame2 = darkroomSimple.load("frame11.bmp")
im frame2(x,y) [float](frame2) end

makeLK( frame1, frame2 ):save("out/lucaskanade.bmp")