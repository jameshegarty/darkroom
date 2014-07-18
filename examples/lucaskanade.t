import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
terralib.require "bilinear"


windowRadius = 2
iterations = 1 -- iterations per level
weighted = false
maxResampleX = 10
maxResampleY = 10
clamp = true

width = 584
height = 388

-- see here for ref: http://www.cs.ucf.edu/~mikel/Research/Optical_Flow.htm
function makeLK(frame1, frame2)
  -- calculate stuff that we will use every iteration
  -- such as derivatives, matrix A^-1 of image gradients, weights.
  -- were calling frame1 F and frame2 G, as in the original LK paper
    
  -- calculate derivatives
  local im Fx(x,y) (frame1(x+1,y)-frame1(x-1,y))/2 end
  local im Fy(x,y) (frame1(x,y+1)-frame1(x,y-1))/2 end

  local im Gx(x,y) (frame2(x+1,y)-frame2(x-1,y))/2 end
  local im Gy(x,y) (frame2(x,y+1)-frame2(x,y-1))/2 end
                
  -- calculate weight W
  local W
  if weighted then
    local im w(x,y) darkroom.sqrt(darkroom.pow(Gx(x,y)-Fx(x,y),2)+darkroom.pow(Gy(x,y)-Fy(x,y),2)) end
    im W(x,y)  if w(x,y)~=0 then 1/w(x,y) else 100 end end
  else
    im W(x,y)  1 end
  end
        
  -- calculate A^-1

  local im Atemp0(x,y)
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
    Fx(x+wx,y+wy)*Fx(x+wx,y+wy)*W(x+wx,y+wy)
    end 
  end

  local im Atemp1(x,y)
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
      Fx(x+wx,y+wy)*Fy(x+wx,y+wy)*W(x+wx,y+wy)
    end 
  end

  local Atemp2 = Atemp1

  local im Atemp3(x,y)
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
      Fy(x+wx,y+wy)*Fy(x+wx,y+wy)*W(x+wx,y+wy)
    end 
  end

  local im denom(x,y) (Atemp0(x,y)*Atemp3(x,y)-Atemp1(x,y)*Atemp2(x,y)) end
  local im det(x,y) if denom(x,y)~=0 then 1/denom(x,y) else 0 end end
                
  local im A0(x,y) det(x,y)*Atemp3(x,y) end
  local im A1(x,y) -det(x,y)*Atemp1(x,y) end
  local im A2(x,y) -det(x,y)*Atemp2(x,y) end
  local im A3(x,y) det(x,y)*Atemp0(x,y) end
  
  -- initial condition: no offset
  local im vectorField(x,y) [float[2]]({0,0}) end

  -- do LK calculation
  -- Notice: instead of iterating the same # of times for each pixel,
  -- we could instead iterate a different # of times for each pixel 
  -- (until the error < epsilon for ex). This would prob make for better
  -- results, but wouldn't be parallelizable

  for i=1,iterations do
    local im xcoord(x,y) vectorField(x,y)[0] end
    local im ycoord(x,y) vectorField(x,y)[1] end

    local G = im(x,y) [resampleBilinear(clamp,frame2,maxResampleX, maxResampleY,xcoord,ycoord)] end
    
    im vectorField(x,y)
      hx = vectorField(x,y)[0]
      hy = vectorField(x,y)[1]

      -- loop over search window
      b = map  wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
              dx = Fx (x+wx, y+wy)
              dy = Fy (x+wx, y+wy)
              w = W (x+wx, y+wy)
        
              F = frame1 (x+wx, y+wy)
            in
              {dx*(G(x+wx,y+wy)-F)*w,dy*(G(x+wx,y+wy)-F)*w}
          end
    in
      {A0(x,y)*(-b[0])+A1(x,y)*(-b[1])+hx,
       A2(x,y)*(-b[0])+A3(x,y)*(-b[1])+hy}
    end
  end

  return im(x,y) [uint8[3]]({(vectorField(x,y)[0])*50+128, (vectorField(x,y)[1])*50+128, 0}) end
end

local frame1 = darkroomSimple.load("frame10.bmp")
im frame1(x,y) [float](frame1) end
local frame2 = darkroomSimple.load("frame11.bmp")
im frame2(x,y) [float](frame2) end

makeLK( frame1, frame2 ):save("out/lucaskanade.bmp")