import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
terralib.require "bilinear"
fpga = terralib.require("fpga")

windowRadius = 2
iterations = 1 -- iterations per level
clamp = true
-- flow is stored as int8. 128/ofRange (so ofRange=10 means there are 10 subpixels)
ofRange = 64
logOfRange = math.log(8)/math.log(2)
rescale = 6

-- The determinant will be stored using a precision equal to the difference between these two bit widths (I think)
determinantPrecisionHigh = 20
determinantPrecisionLow = 5
-- I think the final matrix multiply ends up being a difference of two, large, similar numbers.
-- to give this extra precision (so that the subtract doesn't kill the precision), we multiply
-- the inputs by a bit. But don't go too far or it will overflow!
extraPrec = 2

function log2(inp)
  -- assume input is uint32
  local im maxv(x,y) [uint32]((1 << 31)-1)+(1<<31) end

  return im(x,y)
      [uint8](map i=1,31 reduce(max)
              thisv = [uint32](maxv - (([uint32](1) << [uint32](i))-[uint32](1)))
              in if (inp(x,y) and thisv)~=[uint32](0) then [uint8](i) else [uint8](0) end
          end)
    end
end

function sign(inp)
  return im(x,y) [int8](if inp(x,y)>0 then 1 else (-1) end) end
end

-- see here for ref: http://www.cs.ucf.edu/~mikel/Research/Optical_Flow.htm
function makeLK(frame1, frame2)

  local im frame1(x,y) [int32](frame1) end
  local im frame2(x,y) [int32](frame2) end 

  local im vectorField(x,y) [int8[2]]({0,0}) end

  -- calculate stuff that we will use every iteration
  -- such as derivatives, matrix A^-1 of image gradients, weights.
  -- were calling frame1 F and frame2 G, as in the original LK paper
    
  -- calculate derivatives
  local im Fx(x,y) [int32]((frame1(x+1,y)-frame1(x-1,y)) >> [uint8](1)) end
  local im Fy(x,y) [int32]((frame1(x,y+1)-frame1(x,y-1)) >> [uint8](1)) end

  local im Gx(x,y) [int32]((frame2(x+1,y)-frame2(x-1,y)) >> [uint8](1)) end
  local im Gy(x,y) [int32]((frame2(x,y+1)-frame2(x,y-1)) >> [uint8](1)) end

  -- calculate A^-1

  local im Atemp0(x,y)
    [int32](map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
      Fx(x+wx,y+wy)*Fx(x+wx,y+wy)
    end)
  end

  local im Atemp1(x,y)
    [int32](map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
      Fx(x+wx,y+wy)*Fy(x+wx,y+wy)
    end)
  end

  local Atemp2 = Atemp1

  local im Atemp3(x,y)
    [int32](map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
      Fy(x+wx,y+wy)*Fy(x+wx,y+wy)
    end)
  end

  local p = math.pow(2,determinantPrecisionHigh)
  local im denom(x,y) [int32](Atemp0(x,y)*Atemp3(x,y)-Atemp1(x,y)*Atemp2(x,y)) end

  --[=[
  -- I attempted to adjust the determinant precision per pixel based on the value of the denomenator.
  -- but, this didn't seem to really provide a benefit over just setting it to a constant
  -- remember: log2 will tell you the highest bit in the number
  local det_sign = sign(denom)
  local im abs_denom(x,y) : cropNone, uint32 orion.abs(denom) end
  local det_exp = log2(abs_denom)
  local im detPrec(x,y) : cropNone,uint8 orion.max(det_exp(x,y)-determinantPrecisionHigh+5,5) end
  ]=]

  local im detPrec(x,y) [uint8](determinantPrecisionLow) end
  local im denom(x,y) [int32](if denom(x,y)<(1 << detPrec(x,y)) then (1 << detPrec(x,y)) else denom(x,y) end) end

--local im det(x,y) [int32](p/(denom(x,y) >> detPrec(x,y))) end
  local lol = log2(im(x,y) denom(x,y) >> detPrec(x,y) end)
  local im det(x,y) [int32](p >> lol) end
                
  -- Atemp are using 16 bits                
  local im A0(x,y) (det(x,y)*Atemp3(x,y)) << extraPrec end
  local im A1(x,y) (-det(x,y)*Atemp1(x,y)) << extraPrec end
  local im A2(x,y) (-det(x,y)*Atemp2(x,y)) << extraPrec end
  local im A3(x,y) (det(x,y)*Atemp0(x,y)) << extraPrec end
      
  -- do LK calculation
  -- Notice: instead of iterating the same # of times for each pixel,
  -- we could instead iterate a different # of times for each pixel 
  -- (until the error < epsilon for ex). This would prob make for better
  -- results, but wouldn't be parallelizable

  for i=1,iterations do
  
    local im xcoord(x,y) vectorField(x,y)[0] end
    local im ycoord(x,y) vectorField(x,y)[1] end
  
    local G = im(x,y) [resampleBilinearInt( clamp, frame2, uint8, ofRange, ofRange, xcoord, ycoord)] end

    -- bump up the precision a bit in later rounds b/c the delta will be smaller
    -- actually, this doesn't seem to work b/c it overflows or something
    local EP = {0,0,0,0,0}
    im vectorField(x,y)
      -- loop over search window
      b = (map  wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
              dx = Fx (x+wx, y+wy)
              dy = Fy (x+wx, y+wy)
              F = frame1 (x+wx, y+wy)
              in {[int32](dx*(G(x+wx,y+wy)-F)),[int32](dy*(G(x+wx,y+wy)-F))}
          end) << [extraPrec+EP[i]]
    in
      -- result = Ainv * (-b)
      -- b is about 16 bits
      {[int8](((((A0(x,y)*(-b[0]) >> (determinantPrecisionHigh+detPrec(x,y)))+(A1(x,y)*(-b[1]) >> (determinantPrecisionHigh+detPrec(x,y)))) << logOfRange) >> [extraPrec+extraPrec+EP[i]])+xcoord),
       [int8](((((A2(x,y)*(-b[0]) >> (determinantPrecisionHigh+detPrec(x,y)))+(A3(x,y)*(-b[1]) >> (determinantPrecisionHigh+detPrec(x,y)))) << logOfRange) >> [extraPrec+extraPrec+EP[i]])+ycoord)}
    end

  end

  -- rescale this for display
  return im(x,y) [uint8[3]]({[int32](vectorField[0](x,y))*rescale+128,[int32](vectorField[1](x,y))*rescale+128,0}) end
end

local frame1 = darkroomSimple.load("frame10.bmp")
local frame2 = darkroomSimple.load("frame11.bmp")

lkpipeline = makeLK( frame1, frame2 )
lkpipeline:save("out/lucaskanade.bmp")

--------------
fpgaEstimate = terralib.require("fpgaEstimate")
local est, perline = fpgaEstimate.compile({lkpipeline}, 1280)
io.output("out/lucaskanadeEstimate.txt")
io.write(est)
io.close()
io.output("out/lucaskanadeEstimatePerline.txt")
io.write(perline)
io.close()

-----------------
print("Build For: "..arg[1])
local v, metadata = fpga.compile({{frame1,"uart","frame10.bmp"},{frame2,"uart","frame11.bmp"}},{{lkpipeline,"uart"}}, 128,64, fpga.util.deviceToOptions(arg[1]))

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/"..s..".metadata.lua", metadata)

------------------
local opt = fpga.util.deviceToOptions(arg[1])
opt.stripWidth=200
opt.stripHeight=388
local v, metadata = fpga.compile({{frame1,"sim","frame10.bmp"},{frame2,"sim","frame11.bmp"}},{{lkpipeline,"sim"}}, opt.stripWidth, opt.stripHeight, opt)

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".sim.v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/"..s..".sim.metadata.lua", metadata)
