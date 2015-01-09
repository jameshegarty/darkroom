-- resamples input so that instead of returning its value at [x,y],
-- instead returns its value at [x+offsetX, y+offsetY]
-- as long as abs(offsetX,Y) < maxX,Y
-- note: this will synth a conv engine of size (maxX*2+1)*(maxY*2+1)!

import "darkroom"

local function doclamp(img, max)
  return im(x,y) 
    if img>max then max else (if img<-max then -max else img end) end
  end
end

function resampleBilinear( clamp, input, maxX, maxY, offsetX, offsetY )
  if maxX==nil then darkroom.error("maxX is nil") end
  if maxY==nil then darkroom.error("maxY is nil") end
  if type(maxX)~="number" then darkroom.error("maxX must be a number") end
  if type(maxY)~="number" then darkroom.error("maxY must be a number") end
  if type(clamp)~="boolean" then darkroom.error("clamp must be a bool") end
  assert(darkroom.ast.isAST(input))
  assert(darkroom.ast.isAST(offsetX))
  assert(darkroom.ast.isAST(offsetY))

  im offsetX(x,y) [float]( offsetX(x,y) ) end
  im offsetY(x,y) [float]( offsetY(x,y) ) end

  local im targetX(x,y) [int32]( darkroom.floor(offsetX(x,y)) ) end
  local im targetXp1(x,y) [int32]( darkroom.floor(offsetX(x,y))+1 ) end
  local im targetY(x,y) [int32]( darkroom.floor(offsetY(x,y)) ) end  
  local im targetYp1(x,y) [int32]( darkroom.floor(offsetY(x,y))+1 ) end

  if clamp then
    targetX = doclamp(targetX,maxX)
    targetXp1 = doclamp(targetXp1,maxX)
    targetY = doclamp(targetY,maxY)
    targetYp1 = doclamp(targetYp1,maxY)
  end

  local im valueXY(x,y)  darkroom.gather(input(x,y), targetX(x,y), targetY(x,y), maxX, maxY ) end
  local im valueXp1Y(x,y)  darkroom.gather(input(x,y), targetXp1(x,y), targetY(x,y), maxX, maxY ) end
  local im valueXYp1(x,y)  darkroom.gather(input(x,y), targetX(x,y), targetYp1(x,y), maxX, maxY ) end
  local im valueXp1Yp1(x,y)  darkroom.gather(input(x,y), targetXp1(x,y), targetYp1(x,y), maxX, maxY ) end

  local im wx(x,y) [float]( offsetX(x,y) - targetX(x,y) ) end
  local im wy(x,y) [float]( offsetY(x,y) - targetY(x,y) ) end
  local im a(x,y) valueXY(x,y)*(1-wx(x,y)) + valueXp1Y(x,y)*wx(x,y) end
  local im b(x,y) valueXYp1(x,y)*(1-wx(x,y)) + valueXp1Yp1(x,y)*wx(x,y) end
  local im out(x,y) a(x,y)*(1-wy(x,y))+b(x,y)*wy(x,y) end

  return out
end


-- offsetX and offsetY are int8
-- maxX and maxY determine the precision of offsetX and offsetY. ie if maxX = 8, then there are 128/8 subpixels in X
-- maxX and maxY must be powers of 2
function resampleBilinearInt( clamp, input, inputType, maxX, maxY, offsetX, offsetY)
  if maxX==nil then darkroom.error("maxX is nil") end
  if maxY==nil then darkroom.error("maxY is nil") end
  if type(maxX)~="number" then darkroom.error("maxX must be a number") end
  if maxX~=2 and maxX~=4 and maxX~=8 and maxX~=16 and maxX~=32 and maxX~=64 and maxX~=128 then darkroom.error("maxX must be a power of 2") end
  if type(maxY)~="number" then darkroom.error("maxY must be a number") end
  if maxY~=2 and maxY~=4 and maxY~=8 and maxY~=16 and maxY~=32 and maxY~=64 and maxY~=128 then darkroom.error("maxY must be a power of 2") end
  if type(clamp)~="boolean" then darkroom.error("clamp must be a bool") end
  assert(darkroom.ast.isAST(input))
  assert(darkroom.ast.isAST(offsetX))
  assert(darkroom.ast.isAST(offsetY))
  assert(terralib.types.istype(inputType))

  local gatherRangeX = 128/maxX
  local gatherRangeY = 128/maxY

  local logMaxX = math.log(maxX)/math.log(2)
  local logMaxY = math.log(maxY)/math.log(2)

  -- conveniently, right shifting is the same as floor
  local im targetX(x,y)  (offsetX(x,y) >> logMaxX) end
  local im targetXp1(x,y)  (offsetX(x,y) >> logMaxX)+1 end
  local im targetY(x,y)  (offsetY(x,y) >> logMaxY) end  
  local im targetYp1(x,y)  (offsetY(x,y) >> logMaxY)+1 end

  local im valueXY(x,y)  darkroom.gather(input(x,y), targetX(x,y), targetY(x,y), gatherRangeX, gatherRangeY, clamp) end
  local im valueXp1Y(x,y)  darkroom.gather(input(x,y), targetXp1(x,y), targetY(x,y), gatherRangeX, gatherRangeY, clamp) end
  local im valueXYp1(x,y)  darkroom.gather(input(x,y), targetX(x,y), targetYp1(x,y), gatherRangeX, gatherRangeY, clamp) end
  local im valueXp1Yp1(x,y)  darkroom.gather(input(x,y), targetXp1(x,y), targetYp1(x,y), gatherRangeX, gatherRangeY, clamp) end

  local im wx(x,y) [uint8]( offsetX(x,y) - (targetX(x,y) << logMaxX) ) end -- in subpixel units. always between 0 and maxX
  local im wy(x,y) [uint8]( offsetY(x,y) - (targetY(x,y) << logMaxY) ) end -- in subpixel units. always between 0 and maxY
  local im a(x,y) [inputType]([uint32](valueXY(x,y))*([uint8](maxX)-wx(x,y)) + [uint32](valueXp1Y(x,y))*wx(x,y) >> [uint8](logMaxX)) end
  local im b(x,y) [inputType]([uint32](valueXYp1(x,y))*([uint8](maxX)-wx(x,y)) + [uint32](valueXp1Yp1(x,y))*wx(x,y) >> [uint8](logMaxX)) end
  local im out(x,y) [inputType]([uint32](a(x,y))*([uint8](maxY)-wy(x,y))+[uint32](b(x,y))*wy(x,y) >> [uint8](logMaxY)) end

  return out
end

local gaussianKernel={1,4,7,4,1,
                4,16,26,16,4,
                7,26,41,26,7,
                4,16,26,16,4,
                1,4,7,4,1}
for k,v in ipairs(gaussianKernel) do gaussianKernel[k] = gaussianKernel[k]/273 end

function downsampleGaussianFloat(inp)
  return im(x,y)
    map i=-2,2 j=-2,2 reduce(sum) inp(x*2+i,y*2+j)*gaussianKernel[(j+2)*5+(i+2)] end
  end
end

-- sums to 256
local intGaussianKernel={1,4,7,4,1,
                         4,15,24,15,4,
                         7,24,36,24,7,
                         4,15,24,15,4,
                         1,4,7,4,1} 

function downsampleGaussianUint8(inp)
  return im(x,y)
  map i=-2,2 j=-2,2 reduce(sum) [uint8]([uint16](inp(x*2+i,y*2+j))*[uint16](intGaussianKernel[(j+2)*5+(i+2)])>>[uint16](8)) end
  end
end