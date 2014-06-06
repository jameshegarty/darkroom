-- resamples input so that instead of returning its value at [x,y],
-- instead returns its value at [x+offsetX, y+offsetY]
-- as long as abs(offsetX,Y) < maxX,Y
-- note: this will synth a conv engine of size (maxX*2+1)*(maxY*2+1)!

import "orion"

function resampleBilinearSlow(clamp,input, maxX, maxY, offsetX, offsetY)
  if maxX==nil then orion.error("maxX is nil") end
  if maxY==nil then orion.error("maxY is nil") end
  if type(maxX)~="number" then orion.error("maxX must be a number") end
  if type(maxY)~="number" then orion.error("maxY must be a number") end
  if type(clamp)~="boolean" then orion.error("clamp must be a bool") end
  assert(orion.ast.isAST(input))
  assert(orion.ast.isAST(offsetX))
  assert(orion.ast.isAST(offsetY))

  im offsetX(x,y) : cropNone,float32 offsetX(x,y) end
  im offsetY(x,y) : cropNone,float32 offsetY(x,y) end

  if clamp then
    im offsetX(x,y) : cropNone if offsetX(x,y)>=maxX then maxX-0.0000001 else offsetX(x,y) end end
    im offsetX(x,y) : cropNone if offsetX(x,y)<=-maxX then -maxX+0.0000001 else offsetX(x,y) end end

    im offsetY(x,y) : cropNone if offsetY(x,y)>=maxY then maxY-0.0000001 else offsetY(x,y) end end
    im offsetY(x,y) : cropNone if offsetY(x,y)<=-maxY then -maxY+0.0000001 else offsetY(x,y) end end
  else
    im offsetX(x,y) : cropNone orion.assert(offsetX(x,y),offsetX(x,y),orion.abs(offsetX(x,y))<maxX) end
    im offsetY(x,y) : cropNone orion.assert(offsetY(x,y),offsetY(x,y),orion.abs(offsetY(x,y))<maxY) end
  end

  local im targetX(x,y) : cropNone orion.floor(offsetX(x,y)) end
  local im targetY(x,y) : cropNone orion.floor(offsetY(x,y)) end


  local im valueXY(x,y) : cropNone,float32 0 end
  local im valueXp1Y(x,y) : cropNone,float32 0 end 
  local im valueXYp1(x,y) : cropNone,float32 0 end
  local im valueXp1Yp1(x,y) : cropNone,float32 0 end


  for i=-maxX,maxX do
    for j=-maxY,maxY do

        im valueXY(x,y) : cropNone 
        input(x+i,y+j) * (if i==targetX(x,y) and j==targetY(x,y) then orion.uint8(1) else orion.uint8(0) end) + valueXY(x,y) 
        end

        im valueXp1Y(x,y) : cropNone
        input(x+i,y+j) * (if i==targetX(x,y)+1 and j==targetY(x,y) then orion.uint8(1) else orion.uint8(0) end) + valueXp1Y(x,y) 
        end

        im valueXYp1(x,y) : cropNone
        input(x+i,y+j) * (if i==targetX(x,y) and j==targetY(x,y)+1 then orion.uint8(1) else orion.uint8(0) end) + valueXYp1(x,y) 
        end

        im valueXp1Yp1(x,y) : cropNone 
        input(x+i,y+j) * (if i==targetX(x,y)+1 and j==targetY(x,y)+1 then orion.uint8(1) else orion.uint8(0) end) + valueXp1Yp1(x,y) 
        end

    end
  end

local im wx(x,y) : cropNone offsetX(x,y) - targetX(x,y) end
local im wy(x,y) : cropNone offsetY(x,y) - targetY(x,y) end
local im a(x,y) : cropNone valueXY(x,y)*(1-wx(x,y)) + valueXp1Y(x,y)*wx(x,y) end
local im b(x,y) : cropNone valueXYp1(x,y)*(1-wx(x,y)) + valueXp1Yp1(x,y)*wx(x,y) end
local im out(x,y) : cropNone a(x,y)*(1-wy(x,y))+b(x,y)*wy(x,y) end

  return out
end

function resampleBilinear(clamp,input, maxX, maxY, offsetX, offsetY)
  if maxX==nil then orion.error("maxX is nil") end
  if maxY==nil then orion.error("maxY is nil") end
  if type(maxX)~="number" then orion.error("maxX must be a number") end
  if type(maxY)~="number" then orion.error("maxY must be a number") end
  if type(clamp)~="boolean" then orion.error("clamp must be a bool") end
  assert(orion.ast.isAST(input))
  assert(orion.ast.isAST(offsetX))
  assert(orion.ast.isAST(offsetY))

  im offsetX(x,y) : cropNone,float32 offsetX(x,y) end
  im offsetY(x,y) : cropNone,float32 offsetY(x,y) end

  
  local im targetX(x,y) : cropNone, int32 orion.floor(offsetX(x,y)) end
  local im targetXp1(x,y) : cropNone, int32 orion.floor(offsetX(x,y))+1 end
  local im targetY(x,y) : cropNone, int32 orion.floor(offsetY(x,y)) end  
  local im targetYp1(x,y) : cropNone, int32 orion.floor(offsetY(x,y))+1 end

  local im valueXY(x,y) :cropNone orion.gather(input(x,y), targetX(x,y), targetY(x,y), maxX, maxY, clamp) end
  local im valueXp1Y(x,y) :cropNone orion.gather(input(x,y), targetXp1(x,y), targetY(x,y), maxX, maxY, clamp) end
  local im valueXYp1(x,y) :cropNone orion.gather(input(x,y), targetX(x,y), targetYp1(x,y), maxX, maxY, clamp) end
  local im valueXp1Yp1(x,y) :cropNone orion.gather(input(x,y), targetXp1(x,y), targetYp1(x,y), maxX, maxY, clamp) end

  local im wx(x,y) : cropNone, float32 offsetX(x,y) - targetX(x,y) end
  local im wy(x,y) : cropNone, float32 offsetY(x,y) - targetY(x,y) end
  local im a(x,y) : cropNone valueXY(x,y)*(1-wx(x,y)) + valueXp1Y(x,y)*wx(x,y) end
  local im b(x,y) : cropNone valueXYp1(x,y)*(1-wx(x,y)) + valueXp1Yp1(x,y)*wx(x,y) end
  local im out(x,y) : cropNone a(x,y)*(1-wy(x,y))+b(x,y)*wy(x,y) end

  return out
end


-- offsetX and offsetY are int8
-- maxX and maxY determine the precision of offsetX and offsetY. ie if maxX = 8, then there are 128/8 subpixels in X
-- maxX and maxY must be powers of 2
function resampleBilinearInt(clamp,input, inputType, maxX, maxY, offsetX, offsetY)
  if maxX==nil then orion.error("maxX is nil") end
  if maxY==nil then orion.error("maxY is nil") end
  if type(maxX)~="number" then orion.error("maxX must be a number") end
  if maxX~=2 and maxX~=4 and maxX~=8 and maxX~=16 and maxX~=32 and maxX~=64 and maxX~=128 then orion.error("maxX must be a power of 2") end
  if type(maxY)~="number" then orion.error("maxY must be a number") end
  if maxY~=2 and maxY~=4 and maxY~=8 and maxY~=16 and maxY~=32 and maxY~=64 and maxY~=128 then orion.error("maxY must be a power of 2") end
  if type(clamp)~="boolean" then orion.error("clamp must be a bool") end
  assert(orion.ast.isAST(input))
  assert(orion.ast.isAST(offsetX))
  assert(orion.ast.isAST(offsetY))
  assert(orion.type.isType(inputType))

  local gatherRangeX = 128/maxX
  local gatherRangeY = 128/maxY

  local logMaxX = math.log(maxX)/math.log(2)
  local logMaxY = math.log(maxY)/math.log(2)

  -- conveniently, right shifting is the same as floor
  local im targetX(x,y) : cropNone (offsetX(x,y) >> logMaxX) end
  local im targetXp1(x,y) : cropNone (offsetX(x,y) >> logMaxX)+1 end
  local im targetY(x,y) : cropNone (offsetY(x,y) >> logMaxY) end  
  local im targetYp1(x,y) : cropNone (offsetY(x,y) >> logMaxY)+1 end

  local im valueXY(x,y) :cropNone orion.gather(input(x,y), targetX(x,y), targetY(x,y), gatherRangeX, gatherRangeY, clamp) end
  local im valueXp1Y(x,y) :cropNone orion.gather(input(x,y), targetXp1(x,y), targetY(x,y), gatherRangeX, gatherRangeY, clamp) end
  local im valueXYp1(x,y) :cropNone orion.gather(input(x,y), targetX(x,y), targetYp1(x,y), gatherRangeX, gatherRangeY, clamp) end
  local im valueXp1Yp1(x,y) :cropNone orion.gather(input(x,y), targetXp1(x,y), targetYp1(x,y), gatherRangeX, gatherRangeY, clamp) end

  local im wx(x,y) : cropNone, uint8 offsetX(x,y) - (targetX(x,y) << logMaxX) end -- in subpixel units. always between 0 and maxX
  local im wy(x,y) : cropNone, uint8 offsetY(x,y) - (targetY(x,y) << logMaxY) end -- in subpixel units. always between 0 and maxY
  local im a(x,y) : cropNone, [inputType]  orion.uint32(valueXY(x,y))*(orion.uint8(maxX)-wx(x,y)) + orion.uint32(valueXp1Y(x,y))*wx(x,y) >> orion.uint8(logMaxX) end
  local im b(x,y) : cropNone, [inputType] orion.uint32(valueXYp1(x,y))*(orion.uint8(maxX)-wx(x,y)) + orion.uint32(valueXp1Yp1(x,y))*wx(x,y) >> orion.uint8(logMaxX) end
  local im out(x,y) : cropNone, [inputType] orion.uint32(a(x,y))*(orion.uint8(maxY)-wy(x,y))+orion.uint32(b(x,y))*wy(x,y) >> orion.uint8(logMaxY) end

  return out
end
