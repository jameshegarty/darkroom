-------------------------------------------------------------------------------
-- imgproc.t
-------------------------------------------------------------------------------
-- Darkroom image processing library
-------------------------------------------------------------------------------
-- Contents:
--  clampToRange - Clamp a value to the edge of a range
--  mirrorRange - Clamp a value to a range, mirroring offsets across the 
--                boundary
--  safeGather - Perform a gather operation with defined out-of-bounds behavior
--    oob - modes of operation for out-of-bounds access 
--  map2dx - Convert a map of x-coordinates to displacements
--  map2dy - Convert a map of y-coordinates to displacements
--  remap - Remap an image with arbitrary interpolation
--  resampleBilinear - resampling with bilinear interpolation from offsets
--    interp - modes of interpolation for remap, warp 
--  gradient - image derivatives
-------------------------------------------------------------------------------

-- resamples input so that instead of returning its value at [x,y],
-- instead returns its value at [x+offsetX, y+offsetY]
-- as long as abs(offsetX,Y) < maxX,Y
-- note: this will synth a conv engine of size (maxX*2+1)*(maxY*2+1)!

import "darkroom"

imgproc = {}

-- Modes of operation for out-of-bounds access
imgproc.oob = {
	"value", -- Assign out-of-bounds accesses to a specific value 
	"clamp", -- Clamp to the edge of the stencil
	"mirror" -- Mirror at the stencil boundary
} 

-- Modes of interpolation
imgproc.interp = {
	"nearest",
	"bilinear",
	"cubic"
}

-- clampToRange - Clamp a value to the edge of a range 
--
-- Inputs:
--   x - the value to clamp
--   xMin - the minimum value in the range
--   xMax - the maximum value in the range
--
-- Output:
--   Returns max(min(x, xMax), xMin)
--
-- Examples:
--  clampToRange(6, -5, 5) -- returns 5 
--  clampToRange(-7, -5, 5) -- returns -5
function imgproc.clampToRange(x, xMin, xMax)
  return im(x_, y_) darkroom.max(darkroom.min(x, xMax), xMin) end
end

-- mirrorRange - Helper function for safeGather
--
-- Inputs:
--   x - the value to clamp
--   xMin - the minimum value in the range
--   xMax - the maximum value in the range 
--
-- Output:
--   Returns a value in the range, where the displacement of x from the
--   boundary is preserved. If the refelction overshoots the other boundary,
--   i.e. the displacement is larger than the range itself, this function
--   clamps the value to the other boundary. See the source code for details.
function imgproc.mirrorRange(x, xMin, xMax)
  return im(x_, y_)
    if x > xMax then 
      darkroom.max(2 * xMax - x, xMin)
    else (if x < xMin then
        darkroom.min(2 * xMin - x, xMax)
      else
        x 
      end) 
    end
  end
end

-- safeGather - gather operation with defined out-of-bounds behavior
--
-- Inputs:
--   input, X, Y, xRange, yRange - These work exactly as in the "gather" operator
--   mode - an entry from the "oob" table 
--   optional parameters: if mode is set to "val", defines the value
--
-- Output:
--   This function works exactly as "gather," but with the defined out-of-bounds
--   behavior.
-- 
-- Examples:
--  imgproc.safeGather(im, x, y, xRange, yRange, oob.clamp)
--  imgproc.safeGather(im, x, y, xRange, yRange, oob.mirror)
--  imgproc.safeGather(im, x, y, xRange, yRange, oob.value, 0)
--  imgproc.safeGather(im, x, y, xRange, yRange, oob.value, 255)
function imgproc.safeGather(input, X, Y, xRange, yRange, mode, ...)

  -- Verify the inputs
  if xRange == nil then darkroom.error("xRange is nil") end
  if yRange == nil then darkroom.error("yRange is nil") end
  if type(X) ~= "number" then darkroom.error("X must be a number") end
  if type(Y) ~= "number" then darkroom.error("Y must be a number") end
  if type(xRange) ~= "number" then darkroom.error("xRange must be a number") end
  if type(yRange) ~= "number" then darkroom.error("yRange must be a number") end

  if mode == oob.value then
    -- Verify the value parameter 
    if arg.n < 1 then darkroom.error("No value provided") end
    if type(arg[1]) ~= "number" then darkroom.error("value must be a number") end

    -- Get the absolute value of the indices 
    local im xAbs(x, y) darkroom.abs(X) end
    local im yAbs(x, y) darkroom.abs(Y) end

    return im(x, y)
      -- Return the predefined value if the access is out of bounds
      if (xAbs > xRange or yAbs > yRange) then arg[1]
      -- Else perform a gather
      else darkroom.gather(input, X, Y, xRange, yRange)
      end
    end
  elseif mode == oob.clamp then
    -- Generate the clamped indices 
    local im xClamp(x, y) imgproc.clampToRange(X, -xRange, xRange) end
    local im yClamp(x, y) imgproc.clampToRange(Y, -yRange, yRange) end

    -- Perform a gather on the clamped indices
    return im(x, y) darkroom.gather(input, xClamp, yClamp, xRange, yRange) end
  elseif mode == oob.mirror then
    -- Generate the mirrored indices
    local im xMirror(x, y) imgproc.mirrorRange(X, -xRange, xRange) end
    local im yMirror(x, y) imgproc.mirrorRange(Y, -yRange, yRange) end

    -- Perform a gather on the mirrored indices
    return im(x, y) darkroom.gather(input, xMirror, yMirror, xRange, yRange) end
  else
    darkroom.error("unrecognized mode")
  end
end

function imgproc.map2dx(X)
if type(X) ~= "number" then darkroom.error("X must be a number") end
  return im(x, y)
    X(x, y) - x
  end
end

function imgproc.map2dy(Y)
if type(Y) ~= "number" then darkroom.error("Y must be a number") end
  return im(x, y)
    Y(x, y) - y
  end
end

local function doclamp(img, max)
  return im(x,y) 
    if img>max then max else (if img<-max then -max else img end) end
  end
end

-- remap -- resample an image from a map of x and y coordinates
--
-- Inputs:
--   input - the input image
--   xRange - the maximum displacement in x
--   yRange - the maximum displacement in y 
--   X - the map of x coordinates
--   Y - the map of y coordinates
--   interpMode - the interpolation mode, from the "interp" table
--     TODO: only bilinear is currently supported 
--   oobMode - the out-of-bounds behavior. See "safeGather." 
function imgproc.remap(input, xRange, yRange, X, Y, interpMode, oobMode, ...)

  -- Verify the inputs
  if maxX == nil then darkroom.error("maxX is nil") end
  if maxY == nil then darkroom.error("maxY is nil") end
  if type(X) ~= "number" then darkroom.error("maxX must be a number") end
  if type(Y) ~= "number" then darkroom.error("maxY must be a number") end
  if type(clamp) ~= "boolean" then darkroom.error("clamp must be a bool") end
  assert(darkroom.ast.isAST(input))
  assert(darkroom.ast.isAST(offsetX))
  assert(darkroom.ast.isAST(offsetY))

  -- extract offsets from the map
  local im dx(x, y) imgproc.map2dx(mapX) end
  local im dy(x, y) imgproc.map2dy(mapY) end

  -- Interpolate from the offsets
  -- TODO: pass maps in to general "warp" function
  return resampleBilinear(clamp, input, xRange, yRange, dx, dy)
end

--TODO: Rename this to "warpBilinear"
--TODO: Use safeGather
function imgproc.resampleBilinear( clamp, input, maxX, maxY, offsetX, offsetY )
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

-- atan2 - Standard 2-input arc tangent
--
-- Inputs:
--  X - an image of x-coordiantes (abscissa)
--  Y - an image of y-coordinates (ordinate)
--
-- Outputs:
--  Returns an image of the corresponding quadrant-corrected arc tangents
function imgproc.atan2(X, Y)

    return im(x, y)

	-- Constants
	pi = [float] (math.pi)

	-- Get the vector and arc tangent 
	xf = [float] (X(x, y))
	yf = [float] (Y(x, y))
	angle = [float] (darkroom.arctan(yf, xf))
    
	-- Correct for quadrants
	-- Q1, Q4
	in if xf > 0 then angle
	-- Q2
	else (if xf < 0 and yf >= 0 then angle + pi 
	-- Q3
	else (if xf < 0 and yf < 0 then angle - pi 
	-- Special cases
	else (if xf == 0 and yf > 0 then pi / 2 
	else (if xf == 0 and yf < 0 then -(pi / 2) 
	else [float] (0) end) end) end) end)
	end
    end
end

-- cart2polar - convert cartesian coordinates (x, y) to polar
--
-- Inputs:
--  X - an image of x-coordiantes (abscissa)
--  Y - an image of y-coordiantes (ordinate)
--
-- Outputs:
--  rad - an image of radii corresponding to the (x, y) vectors 
--  angle - an image of angles from the x-axis corresponding to the (x, y) vectors
function imgproc.cart2polar(X, Y)

    -- TODO: xy = merge(X,Y) end mag = norm(xy, "L2") end
    local mag = im(x, y) 
	xf = [float] (X(x, y))
	yf = [float] (Y(x, y))
    	in [float] (darkroom.sqrt(xf * xf + yf * yf))
    end

    -- TODO: atan2
    local angle = im(x, y)
    	[float] ([imgproc.atan2(Y, X)])
    end

    return mag, angle
end

-- gradient - get the x and y derivatives of an image
--
-- Inputs:
--   input - an image
--  
-- Outputs: 
--   dx - The derivative in the x-direction (columns)
--   dy - The derivative in the y-direction (rows)
function imgproc.gradient(input)
	local dx = im(x, y) [float] (0.5 * (input(x + 1, y) - input(x - 1, y))) end
	local dy = im(x, y) [float] (0.5 * (input(x, y + 1) - input(x, y - 1))) end	
	return dx, dy
end

return imgproc
