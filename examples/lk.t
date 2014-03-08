import "orion"

terralib.require "common"

makeConv = false

if arg[1]=="conv" then
  makeConv = true
end

windowRadius = 2
iterations = 1 -- iterations per level
pyramidLevels = 3
weighted = false
maxResampleX = 10
maxResampleY = 10
clamp = true

width = 584
height = 388

-- see here for ref: http://www.cs.ucf.edu/~mikel/Research/Optical_Flow.htm
function makeLK(vectorField, frame1, frame2)
  assert(orion.ast.isAST(vectorField))
  assert(orion.ast.isAST(frame1))
  assert(orion.ast.isAST(frame2))

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
    local im w(x,y) : cropNone orion.sqrt(orion.pow(Gx(x,y)-Fx(x,y),2)+orion.pow(Gy(x,y)-Fy(x,y),2)) end
    im W(x,y)  if w(x,y)~=0 then 1/w(x,y) else 100 end end
  else
    im W(x,y) : cropNone 1 end
  end
        
  -- calculate A^-1

  local im Atemp0(x,y) : cropNone
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
    Fx(x+wx,y+wy)*Fx(x+wx,y+wy)*W(x+wx,y+wy)
    end 
  end

  local im Atemp1(x,y) : cropNone
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
      Fx(x+wx,y+wy)*Fy(x+wx,y+wy)*W(x+wx,y+wy)
    end 
  end

  local Atemp2 = Atemp1

  local im Atemp3(x,y) : cropNone
    map wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
      Fy(x+wx,y+wy)*Fy(x+wx,y+wy)*W(x+wx,y+wy)
    end 
  end

  local im denom(x,y) :cropNone (Atemp0(x,y)*Atemp3(x,y)-Atemp1(x,y)*Atemp2(x,y)) end
  local im det(x,y) :cropNone if denom(x,y)~=0 then 1/denom(x,y) else 0 end end
--local im det(x,y) :cropNone if orion.abs(denom(x,y))>0.0001 then 1/denom(x,y) else 0 end end
                
  local im A0(x,y) :cropNone det(x,y)*Atemp3(x,y) end
  local im A1(x,y) :cropNone -det(x,y)*Atemp1(x,y) end
  local im A2(x,y) :cropNone -det(x,y)*Atemp2(x,y) end
  local im A3(x,y) :cropNone det(x,y)*Atemp0(x,y) end
        
  -- do LK calculation
  -- Notice: instead of iterating the same # of times for each pixel,
  -- we could instead iterate a different # of times for each pixel 
  -- (until the error < epsilon for ex). This would prob make for better
  -- results, but wouldn't be parallelizable

  for i=1,iterations do
    local im xcoord(x,y) : cropNone  vectorField(x,y)[0] end
    local im ycoord(x,y) : cropNone  vectorField(x,y)[1] end

local G = im(x,y) [resampleBilinear(clamp,frame2,maxResampleX, maxResampleY,
    xcoord,
    ycoord)] end
    
    im vectorField(x,y)
      let
      hx = vectorField(x,y)[0]
      hy = vectorField(x,y)[1]

      -- loop over search window
      b = map  wx = -windowRadius, windowRadius wy = -windowRadius, windowRadius reduce(sum)
            let 
              dx = Fx (x+wx, y+wy)
              dy = Fy (x+wx, y+wy)
              w = W (x+wx, y+wy)
        
              F = frame1 (x+wx, y+wy)
--              G = [resampleBilinear(clamp,frame2,maxResampleX, maxResampleY,im(x,y) wx(x,y)+hx(x,y) end , im(x,y) wy(x,y)+hy(x,y) end)]
--              G = frame2(x+wx, y+wy)
            in
              {dx*(G(x+wx,y+wy)-F)*w,dy*(G(x+wx,y+wy)-F)*w}
          end
    in
    -- result = Ainv * (-b)
      {A0(x,y)*(-b[0])+A1(x,y)*(-b[1])+hx,
       A2(x,y)*(-b[0])+A3(x,y)*(-b[1])+hy}
    end
  end

  return {vectorField, im(x,y) : uint8[3] {(vectorField(x,y)[0])*50+128, (vectorField(x,y)[1])*50+128, 0} end}
end


if makeConv then
  local frame1 = orion.image(orion.type.uint(8),width,height)
  local frame2 = orion.image(orion.type.uint(8),width,height)
  local vectorField = orion.image(orion.type.array(orion.type.float(32),2),width,height)

  local kern = makeLK(vectorField,
            im(x,y):float32 frame1(x,y) end,
            im(x,y):float32 frame2(x,y) end)

-- multiout not supported: only write out the flo version
-- of the results, not the human readable bmp version
 local yfile,yrfile = orion.compile({kern[1]}, {verbose=false,debug=false,platform="convolution",straighten=true})
  
  local file = io.open("out/lk.yml","w")
  file:write(yfile)
  file:close()
  
  local rfile = io.open("out/lk_run.yml","w")
  rfile:write(yrfile)
  rfile:close()

  os.exit()
end

-- unfortunately, orion currently only supports fixed image sizes,
-- so we need to compile a separate function for each level

local frame1Ids = global(int[pyramidLevels])
local frame2Ids = global(int[pyramidLevels])
local vectorFieldIds = global(int[pyramidLevels])
local lkFns = global(({}->{Image,Image})[pyramidLevels])
local lk = {}


terra uploadIds(i:int, frame1:int, frame2:int, vectorField:int, lk: {} -> {Image,Image})
  frame1Ids[i] = frame1
  frame2Ids[i] = frame2
  vectorFieldIds[i] = vectorField
  lkFns[i]= lk
end

for l=pyramidLevels-1,0,-1 do
  local frame1 = orion.image(orion.type.uint(8),width,height)
  local frame2 = orion.image(orion.type.uint(8),width,height)
  local vectorField = orion.image(orion.type.array(orion.type.float(32),2),width,height)
  
  local lkFn = orion.compile(
    makeLK(vectorField,
            im(x,y):float32,cropNone frame1(x,y) end,
            im(x,y):float32,cropNone frame2(x,y) end),
    {debug=false, verbose=false})

  lk[l] = lkFn

  uploadIds(l,frame1:id(), frame2:id(), vectorField:id(), lkFn:getpointer())

  width = width/2
  height = height/2
end

terra run()
  -- load the input and build the pyramid
  -- level 0 = coarsest level
  var frame1Pyramid = [&Image](cstdlib.malloc(sizeof(Image)*pyramidLevels))
  var frame2Pyramid = [&Image](cstdlib.malloc(sizeof(Image)*pyramidLevels))
  
  frame1Pyramid[pyramidLevels-1]:initWithFile("frame10.bmp")
  frame2Pyramid[pyramidLevels-1]:initWithFile("frame11.bmp")

  var tmpStr : int8[100]

  var l = pyramidLevels-2
  while l >= 0 do
    cstdio.printf("Make pyramid level %d\n",l)
    frame1Pyramid[l] = frame1Pyramid[l+1]:deepcopyUnstride()
    frame1Pyramid[l]:downsample()
    frame2Pyramid[l] = frame2Pyramid[l+1]:deepcopyUnstride()
    frame2Pyramid[l]:downsample()

    cstdio.snprintf(tmpStr,99,"out/lk_frame1_%d.bmp",l)
    frame1Pyramid[l]:save(tmpStr)
    cstdio.snprintf(tmpStr,99,"out/lk_frame2_%d.bmp",l)
    frame2Pyramid[l]:save(tmpStr)

    l = l - 1
  end

  -- for each pyramid level, run the orion code,
  -- and then upsample outside of orion

  orion.bindConstantArray2Float32(
    vectorFieldIds[0],
    frame1Pyramid[0].width,
    frame1Pyramid[0].height,
    arrayof(float,0,0))

  cstdio.printf("start\n")

  for l=0,pyramidLevels do
    cstdio.printf("Do level %d\n",l)
    orion.bindImage(frame1Ids[l], &frame1Pyramid[l])
    orion.bindImage(frame2Ids[l], &frame2Pyramid[l])

    var newVectorField, newVectorFieldHuman = lkFns[l]()
    cstdio.printf("FINAL SAVE\n")
    cstdio.snprintf(tmpStr,99,"out/lk_level_%d.flo",l)
    newVectorField:save(tmpStr)
    cstdio.snprintf(tmpStr,99,"out/lk_level_%d.bmp",l)
    newVectorFieldHuman:save(tmpStr)

    if l<pyramidLevels-1 then
      newVectorField:upsample()
      cstdio.snprintf( tmpStr, 99, "out/v_input_level_%d.flo", l+1 )
      newVectorField:save(tmpStr)
      
      orion.bindImage( vectorFieldIds[l+1], &newVectorField )
    end
  end

  cstdio.printf("Done\n")
end

run()