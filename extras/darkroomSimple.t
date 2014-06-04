orionSimple = {}

orionSimple.images = {}
orionSimple.imageInputs = {} -- this is the image data we pass in
-- with taps, we pass them in the same order they're created. We just track what ID was last seen to make sure the user always uses this interface
orionSimple.taps = {}
orionSimple.tapInputs = {} -- tap values we pass in
-- track W/H to make sure user always passes the same w/h
orionSimple.width = nil
orionSimple.height = nil

terralib.require("image")

-- convenience function. Loads an image and returns it as an orion function
-- it only makes sense to call this guy at compile time
function orionSimple.load(filename, boundaryCond)
  assert(type(filename)=="string")

  if orion.verbose then print("Load",filename) end

  local terra makeIm( filename : &int8)
    var im : &Image = [&Image](cstdlib.malloc(sizeof(Image)))
    im:initWithFile(filename)

    return im
  end
  
  local im = makeIm(filename)

  if orionSimple.width==nil then
    orionSimple.width = im.width
    orionSimple.height = im.height
  else
    assert(orionSimple.width==im.width)
    assert(orionSimple.height==im.height)
  end

  local _type = orion.type.uint(8)
  if im.bits==8 then
     --print("Bits should be 8, they are " .. im.bits ) 
  elseif im.bits==16 then
    _type = orion.type.uint(16)
  elseif im.bits==32 then
    _type = orion.type.uint(32)
  else 
     print("Bits should be 8, 16, or 32, they are " .. im.bits ) 
     assert(false)
  end

  if im.channels>1 then _type = orion.type.array(_type,im.channels) end
  if orion.verbose then print("channels", im.channels) end

  local inp = orion.input(_type)
  table.insert(orionSimple.images,inp)
  table.insert(orionSimple.imageInputs,`im.data)

  return inp
end

function orionSimple.loadRaw(filename, w,h,bits,header,flipEndian)
  assert(type(filename)=="string")
  assert(type(w)=="number")
  assert(type(h)=="number")
  assert(type(bits)=="number")

  local im
  if header~=nil then
    local terra makeIm( filename : &int8, w:int, h:int, bits:int,header:int, flipEndian:bool)
      var im : &Image = [&Image](cstdlib.malloc(sizeof(Image)))
      im:initWithRaw(filename,w,h,bits,header,flipEndian)
      
      return im
    end
    
    im = makeIm(filename,w,h,bits,header,flipEndian)

  else
    local terra makeIm( filename : &int8, w:int, h:int, bits:int)
      var im : &Image = [&Image](cstdlib.malloc(sizeof(Image)))
      im:initWithRaw(filename,w,h,bits)
      
      return im
    end
    
    im = makeIm(filename,w,h,bits)
  end

  print("orion.loadRaw bits", im.bits)
  local _type = orion.type.uint(im.bits)
--  assert(im.bits==32)

  local idast = orion.image(_type,im.width,im.height)
  orion._boundImages[idast.expr.id+1].filename = filename
  orion.bindImage(idast.expr.id,im)

  local terra freeIm(im:&Image)
    im:free()
    cstdlib.free(im)
  end
  freeIm(im)

  return idast

end

local function makeBindConstant(orionType)
  assert(orion.type.isType(orionType))

  local terraType = orion.type.toTerraType(orionType,false)

  -- if orionType is an array type, figure out what it's an array over
  local baseOrionType = orionType
  if orion.type.isArray(baseOrionType) then baseOrionType = orion.type.arrayOver(orionType) end
  local baseTerraType = orion.type.toTerraType(baseOrionType,false)

  local arrayLength = orion.type.arrayLength(orionType)
  if arrayLength==0 then arrayLength=1 end

  local isFloat = orion.type.isFloat(baseOrionType)
  local isSigned = orion.type.isInt(baseOrionType)

  return terra(boundId : int, w:int, h:int, constantValue : terraType)

    orionAssert( boundId < orion._boundImagesRuntime:count() and boundId>=0, "bindImage id out of bound")
    orionAssert( w>0, "bindConstant w<0")
    orionAssert( h>0, "bindConstant h<0")
    
    -- check if the input image has the right width, height, etc
    if orion._boundImagesRuntime:get(boundId).width>0 then
      orionAssert(orion._boundImagesRuntime:get(boundId).width == w, "incorrect constant width")
      orionAssert(orion._boundImagesRuntime:get(boundId).height == h, "incorrect constant height")
    end

    orionAssert(orion._boundImagesRuntime:get(boundId).bits == sizeof(baseTerraType)*8, "bindConstant bits")
    orionAssert(orion._boundImagesRuntime:get(boundId).floating == isFloat, "floating")
    orionAssert(orion._boundImagesRuntime:get(boundId).isSigned == isSigned, "sign should match in makeBindConstant")
    orionAssert(orion._boundImagesRuntime:get(boundId).channels == arrayLength, "channels")
    
    -- make the new const image
    var c = orion._boundImagesRuntime:get(boundId).channels
    
    var data = [&terraType](cstdlib.malloc(w*h*sizeof(terraType)))
    
    for i=0,w*h do data[i] = constantValue end
    
    var img : Image
    img:init(w,h,w,c,sizeof(baseTerraType)*8,isFloat,isSigned,[&uint8](data),[&uint8](data))
    
    if orion.verbose then cstdio.printf("Bind Constant %d w %d h %d bits %d\n",boundId, w,h,sizeof(baseTerraType)*8) end
    
    -- was something already bound to this image? need to free it
    if orion._boundImagesRuntime:get(boundId).active then
      orion._boundImagesRuntime:get(boundId).image:free()
    end
    
    orion._boundImagesRuntime:getPtr(boundId).active = true
    orion._boundImagesRuntime:getPtr(boundId).image = img
  end
end

orionSimple.bindConstantFloat32 = makeBindConstant(orion.type.float(32))
orionSimple.bindConstantInt32 = makeBindConstant(orion.type.int(32))
orionSimple.bindConstantUint8 = makeBindConstant(orion.type.uint(8))
orionSimple.bindConstantArray2Float32 = makeBindConstant(orion.type.array(orion.type.float(32),2))
orionSimple.bindConstantArray3Float32 = makeBindConstant(orion.type.array(orion.type.float(32),3))
orionSimple.bindConstantArray3Uint8 = makeBindConstant(orion.type.array(orion.type.uint(8),3))
orionSimple.bindConstantArray2Uint8 = makeBindConstant(orion.type.array(orion.type.uint(8),2))

function orionSimple.constant(ty, width, height, constantValue)
  assert(orion.type.isType(ty))
  assert(type(width)=="number")
  assert(type(height)=="number")
  assert(type(constantValue)=="number")

  if orionSimple.width==nil then
    orionSimple.width = width
    orionSimple.height = height
  else
    assert(orionSimple.width==width)
    assert(orionSimple.height==height)
  end

  local idast = orion.image(ty,width,height)
  
  local tty = orion.type.toTerraType(ty)

  if ty == orion.type.float(32) then
    local terra dobind(id : int, inp : float)
      orion.bindConstantFloat32( id, width, height, inp )
    end
    
    dobind(idast:id(),constantValue)
  elseif ty == orion.type.int(32) then
    local terra dobind(id : int, inp : int)
      orion.bindConstantInt32( id, width, height, inp )
    end
    
    dobind(idast:id(),constantValue)
  elseif ty == orion.type.uint(8) then
    local terra dobind(id : int, inp : uint8)
      orion.bindConstantUint8( id, width, height, inp )
    end
    
    dobind(idast:id(),constantValue)
  else
    print("unsupported type pased to orion.constant",ty:str())
    assert(false)
  end

  return idast
end


orionSimple._usedTapNames={}

function orionSimple.tap(ty)
  local r = orion.tap(ty)
  if #orionSimple.taps ~= r.id then 
    orion.error("If you use the simple interface, you must use to for _all_ taps "..#orionSimple.taps.." "..r.id)
  end

  orionSimple.taps[r.id+1] = r
  return r
end

function orionSimple.setTap( ast, value )
  assert(orion.ast.isAST(ast))
  orionSimple.tapInputs[ast.id+1] = value
end

function orionSimple.getTap(ast)
--  assert(orion.ast.isAST(ast) or orion.convIR.isConvIR(ast))
  local terraType = orion.type.toTerraType(ast.type)

  local terra getit(id:int) : terraType
    var v : &terraType = [&terraType](orion.runtime.getTap(id))
    return @v
  end

  return getit(ast.id)
end


-- even though the LUT will be 0 indexed, value is 1 indexed
-- nice one
function orionSimple.setTapLUT(ast,value)
  assert(orion.ast.isAST(ast))
  assert(type(value)=="table")

  local terraType = orion.type.toTerraType(ast.type)

  assert(value[0]==nil)
  assert(#value == ast.count)
  local terra alloc()
    var tptr : &terraType = [&terraType]( cstdlib.malloc(sizeof(terraType)*[ast.count]) )
    return tptr
  end

  local ptr = alloc()

  local terra seti(tptr : &terraType, index : int, value : terraType)
    tptr[index] = value
  end

  for i=1,ast.count do
    assert(type(value[i])=="number")
    seti(ptr,i-1,value[i])
  end

  (terra() orion.runtime.setTapLUT(ast.id, [&opaque](ptr)) end)()
end

function astFunctions:save(filename,compilerOptions)
  local func = orion.compile({self},compilerOptions)
  print("Call",compilerOptions)
  local out = func()
  local terra dosave(im: &Image, filename : &int8)
    im:save(filename)
  end

  dosave(out,filename)
  --out:save(filename)
end

function astFunctions:saveRaw(filename,bits)
  local func = orion.compile({self})
  print("Call")
  local out = func()
  local terra dosave(im: &Image, filename : &int8, bits:int)
    im:saveRaw(filename,bits)
  end

  dosave(out,filename,bits)
  --out:save(filename)
end


function astFunctions:_cparam(key)
  if self.kind~="crop" and self.expr.kind~="special" then
    orion.error("could not determine "..key.." - not an input fn, kind "..self.kind)
  end

  local id = self.expr.id

  if type(orion._boundImages[id+1][key])~="number" then
    orion.error("could not determine "..key.." - wasn't specified at compile time")
  end

  return orion._boundImages[id+1][key]
end

function astFunctions:width()
  return orionSimple.width
end

function astFunctions:height()
  return orionSimple.height
end


function astFunctions:id()
  if self.kind~="crop" or self.expr.kind~="special" then
    orion.error("could not determine "..key.." - not an input fn")
  end

  assert(type(self.expr.id)=="number")
  return self.expr.id
end

function orionSimple.compile(outList, options)

  local outDecl = {}
  local outArgs = {}
  local outRes = {}

  local ocallbackKernelGraph = options.callbackKernelGraph
  options.callbackKernelGraph = function(kernelGraph)
    -- codegen the allocation for the outputs.
    -- we have to do this here, because it has to follow typechecking (we don't know the types until compile has happened)
    for k,v in kernelGraph:inputs() do
      local s = symbol(Image)
      table.insert(outDecl,
        quote 
          var [s] 
          var data : &opaque
          cstdlib.posix_memalign( [&&opaque](&data), 4*1024, [orionSimple.width*orionSimple.height*v.kernel.type:sizeof()])
          s:initSimple([orionSimple.width],[orionSimple.height],[v.kernel.type:channels()],[v.kernel.type:baseType():sizeof()]*8,[v.kernel.type:isFloat()],[v.kernel.type:isInt()],true,data)
        end)
      table.insert(outRes, quote s:toAOS() in s end)
      table.insert(outArgs, `s.data)
    end
    if ocallbackKernelGraph ~= nil then ocallbackKernelGraph(kernelGraph) end
  end
  
  local fn = orion.compile(orionSimple.images,outList,orionSimple.taps,orionSimple.width, orionSimple.height, options)

  options.callbackKernelGraph = ocallbackKernelGraph

  local TapStruct = terralib.types.newstruct("tapstruct")
  TapStruct.metamethods.__getentries = function()
    local r = {}
    for k,v in pairs(orionSimple.taps) do 
      local t = v.type:toTerraType()
      table.insert(r, {field=tostring(v.id), type=t}) 
    end
    return r
  end

  local fin = terra()
    [outDecl]
    var tapArgs : TapStruct = TapStruct {[orionSimple.tapInputs]}
    fn([orionSimple.imageInputs],[outArgs],&tapArgs)
    return [outRes]
  end

  fin:printpretty()
  return fin
end

return orionSimple