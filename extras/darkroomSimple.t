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

function imageToOrionType(im)
  local _type = orion.type.uint(8)

  
  assert(im.isSigned==false)

  if im.floating then
    if im.bits == 32 then
      _type = orion.type.float(32)
    else
      assert(false)
    end

  else
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
  end

  if im.channels>1 then _type = orion.type.array(_type,im.channels) end
  if orion.verbose then print("channels", im.channels) end

  return _type
end

function orionSimple.image(img)
  if orionSimple.width==nil then
    orionSimple.width = img.width
    orionSimple.height = img.height
  else
    assert(orionSimple.width==img.width)
    assert(orionSimple.height==img.height)
  end

  local _type = imageToOrionType(img)

  local inp = orion.input(_type)
  table.insert(orionSimple.images,inp)
  table.insert(orionSimple.imageInputs,quote img:toDarkroomFormat() in img.data end)

  return inp

end

-- convenience function. Loads an image and returns it as an orion function
-- it only makes sense to call this guy at compile time
function orionSimple.load(filename, boundaryCond)
  assert(type(filename)=="string")

  if orion.verbose then print("Load",filename) end

  local terra makeIm( filename : &int8)
    var im : &Image = [&Image](cstdlib.malloc(sizeof(Image)))
    im:initWithFile(filename)
    im:toDarkroomFormat()

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

  local _type = imageToOrionType(im)

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
  assert(ast.kind=="tap")

  if ast.type:isArray() then
    assert(orion.type.arrayLength(ast.type)==#value)
    orionSimple.tapInputs[ast.id+1] = `arrayof([orion.type.arrayOver(ast.type):toTerraType()],value)
  else
    orionSimple.tapInputs[ast.id+1] = value
  end
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

function orionSimple.tapLUT(ty, entries, name)
  assert(type(name)=="string")
  local r = orion.tapLUT(ty, entries, name)
  if #orionSimple.taps ~= r.id then 
    orion.error("If you use the simple interface, you must use to for _all_ taps "..#orionSimple.taps.." "..r.id)
  end

  orionSimple.taps[r.id+1] = r
  return r
end

function astFunctions:save(filename,compilerOptions)
  local func = orionSimple.compile({self},compilerOptions)
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
  options = options or {}
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