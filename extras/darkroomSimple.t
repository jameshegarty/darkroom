orionSimple = {}

orionSimple.images = {} -- images returned from darkroom.input
orionSimple.imageInputs = {} -- this is the image data we pass in
orionSimple.imageWidths = {}
orionSimple.imageHeights = {}
-- with taps, we pass them in the same order they're created. We just track what ID was last seen to make sure the user always uses this interface
orionSimple.taps = {}
orionSimple.tapInputs = {} -- tap values we pass in

require("image")

function imageToOrionType(im)
  local _type = darkroom.type.uint(8)

  assert(im.isSigned==false)

  if im.floating then
    if im.bits == 32 then
      _type = darkroom.type.float(32)
    else
      assert(false)
    end

  else
    if im.bits==8 then
      --print("Bits should be 8, they are " .. im.bits ) 
    elseif im.bits==16 then
      _type = darkroom.type.uint(16)
    elseif im.bits==32 then
      _type = darkroom.type.uint(32)
    else 
      print("Bits should be 8, 16, or 32, they are " .. im.bits ) 
      assert(false)
    end
  end

  if im.channels>1 then _type = darkroom.type.array(_type,im.channels) end
  if darkroom.verbose then print("channels", im.channels) end

  return _type
end

-- img is an Image object
function orionSimple.image(img)
  local _type = imageToOrionType(img)

  local inp = darkroom.input(_type)

  if inp.expr.from ~= #orionSimple.images then
    darkroom.error("If you use the simple interface, you must use to for _all_ inputs "..#orionSimple.images.." "..inp.expr.from)
  end

  table.insert(orionSimple.images,inp)
  table.insert(orionSimple.imageInputs,quote img:toDarkroomFormat() in img.data end)
  table.insert(orionSimple.imageWidths, img.width)
  table.insert(orionSimple.imageHeights, img.height)

  return inp
end

-- convenience function. Loads an image and returns it as an orion function
-- it only makes sense to call this guy at compile time
function orionSimple.load(filename, boundaryCond)
  assert(type(filename)=="string")

  if darkroom.verbose then print("Load",filename) end

  local terra makeIm( filename : &int8)
    var im : &Image = [&Image](cstdlib.malloc(sizeof(Image)))
    im:load(filename)
    return im
  end
  
  local img = makeIm(filename)

  return orionSimple.image(img)
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
      im:loadRaw(filename,w,h,bits,header,flipEndian)
      
      return im
    end
    
    im = makeIm(filename,w,h,bits,header,flipEndian)

  else
    local terra makeIm( filename : &int8, w:int, h:int, bits:int)
      var im : &Image = [&Image](cstdlib.malloc(sizeof(Image)))
      im:loadRaw(filename,w,h,bits)
      
      return im
    end
    
    im = makeIm(filename,w,h,bits)
  end

  print("darkroom.loadRaw bits", im.bits)
  local _type = darkroom.type.uint(im.bits)
--  assert(im.bits==32)

  local idast = darkroom.image(_type,im.width,im.height)
  darkroom._boundImages[idast.expr.id+1].filename = filename
  darkroom.bindImage(idast.expr.id,im)

  local terra freeIm(im:&Image)
    im:free()
    cstdlib.free(im)
  end
  freeIm(im)

  return idast

end

orionSimple._usedTapNames={}

function orionSimple.tap( ty, value )
  if terralib.types.istype(ty)==false then
    darkroom.error("First argument to darkroomSimple.tap must be a type")
  end

  if type(value)~="number" and type(value)~="table" then
    darkroom.error("Second argument to darkroomSimple.tap must be a value")
  end

  local r = darkroom.tap( ty )
  if #orionSimple.taps ~= r.id then 
    darkroom.error("If you use the simple interface, you must use to for _all_ taps "..#orionSimple.taps.." "..r.id)
  end

  orionSimple.taps[r.id+1] = r

  if r.type:isArray() then
    if darkroom.type.arrayLength(r.type)~=#value then
      darkroom.error("Number of elements in tap value doesn't match type")
    end

    orionSimple.tapInputs[r.id+1] = `arrayof([darkroom.type.arrayOver(r.type):toTerraType()],value)
  else
    orionSimple.tapInputs[r.id+1] = value
  end

  return r
end

function orionSimple.tapLUT(ty, entries, value)

  local r = darkroom.tapLUT( ty, entries )

  if #orionSimple.taps ~= r.id then 
    darkroom.error("If you use the simple interface, you must use to for _all_ taps "..#orionSimple.taps.." "..r.id)
  end

  orionSimple.taps[r.id+1] = r

  if darkroom.type.arrayLength(r.type)~=#value then
    darkroom.error("Number of elements in tap value doesn't match type")
  end

  orionSimple.tapInputs[r.id+1] = `arrayof([darkroom.type.arrayOver(r.type):toTerraType()],value)

  return r
end

function astFunctions:save(filename,compilerOptions)
  local func = orionSimple.compile({self},compilerOptions)
  local out = func()
  local terra dosave(im: &Image, filename : &int8)
    im:save(filename)
    im:free()
  end

  dosave(out,filename)
  --out:save(filename)
end

function astFunctions:saveRaw(filename,bits)
  local func = darkroom.compile({self})
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
    darkroom.error("could not determine "..key.." - not an input fn, kind "..self.kind)
  end

  local id = self.expr.id

  if type(darkroom._boundImages[id+1][key])~="number" then
    darkroom.error("could not determine "..key.." - wasn't specified at compile time")
  end

  return darkroom._boundImages[id+1][key]
end

function astFunctions:size()
  local width, height
  self:S("load"):traverse(
    function(n)
      if width==nil then
        width = orionSimple.imageWidths[n.from+1]
        height = orionSimple.imageHeights[n.from+1]
      else
        if orionSimple.imageWidths[n.from+1]~=width or
          orionSimple.imageHeights[n.from+1]~=height then
          darkroom.error(":size() Width/ height of all input images must match!")
        end
      end
    end)
  return width, height
end

function astFunctions:width()
  local w,h = self:size()
  return w
end

function astFunctions:height()
  local w,h = self:size()
  return h
end


function astFunctions:id()
  if self.kind~="crop" or self.expr.kind~="special" then
    darkroom.error("could not determine "..key.." - not an input fn")
  end

  assert(type(self.expr.id)=="number")
  return self.expr.id
end

-- explicitly set image width/height
function orionSimple.setImageSize(width, height)
  assert(type(width)=="number")
  assert(type(height)=="number")
  orionSimple.width = width
  orionSimple.height = height
end

function orionSimple.compile(outList, options)
  options = options or {}
  local outDecl = {}
  local outArgs = {}
  local outRes = {}

  -- check that the width/height of all _used_ images match
  local width = orionSimple.width
  local height = orionSimple.height
  for _,v in pairs(outList) do
    local w,h = v:size()
    if width==nil then
      width=w; height=h
    elseif w~=nil and (w~=width or h~=height) then
      darkroom.error("Width/ height of all input images must match!")
    end
  end

  if width==nil then
    darkroom.error("Width/Height of output could not be deteremined! If no image inputs, it must be specified!")
  end

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
          cstdlib.posix_memalign( [&&opaque](&data), 4*1024, [upToNearest(options.V,width)*height*v.kernel.type:sizeof()])
          s:init([width],[height],[upToNearest(options.V,width)],[v.kernel.type:channels()],[v.kernel.type:baseType():sizeof()]*8,[v.kernel.type:isFloat()],[v.kernel.type:isInt()],true,data,data)
        end)
      table.insert(outRes, quote s:toAOS() in s end)
      table.insert(outArgs, `s.data)
    end
    if ocallbackKernelGraph ~= nil then ocallbackKernelGraph(kernelGraph) end
  end
  
  local fn = darkroom.compile( orionSimple.images, outList, orionSimple.taps, width, height, options)

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

  return fin
end

return orionSimple