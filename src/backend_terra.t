cmath = terralib.includec("math.h")
cstdio = terralib.includec("stdio.h")
cassert = terralib.includec("assert.h")
cstdlib = terralib.includec("stdlib.h")
cpthread = terralib.includec("pthread.h")

darkroom.terracompiler = {}

-- This is basically Array<T>. If you call a method on an table with this MT, it dispatches the method to each element in the table.
-- This is used for the input/output image lists. One bit of shenanigans: if one of the arguments is an array, it is indexed by the
-- same key as the table. This is used to support multichannel input/output
pointwiseDispatchMT = {
  __index = function(self, method) 
    return function(self, ...)
      local res = {}
      for i, element in pairs(self) do -- for each channel or image (remember, this nests)
        local varargs = {...}
        for k,v in ipairs(varargs) do 
          if type(v)=="table" and keycount(v)==#v then -- if this argument is an array of chennels, break it open
            varargs[k] = v[i]; 
          end
        end
        table.insert(res, element[method](element,unpack(varargs)))
      end
      return quote res end
  end
  end
}

function strideMod(downsampleStride, upsampleStride, clock)
  if clock==nil then return 1 end
  assert(terralib.isquote(clock) or terralib.issymbol(clock))

  if upsampleStride==1 then
    return downsampleStride
  elseif downsampleStride==1 then
    return `terralib.select(clock % upsampleStride==(upsampleStride-1), 1, 0)
  else
    print(downsampleStride, upsampleStride, clock)
    assert(false)
  end

end

function looprate(N,D,max)
  if N==0 or D==0 then return 1
  else
    local r = (D*max)/N
    assert(r==math.floor(r))
    return r
  end
end

function imageSize(w,N,D)
  if D==0 then return w end
  return math.ceil(w*(N/D))
end

function ratioToScale(N,D)
  local scale = 1
  if D~=0 then scale = N/D end -- images with scale free values, eg numbers
  return scale
end

function calculateStride(producerN, producerD, consumerN, consumerD)
  if consumerN==nil or (producerN==0 and producerD==0 and consumerN==0 and consumerD==0) then return 1,1 end
  if producerN==0 and producerD==0 then producerN=1; producerD=1 end
  if consumerN==0 and consumerD==0 then consumerN=1; consumerD=1 end
  producerN, producerD = ratioFactor(producerN, producerD)
  consumerN, consumerD = ratioFactor(consumerN, consumerD)

  if (consumerN/consumerD)<=(producerN/producerD) then
    -- a downsample
    local d = (producerN/producerD)/(consumerN/consumerD)
    assert(d==math.floor(d))
    return d,1
  elseif (producerN/producerD)<=(consumerN/consumerD) then
    -- a upsample
--    local d = (consumerN/consumerD)/(producerN/producerD)
    local d = (consumerN*producerD)/(consumerD*producerN) -- line above was causing floating point errors, making assert below fail
    assert(d==math.floor(d))
    return 1,d
  end
  print(producerN,producerD,consumerN,consumerD)
  assert(false)
end

function scaledAbsolute(v,upsampleStride, downsampleStride)
  assert(terralib.isquote(v) or terralib.issymbol(v))
  if upsampleStride==1 and downsampleStride==1 then
    return v
  elseif downsampleStride==1 then
    return `floorDivide(v,upsampleStride)
  elseif upsampleStride==1 then
    return `v*downsampleStride
  else
    assert(false)
  end
end

function scaledDelta( v, upsampleStride, downsampleStride, readerPos)
  assert(terralib.isquote(v) or terralib.issymbol(v) or type(v)=="number"); 
  assert(type(upsampleStride)=="number");
  if upsampleStride==1 and downsampleStride==1 then
    return v
--  elseif downsampleStride==1 and (upsampleStride==2 or upsampleStride==4) and type(v)=="number" then
    -- special case for performance
--    return math.floor(v/upsampleStride)
  elseif downsampleStride==1 then
    return `floorDivide(readerPos+v, upsampleStride)-floorDivide(readerPos, upsampleStride)
  elseif upsampleStride==1 then
    return `v*downsampleStride
  else
    assert(false)
  end
end

-----------------------------------
local vmIV = terralib.includecstring [[
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

// lines in the linebuffer may cross the pages, so we have to triple buffer it
void * makeCircular (void* address, int bytes) {
  char path[] = "/tmp/ring-buffer-XXXXXX";
  int file_descriptor = mkstemp(path);

  assert(bytes > 0); 
  assert(bytes % (4*1024) == 0);
  assert((int)address % (4*1024) == 0);

  if ( file_descriptor<0){
    printf("Error, could not allocate file %s for linebuffer\n",path);
  }

  assert(!unlink(path));
 
  assert(!ftruncate(file_descriptor, bytes));

  void * addressp =
    mmap(address, bytes, PROT_READ | PROT_WRITE,
         MAP_FIXED | MAP_SHARED, file_descriptor, 0);

    assert(address == addressp);
  
    addressp = mmap ((char*)address + bytes,
                  bytes, PROT_READ | PROT_WRITE,
                  MAP_FIXED | MAP_SHARED, file_descriptor, 0);
    assert(addressp == (char*)address + bytes);

    ///////////// triple buffer
    addressp = mmap ((char*)address + bytes*2,
                  bytes, PROT_READ | PROT_WRITE,
                  MAP_FIXED | MAP_SHARED, file_descriptor, 0);
    assert(addressp == (char*)address + bytes*2);
    ////////////////

    assert(!close (file_descriptor));
  return address;
                                                  }

void* allocateCircular( int bytes ){
  if(bytes==0){ return 0; } // its possible we have no linebuffers
  void * address = mmap (NULL, bytes << 1, PROT_NONE,
                         MAP_ANON | MAP_PRIVATE, -1, 0);
 
  assert(address != MAP_FAILED);
  return address;
                                   }
                                      ]]

LineBufferWrapperFunctions = {}
LineBufferWrapperMT={__index=LineBufferWrapperFunctions}
linebufferCount = 7 -- just start with a random id to make the debug checks more effective
function isLineBufferWrapper(b) return getmetatable(b)==LineBufferWrapperMT end

function newLineBufferWrapper( lines, orionType, leftStencil, stripWidth, rightStencil, debug, V, debugImagePath, debugBounds )
  assert(type(lines)=="number")
  assert(type(leftStencil)=="number")
  assert(type(stripWidth)=="number")
  assert(type(rightStencil)=="number")
  assert(type(debug)=="boolean")
  assert(darkroom.type.isType(orionType))
  assert(type(V)=="number")

  assert( stripWidth % V == 0 ) -- this had better be the case, or sets will fail

  assert(lines > 0) -- a line buffer had better contain lines
  assert(stripWidth > 0)
  assert(stripWidth - leftStencil + rightStencil > 0)

  local tab = {lines=lines, 
               id = linebufferCount, -- for debugging
               orionType=orionType, 
               leftStencil = leftStencil, 
               stripWidth = stripWidth, 
               rightStencil=rightStencil,
               linebufferPosition = 0,
               debug=debug or debugImagePath~=nil,
               iv={}, 
               ivDebugX = {}, 
               ivDebugY={}, 
               ivDebugId = {}, 
               posX={}, posY={},
               debugImagePath = debugImagePath,
               debugBounds = debugBounds}

  linebufferCount = linebufferCount + 1

  return setmetatable(tab,LineBufferWrapperMT)
end

function LineBufferWrapperFunctions:declareMainThread()
  return quote end
end

function LineBufferWrapperFunctions:declare( loopid, x, y, clock, core, stripId, options, linebufferBase )
  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y)) -- we don't actually use this - we don't care what the actual coord is
  assert(terralib.isquote(clock) or terralib.issymbol(clock))
  assert(terralib.isquote(core) or terralib.issymbol(core))
  assert(terralib.issymbol(stripId))
  assert(terralib.issymbol(linebufferBase))
  assert(type(options)=="table")

  local res = {}

  if self.iv[loopid]==nil then self.iv[loopid] = symbol(&self.orionType:toTerraType(),"iv") end
    
  if self.debug then
    if self.ivDebugX[loopid]==nil then self.ivDebugX[loopid] = symbol(&int,"ivDebugX") end
    if self.ivDebugY[loopid]==nil then self.ivDebugY[loopid] = symbol(&int,"ivDebugY") end
    if self.ivDebugId[loopid]==nil then self.ivDebugId[loopid] = symbol(&int,"ivDebugId") end
    self.posX[loopid] = symbol(int,"posX")
    self.posY[loopid] = symbol(int,"posY")
  end

  if self.startClock==nil then self.startClock=symbol(int,"startClock"); table.insert(res, quote var [self.startClock] = clock end) end

  if self.debug then table.insert(res, quote var [self.posX[loopid]] = x; var [self.posY[loopid]] = clock; end) end

  local l = {self.iv[loopid]}
  local baseKey = {"base"}
  local bufType = {self.orionType:toTerraType()}
  if self.debug then l = {self.iv[loopid], self.ivDebugX[loopid], self.ivDebugY[loopid], self.ivDebugId[loopid] } end
  if self.debug then baseKey = {"base","baseDebugX","baseDebugY","baseDebugId"} end
  if self.debug then bufType = {self.orionType:toTerraType(), int, int, int} end

  for k,buf in pairs(l) do
    -- only allocate space the first time
    if self[baseKey[k]]==nil then
      self[baseKey[k]] = symbol(&bufType[k],baseKey[k])
      table.insert(res, 
        quote
          vmIV.makeCircular(linebufferBase,[self:modularSize(bufType[k])*terralib.sizeof(bufType[k])])
          var [self[baseKey[k]]] = [&bufType[k]](linebufferBase)
          -- this is *3 b/c we triple buffer the circular buffers (one duplicate before, one after)
          linebufferBase = [&int8](linebufferBase) + [self:modularSize(bufType[k])*terralib.sizeof(bufType[k])*3]
        end)
    end

    table.insert(res, 
      quote 
        -- we always start in the second segment of the circular buffer b/c we always read from smaller addresses
        var leftBound = [self.stripWidth]*stripId + [self.leftStencil]
        var [buf] = [self[baseKey[k]]]+(x-leftBound)+[self:modularSize(bufType[k])]+fixedModulus((clock-[self.startClock])*[self:lineWidth()],[self:modularSize(bufType[k])])
      end)

  end

  if self.debugImagePath~=nil then
    self.debugImageFile = symbol()
    table.insert(res, quote  var [self.debugImageFile] = cstdio.fopen([self.debugImagePath..".raw"],"wb")  end)
  end

  return quote res end
end

-- lineWidth in pixels
function LineBufferWrapperFunctions:lineWidth() return self.stripWidth-self.leftStencil+self.rightStencil end
-- this is the number of pixels in the linebuffer
-- we round up to the nearest page size (for the circular buffer that must be page aligned). The double for the double mapping in VM space
function LineBufferWrapperFunctions:entries() return self.lines*self:lineWidth() end
-- this return the size of the allocated linebuffer in pixels for a given type. ie, how many pixels to subtract to wrap from the end of the buffer to the start
function LineBufferWrapperFunctions:modularSize(terraType) return upToNearest(4*1024,self:entries()*terralib.sizeof(terraType))/terralib.sizeof(terraType) end
-- debugging requires 3 extra buffers
function LineBufferWrapperFunctions:allocateSize() 
  local s = upToNearest(4*1024,self:entries()*self.orionType:sizeof())*3
  -- 3 debug buffers, triple buffered
  if self.debug then  s = s + upToNearest(4*1024,self:entries()*terralib.sizeof(int))*3*3 end
  return s
end

function LineBufferWrapperFunctions:set( loopid, value, V )
  assert( terralib.isquote( value ) or terralib.issymbol( value ) )
  assert(type(loopid)=="number")
  assert(type(V)=="number")

    local res = {}

    if self.debug then

      for i=0,V-1 do
        table.insert(res,quote
                       @([self.ivDebugX[loopid]]+i) = [self.posX[loopid]]+i
                       @([self.ivDebugY[loopid]]+i) = [self.posY[loopid]]
                       @([self.ivDebugId[loopid]]+i) = [self.id]
        end)
      end
      table.insert(res, quote 
                     orionAssert(uint64([self.iv[loopid]]) % (V*sizeof([self.orionType:toTerraType()])) == 0,"lb set not aligned") end)
    end

  table.insert(res,quote @[&vector(self.orionType:toTerraType(),V)]([self.iv[loopid]]) = value end)

  if self.debugImagePath~=nil then
    table.insert( res, self:debugImageWrite( loopid, value, V ) )
  end

  return quote res end
end

function LineBufferWrapperFunctions:debugImageWrite( loopid, value, V )
  return quote 
    var debugImageData : vector(self.orionType:toTerraType(),V) = value
    
    for i=0,V do
      var x = [self.posX[loopid]]+i
      var y = [self.posY[loopid]]
      var w = [self.debugBounds.maxX-self.debugBounds.minX]
      var h = [self.debugBounds.maxY-self.debugBounds.minY]
      
      if x >= self.debugBounds.minX and x < self.debugBounds.maxX and
        y >= self.debugBounds.minY and y < self.debugBounds.maxY then
        
        cstdio.fseek( self.debugImageFile, sizeof([self.orionType:toTerraType()])*((y-self.debugBounds.minY)*w+(x-self.debugBounds.minX)), cstdio.SEEK_SET )
        var d = debugImageData[i]
        cstdio.fwrite( &d, sizeof([self.orionType:toTerraType()]), 1, self.debugImageFile ) 
      end
    end 
  end
end

-- relX and relY should be integer constants relative to
-- the current location
function LineBufferWrapperFunctions:get(loopid, gather, relX, relY, V, valid)
  assert(type(loopid)=="number")
  assert(type(gather)=="boolean")
  assert(type(relX)=="number" or terralib.isquote(relX))
  assert(type(relY)=="number" or terralib.isquote(relY))
  assert(type(V)=="number")

  local res

  if gather then 
    local vres = {}
    for i=0,V-1 do table.insert(vres, `@([self.iv[loopid]] + relY[i]*[self:lineWidth()] + relX[i] + i) ) end
    res = `vectorof([self.orionType:toTerraType()], vres)
  else
    res = `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.iv[loopid]] + relY*[self:lineWidth()]+ relX),{align=[self.orionType:sizeof()]})
  end


  if self.debug then
    local i = symbol(int,"lbGetDebugI")

    local lrelX = relX
    local lrelY = relY
    
    if gather then
      lrelX = `relX[i]
      lrelY = `relY[i]
    end

    res = quote 
      for [i]=0,V do
                     -- because we expand out the valid region to the largest vector size (to save having to codegen the non-vectorized dangling region), 
                     -- some of the area we compute is garbage, and thus
                     -- will read invalid values (but we will overwrite it later so its ok). Bypass the debug checks on those regions.
        if [valid(i)] then
                       orionAssert(@([self.ivDebugId[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.id], "incorrect LB Id")
                       orionAssert(@([self.ivDebugY[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.posY[loopid]]+lrelY, "incorrect LB Y") 
                       orionAssert(@([self.ivDebugX[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.posX[loopid]]+i+lrelX, "incorrect LB X")
                     end

      end
      in res end
  end

  return res
end

function LineBufferWrapperFunctions:next( loopid, v )
  assert(type(loopid)=="number")
  assert(type(v)=="number" or terralib.isquote(v))

  local res = {}

  table.insert(res, quote [self.iv[loopid]] = [self.iv[loopid]] + v end)

  if self.debug then
    table.insert(res, quote [self.ivDebugX[loopid]] = [self.ivDebugX[loopid]] + v end)
    table.insert(res, quote [self.ivDebugY[loopid]] = [self.ivDebugY[loopid]] + v end)
    table.insert(res, quote [self.ivDebugId[loopid]] = [self.ivDebugId[loopid]] + v end)
    table.insert(res, quote [self.posX[loopid]] = [self.posX[loopid]] + v end)
  end

  return quote res end
end

-- sub: this is the number of pixels we looped over since last calling nextline
-- basically: the number of times we called nextVector this row * the vector width
function LineBufferWrapperFunctions:nextLine( loopid,  subX, strideY )
  assert( terralib.isquote( subX ) or terralib.issymbol( subX ) )
  assert( terralib.isquote( strideY ) or terralib.issymbol( strideY ) )

  local res = {}

  local buf = {self.iv[loopid]}
  local base = {self.base}
  local bufType = {self.orionType:toTerraType()}

  if self.debug then
    buf = {self.iv[loopid], self.ivDebugX[loopid], self.ivDebugY[loopid], self.ivDebugId[loopid]}
    base = {self.base, self.baseDebugX, self.baseDebugY, self.baseDebugId}
    bufType = {self.orionType:toTerraType(),int,int,int}

    table.insert(res, quote [self.posY[loopid]] = [self.posY[loopid]] + strideY end)
    table.insert(res, quote [self.posX[loopid]] = [self.posX[loopid]] + subX end)
  end

  for k,v in pairs(buf) do
    table.insert(res, 
      quote 
        [buf[k]] = [buf[k]] + subX + [self:lineWidth()]*[strideY]
        if [buf[k]] >= [base[k]]+[self:modularSize(bufType[k])*2] then [buf[k]] = [buf[k]] - [self:modularSize(bufType[k])] end
      end)
  end


  return quote res end
end

ScaleLineBufferWrapperFunctions = {}
ScaleLineBufferWrapperMT={__index=ScaleLineBufferWrapperFunctions}

function newScaleLineBufferWrapper( lines, orionType, leftStencil, stripWidth, rightStencil, debug, scaleN1, scaleD1, scaleN2, scaleD2, largestScaleY, V, debugImagePath, debugBounds )
  assert(type(lines)=="number")
  assert(type(leftStencil)=="number")
  assert(type(stripWidth)=="number")
  assert(type(rightStencil)=="number")
  assert(type(debug)=="boolean")
  assert(type(scaleN1)=="number")
  assert(type(scaleD1)=="number")
  assert(type(scaleN2)=="number")
  assert(type(scaleD2)=="number")
  assert(type(largestScaleY)=="number")
  assert(darkroom.type.isType(orionType))
  assert(type(V)=="number")

  assert( stripWidth % V == 0 ) -- this had better be the case, or sets will fail

  assert(lines > 0) -- a line buffer had better contain lines
  assert(stripWidth > 0)
  assert(stripWidth - leftStencil + rightStencil > 0)


  local tab = {lb = newLineBufferWrapper( lines, orionType, leftStencil, stripWidth, rightStencil, debug, V, debugImagePath, debugBounds ),
               debug = debug,
               scaleN1=scaleN1,
               scaleD1=scaleD1,
               scaleN2=scaleN2,
               scaleD2=scaleD2,
               readerPosX={}, -- the x,y of the kernel that's reading this image. Not necessarily the same as posX,posY with up/downsample!
               readerPosY={},
               largestScaleY=largestScaleY,
               downsampleStrideX={},
               upsampleStrideX={},
               downsampleStrideY={},
               upsampleStrideY={}}

  return setmetatable(tab,ScaleLineBufferWrapperMT)
end

function ScaleLineBufferWrapperFunctions:declare( loopid, x, y, clock, core, stripId, options, scaleN1, scaleD1, scaleN2, scaleD2,linebufferBase )
  self.downsampleStrideX[loopid], self.upsampleStrideX[loopid] = calculateStride(self.scaleN1, self.scaleD1, scaleN1, scaleD1)
  self.downsampleStrideY[loopid], self.upsampleStrideY[loopid] = calculateStride(self.scaleN2, self.scaleD2, scaleN2, scaleD2)

  clock = `floorDivide(clock,[looprate(self.scaleN2,self.scaleD2,self.largestScaleY)])

  local res = {}

  local scaledX = `[scaledAbsolute( x, self.upsampleStrideX[loopid], self.downsampleStrideX[loopid] )]

  if self.upsampleStrideX[loopid]>1 or self.debug then self.readerPosX[loopid] = symbol(int,"readerPosX"); table.insert(res, quote var [self.readerPosX[loopid]] = x; end) end
  if self.upsampleStrideY[loopid]>1 or self.debug then self.readerPosY[loopid] = symbol(int,"readerPosY"); table.insert(res, quote var [self.readerPosY[loopid]] = y; end) end

  table.insert(res, self.lb:declare( loopid, scaledX, y, clock, core, stripId, options, linebufferBase ) )

  return quote res end
end
function ScaleLineBufferWrapperFunctions:declareMainThread() return self.lb:declareMainThread() end
function ScaleLineBufferWrapperFunctions:allocateSize() return self.lb:allocateSize() end

function ScaleLineBufferWrapperFunctions:set( loopid, value, V ) return self.lb:set( loopid, value, V ) end

function ScaleLineBufferWrapperFunctions:get(loopid, gather, relX, relY, V, validLeft, validRight )
  local downsampleStride = self.downsampleStrideX[loopid]
  local upsampleStride = self.upsampleStrideX[loopid]

  local valid = function(i) return `[self.readerPosX[loopid]]+i >= validLeft and [self.readerPosX[loopid]]+i < validRight end

  if downsampleStride==1 and upsampleStride==1 then
    return self.lb:get( loopid, gather, relX, relY, V, valid )
  else
    assert(gather==false)
    local xoff = map( range(V), function(i) return `[scaledDelta(i-1, upsampleStride, downsampleStride, self.readerPosX[loopid])]-(i-1) end)
    local yoff = map( range(V), function(i) return 0 end )
    return self.lb:get( loopid, true, `vectorof(int, xoff)+relX, `vectorof(int,yoff)+relY, V, valid )
  end
end

function ScaleLineBufferWrapperFunctions:next( loopid, vorig )
  local v = scaledDelta(vorig, self.upsampleStrideX[loopid], self.downsampleStrideX[loopid], self.readerPosX[loopid])

  local res = { self.lb:next( loopid, v ) }
  if self.debug or self.upsampleStrideX[loopid]>1 then table.insert(res, quote [self.readerPosX[loopid]] = [self.readerPosX[loopid]] + vorig end) end
  return quote res end
end

function ScaleLineBufferWrapperFunctions:nextLine( loopid,  sub )
  local res = {}
  local subX = symbol(int,"subX")
  table.insert(res, quote var [subX] = [scaledDelta(`-sub, self.upsampleStrideX[loopid], self.downsampleStrideX[loopid], self.readerPosX[loopid])] end)
  local strideY = symbol(int,"strideY")
  table.insert(res, quote var [strideY] = [scaledDelta(1, self.upsampleStrideY[loopid], self.downsampleStrideY[loopid], self.readerPosY[loopid])] end)

  table.insert( res, self.lb:nextLine( loopid, subX, strideY ) )

  if self.debug or self.upsampleStrideY[loopid]>1 then table.insert(res, quote [self.readerPosY[loopid]] = [self.readerPosY[loopid]] + 1 end) end
  if self.debug or self.upsampleStrideX[loopid]>1 then table.insert(res, quote [self.readerPosX[loopid]] = [self.readerPosX[loopid]] - sub end) end

  return quote res end
end

ImageWrapperFunctions = {}
ImageWrapperMT={__index=ImageWrapperFunctions}
function isImageWrapper(b) return getmetatable(b)==ImageWrapperMT end

-- tab.terraType should be the base type of the data stored in this image
-- ie, if it's a floating point image, tab.terraType should be float
function newImageWrapper( basePtr, orionType, stride, debug, scaleN1, scaleD1, scaleN2, scaleD2, largestScaleY, sparse, mainThreadBasePtr )
  assert(darkroom.type.isType(orionType))
  assert(type(stride)=="number")
  assert(type(debug) == "boolean")
  assert(type(scaleN1)=="number")
  assert(type(scaleD1)=="number")
  assert(type(scaleN2)=="number")
  assert(type(scaleD2)=="number")
  assert(type(largestScaleY)=="number")
  assert(type(sparse)=="boolean")

  local tab = {data={},basePtr=basePtr,orionType=orionType, stride=stride, debug=debug, scaleN1=scaleN1, scaleD1=scaleD1, scaleN2=scaleN2, scaleD2=scaleD2, largestScaleY=largestScaleY, downsampleStrideX={}, downsampleStrideY={}, upsampleStrideX={}, upsampleStrideY={}, readerPosX={}, readerPosY={}, sparse=sparse, posX={}, posY={}, mainThreadBasePtr = mainThreadBasePtr}

  return setmetatable(tab,ImageWrapperMT)
end

function ImageWrapperFunctions:declare( loopid, x, y, clock, core, stripId, options, scaleN1, scaleD1, scaleN2, scaleD2 )
  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y))
  assert(terralib.isquote(clock) or terralib.issymbol(clock)) -- not used
  assert(terralib.isquote(core) or terralib.issymbol(core))
  assert(terralib.isquote(stripId) or terralib.issymbol(stripId))
  assert(type(options)=="table")
  assert(type(scaleD2)=="number")

  -- we never access inputs or outputs at anything other than their native scale
  assert( ratioToScale(self.scaleN1, self.scaleD1) == ratioToScale(scaleN1, scaleD1) )
  assert( ratioToScale(self.scaleN2, self.scaleD2) == ratioToScale(scaleN2, scaleD2) )

  y = `floorDivide(y,[looprate(self.scaleN2,self.scaleD2,self.largestScaleY)])

  if self.data[loopid]==nil then self.data[loopid] = symbol(&(self.orionType:toTerraType()),"data") end

  local res = {}
  if self.sparse then 
    self.posX[loopid] = symbol(int, "sparsePosX")
    self.posY[loopid] = symbol(int, "sparsePosY")
    return quote var [self.posX[loopid]] = x; var [self.posY[loopid]] = y; var [self.data[loopid]] =  [&self.orionType:toTerraType()]([&int]([self.basePtr])+1) end
  else
    return quote var [self.data[loopid]] =  [&self.orionType:toTerraType()]([self.basePtr] + y*[self.stride] + x) end
  end
end

function ImageWrapperFunctions:declareMainThread()
  if self.sparse then
    -- we need to initialize the count once, before any of the threads run
    return quote @[&uint](self.mainThreadBasePtr) = 0; end
  end
  return quote end
end

local atomicStack = terralib.includecstring [[
#include <stdio.h>

void add(unsigned int* basePtr, unsigned char* dataPtr, unsigned char* data, int dataLen){
  // basePtr holds the count of items. This starts at 0. We atomically reserve
  // a slot in the stack, and then write to it.
  // TODO: a better way to do this would be to keep a local (1k?) stack per thread,
  // and only do an atomic when this stack filled. This would cut down on the number
  // of atomic accesses.

  unsigned int reservedBasePtr = __sync_fetch_and_add(basePtr,1);
  for(int i=0; i<dataLen; i++){
    *(dataPtr+reservedBasePtr*dataLen+i) = data[i];
  }
}
                                             ]]

function ImageWrapperFunctions:set( loopid, value, V, notFiltered )
  assert(terralib.isquote(value))
  assert(type(loopid)=="number")
  assert(type(V)=="number")

  local res = {}
    
  if self.debug then
    table.insert(res,
                   quote
--                     orionAssert( ([self.data[loopid]]-[self.basePtr])<maxv,"wrote beyond end of array")
                     orionAssert( (int64([self.data[loopid]])-int64([self.basePtr]))>=0,"wrote before start of array")
                     orionAssert( uint64([self.data[loopid]]) % (V*sizeof([self.orionType:toTerraType()])) == 0, "write is not aligned!")
      end)
  end

  if self.sparse then
    if terralib.isquote(notFiltered) then
      local datalen = terralib.sizeof(int)*2+self.orionType:sizeof()
      return quote 
        for v=0,V do
          if notFiltered[v] then
            var dataPacket : uint8[datalen]
            @[&int](&dataPacket) = [self.posX[loopid]]+v;
            @([&int](&dataPacket)+1) = [self.posY[loopid]];
            @([&self.orionType:toTerraType()]([&int](&dataPacket)+2)) = value[v]
            atomicStack.add([&uint32](self.basePtr), [&uint8]([self.data[loopid]]), dataPacket, datalen)
          end
        end
      end
    end
  else
    table.insert(res,quote terralib.attrstore([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]]),value,{nontemporal=true}) end)
  end

  return quote res end
end

-- relX and relY should be integer constants relative to
-- the current location
function ImageWrapperFunctions:get(loopid, gather, relX, relY, V)
  assert(type(loopid)=="number")
  assert(type(gather)=="boolean")
  assert(type(relX)=="number" or terralib.isquote(relX))
  assert(type(relY)=="number" or terralib.isquote(relY))
  assert(type(V)=="number")

  assert(self.sparse==false)

  local expr

  if gather then
    local res = {}
    for i=0,V-1 do table.insert(res, `@([&self.orionType:toTerraType()]([self.data[loopid]] + relY[i]*[self.stride] + relX[i])) ) end
    expr = `vectorof([self.orionType:toTerraType()], res)
  else
    expr = `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]] + relY*[self.stride] + relX),{align=[self.orionType:sizeof()]})
  end

  if self.debug then
    local i = symbol(int,"lbGetDebugI")

    local lrelX = relX
    local lrelY = relY
    
    if gather then
      lrelX = `relX[i]
      lrelY = `relY[i]
    end

    return quote
      for [i]=0,[V-1] do
        var this = [self.data[loopid]] + lrelY*[self.stride] + lrelX
      
        if (this-[self.basePtr])<0 then cstdio.printf("i:%d this:%d start:%d self.data:%d relY:%d stride:%d relX%d\n",
                                                     i,this,[self.basePtr],[self.data[loopid]],lrelY,[self.stride],lrelX) 
        end
        orionAssert( (this-[self.basePtr])>=0,"read before start of array")
      end
      in expr end
  end

  return expr
end

function ImageWrapperFunctions:next( loopid, v )
  assert(type(loopid)=="number")
  assert(type(v)=="number" or terralib.isquote(v))
  local res = {}
  if self.sparse then return quote [self.posX[loopid]] = [self.posX[loopid]]+v end
  else return quote [self.data[loopid]] = [self.data[loopid]] + v end end
end

-- sub: this is the number of pixels we looped over since last calling nextline
-- basically: the number of times we called nextVector this row * the vector width
function ImageWrapperFunctions:nextLine( loopid, sub )
  assert(terralib.isquote(sub))
  if self.sparse then return quote [self.posX[loopid]] = [self.posX[loopid]] - sub; [self.posY[loopid]] = [self.posY[loopid]] + 1; end
  else return quote [self.data[loopid]] = [self.data[loopid]] + [self.stride] - sub end end
end


-- this is the source for the terra compiler for orion

-- if pointer is true, generate a pointer instead of a value
function darkroom.terracompiler.symbol(_type,pointer,vectorN)
  assert(darkroom.type.isType(_type))
  assert(vectorN==nil or type(vectorN)=="number")
  
  return symbol(_type:toTerraType(pointer,vectorN))
end

function darkroom.terracompiler.vectorizeBinaryPointwise(func,lhs,rhs,V)
  assert(terralib.isfunction(func))
  assert(terralib.isquote(lhs))
  assert(terralib.isquote(rhs))
  assert(type(V)=="number")

  -- if we didn't store these in a variable, they may not be a vector type
  lhs = `[vector(float,V)](lhs)
  rhs = `[vector(float,V)](rhs)

  local q = {}
  for i=1,V do
    table.insert(q,`func(lhs[i-1],rhs[i-1]) )
  end
  assert(#q==V)
  
  return `vector(q)
end

darkroom.terracompiler.numberBinops={
  ["+"]=function(lhs,rhs) return `lhs+rhs end,
  ["-"]=function(lhs,rhs) return `lhs-rhs end,
  ["/"]=function(lhs,rhs) return `lhs/rhs end,
  ["*"]=function(lhs,rhs) return `lhs*rhs end,
  ["<"]=function(lhs,rhs) return `lhs<rhs end,
  ["<="]=function(lhs,rhs) return `lhs<=rhs end,
  [">"]=function(lhs,rhs) return `lhs>rhs end,
  [">="]=function(lhs,rhs) return `lhs>=rhs end,
  ["=="]=function(lhs,rhs) return `lhs==rhs end,
  ["~="]=function(lhs,rhs) return `lhs~=rhs end,
  ["%"]=function(lhs,rhs) return `lhs%rhs end,
  ["^"]=function(lhs,rhs) return `lhs^rhs end,
  ["and"]=function(lhs,rhs) return `lhs and rhs end,
  ["or"]=function(lhs,rhs) return `lhs or rhs end,
  ["<<"]=function(lhs,rhs) return `lhs<<rhs end,
  [">>"]=function(lhs,rhs) return `lhs>>rhs end,
  ["min"]=function(lhs,rhs) return `terralib.select(lhs<rhs,lhs,rhs) end,
  ["max"]=function(lhs,rhs) return `terralib.select(lhs>rhs,lhs,rhs) end,
  ["floorDivide"]=function(lhs,rhs,V) return darkroom.terracompiler.vectorizeBinaryPointwise(floorDivide,lhs,rhs,V) end, -- used to modify Y coordinate when we shift scaled images
  ["pow"]=function(lhs,rhs,V)
    return darkroom.terracompiler.vectorizeBinaryPointwise(cmath.pow,lhs,rhs,V)
  end
}


-- func should take a single scalar value, and return a single scalar value
function darkroom.terracompiler.vectorizeUnaryPointwise(func,expr,V)
  assert(terralib.isfunction(func))
  assert(terralib.isquote(expr))
  assert(type(V)=="number")

  local q = {}
  for i=1,V do
    table.insert(q,`func(expr[i-1]))
  end
  assert(#q==V)

  return `vector(q)
end

darkroom.terracompiler.numberUnary={
  ["floor"] = function(expr,ast,V)
    return darkroom.terracompiler.vectorizeUnaryPointwise(cmath.floor,expr,V)
  end,
  ["ceil"] = function(expr,ast,V)
    return darkroom.terracompiler.vectorizeUnaryPointwise(cmath.ceil,expr,V)
  end,
  ["-"] = function(expr,ast,V) return `-expr end,
  ["not"] = function(expr,ast,V) return `not expr end,
  ["abs"] = function(expr,ast,V)
    if ast.type:baseType():isFloat() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(cmath.fabs,expr,V)
    elseif ast.type:baseType():isUint() then
      -- a uint is always positive
      return expr
    elseif ast.type:baseType():isInt() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(cstdlib.abs,expr,V)
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["sin"] = function(expr,ast,V)
    if ast.type:baseType():isFloat() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(cmath.sin,expr,V)
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["cos"] = function(expr,ast,V)
    if ast.type:baseType():isFloat() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(  cmath.cos, expr, V )
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["arctan"] = function(expr,ast,V)
    if ast.type:baseType():isFloat() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(  cmath.atan, expr, V )
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["exp"] = function(expr,ast,V)
    if ast.type:baseType():isFloat() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(  cmath.exp, expr, V )
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["sqrt"] = function(expr,ast,V)
    if ast.type:baseType():isFloat() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(  cmath.sqrt, expr, V )
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["ln"] = function(expr,ast,V)
    if ast.type:baseType():isFloat() then
      return darkroom.terracompiler.vectorizeUnaryPointwise(  cmath.log, expr, V )
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["print"]= function(expr,ast,V)
    local stat = {}

    for v = 0,V-1 do
      if ast.type:baseType():isFloat() then
        table.insert(stat,quote cstdio.printf("darkroom.printf:%f\n",expr[v]) end)
      elseif ast.type:baseType():isInt() or ast.type:baseType():isUint() then
        table.insert(stat,quote cstdio.printf("darkroom.printd:%d\n",expr[v]) end)
      else
        ast.type:print()
        assert(false)
      end
    end
    return quote stat in expr end
  end
}

darkroom.terracompiler.boolBinops={
  ["and"]=function(lhs,rhs) return `lhs and rhs end,
  ["or"]=function(lhs,rhs) return `lhs or rhs end
}

function eliminateIterate(typedAST)
  assert(darkroom.typedAST.isTypedAST(typedAST))

  local res = typedAST:S(function(n) return n.kind=="iterate" or n.kind=="gatherColumn" or n.kind=="iterateload" end):process(
    function(n)
      if n.kind=="iterate" then
        local nn = {}
        nn.kind="mapreduce"
        nn.reduceop = n.reduceop
        nn.type = n.type
        nn.varname1 = n.iteratorName
        nn.varlow1 = n.iterationSpaceLow
        nn.varhigh1 = n.iterationSpaceHigh
        local mapreducevar = {kind="mapreducevar", type=darkroom.type.int(32), id=1, mapreduceNodeKey = nn.__key}
        mapreducevar = darkroom.typedAST.new(mapreducevar):copyMetadataFrom(n)

        local mapreducevarAST = {kind="mapreducevar", id = 1, mapreduceNode = nn.__varid1}
        mapreducevarAST = darkroom.ast.new(mapreducevarAST):copyMetadataFrom(n)

        nn.expr = n.expr:S("*"):process(
          function(iv) 
            if iv.kind=="iterationvar" then
              return mapreducevar 
            else
              local n = iv:shallowcopy()
              local changed = false
              for k,v in pairs(iv) do
                if darkroom.ast.isAST(v) then
                  changed = true
                  n[k] = v:S("iterationvar"):process(function(iiv) return mapreducevarAST end)
                end
              end
              if changed then return darkroom.typedAST.new(n):copyMetadataFrom(iv) end
            end
          end)

        return darkroom.typedAST.new(nn):copyMetadataFrom(n)
      elseif n.kind=="gatherColumn" then
        local array = {kind="array", type = n.type}
        local c = 1
        -- row major
        for y=n.columnStartY, n.columnEndY do
          for x=0,n.rowWidth-1 do
            local xast = {kind="value", value=x, type=darkroom.type.int(32)}
            xast = darkroom.typedAST.new(xast):copyMetadataFrom(n)
            local xb = {kind="binop",lhs=n.x,rhs=xast,op="+",type=darkroom.type.int(32)}
            xb = darkroom.typedAST.new(xb):copyMetadataFrom(n)
            local yast = {kind="value", value=y, type=darkroom.type.int(32)}
            yast = darkroom.typedAST.new(yast):copyMetadataFrom(n)

            local g = {kind="gather",x=xb,y=yast,_input=n._input,type=n.type:baseType(),minX=n.columnStartX,maxX=n.columnEndX,minY=n.columnStartY,maxY=n.columnEndY}
            g = darkroom.typedAST.new(g):copyMetadataFrom(n)

            array["expr"..c] = g
            c = c + 1
          end
        end
        return darkroom.typedAST.new(array):copyMetadataFrom(n)
      elseif n.kind=="iterateload" then
        return n._expr
      end
    end)
  
  res:S(function(n) return n.kind=="iterate" or n.kind=="gatherColumn" or n.kind=="iterationvar" end):process(function(n) assert(false) end)

  return res
end

function darkroom.terracompiler.codegen(
  inkernel, V, xsymb, ysymb, loopid, stripCount, kernelNode, inputImages, outputs, taps, TapStruct, validLeft, validRight, debug)

  assert(type(loopid)=="number")
  assert(type(stripCount)=="number")
  assert(type(TapStruct)=="table")
  assert(type(debug)=="boolean")

  inkernel = eliminateIterate(inkernel)

  local stat = {}
  local statSeen = {} -- for debugging
  local mapreducevarSymbols = {}
  local codegenResult = {}

  local function addstat(bb,qte,contents)
    stat[bb] = stat[bb] or {}
    statSeen[bb] = statSeen[bb] or {}
    assert(statSeen[bb][qte] == nil);

    if contents~=nil then
      for k,v in pairs(contents) do
        for kk,vv in pairs(v) do
          assert(statSeen[bb][vv] == nil);
        end
      end
    end

    table.insert(stat[bb],qte)
    statSeen[bb][qte] = 1
  end

  local res = inkernel:visitEach(
    function(node,args)

      local inputs = {}
      local packedSymbol = {}
      for k,v in pairs(args) do 
        inputs[k] = args[k][1]
        packedSymbol[k] = args[k][2]
      end

      local finalOut = {}

      -- mapreduce mixes channels is weird ways, so codegen this separately
      if node.kind=="mapreduce" then

        local bb = node:calculateMinBB(inkernel)
        local exprbb = node.expr:calculateMinBB(inkernel)
        local statexprbb = stat[exprbb]
        if bb==exprbb then statexprbb={} end -- eg a mapreduce with a constant in the body

        if node.reduceop=="sum" or node.reduceop=="max" or node.reduceop=="min" then
          local finalOut = {}

          for c = 1, node.type:channels() do
            local sum = symbol(node.type:baseType():toTerraType(false,V), node.reduceop)

            local out
            if node.reduceop=="sum" then
              out = quote [statexprbb]; sum = sum + [inputs["expr"][c]] end
            elseif node.reduceop=="max" then
              out = quote [statexprbb]; sum = terralib.select([inputs["expr"][c]]>sum,[inputs["expr"][c]],sum) end
            elseif node.reduceop=="min" then
              out = quote [statexprbb]; sum = terralib.select([inputs["expr"][c]]<sum,[inputs["expr"][c]],sum) end
            end
          
            local i = 1
            while node["varname"..i] do
              assert(mapreducevarSymbols[node["varnode"..i]]~=nil)
              local low = codegenResult[node["varnode"..i].low][1]
              assert(terralib.isquote(low))
              local high = codegenResult[node["varnode"..i].high][1]
              assert(terralib.isquote(high))
              out = quote for [mapreducevarSymbols[node["varnode"..i]]]=low[0],high[0]+1 do out end end
              i = i + 1
            end

            addstat(bb, quote var [sum] = 0; out end,{statexprbb})
            table.insert(finalOut, sum)
          end

          local packedSymbol = symbol(node.type:baseType():toTerraType(false,V)[node.type:channels()],"pack")
          addstat(bb, quote var [packedSymbol] = array(finalOut) end)
          for c=0,node.type:channels()-1 do finalOut[c+1] = `[packedSymbol][c] end
          codegenResult[node] = finalOut
          return {finalOut, `[packedSymbol]}

        elseif node.reduceop=="argmin" or node.reduceop=="argmax" then
          -- special case: for argmin, the multiple channels are generated by the one loop
          -- (they are the mapreducevars that yield the smallest value)

          local set = symbol(bool, "set")
          local value = symbol(node.type:baseType():toTerraType(false,V), node.reduceop.."Value")
          local results = {}
          local declareResults = {}
          local assign = {}
          local assignSelect = {}

          local i = 1
          while node["varname"..i] do
            table.insert(results, symbol(vector(int,V)))
            table.insert(declareResults, quote var [results[#results]] end)
            table.insert(assign, quote [results[#results]] = [mapreducevarSymbols[node["varnode"..i]]] end)

            if node.reduceop=="argmin" then
              table.insert(assignSelect, quote [results[#results]] = terralib.select( [inputs["expr"][1]] < value, [mapreducevarSymbols[node["varnode"..i]]], [results[#results]]) end)
            else
              table.insert(assignSelect, quote [results[#results]] = terralib.select( [inputs["expr"][1]] > value, [mapreducevarSymbols[node["varnode"..i]]], [results[#results]]) end)
            end
            i = i + 1
          end

          local assignValue
          if node.reduceop=="argmin" then
            assignValue = quote value = terralib.select( [inputs["expr"][1]] < value, [inputs["expr"][1]], value) end
          else
            assignValue = quote value = terralib.select( [inputs["expr"][1]] > value, [inputs["expr"][1]], value) end
          end
          -- inner loop
          local out = quote [statexprbb]; 
            if set==false then
              assign; set=true; value = [inputs["expr"][1]];
            else
              assignSelect;
              assignValue;
            end
          end

          local i = 1
          while node["varname"..i] do
            out = quote for [mapreducevarSymbols[node["varnode"..i]]]=[codegenResult[node["varnode"..i].low][1]][0],[codegenResult[node["varnode"..i].high][1]][0]+1 do out end end
            i = i + 1
          end

          addstat(bb, quote var [set] = false; var [value] = 0; declareResults; out end,{statexprbb})
          local packedSymbol = symbol(node.type:baseType():toTerraType(false,V)[node.type:channels()],"pack")
          
          table.insert(results, value) -- last value returned is the extremal value
          addstat(bb, quote var [packedSymbol] = array(results) end)
          local finalOut = {}
          for c=0,node.type:channels()-1 do finalOut[c+1] = `[packedSymbol][c] end
          codegenResult[node] = finalOut
          return {finalOut, `[packedSymbol]}

        elseif node.reduceop=="none" then
          assert(node.varnode1.low.constLow==0)
          assert(node.varnode1.high.constLow>=0)
          local packedSymbol = symbol(node.type:baseType():toTerraType(false,V)[node.type:channels()],"pack")
          addstat(bb, quote 
                         var [packedSymbol]
                         for [mapreducevarSymbols[node.varnode1]]=[codegenResult[node.varnode1.low][1]][0],[codegenResult[node.varnode1.high][1]][0]+1 do 
                           [statexprbb]; 
                           [packedSymbol][
                             [mapreducevarSymbols[node.varnode1]]
                                         ] = [inputs.expr[1]];
                         end end,{statexprbb})
          local finalOut = {}
          for c=0,node.type:channels()-1 do finalOut[c+1] = `[packedSymbol][c] end
          codegenResult[node] = finalOut
          return {finalOut, `[packedSymbol]}
        else
          print(node.reduceop)
          assert(false)
        end

      elseif node.kind=="filter" then
        local finalOut = {}

        local bb = darkroom.typedAST._topbb -- always at the top by defn
        local exprbb = node.expr:calculateMinBB(inkernel)
        local condbb = node.cond:calculateMinBB(inkernel)

        local cond = `[inputs.cond[1]][0]
        for v=1,V-1 do cond = `[cond] or [inputs.cond[1]][v] end

        local decl = {}
        local res = {}
        local notFiltered = {}
        for c = 1, node.type:channels() do
          -- Shenanigans: cond is always a scalar boolean, but when we call :set
          -- on the image, we use this array. And if we are filtering multichannel
          -- stuff, the way that we dispatch functions expects this to 
          -- be an array of the same size.
          table.insert(notFiltered, inputs.cond[1])

          table.insert(finalOut, symbol(node.type:baseType():toTerraType(false,V),"filteredOut"))
          table.insert(decl,quote var [finalOut[c]] = 0 end)
          table.insert(res,quote [finalOut[c]] = [inputs.expr[c]]; end)
          finalOut[c] = `[finalOut[c]]
        end

        addstat(bb,quote
                  [decl]
                  [stat[condbb]];
                  if cond then
                    [stat[exprbb]];
                    [res]
                  end end,{stat[condbb],stat[exprbb]})
        
        local packedSymbol = symbol(node.type:baseType():toTerraType(false,V)[node.type:channels()],"pack")
        addstat(bb, quote var [packedSymbol] = array(finalOut) end)

        codegenResult[node] = finalOut
        return {finalOut, `[packedSymbol], notFiltered}
      end

      for c=1,node.type:channels() do
        local out
        local resultSymbol = darkroom.terracompiler.symbol(node.type:baseType(), false, V)

        if node.kind=="load" then
          assert(darkroom.kernelGraph.isKernelGraph(node.from) or type(node.from)=="number")

          local isGather = type(node.relX.constLow)~="number" or type(node.relX.constHigh)~="number"

          local x,y = `[inputs.relX[1]][0], `[inputs.relY[1]][0]
          if isGather then x,y = inputs.relX[1], inputs.relY[1] end
          out = inputImages[kernelNode][node.from][c]:get( loopid, isGather, x, y,  V, validLeft, validRight );
          
          if isGather and debug then
            out = quote
              for i = 0,V do
                if x[i] > [inputs.maxX[1]][0] or x[i] < [inputs.minX[1]][0] then
                  cstdio.printf("error, gathered outside of stencil X %d (min %d max %d)\n", x[i], [inputs.minX[1]][0], [inputs.maxX[1]][0])
                  orionAssert(false,"gathered outside of stencil")
                end
                
                if y[i] > [inputs.maxY[1]][0] or y[i] < [inputs.minY[1]][0] then
                  cstdio.printf("error, gathered outside of stencil Y %d (min %d max %d)\n", y[i], [inputs.minY[1]][0], [inputs.maxY[1]][0])
                  orionAssert(false,"gathered outside of stencil")
                end
              end
            in out end
          end
        elseif node.kind=="binop" then
          local lhs = inputs["lhs"][c]
          local rhs = inputs["rhs"][c]

          if node.op=="dot" then
            local i = 1
            while inputs["lhs"][i] do
              if out==nil then 
                out = `[inputs["lhs"][i]] * [inputs["rhs"][i]]
              else
                out = `out + [inputs["lhs"][i]] * [inputs["rhs"][i]]
              end
              i = i + 1
            end

          elseif node.lhs.type:baseType():isNumber() and node.rhs.type:baseType():isNumber() then
          
            if darkroom.terracompiler.numberBinops[node.op]==nil then
              darkroom.error("Unknown scalar op "..node.op)
            end
          
            out = darkroom.terracompiler.numberBinops[node.op](lhs,rhs,V)
          elseif node.lhs.type:baseType():isBool() and node.rhs.type:baseType():isBool() then
            
            if darkroom.terracompiler.boolBinops[node.op]==nil then
              darkroom.error("Unknown scalar bool op "..node.op)
            end
            
            out = darkroom.terracompiler.boolBinops[node.op](lhs,rhs)
          else
            print("Unknown/bad type to binop", node.lhs.type:toString(), node.rhs.type:toString() )
            os.exit()
          end
        elseif node.kind=="unary" then
          local expr = inputs["expr"][c]
          
          if node.op=="arrayAnd" then
            local i = 1
            while inputs["expr"][i] do
              if out==nil then 
                out = inputs["expr"][i]
              else
                out = `out and [inputs["expr"][i]]
              end
              i = i + 1
            end

          elseif darkroom.terracompiler.numberUnary[node.op]==nil then
            darkroom.error("Unknown unary op "..node.op)
          else
            out = darkroom.terracompiler.numberUnary[node.op]( expr, node.expr, V )
          end

        elseif node.kind=="value" then
          local val = node.value

          if type(node.value)=="table" then
            val = node.value[c]
          end

          out = `[node.type:baseType():toTerraType(false,V)](val)
        elseif node.kind=="tap" then
          -- kind of a cheap hack to save threading around some state
          local entry
          for k,v in pairs(TapStruct:getentries()) do if v.field==tostring(node.id) then entry=v.field end end
            out = `taps.[entry]
        elseif node.kind=="tapLUTLookup" then
          local index = inputs["index1"][1]
          
          local entry
          for k,v in pairs(TapStruct:getentries()) do if v.field==tostring(node.id) then entry=v.field end end

          local q = {}
          for i=1,V do
            table.insert(q, quote 
                           if index[i-1]<0 or index[i-1]>=[node.count] then
                             cstdio.printf("Error, read tap lut value out of range %d\n", index[i-1])
                             orionAssert(false, "read tap lut value out of range")
                           end
                           in (taps.[entry])[index[i-1]] end)
          end
          out = `vector(q)
        elseif node.kind=="cast" then

          if node.type:baseType():toTerraType()==nil then
            darkroom.error("Cast to "..darkroom.type.typeToString(node.type:baseType()).." not implemented!")
            assert(false)
          end

          local ttype = node.type:baseType():toTerraType(false, V)

          local expr
          if node.type:isArray() and node.expr.type:isArray()==false then
            expr = inputs["expr"][1] -- broadcast
          else
            expr = inputs["expr"][c]
          end

          out = `ttype(expr)

        elseif node.kind=="select" or node.kind=="vectorSelect" then
          local cond

          if node.kind=="vectorSelect" then
            cond = inputs["cond"][c]
          else
            cond = inputs["cond"][1]
          end

          local a = inputs["a"][c]
          local b = inputs["b"][c]

          out = `terralib.select(cond,a,b)
        elseif node.kind=="position" then

          if node.coord=="x" then
            -- xsymb is a scalar, we need to convert it to a vector
            --out = `xsymb
            local tmp = {}
            for i=0,V-1 do table.insert(tmp,`xsymb+i) end
            out = `vector(tmp)
          elseif node.coord=="y" then
            out = `ysymb
          elseif node.coord=="z" then
            assert(false)
          else
            assert(false)
          end

        elseif node.kind=="crop" then
          -- just pass through, crop only affects loop iteration space
          out = inputs["expr"][c]
        elseif node.kind=="array" then
          out = inputs["expr"..c][1]
        elseif node.kind=="index" then
          local idx = `[inputs.index1[1]][0]
          out = `[packedSymbol["expr"]][idx]
        elseif node.kind=="mapreducevar" then
          assert(mapreducevarSymbols[node.mapreduceNode]==nil)
          mapreducevarSymbols[node] = symbol(int,node.varname)
          
          out = `[mapreducevarSymbols[node]]
        elseif node.kind=="reduce" then
    
          local list = node:map("expr",function(v,i) return inputs["expr"..i][c] end)
          assert(#list == node:arraySize("expr"))

          -- theoretically, a tree reduction is faster than a linear reduction
          -- starti and endi are both inclusive
          local function foldt(list,starti,endi)
            assert(type(list)=="table")
            assert(type(starti)=="number")
            assert(type(endi)=="number")
            assert(starti<=endi)
            if starti==endi then
              return list[starti]
            else
              local half = math.floor((endi-starti)/2)
              local lhs = foldt(list,starti,starti+half)
              local rhs = foldt(list,starti+half+1,endi)

              if node.op=="sum" then
                return `(lhs+rhs)
              elseif node.op=="min" then
                return `terralib.select(lhs<rhs,lhs,rhs)
              elseif node.op=="max" then
               return `terralib.select(lhs>rhs,lhs,rhs)
              else
                assert(false)
              end
            end
          end

          out = foldt(list,1,#list)
        else
          darkroom.error("Internal error, unknown ast kind "..node.kind)
        end

        --print(node.kind)
        assert(terralib.isquote(out))

        -- make absolutely sure that we end up with the type we expect
        out = `[node.type:baseType():toTerraType(false,V)](out)

        -- only make a statement if necessary
        if false and node:parentCount(inkernel)==1 then
          finalOut[c] = out
        else
          local bb = node:calculateMinBB(inkernel)
          addstat(bb, quote var [resultSymbol] = out end )
          finalOut[c] = `[resultSymbol]
        end

      end

      -- if this is an array and we index into it, pack it into a terra array
      local packedSymbol = symbol(node.type:baseType():toTerraType(false,V)[node.type:channels()],"pack")
      local bb = node:calculateMinBB(inkernel)
      addstat(bb, quote var [packedSymbol] = array(finalOut) end )

      for c=0,node.type:channels()-1 do
        finalOut[c+1] = `[packedSymbol][c]
      end

      codegenResult[node] = finalOut
      return {finalOut, `[packedSymbol]}
    end)

  for k,v in ipairs(res[1]) do assert(terralib.isquote(res[1][k])) end
  
  if darkroom.printstage then
    print("terracompiler.codegen astNodes:",inkernel:S("*"):count()," statements:",#res[3],inkernel:name())
  end

  return res[1], stat[darkroom.typedAST._topbb], res[3]
end

-- user is expected to allocate an image that is padded to the (vector size)*(stripCount)
function stripWidth(options, scaleN1, scaleD1)
  assert(type(scaleN1)=="number")
  assert(type(scaleD1)=="number")
  local res = math.floor((upToNearest(options.V*options.stripcount,options.width) / options.stripcount) * ratioToScale(scaleN1,scaleD1))

  -- in the case of pyramids, the strip width for all scales must have V as a factor.
  -- This is hard in general, b/c you might have 3 as a scale factor for ex.
  assert(res % options.V == 0) -- strips had better have V as a factor, or sets will be unaligned
  return res
end


_neededCache = {}
_neededCache[true] = setmetatable({}, {__mode="k"})
_neededCache[false] = setmetatable({}, {__mode="k"})

-- Remember: Y is in terms of clocks, X is in terms of pixels. This is not necessarily the same thing!
-- Particularly when there are upsample/downsamples.
function neededStencil( interior, kernelGraph, kernelNode, largestScaleY, shifts)
  assert(type(interior)=="boolean");assert(type(kernelGraph)=="table");assert(type(kernelNode)=="table");assert(type(largestScaleY)=="number")
  -- assert(type(shifts)=="table"); can be nil for HW backend
  assert(kernelNode.kernel~=nil)

  if _neededCache[interior][kernelNode]==nil then
    local s = Stencil.new()

    for node,k in kernelNode:parents(kernelGraph) do
      local downsampleStrideX, upsampleStrideX
      local downsampleStrideY, upsampleStrideY

      if node.kernel~=nil then
        downsampleStrideX, upsampleStrideX = calculateStride(kernelNode.kernel.scaleN1, kernelNode.kernel.scaleD1, node.kernel.scaleN1, node.kernel.scaleD1)
        downsampleStrideY, upsampleStrideY = calculateStride(kernelNode.kernel.scaleN2, kernelNode.kernel.scaleD2, node.kernel.scaleN2, node.kernel.scaleD2)
      end
      
      local clockrate = looprate( kernelNode.kernel.scaleN2, kernelNode.kernel.scaleD2, largestScaleY )

      if node.kernel==nil then -- ie, kernelNode is an output
        if shifts==nil then
          s = s:unionWith(Stencil.new():add(0,0,0))
        else
          s = s:unionWith(Stencil.new():add(0,shifts[kernelNode],0))
        end
      elseif node.kernel.kind=="crop" and interior==false then -- on the interior of strips, crops have no effect
        s = s:unionWith(node.kernel:stencil(kernelNode, node.kernel):scale(1,clockrate,1):sum(Stencil.new():add(0,node.kernel.shiftY,0)))
      else
        s = s:unionWith(node.kernel:stencil(kernelNode, node.kernel):scale(1,clockrate,1):sum(neededStencil( interior, kernelGraph, node, largestScaleY, shifts):scale(downsampleStrideX,1,1):downToNearestY(upsampleStrideY)))
      end
    end

    assert(s:area()>0)
    _neededCache[interior][kernelNode] = s
  end
  
  return _neededCache[interior][kernelNode]
end

function validStencil(kernelGraph, kernelNode, largestScaleY, shifts)
  if kernelNode.kernel.kind=="crop" then
    return Stencil.new():add(0,kernelNode.kernel.shiftY,0)
  else
    return neededStencil( false, kernelGraph, kernelNode, largestScaleY, shifts)
  end
end

function stripLeft(strip, options, scaleN1, scaleD1) return `strip*[stripWidth(options, scaleN1, scaleD1)] end
function stripRight(strip, options, scaleN1, scaleD1) return quote var w = (strip+1)*[stripWidth(options, scaleN1, scaleD1)]; var tw = [math.floor(options.width*ratioToScale(scaleN1,scaleD1))] in terralib.select(w>tw,tw,w) end end

-- return interiorValue or exteriorValue depending if this strip's edge is on the exterior of the region we're calculating or not
terra interiorSelectLeft(strip : int, interiorValue : int, exteriorValue : int)
  if strip==0 then return exteriorValue end
  return interiorValue
end

terra interiorSelectRight(strip : int, stripcount : int, interiorValue : int, exteriorValue : int)
  if strip==stripcount-1 then return exteriorValue end
  return interiorValue
end

function needed(kernelGraph, kernelNode, strip, shifts, largestScaleY, options)
  assert(type(kernelGraph)=="table");assert(type(kernelNode)=="table");assert(type(shifts)=="table");assert(type(largestScaleY)=="number");assert(type(options)=="table");

  return {left = `[stripLeft(strip,options,kernelNode.kernel.scaleN1,kernelNode.kernel.scaleD1)]+interiorSelectLeft(strip,[neededStencil( true, kernelGraph, kernelNode, largestScaleY, shifts):min(1)], [neededStencil( false, kernelGraph, kernelNode, largestScaleY, shifts):min(1)]),
          right = `[stripRight(strip,options,kernelNode.kernel.scaleN1,kernelNode.kernel.scaleD1)]+interiorSelectRight(strip,[options.stripcount],[neededStencil( true, kernelGraph, kernelNode, largestScaleY, shifts):max(1)],[neededStencil( false, kernelGraph, kernelNode, largestScaleY, shifts):max(1)]),
          top = `[neededStencil( false, kernelGraph, kernelNode, largestScaleY, shifts):max(2)]+[options.height*largestScaleY], 
          bottom = `[neededStencil( false, kernelGraph, kernelNode, largestScaleY, shifts):min(2)]}
end

function valid(kernelGraph, kernelNode, strip, shifts, largestScaleY, options)
  assert(type(kernelGraph)=="table");assert(type(kernelNode)=="table");assert(type(shifts)=="table");assert(type(largestScaleY)=="number");assert(type(options)=="table");

  local v = {left = `[stripLeft(strip,options,kernelNode.kernel.scaleN1,kernelNode.kernel.scaleD1)]+interiorSelectLeft(strip,[neededStencil( true, kernelGraph, kernelNode, largestScaleY, shifts):min(1)],[validStencil(kernelGraph, kernelNode, largestScaleY, shifts):min(1)]),
          right = `[stripRight(strip,options,kernelNode.kernel.scaleN1,kernelNode.kernel.scaleD1)]+interiorSelectRight(strip,[options.stripcount], [neededStencil( true, kernelGraph, kernelNode, largestScaleY, shifts):max(1)], [validStencil(kernelGraph, kernelNode, largestScaleY, shifts):max(1)]),
          top = `[options.height*largestScaleY]+[validStencil(kernelGraph, kernelNode, largestScaleY, shifts):max(2)],
          bottom = `[validStencil(kernelGraph, kernelNode, largestScaleY, shifts):min(2)]}

  -- valid should never be larger than needed
  local n = needed( kernelGraph, kernelNode, strip, shifts, largestScaleY, options )

  v.left = `terralib.select(v.left < n.left, n.left, v.left)
  v.right = `terralib.select(v.right > n.right, n.right, v.right)
  
  return v
end

-- validVector is always >= vector. We expand out the valid region to 
-- be vector aligned (and also expand the needed regions so that this works).
-- after overcomputing the valid region, we write of it with the boundary info
function vectorizeRegion(r, V) assert(type(V)=="number");return {left=`downToNearestTerra(V, r.left), right=`upToNearestTerra(V, r.right), top=r.top, bottom=r.bottom} end
function shiftRegion(r, shift) return {left=r.left, right=r.right, top=`r.top+shift, bottom = `r.bottom+shift} end

-- wrap the regions in symbols, to make code easier to read
function memo(name, t)
  local symbs ={}
  local stats ={}
  for k,v in pairs(t) do
    symbs[k] = symbol(int32,name..k)
    table.insert(stats, quote var [symbs[k]] = v end)
  end
  return symbs, stats
end

local function boundary(n)
  if n.kernel.type:baseType():isBool() then
    return `false
  else
    return `0
  end
end

-- codegen all the stuff in the inner loop
function darkroom.terracompiler.codegenInnerLoop(
    core, 
    strip, 
    kernelGraph,
    inputs,
    outputs,
    taps,
    TapStruct,
    clock,
    linebufferBase,
    shifts,
    largestScaleY,
    options)

  assert(type(shifts)=="table")
  assert(type(options)=="table")
  assert(type(largestScaleY)=="number")

  local x = symbol(int,"x")

  local loopStartCode = {}
  local loopCode = {}
  local loopid = 0

  kernelGraph:S("*"):traverse(
    function(n)
      if n==kernelGraph or n.isInputImage~=nil then -- root is just a list of inputs, inputs are fake
return
      end

      loopid = loopid + 1

      -- notice the subtle relationship that exists between the clock and the (x,y) coords that the user wants to render.
      -- As implemented here, x is not affected by shifts, but y is. Also, this implementation of
      -- line buffered pipelines has chosen to apply shifts to the kernel graph prior to 
      -- the code generator. I found this more principled than having shifts applied to values all over 
      -- the place in the code generator. As a result, the needed, valid, validVectorized regions
      -- are in terms of clock space. eg needed tells us what clock cycles the user is interested in. If they
      -- requested y=[0,n] of a function shifted by s, then neededY=[s,n+s]. As a result, this implementation
      -- closely resembles hardware that produces 1 line per clock. 

      local needed, neededStat = memo("needed",needed( kernelGraph, n, strip, shifts, largestScaleY, options )) -- clock space
      local neededImageSpace, neededImageSpaceStat = memo("neededImageSpace",shiftRegion( needed, -shifts[n] ))
      local valid, validStat = memo("valid",valid( kernelGraph, n, strip, shifts, largestScaleY, options ))
      local validVectorized, validVectorizedStat = memo("validVectorized",vectorizeRegion(valid, options.V)) -- always >= valid to the nearest vector width

      table.insert(loopStartCode,
        quote
          neededStat; neededImageSpaceStat; validStat; validVectorizedStat;
          -- we use image space needed region here b/c These are the actual pixel coords we will write to
          -- since all image accesses use relative coordinates, This doesn't cause problems
          [inputs[n]:declare( loopid, neededImageSpace.left, neededImageSpace.bottom, needed.bottom, core, strip, options, n.kernel.scaleN1, n.kernel.scaleD1, n.kernel.scaleN2, n.kernel.scaleD2, linebufferBase ) ];
          [outputs[n]:declare( loopid,neededImageSpace.left, neededImageSpace.bottom, needed.bottom, core, strip, options, n.kernel.scaleN1, n.kernel.scaleD1, n.kernel.scaleN2, n.kernel.scaleD2, linebufferBase ) ];
          
          if options.verbose then
            cstdio.printf("--- %s V %d cores %d core %d shift %d\n",[n.kernel:name()],options.V, options.cores, strip, [shifts[n]])
            cstdio.printf("looprate %d largest %d\n",[looprate(n.kernel.scaleN2,n.kernel.scaleD2,largestScaleY)], largestScaleY)
            cstdio.printf("valid l %d r %d t %d b %d\n",[valid.left],[valid.right],[valid.top],[valid.bottom])
            cstdio.printf("validVectorized l %d r %d t %d b %d\n",[validVectorized.left],[validVectorized.right],[validVectorized.top],[validVectorized.bottom])
            cstdio.printf("needed l %d r %d t %d b %d\n",[needed.left],[needed.right],[needed.top],[needed.bottom])
            cstdio.printf("needed image space l %d r %d t %d b %d\n",[neededImageSpace.left],[neededImageSpace.right],[neededImageSpace.top],[neededImageSpace.bottom])
          end
        end)

      local expr, statements, notFiltered = darkroom.terracompiler.codegen( n.kernel,  options.V, x, clock, loopid, options.stripcount, n, inputs, outputs, taps, TapStruct, valid.left, valid.right, options.debug)

      table.insert(loopCode,
        quote
          if clock >= [needed.bottom] and clock < [needed.top] and (fixedModulus(clock-needed.bottom,[looprate(n.kernel.scaleN2,n.kernel.scaleD2,largestScaleY)]) == 0 or [n.kernel.scaleD2]==0) then
            if clock < [valid.bottom] or clock >= [valid.top]  then

              -- top/bottom row(s) (all boundary)
              -- theoretically we could do some of this vectorized, but it shouldn't really matter

              for [x] = [needed.left], [needed.right] do
                [outputs[n]:set( loopid, boundary(n), 1 )];
                [outputs[n]:next( loopid, 1 )];
                [inputs[n]:next( loopid, 1 )];
              end
              [outputs[n]:nextLine( loopid, `[needed.right]-[needed.left])];
              [inputs[n]:nextLine( loopid, `[needed.right]-[needed.left])];
            else
              -- interior row(s), mixed boundary and calculated region

              [outputs[n]:next( loopid, `validVectorized.left-needed.left )];
              [inputs[n]:next( loopid, `validVectorized.left-needed.left )];

              for [x] = validVectorized.left, validVectorized.right, options.V do
                statements;
                [outputs[n]:set( loopid, expr, options.V, notFiltered )];
                [outputs[n]:next( loopid, options.V )];
                [inputs[n]:next( loopid, options.V )];
              end

              [outputs[n]:next( loopid, `-(validVectorized.right-needed.left) )];
              [inputs[n]:next( loopid, `-(validVectorized.right-needed.left) )];
              
              -- these need to happen out of order b/c we may _overwrite_ sections of the array that were written by the loop above
              for [x] = needed.left, valid.left do
                [outputs[n]:set( loopid, boundary(n), 1 )];
                [outputs[n]:next( loopid, 1 )];
                [inputs[n]:next( loopid, 1 )];
              end

              [outputs[n]:next( loopid, `valid.right-valid.left )];
              [inputs[n]:next( loopid, `valid.right-valid.left )];
              
              for [x] = valid.right, needed.right do
                [outputs[n]:set( loopid, boundary(n), 1 )];
                [outputs[n]:next( loopid, 1 )];
                [inputs[n]:next( loopid, 1 )];
              end
          
              [outputs[n]:nextLine( loopid, `needed.right-needed.left)];
              [inputs[n]:nextLine( loopid, `needed.right-needed.left)];
            end
          end
      end)
  end)

 return loopStartCode, loopCode
end

-- codegen all the code that runs per thread (and preamble)
function darkroom.terracompiler.codegenThread(
    kernelGraph, 
    input,
    inputArgs,
    inputWrappers, 
    outputWrappers, 
    declareInputImages,
    declareOutputImages,
    demarshalCount,
    linebufferSize,
    TapStruct, 
    shifts, 
    largestScaleY, 
    options)

  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(shifts)=="table")
  assert(type(largestScaleY)=="number")
  assert(type(options)=="table")

  local core = symbol(int, "core")
  local clock = symbol(int, "clock")

  local loopCode = {}
  assert(options.stripcount % options.cores == 0)
  local stripsPerCore = math.floor(options.stripcount/options.cores)

  ------
  local strip = symbol(int,"strip")
  local taps = symbol(&TapStruct,"taps")

  local linebufferBase = symbol(&opaque,"linebufferBase")
  local thisLoopStartCode, thisLoopCode = darkroom.terracompiler.codegenInnerLoop(
    core,
    strip,
    kernelGraph,
    inputWrappers,
    outputWrappers,
    taps,
    TapStruct,
    clock, 
    linebufferBase,
    shifts,
    largestScaleY,
    options)


  -- The multiple outputs might be shifted relative to each other.
  -- make sure we run enough clock cycles that we compute the whole image for the user
  local startClock=10000000
  local endClock=-100000
  kernelGraph:S("*"):traverse(
    function(n)
      if n.kernel~=nil then
        local scale = looprate( n.kernel.scaleN2, n.kernel.scaleD2, largestScaleY)
        local a = neededStencil(false, kernelGraph, n, largestScaleY, shifts):max(2)
        local b = neededStencil(false, kernelGraph, n, largestScaleY, shifts):min(2)
        if a > endClock then endClock=a end
        if b<startClock then startClock=b end
      end
    end)

  return terra( [input] ) : &opaque
    var [core] = @([&int](input))
    declareInputImages
    declareOutputImages

    var [taps] = [&TapStruct](inputArgs+demarshalCount)

    if options.verbose then cstdio.printf("Run Thread %d\n",core) end

    -- allocate line buffer for this thread
    var linebuffers : &opaque = vmIV.allocateCircular([linebufferSize])

    var start = darkroom.currentTimeInSeconds()
    for i=0,stripsPerCore do

      var [strip] = core*stripsPerCore+i

      var [linebufferBase] = linebuffers -- the image wrappers will mutate linebufferBase each time they allocate a buffer in the start code
      thisLoopStartCode

      for [clock] = startClock, [options.height*largestScaleY]+endClock do
        thisLoopCode
      end

    end
    var endt = darkroom.currentTimeInSeconds()
  end

end

function choosePadding( kernelGraph, imageWidth, imageHeight, smallestScaleX, smallestScaleY)
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")
  assert(type(smallestScaleX)=="number")
  assert(type(smallestScaleY)=="number")

  local maxStencil = kernelGraph:maxStencil()

  local padMinX = downToNearest(smallestScaleX,maxStencil:min(1))
  local padMaxX = upToNearest(smallestScaleX,maxStencil:max(1))
  local padMinY = downToNearest(smallestScaleY,maxStencil:min(2))
  local padMaxY = upToNearest(smallestScaleY,maxStencil:max(2))
  return imageWidth+padMaxX-padMinX, imageHeight+padMaxY-padMinY, padMinX, padMaxX, padMinY, padMaxY
end

function darkroom.terracompiler.allocateImageWrappers(
    kernelGraph, 
    inputImageSymbolMap, 
    outputImageSymbolMap, 
    outputImageMainThreadSymbolMap,
    shifts,
    largestScaleY,
    options)

  assert(darkroom.IR.isIR(kernelGraph))
  assert(type(shifts)=="table")
  assert(type(largestScaleY)=="number")
  assert(type(options)=="table")

  local inputs = {} -- kernelGraphNode->{input wrappers}
  local outputs = {} -- kernelGraphNode->output wrapper
  local linebufferSize = 0

  local function channelPointer(c,ptr,ty,width,height)
    -- always round the width up so that aligned stores work
    return `[&ty](ptr)+[upToNearest(options.V, width)*height]*c
  end

  local inputWrappers = {}

  local function parentIsOutput(node)
    for v,k in node:parents(kernelGraph) do if v==kernelGraph then return k end end
    return nil
  end

  local debugBounds
  if options.debugImages~=nil then
    local _,_, padMinX, padMaxX, padMinY, padMaxY = choosePadding( kernelGraph, options.width, options.height, 1, 1)
    debugBounds = {minX=padMinX, maxX=options.width+padMaxX, minY=padMinY, maxY=options.height+padMaxY}
  end

  kernelGraph:S("*"):traverse(
    function(n)
      inputs[n] = {}
      setmetatable(inputs[n],pointwiseDispatchMT)

      -- collect the inputs
      n:map("child", function(child,i) 
              inputs[n][child] = outputs[child]
                     end)

      if n.isInputImage~=nil then
        -- this node provides an input file
        if inputWrappers[n.isInputImage]==nil then
          inputWrappers[n.isInputImage] = {}
          local type = n.kernel.type
          -- we don't actually care about the alignment of inputs b/c we do unaligned loads
          for c = 1,type:channels() do inputWrappers[n.isInputImage][c] = newImageWrapper( channelPointer(c-1,inputImageSymbolMap[n.isInputImage], type:baseType():toTerraType(), options.width, options.height), type:baseType(), options.width, options.debug ,1,1,1,1, largestScaleY, false) end
          setmetatable(inputWrappers[n.isInputImage],pointwiseDispatchMT)
        end
        outputs[n] = inputWrappers[n.isInputImage]

      elseif n~=kernelGraph then -- root is just a list of outputs
        -- make the output
        if parentIsOutput(n)~=nil then
          outputs[n] = {}
          for c=1,n.kernel.type:channels() do 
            local W = imageSize(options.width,n.kernel.scaleN1,n.kernel.scaleD1)
            local H = imageSize(options.height,n.kernel.scaleN2,n.kernel.scaleD2)
            outputs[n][c] = newImageWrapper( 
              channelPointer(c-1,outputImageSymbolMap[parentIsOutput(n)], n.kernel.type:baseType():toTerraType(),W,H), 
              n.kernel.type:baseType(), upToNearest(options.V, W), options.debug, n.kernel.scaleN1, n.kernel.scaleD1, n.kernel.scaleN2, n.kernel.scaleD2, largestScaleY, n.kernel.kind=="filter",
              channelPointer(c-1,outputImageMainThreadSymbolMap[parentIsOutput(n)], n.kernel.type:baseType():toTerraType(),W,H)) 
          end
          setmetatable(outputs[n],pointwiseDispatchMT)
        else
          outputs[n] = {}

          for c = 1, n.kernel.type:channels() do
            local debugImagesStr
            if options.debugImages~=nil then 
              debugImagesStr = options.debugImages..n:name().."_"..c 
            end

            outputs[n][c] = newScaleLineBufferWrapper(
              n:bufferSize(kernelGraph), 
              n.kernel.type:baseType(),
              downToNearest(options.V, neededStencil( true, kernelGraph, n, largestScaleY, shifts):min(1)), -- use the more conservative stencil
              stripWidth(options, n.kernel.scaleN1, n.kernel.scaleD1),
              upToNearest(options.V, neededStencil(true,kernelGraph,n, largestScaleY,shifts ):max(1)),
              options.debug,
              n.kernel.scaleN1, 
              n.kernel.scaleD1,
              n.kernel.scaleN2, 
              n.kernel.scaleD2,
              largestScaleY,
              options.V,
              debugImagesStr,
              debugBounds)

            linebufferSize = linebufferSize + outputs[n][c]:allocateSize()
          end
          setmetatable(outputs[n], pointwiseDispatchMT)
        end
      end
    end)

  return inputs, outputs, linebufferSize
end

-- this should return a terra function that when called executes the pipeline
function darkroom.terracompiler.compile(
    kernelGraph, 
    inputImages,
    taps,
    shifts,
    largestScaleY,
    options)

  if darkroom.verbose then print("compile") end
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(inputImages)=="table")
  assert(type(taps)=="table")
  assert(type(largestScaleY)=="number")
  assert(type(options)=="table")

  -- make symbols for the input images
  local inputImageSymbolTable = {} -- symbols in order
  local marshalInputs = {}
  local marshalBytes = 4 -- first thing it contains is an int32 thread id
  local stripStorePtr = symbol(&&opaque)
  for _,v in ipairs(inputImages) do
    assert(v.kind=="crop")
    assert(v.expr.kind=="load")
    assert(type(v.expr.from)=="number")
    table.insert(inputImageSymbolTable, symbol(&opaque))
    table.insert(marshalInputs, quote @stripStorePtr = [inputImageSymbolTable[#inputImageSymbolTable]]; stripStorePtr = stripStorePtr + 1 end)
    marshalBytes = marshalBytes + terralib.sizeof(&opaque)
  end

  local outputImageSymbolTable = {}
  local outputImageMainThreadSymbolMap = {}
  local marshalOutputs = {}
  kernelGraph:map("child",function(v,k)
    table.insert(outputImageSymbolTable, symbol(&opaque,"basePtr")) 
    outputImageMainThreadSymbolMap["child"..k] = outputImageSymbolTable[#outputImageSymbolTable]
    table.insert(marshalOutputs, quote @stripStorePtr = [outputImageSymbolTable[#outputImageSymbolTable]]; stripStorePtr = stripStorePtr + 1 end)
    marshalBytes = marshalBytes + terralib.sizeof(&opaque)
  end)

  local TapStruct = terralib.types.newstruct("tapstruct")
  TapStruct.metamethods.__getentries = function()
    local r = {}
    for k,v in pairs(taps) do 
      local t = v.type:toTerraType()
      table.insert(r, {field=tostring(v.id), type=t}) 
    end
    return r
  end
  marshalBytes = marshalBytes + terralib.sizeof(TapStruct)

  -- generate code to deserialize the input image array from the input argument
  local input = symbol(&opaque)
  local inputArgs = `[&&opaque](&([&int8](input)[4]))
  local inputImageSymbolMap = {}
  local declareInputImages = {}
  local demarshalCount = 0
  for k,v in pairs(inputImages) do
    inputImageSymbolMap[v.expr.from] = symbol(&(v.expr.type:toTerraType()),"inputImage")
    table.insert(declareInputImages,quote var [inputImageSymbolMap[v.expr.from]] = [&(v.expr.type:toTerraType())](inputArgs[demarshalCount]) end)
    demarshalCount = demarshalCount + 1
  end

  local outputImageSymbolMap = {}
  local declareOutputImages = {}
  kernelGraph:map("child",function(v,k)
    outputImageSymbolMap["child"..k] = symbol(&(v.kernel.type:toTerraType()),"outputImage")
    table.insert(declareOutputImages,quote var [outputImageSymbolMap["child"..k]] = [&(v.kernel.type:toTerraType())](inputArgs[demarshalCount]) end)
    demarshalCount = demarshalCount + 1
  end)

  -- add the input lists and output to the kernelGraph
  local inputWrappers, outputWrappers, linebufferSize = darkroom.terracompiler.allocateImageWrappers(kernelGraph, inputImageSymbolMap, outputImageSymbolMap, outputImageMainThreadSymbolMap, shifts, largestScaleY,options)

  local mainThreadDeclarations = {}
  kernelGraph:S("*"):traverse(
    function(n)
      if outputWrappers[n]~=nil then
        table.insert( mainThreadDeclarations, outputWrappers[n]:declareMainThread()) 
      end
    end)

  local threadCode, outputs = darkroom.terracompiler.codegenThread( 
    kernelGraph, 
    input, inputArgs,
    inputWrappers, outputWrappers, 
    declareInputImages,
    declareOutputImages,
    demarshalCount,
    linebufferSize,
    TapStruct, 
    shifts, 
    largestScaleY, 
    options )

--  threadCode:printpretty(false)

  local fin = terra([inputImageSymbolTable], [outputImageSymbolTable], tapsIn : &opaque)
    var start = darkroom.currentTimeInSeconds()
    
    var taps : &TapStruct = [&TapStruct](tapsIn)
    var threads : cpthread.pthread_t[options.cores]
    var stripStore : int8[options.cores*marshalBytes]
    
    for l=0, options.looptimes do
      -- launch the kernel for each strip
      [mainThreadDeclarations]

      if options.cores==1 then
        -- don't launch a thread to save thread launch overhead
        @([&int32](&stripStore)) = 0 -- core ID
        var [stripStorePtr] = [&&opaque](&stripStore[4])
        marshalInputs
        marshalOutputs
        @[&TapStruct](stripStorePtr) = @taps
        threadCode(&stripStore)
      else
        for i=0,options.cores do
          @([&int32](&stripStore[i*marshalBytes])) = i -- core ID
          var [stripStorePtr] = [&&opaque](&stripStore[i*marshalBytes+4])
          marshalInputs
          marshalOutputs
          @[&TapStruct](stripStorePtr) = @taps
          cpthread.pthread_create(&threads[i],nil,threadCode, &stripStore[i*marshalBytes]);
        end
        for i=0,options.cores do
          cpthread.pthread_join(threads[i],nil)
        end
      end
    end
    
    if options.printruntime then
      var len : double = (darkroom.currentTimeInSeconds()-start)/options.looptimes
      var bytes :double= 0
      var gbps :double= (bytes/len)/(1024*1024*1024)
      var gb :double= bytes / (1024*1024*1024)
      var lt : int = options.looptimes
      cstdio.printf("loopTimes:%d avgRuntime:%f GB/s:%f GB:%f\n",lt,len,gbps,gb)
    end
    
  end

  return fin
end