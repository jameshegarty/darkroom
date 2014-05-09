cmath = terralib.includec("math.h")
cstdio = terralib.includec("stdio.h")
cassert = terralib.includec("assert.h")
cstdlib = terralib.includec("stdlib.h")
cpthread = terralib.includec("pthread.h")

orion.terracompiler = {}


function declareInputImages( inputs, kernelNode, loopid, x, y, core, stripId, linebufferBase )
  assert(type(loopid)=="number")
  local res = {}
  for _,v in pairs(inputs[kernelNode]) do table.insert(res, v:declare(loopid,x,y,core,stripId,linebufferBase)) end
  return res
end

function inputImagesNext( inputs, kernelNode, loopid, inc )
  assert(type(loopid)=="number")
  assert(type(inc)=="number")
  local res = {}
  for _,v in pairs(inputs[kernelNode]) do table.insert( res, v:next(loopid,inc) ) end
  return res
end

function nextInputImagesLine( inputs, kernelNode, loopid,  sub )
  assert(type(loopid)=="number")
  assert(terralib.isquote(sub))
  local res = {}
  for _,v in pairs(inputs[kernelNode]) do table.insert( res, v:nextLine( loopid, sub)) end
  return res
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
  
  assert(file_descriptor > 0);
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
  void * address = mmap (NULL, bytes << 1, PROT_NONE,
                         MAP_ANON | MAP_PRIVATE, -1, 0);
 
  assert(address != MAP_FAILED);
  return address;
                                   }
                                      ]]

LineBufferWrapperFunctions = {}
LineBufferWrapperMT={__index=LineBufferWrapperFunctions}

function newLineBufferWrapper( lines, orionType, leftStencil, stripWidth, rightStencil, debug )
  assert(type(lines)=="number")
  assert(type(leftStencil)=="number")
  assert(type(stripWidth)=="number")
  assert(type(rightStencil)=="number")
  assert(type(debug)=="boolean")
  assert(orion.type.isType(orionType))

  local tab = {lines=lines, 
               orionType=orionType, 
               leftStencil = leftStencil, 
               stripWidth = stripWidth, 
               rightStencil=rightStencil,
               linebufferPosition = 0,
               debug=debug,
               iv={},
               base={}}

  tab.ivDebugX = {}
  tab.ivDebugY = {}
  tab.ivDebugId = {}
  tab.posX = {}
  tab.posY = {}
  
  return setmetatable(tab,LineBufferWrapperMT)
end


function LineBufferWrapperFunctions:declare( loopid, x, y, core, stripId, linebufferBase )
  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y))
  assert(terralib.isquote(core) or terralib.issymbol(core))
  assert(terralib.issymbol(stripId))

  local res = {}

  if self.iv[loopid]==nil then self.iv[loopid] = symbol(&self.orionType:toTerraType()) end
  if self.base[loopid]==nil then self.base[loopid] = symbol(&self.orionType:toTerraType()) end
    
  if self.debug then
    if self.ivDebugX[loopid]==nil then self.ivDebugX[loopid] = symbol(&int) end
    if self.ivDebugY[loopid]==nil then self.ivDebugY[loopid] = symbol(&int) end
    if self.ivDebugId[loopid]==nil then self.ivDebugId[loopid] = symbol(&int) end
    self.posX[loopid] = symbol(int)
    self.posY[loopid] = symbol(int)
  end

  -- as a simplification, we make sure that everybody starts on the same line. Pipeline scheduling guarantees this will always work
  if self.startPosY==nil then self.startPosY=symbol(int); table.insert(res, quote var [self.startPosY] = y end) end
  table.insert(res,quote orionAssert(y==[self.startPosY], "every linebuffer must start on same line") end)

  if self.debug then table.insert(res, quote var [self.posX[loopid]] = x; var [self.posY[loopid]] = y; end) end

  local l = {self.iv[loopid]}
  local base = {self.base[loopid]}
  if self.debug then l = {self.iv[loopid], self.ivDebugX[loopid], self.ivDebugY[loopid], self.ivDebugId[loopid] } end

  for k,buf in pairs(l) do
    table.insert(res, 
      quote 
        vmIV.makeCircular(linebufferBase,[self:size()])
        -- we always start in the second segment of the circular buffer b/c we always read from smaller addresses
        var leftBound = [self.stripWidth]*stripId + [self.leftStencil]
        var [base[k]] = [&self.orionType:toTerraType()](linebufferBase)
        var [buf] = [&self.orionType:toTerraType()](linebufferBase)+(x-leftBound)+([self:size()]/(2*[self.orionType:sizeof()]))
        linebufferBase = [&int8](linebufferBase) + [self:size()]
      end)
  end

  return quote res end
end

function LineBufferWrapperFunctions:lineWidth() return self.stripWidth-self.leftStencil+self.rightStencil end
-- this is how many bytes this linebuffer will consume
-- we round up to the nearest page size (for the circular buffer that must be page aligned). The double for the double mapping in VM space
function LineBufferWrapperFunctions:size() return upToNearest(4*1024,self.lines*self:lineWidth()) end
-- debugging requires 3 extra buffers
function LineBufferWrapperFunctions:allocateSize() if self.debug then return self:size()*3*4 else return self:size()*3 end end


function LineBufferWrapperFunctions:set( loopid, value, V )
  assert(terralib.isquote(value))
  assert(type(loopid)=="number")
  assert(type(V)=="number")

    local res = {}

    if self.debug then
      for i=0,V-1 do
        table.insert(res,quote


--[=[
                       cstdio.printf("SET id:%d i:%d ex:%d ey:%d px:%d\n",
                                     self.id,
                                     i,
                                     [self.posX[loopid]]+i, [self.posY[loopid]],
                                     [self.dataDebugX[0][loopid]]+i)
                       ]=]

                       @([self.dataDebugX[0][loopid]]+i) = [self.posX[loopid]]+i
                       @([self.dataDebugY[0][loopid]]+i) = [self.posY[loopid]]
                       @([self.dataDebugId[0][loopid]]+i) = [self.id]

                       orionAssert(uint64([self.data[0][loopid]]) % (V*sizeof([self.orionType:toTerraType()])) == 0,"lb set not aligned")
        end)
      end
    end

    table.insert(res,quote @[&vector(self.orionType:toTerraType(),V)]([self.iv[loopid]]) = value end)

    return res
end

-- relX and relY should be integer constants relative to
-- the current location
function LineBufferWrapperFunctions:get(loopid, relX,relY, V)
  assert(type(loopid)=="number")
  assert(type(relX)=="number")
  assert(type(relY)=="number")
  assert(type(V)=="number")

  assert(relY<=0)

  if self.debug then
      for i=0,V-1 do
        table.insert(debugChecks,quote

--[=[
                       cstdio.printf("GET id:%d i:%d did:%d dx:%d dy:%d ex:%d ey:%d px:%d relx:%d rely:%d\n",
                                     self.id,
                                     i,
                                     @([self.dataDebugId[relY][loopid]]+i+relX),
                                     @([self.dataDebugX[relY][loopid]]+i+relX),
                                     @([self.dataDebugY[relY][loopid]]+i+relX), 
                                     [self.posX[loopid]]+i+relX, [self.posY[loopid]]+origRelY,
                                     ([self.dataDebugX[relY][loopid]]+i+relX),
                                    relX,
                                    relY)
                       cstdio.printf("RT self:%d c:%d origY:%d\n",self.retime,retime,origRelY)
                       cstdio.printf("LID %d\n",loopid)
                         ]=]
                       orionAssert(@([self.dataDebugId[relY][loopid]]+i+relX) == [self.id], "incorrect LB Id")
                       orionAssert(@([self.dataDebugX[relY][loopid]]+i+relX) == [self.posX[loopid]]+i+relX, "incorrect LB X")
                       orionAssert(@([self.dataDebugY[relY][loopid]]+i+relX) == [self.posY[loopid]]+origRelY, "incorrect LB Y")
        end)
      end
  else
    return `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.iv[loopid]] + relY*[self:lineWidth()]+ relX),{align=V})
  end
end

function LineBufferWrapperFunctions:gather(
    loopid, 
    clamp,
    relX,relY, 
    blX, blY, trX, trY,
    V,
    stripCount,
    retime)

  assert(type(loopid)=="number")
  assert(type(clamp)=="boolean")
  assert(terralib.isquote(relX))
  assert(terralib.isquote(relY))
  assert(type(blX)=="number")
  assert(type(blY)=="number")
  assert(type(trX)=="number")
  assert(type(trY)=="number")
  assert(type(V)=="number")
  assert(type(stripCount)=="number")
  assert(type(retime)=="number")

--  local origY = relY
--  if self.kind=="linebuffer" then
--    relY = `relY + [self.retime-retime]
--  end

  local debugChecks = {}

    if clamp then
      relX = `terralib.select( relX<blX, blX, relX )
      relX = `terralib.select( relX>trX, trX, relX )
      
      relY = `terralib.select( relY<blY, blY, relY )
      relY = `terralib.select( relY>trY, trY, relY )
    else
      for i=0,V-1 do
        table.insert(debugChecks, 
          quote 
            if relX[i] < blX or relX[i] >trX then
              cstdio.printf("Error, gathered outside of valid area! x=%d (remember, it's %d to %d due to CSE)\n",relX[i],blX,trX)
              cstdlib.exit(1)
            end 
            if relY[i] < blY or relY[i] > trY then
              cstdio.printf("Error, gathered outside of valid area! y=%d (remember, it's %d to %d due to CSE)\n",relY[i],blY,trY)
              cstdlib.exit(1)
            end 
          end)
      end
    end

    local resTable = {}
    local resTableDebugX = {}
    local resTableDebugY = {}
    local resTableDebugId = {}

    self.gatherPointerUsed[loopid] = true

    local res = {}
    for i=0,V-1 do
      local l = symbol(int)
      table.insert(res,
        quote
          var [l] = (relY[i] - [self.gatherPointerY[loopid]]) % self.lines
          if l<0 then l=l+self.lines end
        end)

      local addr = `[self.gatherPointer[loopid]] + l*[self:lineWidth(stripCount)]+ relX[i]+i
      local addrDebugX = `[self.gatherPointerDebugX[loopid]] + l*[self:lineWidth(stripCount)]+ relX[i]+i
      local addrDebugY = `[self.gatherPointerDebugY[loopid]] + l*[self:lineWidth(stripCount)]+ relX[i]+i
      local addrDebugId = `[self.gatherPointerDebugId[loopid]] + l*[self:lineWidth(stripCount)]+ relX[i]+i

--      addrTable[i+1] = addr
      resTable[i+1] = `@(addr)
      resTableDebugX[i+1] = `@(addrDebugX)
      resTableDebugY[i+1] = `@(addrDebugY)
      resTableDebugId[i+1] = `@(addrDebugId)

      if self.debug then
        table.insert(debugChecks,
          quote


--[=[
                         cstdio.printf("addr  %d gpy %d gpdx %d rely %d lw %d lines %d correct %d %d\n",
                                       addrDebugX, [self.gatherPointerY[loopid]], [self.gatherPointerDebugX[loopid]], 
                                       relY[i], [self:lineWidth(stripCount)],
                                      self.lines,
                                      DaddrDebugX,
                                      @DaddrDebugX)
                         ]=]

                       orionAssert([resTableDebugX[i+1]] == [self.posX[loopid]]+i+relX[i], "incorrect gather LB X")
                       orionAssert([resTableDebugY[i+1]] == [self.posY[loopid]]+relY[i], "incorrect gather LB Y")
                       orionAssert([resTableDebugId[i+1]] == [self.id], "incorrect gather LB Id")
      end)
      end
    end

    return quote [res];[debugChecks] in vectorof([self.orionType:toTerraType()],resTable) end


end

function LineBufferWrapperFunctions:next(loopid,v)
  assert(type(loopid)=="number")
  assert(type(v)=="number")
  
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
function LineBufferWrapperFunctions:nextLine(loopid,  sub)
  assert(terralib.isquote(sub))

  local res = {}

  table.insert(res, 
    quote 
      [self.iv[loopid]] = [self.iv[loopid]] - sub + [self:lineWidth()] 
      if [self.iv[loopid]] >= [self.base[loopid]]+[self:size()*2] then [self.iv[loopid]] = [self.iv[loopid]] - [self:size()] end
    end)

  if self.debug then
    table.insert(res, quote [self.dataDebugX[line][loopid]] = [tempDebugX[i]] end)
    table.insert(res, quote [self.dataDebugY[line][loopid]] = [tempDebugY[i]] end)
    table.insert(res, quote [self.dataDebugId[line][loopid]] = [tempDebugId[i]] end)
    table.insert(res, quote [self.posY[loopid]] = [self.posY[loopid]] + 1 end)
    table.insert(res, quote [self.posX[loopid]] = [self.posX[loopid]] - sub end)
  end

  return quote res end
end


ImageWrapperFunctions = {}
ImageWrapperMT={__index=ImageWrapperFunctions}

-- tab.terraType should be the base type of the data stored in this image
-- ie, if it's a floating point image, tab.terraType should be float
function newImageWrapper( basePtr, orionType, stride, debug )
  assert(orion.type.isType(orionType))
  assert(type(stride)=="number")
  assert(type(debug) == "boolean")

  local tab = {data={},basePtr=basePtr,orionType=orionType, stride=stride, debug=debug}

  return setmetatable(tab,ImageWrapperMT)
end

function ImageWrapperFunctions:declare( loopid, x, y, core, stripId )
  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y))
  assert(terralib.isquote(core) or terralib.issymbol(core))
  assert(terralib.isquote(stripId) or terralib.issymbol(stripId))

  if self.data[loopid]==nil then self.data[loopid] = symbol(&(self.orionType:toTerraType())) end
  return quote var [self.data[loopid]] =  [&self.orionType:toTerraType()]([self.basePtr] + y*[self.stride] + x) end
end

function ImageWrapperFunctions:set( loopid, value, V )
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


  table.insert(res,quote terralib.attrstore([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]]),value,{nontemporal=true}) end)

  return res
end

-- relX and relY should be integer constants relative to
-- the current location
function ImageWrapperFunctions:get(loopid, relX,relY, V)
  assert(type(loopid)=="number")
  assert(type(relX)=="number")
  assert(type(relY)=="number")
  assert(type(V)=="number")


  local debugChecks = {}

  if self.debug then
--    local regionWidth = self.region:growToNearestX(orion.tune.V):getWidth()
    table.insert(debugChecks,
                 quote
                 var this = [self.data[loopid]] + relY*[self.stride] + relX

                 if (this-[self.basePtr])<0 then cstdio.printf("this:%d start:%d self.data:%d relY:%d stride:%d relX%d\n",
                                                               this,[self.basePtr],[self.data[loopid]],relY,[self.stride],relX) 
                 end
                 orionAssert( (this-[self.basePtr])>=0,"read before start of array")
--                 orionAssert( (this-[self.basePtr])<maxv,"read beyond end of array")
  end)
  else
    return `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]] + relY*[self.stride] + relX),{align=V})
  end

end

function ImageWrapperFunctions:gather(
    loopid, 
    clamp,
    relX,relY, 
    blX, blY, trX, trY,
    V,
    stripCount,
    retime)

  assert(type(loopid)=="number")
  assert(type(clamp)=="boolean")
  assert(terralib.isquote(relX))
  assert(terralib.isquote(relY))
  assert(type(blX)=="number")
  assert(type(blY)=="number")
  assert(type(trX)=="number")
  assert(type(trY)=="number")
  assert(type(V)=="number")
  assert(type(stripCount)=="number")
  assert(type(retime)=="number")



  local debugChecks = {}


  if self.debug then
    local regionWidth = self.region:growToNearestX(orion.tune.V):getWidth()

    table.insert(debugChecks,
      quote
        var start =  [&self.terraType](orion.runtime.getRegister(self.register))
        var maxv = orion.runtime.registerSize
        
        for i=0,V do
          var this = [self.data[loopid]] + relY[i]*regionWidth + relX[i]
          
          if (this-start)<0 then cstdio.printf("this:%d start:%d self.data:%d relY:%d regionWidth:%d relX%d\n",
                                               this,start,[self.data[loopid]],relY[i],regionWidth,relX[i]) 
            cstdio.printf("reg:%d\n",self.register)
          end
          orionAssert( (this-start)>=0,"gather before start of array")
          orionAssert( (this-start)<maxv,"gather beyond end of array")
        end
      end)
  end


    local regionWidth

    if self.kind=="buffer" then
      regionWidth = self.region:growToNearestX(orion.tune.V):getWidth()
    else
      regionWidth = self.region:getWidth()
    end

    local resTable = {}

    for i=1,V do
      local im1=i-1
      resTable[i] = `@([self.data[loopid]] + relY[im1]*regionWidth + relX[im1]+im1)
    end

    return quote [debugChecks] in vectorof(self.terraType,resTable) end

end

function ImageWrapperFunctions:next(loopid,v)
  assert(type(loopid)=="number")
  assert(type(v)=="number")
  return quote [self.data[loopid]] = [self.data[loopid]] + v end
end

-- sub: this is the number of pixels we looped over since last calling nextline
-- basically: the number of times we called nextVector this row * the vector width
function ImageWrapperFunctions:nextLine(loopid, sub)
  assert(terralib.isquote(sub))
  return quote [self.data[loopid]] = [self.data[loopid]] + [self.stride] - sub end
end


-- this is the source for the terra compiler for orion

-- if pointer is true, generate a pointer instead of a value
function orion.terracompiler.symbol(_type,pointer,vectorN)
  assert(orion.type.isType(_type))
  assert(vectorN==nil or type(vectorN)=="number")
  
  return symbol(orion.type.toTerraType(_type,pointer,vectorN))
end

function orion.terracompiler.vectorizeBinaryPointwise(func,lhs,rhs,V)
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

orion.terracompiler.numberBinops={
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
  ["&"]=function(lhs,rhs) return `lhs and rhs end,
  ["<<"]=function(lhs,rhs) return `lhs<<rhs end,
  [">>"]=function(lhs,rhs) return `lhs>>rhs end,
  ["min"]=function(lhs,rhs) return `terralib.select(lhs<rhs,lhs,rhs) end,
  ["max"]=function(lhs,rhs) return `terralib.select(lhs>rhs,lhs,rhs) end,
  ["pow"]=function(lhs,rhs,V)
    return orion.terracompiler.vectorizeBinaryPointwise(cmath.pow,lhs,rhs,V)
  end
}


-- func should take a single scalar value, and return a single scalar value
function orion.terracompiler.vectorizeUnaryPointwise(func,expr,V)
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

orion.terracompiler.numberUnary={
  ["floor"] = function(expr,ast,V)
    return orion.terracompiler.vectorizeUnaryPointwise(cmath.floor,expr,V)
  end,
  ["-"] = function(expr,ast,V) return `-expr end,
  ["not"] = function(expr,ast,V) return `not expr end,
  ["abs"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(cmath.fabs,expr,V)
    elseif orion.type.isUint(ast.type) then
      -- a uint is always positive
      return expr
    elseif ast.type==orion.type.int(32) or 
      ast.type==orion.type.int(64) or 
      ast.type==orion.type.int(16) then
      return orion.terracompiler.vectorizeUnaryPointwise(cstdlib.abs,expr,V)
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["sin"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(cmath.sin,expr,V)
    else
      assert(false)
    end
  end,
  ["cos"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(  cmath.cos, expr, V )
    else
      assert(false)
    end
  end,
  ["exp"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(  cmath.exp, expr, V )
    else
      assert(false)
    end
  end,
  ["print"]= function(expr,ast,V)
    if V>1 then
      assert(false)
    else
      if node.expr.type==orion.type.float(32) then
        table.insert(stat,quote cstdio.printf("orion.printf:%f\n",expr) end)
      elseif node.expr.type==orion.type.uint(8) then
        table.insert(stat,quote cstdio.printf("orion.printd:%d\n",expr) end)
      else
        print(orion.type.typeToString(node.expr.type))
        assert(false)
      end
   end
  end
}

orion.terracompiler.boolBinops={
  ["and"]=function(lhs,rhs) return `lhs and rhs end,
  ["or"]=function(lhs,rhs) return `lhs or rhs end
}

function orion.terracompiler.codegen(
  inkernel, V, xsymb, ysymb, loopid, stripCount, kernelNode, inputImages, outputs)

  assert(type(loopid)=="number")
  assert(type(stripCount)=="number")

  local stat = {}

  local expr =inkernel:visitEach(
    function(node,inputs)


      local out
      local resultSymbol = orion.terracompiler.symbol(node.type, false, V)

      for k,v in node:inputs() do
        assert(terralib.isquote(inputs[k]))
      end
      
      if node.kind=="load" then
        assert(orion.kernelGraph.isKernelGraph(node.from))

        print("node",kernelNode)
        print("image/lb wrapper:",inputImages[node.from],"node.from",node.from,node.from:name(),"outputs",outputs,"inputs",inputImages)
        out = inputImages[kernelNode][node.from]:get(loopid, 0,0,  V);
      elseif node.kind=="input" then
        out = inputImages[kernelNode][node.id]:get(loopid,0,0,V)
      elseif node.kind=="binop" then
        local lhs = inputs["lhs"]
        local rhs = inputs["rhs"]
        
        if orion.type.astIsNumber(node.lhs) and orion.type.astIsNumber(node.rhs) then
          
          if orion.terracompiler.numberBinops[node.op]==nil then
            orion.error("Unknown scalar op "..node.op)
          end
          
          out = orion.terracompiler.numberBinops[node.op](lhs,rhs,V)
        elseif orion.type.astIsBool(node.lhs) and orion.type.astIsBool(node.rhs) then
          
          if orion.terracompiler.boolBinops[node.op]==nil then
            orion.error("Unknown scalar bool op "..node.op)
          end
          
          out = orion.terracompiler.boolBinops[node.op](lhs,rhs)
        else
          print("Unknown/bad type to binop")
          print(orion.type.typeToString(orion.getType(node.lhs)))
          print(orion.type.typeToString(orion.getType(node.rhs)))
          os.exit()
        end
      elseif node.kind=="multibinop" then

        if node.op=="dot" then
          local dotresult

          node:map("lhs",
                   function(n,i)
                     if dotresult==nil then
                       dotresult = `([inputs["lhs"..i]]*[inputs["rhs"..i]])
                     else
                       dotresult = `dotresult + ([inputs["lhs"..i]]*[inputs["rhs"..i]])
                     end
                   end)

          out = dotresult
       else
          assert(false)
       end
      elseif node.kind=="multiunary" then
 
        if node.op=="arrayAnd" then

          local cmpresult

          node:map("expr",
                   function(n,i)
                     if cmpresult==nil then
                       cmpresult = inputs["expr"..i]
                     else
                       cmpresult = `cmpresult and [inputs["expr"..i]]
                     end
                   end)

          out = cmpresult
        else
          assert(false)
        end
      elseif node.kind=="unary" then
        local expr = inputs["expr"]
        
        if orion.terracompiler.numberUnary[node.op]==nil then
          orion.error("Unknown unary op "..node.op)
        end

        out = orion.terracompiler.numberUnary[node.op](expr,node.expr,V)
      elseif node.kind=="value" then
        out = `[orion.type.toTerraType(node.type,false,V)](node.value)
      elseif node.kind=="tap" then
        out = `@[&orion.type.toTerraType(node.type)](orion.runtime.getTap(node.id))
      elseif node.kind=="tapLUTLookup" then
        local index = inputs["index"]

        local q = {}
        for i=1,V do
          local debugQ = quote end

          if orion.debug then
            debugQ = quote orionAssert(index[i-1]>=0,"LUT index <0");
              if index[i-1]>=[node.count] then
                cstdio.printf("i %d, c %d\n",index[i-1],[node.count]);
                orionAssert(false,"LUT index >= count");
                end end
          end

          table.insert(q,quote debugQ in [&orion.type.toTerraType(node.type)](orion.runtime.getTapLUT(node.id))[index[i-1]] end)
        end
        out = `vector(q)
      elseif node.kind=="cast" then

        local input = inputs["expr"]

        if node.type==orion.type.float(32) then
        elseif node.type==orion.type.float(64) then
        elseif node.type==orion.type.uint(8) then
        elseif node.type==orion.type.int(8) then
        elseif node.type==orion.type.int(32) then
        elseif node.type==orion.type.int(64) then
        elseif node.type==orion.type.uint(32) then
        elseif node.type==orion.type.int(16) then
        elseif node.type==orion.type.uint(16) then
        elseif node.type==orion.type.bool() then
        else
          orion.error("Cast to "..orion.type.typeToString(node.type).." not implemented!")
          assert(false)
        end

        local ttype = orion.type.toTerraType(node.type,false, V)

        local expr = inputs["expr"]
        out = `ttype(expr)

      elseif node.kind=="select" or node.kind=="vectorSelect" then
        local cond = inputs["cond"]
        local a = inputs["a"]
        local b = inputs["b"]

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

      elseif node.kind=="assert" then
        local expr = inputs["expr"]

        if orion.debug then
          local cond = inputs["cond"]
          local printval = inputs["printval"]
      
          if node.printval.type==orion.type.float(32) then
            for i = 1,V do
              table.insert(stat,quote if cond[i-1]==false then 
                               cstdio.printf("ASSERT FAILED, value %f line %d x:%d y:%d\n",printval[i-1],[node:linenumber()],xsymb,ysymb);
                             cstdlib.exit(1); 
                                      end end)
            end
          elseif node.printval.type==orion.type.int(32) then
            for i = 1,V do
              table.insert(stat,quote if cond[i-1]==false then 
                               cstdio.printf("ASSERT FAILED, value %d file %s line %d x:%d y:%d\n",printval[i-1],[node:filename()],[node:linenumber()],xsymb,ysymb);
                             cstdlib.exit(1); 
                                      end end)
            end
          else
            assert(false)
          end
        end

        out = expr
      elseif node.kind=="reduce" then
    
        local list = node:map("expr",function(v,i) return inputs["expr"..i] end)
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
      elseif node.kind=="gatherConcrete" then
        local inpX = inputs["x"]
        local inpY = inputs["y"]

        -- remember: the CSE algorithm transforms everything to be from hackBL to hackTR
        local blX = node.translate1_hackBL
        local blY = node.translate2_hackBL
        local trX = node.translate1_hackTR
        local trY = node.translate2_hackTR

        out = node.from:gather( 
          loopid,  node.clamp, 
          inpX, inpY, 
          blX, blY, trX, trY,
          V,
          stripCount,
          retime);
      elseif node.kind=="crop" then
        -- just pass through, crop only affects loop iteration space
        out = inputs["expr"]
      elseif node.kind=="transformBaked" then
        out = inputs["expr"]
      else
--        node:printpretty()
        orion.error("Internal error, unknown ast kind "..node.kind)
      end

      --print(node.kind)
      assert(terralib.isquote(out))

      -- make absolutely sure that we end up with the type we expect
      out = `[orion.type.toTerraType(node.type,false,V)](out)

      -- only make a statement if necessary
      if node:parentCount(inkernel)==1 then
        return out
      else
        table.insert(stat,quote var [resultSymbol] = out end)
        return `[resultSymbol]
      end
    end)

  assert(terralib.isquote(expr))
  
  if orion.printstage then
    print("terracompiler.codegen astNodes:",inkernel:S("*"):count()," statements:",#stat,inkernel:name())
  end

  return expr,stat
end

-- user is expected to allocate an image that is padded to the (vector size)*(stripCount)
function stripWidth(options)
  return upToNearest(options.V*options.stripcount,options.width) / options.stripcount
end

_stencilInteriorCache = setmetatable({}, {__mode="k"})
function neededInteriorStencil(kernelGraph, kernelNode)
  if _stencilInteriorCache[kernelNode]==nil then
    local s = Stencil.new():add(0,0,0)
    for k,node in kernelNode:parents(kernelGraph) do
      if node.kernel==nil then
        
      elseif node.kernel.kind=="crop" then
        s = s:unionWith(node:stencil(kernelNode))
      else
        s = s:unionWith(node:stencil(kernelNode):product(neededInteriorStencil(kernelGraph,node)))
      end
    end
    _stencilInteriorCache[kernelNode] = s
  end

  return _stencilInteriorCache[kernelNode]

end

_stencilExteriorCache = setmetatable({}, {__mode="k"})
function neededExteriorStencil(kernelGraph, kernelNode)
  if _stencilExteriorCache[kernelNode]==nil then
    local s = Stencil.new():add(0,0,0)
      for k,node in kernelNode:parents(kernelGraph) do
        if node.kernel~=nil then s = s:unionWith(node.kernel:stencil(kernelNode):product(neededInteriorStencil(kernelGraph,node))) end
    end
    _stencilExteriorCache[kernelNode] = s
  end

  return _stencilExteriorCache[kernelNode]
end

function validStencil(kernelGraph, kernelNode)
  if kernelNode.kernel.kind=="crop" then
    return Stencil.new():add(0,0,0)
  else
    return neededExteriorStencil(kernelGraph, kernelNode)
  end
end

function stripLeft(strip, options) return `strip*[stripWidth(options)] end
function stripRight(strip, options) return `(strip+1)*[stripWidth(options)] end

-- return interiorValue or exteriorValue depending if this strip's edge is on the exterior of the region we're calculating or not
terra interiorSelectLeft(strip : int, interiorValue : int, exteriorValue : int)
  if strip==0 then return exteriorValue end
  return interiorValue
end

terra interiorSelectRight(strip : int, stripcount : int, interiorValue : int, exteriorValue : int)
  if strip==stripcount-1 then return exteriorValue end
  return interiorValue
end

function needed(kernelGraph, kernelNode, strip, options)
  local lInterior = upToNearest(options.V, neededInteriorStencil(kernelGraph, kernelNode):min(1))
  local lExterior = upToNearest(options.V,neededExteriorStencil(kernelGraph, kernelNode):min(1))
  return {left = `[stripLeft(strip,options)]+interiorSelectLeft(strip,lInterior,lExterior),
          right = `[stripRight(strip,options)]+interiorSelectRight(strip,[options.stripcount],[upToNearest(options.V,neededInteriorStencil(kernelGraph, kernelNode):max(1))],[upToNearest(options.V,neededExteriorStencil(kernelGraph, kernelNode):max(1))]),
          top = `options.height+[neededExteriorStencil(kernelGraph, kernelNode):max(2)],
          bottom = `[neededExteriorStencil(kernelGraph, kernelNode):min(2)]}
end

function valid(kernelGraph, kernelNode, strip, options)
  return {left = `[stripLeft(strip,options)]+interiorSelectLeft(strip,0,[validStencil(kernelGraph, kernelNode):min(1)]),
          right = `[stripRight(strip,options)]+interiorSelectRight(strip,[options.stripcount],0,[validStencil(kernelGraph, kernelNode):max(1)]),
          top = `options.height+[validStencil(kernelGraph, kernelNode):max(2)],
          bottom = `[validStencil(kernelGraph, kernelNode):min(2)]}
end

-- validVector is always >= vector. We expand out the valid region to 
-- be vector aligned (and also expand the needed regions so that this works).
-- after overcomputing the valid region, we write of it with the boundary info
function validVector(kernelGraph, kernelNode, strip, options)
  return {left = `[stripLeft(strip,options)]+interiorSelectLeft(strip,0,[upToNearest(options.V,validStencil(kernelGraph, kernelNode):min(1))]),
          right = `[stripRight(strip,options)]+interiorSelectRight(strip,[options.stripcount],0,[upToNearest(options.V,validStencil(kernelGraph, kernelNode):max(1))]),
          top = `options.height+[validStencil(kernelGraph, kernelNode):max(2)],
          bottom = `[validStencil(kernelGraph, kernelNode):min(2)]}
end

-- codegen all the stuff in the inner loop
function orion.terracompiler.codegenInnerLoop(
    core, 
    strip, 
    kernelGraph,
    inputs,
    outputs,
    y,
    linebufferBase,
    options)

  assert(type(options)=="table")

  local x = symbol(int)

  local loopStartCode = {}
  local loopCode = {}
  local loopid = 0

  kernelGraph:S("*"):traverse(
    function(n)
      if n==kernelGraph then -- root is just a list of inputs
return
      end

      loopid = loopid + 1

      local needed = needed(kernelGraph,n,strip,options)
      local valid = valid(kernelGraph,n,strip,options)
      local validVectorized = validVector(kernelGraph,n,strip,options) -- always larger than valid to the nearest vector width

      table.insert(loopStartCode,
        quote
          [declareInputImages( inputs, n, loopid, needed.left, needed.bottom, core, strip, linebufferBase)];
          [outputs[n]:declare(loopid, needed.left, needed.bottom, core, strip, linebufferBase )];
          
          if orion.verbose then
            cstdio.printf("V %d cores %d\n",options.V,options.cores)
            cstdio.printf("valid l %d r %d t %d b %d\n",[valid.left],[valid.right],[valid.top],[valid.bottom])
            cstdio.printf("validVectorized l %d r %d t %d b %d\n",[validVectorized.left],[validVectorized.right],[validVectorized.top],[validVectorized.bottom])
            cstdio.printf("needed l %d r %d t %d b %d\n",[needed.left],[needed.right],[needed.top],[needed.bottom])
          end
        end)

      local expr,statements=orion.terracompiler.codegen( n.kernel,  options.V, x, y, loopid, options.stripcount, n, inputs, outputs)

      table.insert(loopCode,
        quote
          cstdio.printf("dokernel %s\n",[n:name()])

          if (y >= [needed.bottom] and y < [valid.bottom]) or (y >= valid.top and y < needed.top) then
                                                                 -- top/bottom row(s) (all boundary)
          -- theoretically we could do some of this vectorized, but it shouldn't really matter

            for [x] = [needed.left], [needed.right] do
              [outputs[n]:set( loopid, `0, 1 )];
              [outputs[n]:next( loopid, 1 )];
              [inputImagesNext( inputs, n, loopid, 1 )];
            end
            [outputs[n]:nextLine( loopid, `[needed.right]-[needed.left], stripCount)];
            [nextInputImagesLine( inputs, n, loopid, `[needed.right]-[needed.left], stripCount)];
          else
            -- interior row(s), mixed boundary and calculated region

            for [x] = validVectorized.left, validVectorized.right, options.V do
              statements;
              [outputs[n]:set( loopid, expr, options.V )];
              [outputs[n]:next( loopid, options.V )];
              [inputImagesNext( inputs, n, loopid, options.V )];
            end

            for [x] = needed.left, valid.left do
              [outputs[n]:set( loopid, `0, 1 )];
              [outputs[n]:next( loopid, 1 )];
              [inputImagesNext( inputs, n, loopid, 1 )];
            end

            for [x] = valid.right, needed.right do
              [outputs[n]:set( loopid, `0, 1 )];
              [outputs[n]:next( loopid, 1 )];
              [inputImagesNext( inputs, n, loopid, 1 )];
            end
          
            [outputs[n]:nextLine( loopid, `needed.right-needed.left)];
            [nextInputImagesLine( inputs, n, loopid, `needed.right-needed.left)];
          end

          cstdio.printf("kernel done %s\n",[n:name()])
      end)
  end)

 return loopStartCode, loopCode
end

-- codegen all the code that runs per thread (and preamble)
function orion.terracompiler.codegenThread(kernelGraph, inputs, options)
  assert(orion.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(inputs)=="table")
  assert(type(options)=="table")

  local core = symbol(int)
  local y = symbol(int)
  local input = symbol(&opaque)
  local inputArgs = `[&&opaque](&([&int8](input)[4]))

  -- generate code to deserialize the input image array from the input argument
  local inputImageSymbolMap = {}
  local declareInputImages = {}
  local demarshalCount = 0
  for k,v in pairs(inputs) do
    inputImageSymbolMap[v.expr.id] = symbol(&(v.expr.type:toTerraType()))
    table.insert(declareInputImages,quote var [inputImageSymbolMap[v.expr.id]] = [&(v.expr.type:toTerraType())](inputArgs[demarshalCount]) end)
    demarshalCount = demarshalCount + 1
  end

  local outputImageSymbolMap = {}
  local declareOutputImages = {}
  for k,v in kernelGraph:inputs() do     
    outputImageSymbolMap[k] = symbol(&(v.kernel.type:toTerraType()))
    table.insert(declareOutputImages,quote var [outputImageSymbolMap[k]] = [&(v.kernel.type:toTerraType())](inputArgs[demarshalCount]) end)
    demarshalCount = demarshalCount + 1
  end

  -- add the input lists and output to the kernelGraph
  local inputs, outputs, linebufferSize = orion.terracompiler.allocateImageWrappers(kernelGraph, inputImageSymbolMap, outputImageSymbolMap, options)

  local loopCode = {}
  assert(options.stripcount % options.cores == 0)
  local stripsPerCore = math.floor(options.stripcount/options.cores)

  ------
  local strip = symbol(int)

  local linebufferBase = symbol(&opaque,"linebufferBase")
  local thisLoopStartCode, thisLoopCode = orion.terracompiler.codegenInnerLoop(
    core,
    strip,
    kernelGraph,
    inputs,
    outputs,
    y, 
    linebufferBase,
    options)

  if options.printstage then
    print("strip list count",#stripList)
  end

  return terra( [input] ) : &opaque
    var [core] = @([&int](input))
    declareInputImages
    declareOutputImages

    if options.verbose then cstdio.printf("Run Thread %d\n",core) end

    -- allocate line buffer for this thread
    var linebuffers : &opaque = vmIV.allocateCircular([linebufferSize])

    var start = orion.currentTimeInSeconds()
    for i=0,stripsPerCore do

      var [strip] = core*stripsPerCore+i

      var [linebufferBase] = linebuffers -- the image wrappers will mutate linebufferBase each time they allocate a buffer in the start code
      thisLoopStartCode

      for [y] = 0, options.height do
        cstdio.printf("doline %d\n",y)
        thisLoopCode
      end

    end
    var endt = orion.currentTimeInSeconds()
  end

end


function orion.terracompiler.allocateImageWrappers(kernelGraph, inputImageSymbolMap, outputImageSymbolMap, options)
  assert(orion.IR.isIR(kernelGraph))
  assert(type(options)=="table")

  local inputs = {} -- kernelGraphNode->{input wrappers}
  local outputs = {} -- kernelGraphNode->output wrapper
  local linebufferSize = 0

  local inputWrappers = {}
  local function getInputWrapper(id,type)
    if inputWrappers[id]==nil then
      inputWrappers[id] = newImageWrapper( inputImageSymbolMap[id],type,options.width, options.terradebug )
    end
    return inputWrappers[id]
  end

  local function parentIsOutput(node)
    for v,k in node:parents(kernelGraph) do if v==kernelGraph then return k end end
    return nil
  end

  kernelGraph:S("*"):traverse(
    function(n)
      inputs[n] = {}

      -- collect the inputs
      n:map("child", function(child,i) 
              print("input","KernelNode",n,"from",child,"wrapper",outputs[child],"inputs",inputs)
              inputs[n][child] = outputs[child]
                     end)

      -- if this uses any input files, we need to add those too
      if n~=kernelGraph then -- root is just a list of outputs

        n.kernel:S("input"):traverse(
          function(node)
            print("inputs","kernelNode",n,"from",node.id)
            inputs[n][node.id] = getInputWrapper(node.id,node.type)
          end)

        -- make the output
        if parentIsOutput(n)~=nil then
          outputs[n] = newImageWrapper( outputImageSymbolMap[parentIsOutput(n)], n.kernel.type, options.width, options.terradebug)

          print("OUT imageWrapper","kernelNode:",n,n:name(),"wrapper",outputs[n])
        else
          outputs[n] = newLineBufferWrapper(
            n:bufferSize(kernelGraph), 
            n.kernel.type,
            neededInteriorStencil(kernelGraph,n):min(1),
            stripWidth(options),
            neededInteriorStencil(kernelGraph,n):max(1),
            options.terradebug)

          linebufferSize = linebufferSize + outputs[n]:allocateSize()
          print("OUT lbwrapper","kernelNode",n,n:name(),"wrapper",outputs[n],"outputs",outputs,"lines",n:bufferSize(kernelGraph))
        end
      end
    end)

  for k,v in pairs(inputs) do
    for kk,vv in pairs(v) do
      print(inputs,k,kk,vv)
    end
  end

  return inputs, outputs, linebufferSize
end

-- this should return a terra function that when called executes the pipeline
function orion.terracompiler.compile(
    kernelGraph, 
    inputImages,
    options)

  if orion.verbose then print("compile") end
  assert(orion.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(inputImages)=="table")
  assert(type(options)=="table")

  -- make symbols for the input images
  local inputImageSymbolTable = {} -- symbols in order
  local marshalInputs = {}
  local marshalBytes = 4 -- first thing it contains is an int32 thread id
  local stripStorePtr = symbol(&&opaque)
  for _,v in ipairs(inputImages) do
    assert(v.kind=="crop")
    assert(v.expr.kind=="input")
    assert(type(v.expr.id)=="number")
    table.insert(inputImageSymbolTable, symbol(&opaque))
    table.insert(marshalInputs, quote @stripStorePtr = [inputImageSymbolTable[#inputImageSymbolTable]]; stripStorePtr = stripStorePtr + 1 end)
    marshalBytes = marshalBytes + terralib.sizeof(&opaque)
  end

  local outputImageSymbolTable = {}
  local marshalOutputs = {}
  for k,v in kernelGraph:inputs() do 
    table.insert(outputImageSymbolTable, symbol(&opaque)) 
    table.insert(marshalOutputs, quote @stripStorePtr = [outputImageSymbolTable[#outputImageSymbolTable]]; stripStorePtr = stripStorePtr + 1 end)
    marshalBytes = marshalBytes + terralib.sizeof(&opaque)
  end

  -- schedule nodes to determine linebuffer size
  local schedule = schedule(kernelGraph)

  local threadCode = orion.terracompiler.codegenThread( kernelGraph, inputImages, options )

  threadCode:printpretty()

  local fin = terra([inputImageSymbolTable], [outputImageSymbolTable])
    var start = orion.currentTimeInSeconds()
    
    var threads : cpthread.pthread_t[options.cores]
    var stripStore : int8[options.cores*marshalBytes]
    
    for l=0, options.looptimes do
      -- launch the kernel for each strip
      
      if options.cores==1 then
        -- don't launch a thread to save thread launch overhead
        @([&int32](&stripStore)) = 0
        var [stripStorePtr] = [&&opaque](&stripStore[4])
        marshalInputs
        marshalOutputs
        threadCode(&stripStore)
      else
        for i=0,options.cores do
          @([&int32](stripStore[i*marshalBytes])) = 0
          var [stripStorePtr] = [&&opaque](&stripStore[i*marshalBytes])
          marshalInputs
          marshalOutputs
          cpthread.pthread_create(&threads[i],nil,threadCode, &stripStore);
        end
        for i=0,options.cores do
          cpthread.pthread_join(threads[i],nil)
        end
      end
    end
    
    if options.printruntime then
      var len : double = (orion.currentTimeInSeconds()-start)/options.looptimes
      var bytes :double= 0
      var gbps :double= (bytes/len)/(1024*1024*1024)
      var gb :double= bytes / (1024*1024*1024)
      var lt : int = options.looptimes
      cstdio.printf("loopTimes:%d avgRuntime:%f GB/s:%f GB:%f\n",lt,len,gbps,gb)
    end
    
  end
  fin:printpretty()
  return fin
end