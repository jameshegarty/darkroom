cmath = terralib.includec("math.h")
cstdio = terralib.includec("stdio.h")
cassert = terralib.includec("assert.h")
cstdlib = terralib.includec("stdlib.h")
cpthread = terralib.includec("pthread.h")

orion.terracompiler = {}


function declareInputImages( inputs, kernelNode, loopid, x, y, clock, core, stripId, linebufferBase )
  assert(type(loopid)=="number")
  local res = {}
  for _,v in pairs(inputs[kernelNode]) do table.insert(res, v:declare( loopid, x, y, clock, core, stripId, linebufferBase )) end
  return res
end

function inputImagesNext( inputs, kernelNode, loopid, inc )
  assert(type(loopid)=="number")
  assert(type(inc)=="number" or terralib.isquote(inc))
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
 
  assert(bytes % (4*1024) == 0);
  assert((int)address % (4*1024) == 0);

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

function newLineBufferWrapper( lines, orionType, leftStencil, stripWidth, rightStencil, debug )
  assert(type(lines)=="number")
  assert(type(leftStencil)=="number")
  assert(type(stripWidth)=="number")
  assert(type(rightStencil)=="number")
  assert(type(debug)=="boolean")
  assert(orion.type.isType(orionType))

  local tab = {lines=lines, 
               id = linebufferCount, -- for debugging
               orionType=orionType, 
               leftStencil = leftStencil, 
               stripWidth = stripWidth, 
               rightStencil=rightStencil,
               linebufferPosition = 0,
               debug=debug,
               iv={}, 
               ivDebugX = {}, 
               ivDebugY={}, 
               ivDebugId = {}, 
               posX={}, posY={}}

  linebufferCount = linebufferCount + 1

  return setmetatable(tab,LineBufferWrapperMT)
end


function LineBufferWrapperFunctions:declare( loopid, x, y, clock, core, stripId, linebufferBase )
  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y)) -- we don't actually use this - we don't care what the actual coord is
  assert(terralib.isquote(clock) or terralib.issymbol(clock))
  assert(terralib.isquote(core) or terralib.issymbol(core))
  assert(terralib.issymbol(stripId))

  local res = {}

  if self.iv[loopid]==nil then self.iv[loopid] = symbol(&self.orionType:toTerraType(),"iv") end
    
  if self.debug then
    if self.ivDebugX[loopid]==nil then self.ivDebugX[loopid] = symbol(&int,"ivDebugX") end
    if self.ivDebugY[loopid]==nil then self.ivDebugY[loopid] = symbol(&int,"ivDebugY") end
    if self.ivDebugId[loopid]==nil then self.ivDebugId[loopid] = symbol(&int,"ivDebugId") end
    self.posX[loopid] = symbol(int,"posY")
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
        cstdio.printf("init iv %s loopid %d addr %d\n", [baseKey[k]], loopid, buf)
      end)

--    if self.debug then
--      table.insert(res, quote orionAssert(uint64(buf) % (4*sizeof([self.orionType:toTerraType()])) == 0, "initial IV not aligned") end)
--    end

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
  assert(terralib.isquote(value))
  assert(type(loopid)=="number")
  assert(type(V)=="number")

    local res = {}

    if self.debug then
      for i=0,V-1 do
        table.insert(res,quote
                       @([self.ivDebugX[loopid]]+i) = [self.posX[loopid]]+i
                       @([self.ivDebugY[loopid]]+i) = [self.posY[loopid]]
                       @([self.ivDebugId[loopid]]+i) = [self.id]

                       orionAssert(uint64([self.iv[loopid]]) % (V*sizeof([self.orionType:toTerraType()])) == 0,"lb set not aligned")
        end)
      end
    end

  table.insert(res,quote @[&vector(self.orionType:toTerraType(),V)]([self.iv[loopid]]) = value end)

  return quote res end
end

-- relX and relY should be integer constants relative to
-- the current location
function LineBufferWrapperFunctions:get(loopid, relX,relY, V, validLeft, validRight)
  assert(type(loopid)=="number")
  assert(type(relX)=="number" or terralib.isquote(relX))
  assert(type(relY)=="number" or terralib.isquote(relY))
  assert(type(V)=="number")

  local debugChecks = {}

  if self.debug then
    for i=0,V-1 do
      local lrelX = relX
      local lrelY = relY

      if terralib.isquote(relX) then
        lrelX = `relX[i]
        lrelY = `relY[i]
      end

      table.insert(debugChecks, 
                   quote 
                     -- because we expand out the valid region to the largest vector size, some of the area we compute is garbage, and
                     -- will read invalid values (but we will overwrite it so its ok). Bypass the debug checks on those regions.
                     if [self.posX[loopid]]+i+lrelX >= validLeft and [self.posX[loopid]]+i+lrelX < validRight then
                       orionAssert(@([self.ivDebugId[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.id], "incorrect LB Id")
                       orionAssert(@([self.ivDebugY[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.posY[loopid]]+lrelY, "incorrect LB Y") 
                       orionAssert(@([self.ivDebugX[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.posX[loopid]]+i+lrelX, "incorrect LB X")
                     end
      end)
    end
  end

  if terralib.isquote(relX) then -- gather
    local res = {}
    for i=0,V-1 do table.insert(res, `@([self.iv[loopid]] + relY[i]*[self:lineWidth()] + relX[i] + i) ) end
    return quote debugChecks in vectorof([self.orionType:toTerraType()], res) end
  else
    return quote debugChecks in terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.iv[loopid]] + relY*[self:lineWidth()]+ relX),{align=V}) end
  end
end

function LineBufferWrapperFunctions:next(loopid,v)
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
function LineBufferWrapperFunctions:nextLine(loopid,  sub)
  assert(terralib.isquote(sub))

  local res = {}

  local buf = {self.iv[loopid]}
  local base = {self.base}
  local bufType = {self.orionType:toTerraType()}

  if self.debug then
    buf = {self.iv[loopid], self.ivDebugX[loopid], self.ivDebugY[loopid], self.ivDebugId[loopid]}
    base = {self.base, self.baseDebugX, self.baseDebugY, self.baseDebugId}
    bufType = {self.orionType:toTerraType(),int,int,int}

    table.insert(res, quote [self.posY[loopid]] = [self.posY[loopid]] + 1 end)
    table.insert(res, quote [self.posX[loopid]] = [self.posX[loopid]] - sub end)
  end

  for k,v in pairs(buf) do
    table.insert(res, 
      quote 
        [buf[k]] = [buf[k]] - sub + [self:lineWidth()] 
        if [buf[k]] >= [base[k]]+[self:modularSize(bufType[k])*2] then [buf[k]] = [buf[k]] - [self:modularSize(bufType[k])] end
      end)
  end

  return quote res end
end

ImageWrapperFunctions = {}
ImageWrapperMT={__index=ImageWrapperFunctions}
function isImageWrapper(b) return getmetatable(b)==ImageWrapperMT end

-- tab.terraType should be the base type of the data stored in this image
-- ie, if it's a floating point image, tab.terraType should be float
function newImageWrapper( basePtr, orionType, stride, debug )
  assert(orion.type.isType(orionType))
  assert(type(stride)=="number")
  assert(type(debug) == "boolean")

  local tab = {data={},basePtr=basePtr,orionType=orionType, stride=stride, debug=debug}

  return setmetatable(tab,ImageWrapperMT)
end

function ImageWrapperFunctions:declare( loopid, x, y, clock, core, stripId )
  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y))
  assert(terralib.isquote(clock) or terralib.issymbol(clock)) -- not used
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

  return quote res end
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
    return `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]] + relY*[self.stride] + relX),{align=V})
  else
    return `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]] + relY*[self.stride] + relX),{align=V})
  end

end

function ImageWrapperFunctions:next(loopid,v)
  assert(type(loopid)=="number")
  assert(type(v)=="number" or terralib.isquote(v))
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
  inkernel, V, xsymb, ysymb, loopid, stripCount, kernelNode, inputImages, outputs, taps, TapStruct, validLeft, validRight)

  assert(type(loopid)=="number")
  assert(type(stripCount)=="number")
  assert(type(TapStruct)=="table")

  local stat = {}

  local expr =inkernel:visitEach(
    function(node,inputs)


      local out
      local resultSymbol = orion.terracompiler.symbol(node.type, false, V)

      for k,v in node:inputs() do
        assert(terralib.isquote(inputs[k]))
      end
      
      if node.kind=="load" then
        assert(orion.kernelGraph.isKernelGraph(node.from) or type(node.from)=="number")
        out = inputImages[kernelNode][node.from]:get(loopid, node.relX, node.relY,  V, validLeft, validRight);
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
        -- kind of a cheap hack to save threading around some state
        local entry
        for k,v in pairs(TapStruct:getentries()) do print(k,v,v.field);if v.field==tostring(node.id) then entry=v.field end end
        out = `taps.[entry]
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
      elseif node.kind=="gather" then
        local inpX = inputs["x"]
        local inpY = inputs["y"]

        assert(node.input.kind=="load")
        assert(orion.kernelGraph.isKernelGraph(node.input.from) or type(node.input.from)=="number")

        out = inputImages[kernelNode][node.input.from]:get(loopid, inpX, inpY,  V, validLeft, validRight);
      elseif node.kind=="crop" then
        -- just pass through, crop only affects loop iteration space
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


_neededCache = {}
_neededCache[true] = setmetatable({}, {__mode="k"})
_neededCache[false] = setmetatable({}, {__mode="k"})

function neededStencil( interior, kernelGraph, kernelNode, shifts)
  assert(type(interior)=="boolean");assert(type(kernelGraph)=="table");assert(type(kernelNode)=="table");assert(type(shifts)=="table");

  if _neededCache[interior][kernelNode]==nil then
    local s = Stencil.new()
      
    for node,k in kernelNode:parents(kernelGraph) do
      if node.kernel==nil then -- ie, kernelNode is an output
        s = s:unionWith(Stencil.new():add(0,shifts[kernelNode],0))
      elseif node.kernel.kind=="crop" and interior==false then -- on the interior of strips, crops have no effect
        s = s:unionWith(node.kernel:stencil(kernelNode):product(Stencil.new():add(0,node.kernel.shiftY,0)))
      else
        print("stencil of",node.kernel:name(),"onto",kernelNode.kernel:name(),node.kernel:stencil(kernelNode):min(2), "max",node.kernel:stencil(kernelNode):max(2))
        s = s:unionWith(node.kernel:stencil(kernelNode):product(neededStencil( interior,kernelGraph,node, shifts)))
      end
    end
    
    _neededCache[interior][kernelNode] = s
  end
  
  return _neededCache[interior][kernelNode]
end

function validStencil(kernelGraph, kernelNode, shifts)
  if kernelNode.kernel.kind=="crop" then
    return Stencil.new():add(0,kernelNode.kernel.shiftY,0)
  else
    return neededStencil( false, kernelGraph, kernelNode, shifts)
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

function needed(kernelGraph, kernelNode, strip, shifts, options)
  assert(type(kernelGraph)=="table");assert(type(kernelNode)=="table");assert(type(shifts)=="table");assert(type(options)=="table");

  return {left = `[stripLeft(strip,options)]+interiorSelectLeft(strip,[neededStencil( true, kernelGraph, kernelNode, shifts):min(1)], [neededStencil( false, kernelGraph, kernelNode, shifts):min(1)]),
          right = `[stripRight(strip,options)]+interiorSelectRight(strip,[options.stripcount],[neededStencil( true, kernelGraph, kernelNode, shifts):max(1)],[neededStencil( false, kernelGraph, kernelNode, shifts):max(1)]),
          top = `options.height+[neededStencil( false, kernelGraph, kernelNode, shifts):max(2)],
          bottom = `[neededStencil( false, kernelGraph, kernelNode, shifts):min(2)]}
end

function valid(kernelGraph, kernelNode, strip, shifts, options)
  local v = {left = `[stripLeft(strip,options)]+interiorSelectLeft(strip,[neededStencil( true, kernelGraph, kernelNode, shifts):min(1)],[validStencil(kernelGraph, kernelNode, shifts):min(1)]),
          right = `[stripRight(strip,options)]+interiorSelectRight(strip,[options.stripcount], [neededStencil( true, kernelGraph, kernelNode, shifts):max(1)], [validStencil(kernelGraph, kernelNode, shifts):max(1)]),
          top = `options.height+[validStencil(kernelGraph, kernelNode, shifts):max(2)],
          bottom = `[validStencil(kernelGraph, kernelNode, shifts):min(2)]}

  -- valid should never be larger than needed
  local n = needed( kernelGraph, kernelNode, strip, shifts, options )

  v.left = `terralib.select(v.left < n.left, n.left, v.left)
  v.right = `terralib.select(v.right > n.right, n.right, v.right)
  
  return v
end

-- validVector is always >= vector. We expand out the valid region to 
-- be vector aligned (and also expand the needed regions so that this works).
-- after overcomputing the valid region, we write of it with the boundary info
function vectorizeRegion(r, V) assert(type(V)=="number");return {left=`downToNearestTerra(V, r.left), right=`upToNearestTerra(V, r.right), top=r.top, bottom=r.bottom} end
function shiftRegion(r, shift) return {left=r.left, right=r.right, top=`r.top+shift, bottom = `r.bottom+shift} end

-- codegen all the stuff in the inner loop
function orion.terracompiler.codegenInnerLoop(
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
    options)

  assert(type(shifts)=="table")
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

      -- notice the subtle relationship that exists between the clock and the (x,y) coords that the user wants to render.
      -- As implemented here, x is not affected by shifts, but y is. Also, this implementation of
      -- line buffered pipelines has chosen to apply shifts to the kernel graph prior to 
      -- the code generator. I found this more principled than having shifts applied to values all over 
      -- the place in the code generator. As a result, the needed, valid, validVectorized regions
      -- are in terms of clock space. eg needed tells us what clock cycles the user is interested in. If they
      -- requested y=[0,n] of a function shifted by s, then neededY=[s,n+s]. As a result, this implementation
      -- closely resembles hardware that produces 1 line per clock. 

      local needed = needed( kernelGraph, n, strip, shifts, options ) -- clock space
      local neededImageSpace = shiftRegion( needed, -shifts[n] )
      local valid = valid( kernelGraph, n, strip, shifts, options )
      local validVectorized = vectorizeRegion(valid, options.V) -- always >= valid to the nearest vector width

      table.insert(loopStartCode,
        quote
          -- we use image space needed region here b/c These are the actual pixel coords we will write to
          -- since all image accesses use relative coordinates, This doesn't cause problems
          [declareInputImages( inputs, n, loopid, neededImageSpace.left, neededImageSpace.bottom, needed.bottom, core, strip, linebufferBase)];
          [outputs[n]:declare(loopid, neededImageSpace.left, neededImageSpace.bottom, needed.bottom, core, strip, linebufferBase )];
          
          if orion.verbose then
            cstdio.printf("--- %s V %d cores %d core %d shift %d\n",[n.kernel:name()],options.V, options.cores, strip, [shifts[n]])
            cstdio.printf("valid l %d r %d t %d b %d\n",[valid.left],[valid.right],[valid.top],[valid.bottom])
            cstdio.printf("validVectorized l %d r %d t %d b %d\n",[validVectorized.left],[validVectorized.right],[validVectorized.top],[validVectorized.bottom])
            cstdio.printf("needed l %d r %d t %d b %d\n",[needed.left],[needed.right],[needed.top],[needed.bottom])
            cstdio.printf("needed image space l %d r %d t %d b %d\n",[neededImageSpace.left],[neededImageSpace.right],[neededImageSpace.top],[neededImageSpace.bottom])
          end
        end)

      local expr,statements=orion.terracompiler.codegen( n.kernel,  options.V, x, clock, loopid, options.stripcount, n, inputs, outputs, taps, TapStruct, valid.left, valid.right)

      table.insert(loopCode,
        quote
          cstdio.printf("dokernel %s\n",[n:name()])

          if clock >= [needed.bottom] and clock < [needed.top] then
            if clock < [valid.bottom] or clock >= [valid.top]  then
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

              cstdio.printf("vec region %d\n", validVectorized.left-needed.left)
              [outputs[n]:next( loopid, `validVectorized.left-needed.left )];
              [inputImagesNext( inputs, n, loopid, `validVectorized.left-needed.left )];

              for [x] = validVectorized.left, validVectorized.right, options.V do
                statements;
                [outputs[n]:set( loopid, expr, options.V )];
                [outputs[n]:next( loopid, options.V )];
                [inputImagesNext( inputs, n, loopid, options.V )];
              end

              [outputs[n]:next( loopid, `-(validVectorized.right-needed.left) )];
              [inputImagesNext( inputs, n, loopid, `-(validVectorized.right-needed.left) )];
              
              cstdio.printf("lb region %d %d\n",needed.left,valid.left)
              -- these need to happen out of order b/c we may _overwrite_ sections of the array that were written by the loop above
              for [x] = needed.left, valid.left do
                [outputs[n]:set( loopid, `0, 1 )];
                [outputs[n]:next( loopid, 1 )];
                [inputImagesNext( inputs, n, loopid, 1 )];
              end

              [outputs[n]:next( loopid, `valid.right-valid.left )];
              [inputImagesNext( inputs, n, loopid, `valid.right-valid.left )];
              
              cstdio.printf("lr region %d %d\n", valid.right, needed.right)
              for [x] = valid.right, needed.right do
                [outputs[n]:set( loopid, `0, 1 )];
                [outputs[n]:next( loopid, 1 )];
                [inputImagesNext( inputs, n, loopid, 1 )];
              end
          
              [outputs[n]:nextLine( loopid, `needed.right-needed.left)];
              [nextInputImagesLine( inputs, n, loopid, `needed.right-needed.left)];
            end
          else
            cstdio.printf("Skip\n")
          end
          
          cstdio.printf("kernel done %s\n",[n:name()])
      end)
  end)

 return loopStartCode, loopCode
end

-- codegen all the code that runs per thread (and preamble)
function orion.terracompiler.codegenThread(kernelGraph, inputs, TapStruct, shifts, options)
  assert(orion.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(inputs)=="table")
  assert(type(shifts)=="table")
  assert(type(options)=="table")

  local core = symbol(int)
  local clock = symbol(int)
  local input = symbol(&opaque)
  local inputArgs = `[&&opaque](&([&int8](input)[4]))

  -- generate code to deserialize the input image array from the input argument
  local inputImageSymbolMap = {}
  local declareInputImages = {}
  local demarshalCount = 0
  for k,v in pairs(inputs) do
    inputImageSymbolMap[v.expr.from] = symbol(&(v.expr.type:toTerraType()),"inputImage")
    table.insert(declareInputImages,quote var [inputImageSymbolMap[v.expr.from]] = [&(v.expr.type:toTerraType())](inputArgs[demarshalCount]) end)
    demarshalCount = demarshalCount + 1
  end

  local outputImageSymbolMap = {}
  local declareOutputImages = {}
  for k,v in kernelGraph:inputs() do     
    outputImageSymbolMap[k] = symbol(&(v.kernel.type:toTerraType()),"outputImage")
    table.insert(declareOutputImages,quote var [outputImageSymbolMap[k]] = [&(v.kernel.type:toTerraType())](inputArgs[demarshalCount]) end)
    demarshalCount = demarshalCount + 1
  end

  -- add the input lists and output to the kernelGraph
  local inputs, outputs, linebufferSize = orion.terracompiler.allocateImageWrappers(kernelGraph, inputImageSymbolMap, outputImageSymbolMap, shifts, options)

  local loopCode = {}
  assert(options.stripcount % options.cores == 0)
  local stripsPerCore = math.floor(options.stripcount/options.cores)

  ------
  local strip = symbol(int,"strip")
  local taps = symbol(&TapStruct)

  local linebufferBase = symbol(&opaque,"linebufferBase")
  local thisLoopStartCode, thisLoopCode = orion.terracompiler.codegenInnerLoop(
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
    options)

  if options.printstage then
    print("strip list count",#stripList)
  end

  -- The multiple outputs might be shifted relative to each other.
  -- make sure we run enough clock cycles that we compute the whole image for the user
  local startClock=10000000
  local endClock=-100000
  kernelGraph:S("*"):traverse(
    function(n)
      if n.kernel~=nil then
        local a = neededStencil(false, kernelGraph,n, shifts):max(2)
        local b = neededStencil(false, kernelGraph,n, shifts):min(2)
        if a > endClock then endClock=a end
        if b<startClock then startClock=b end
      end
    end)

  return terra( [input] ) : &opaque
    var [core] = @([&int](input))
    declareInputImages
    declareOutputImages

    var [taps] : &TapStruct = [&TapStruct](inputArgs[demarshalCount])

    if options.verbose then cstdio.printf("Run Thread %d\n",core) end

    -- allocate line buffer for this thread
    var linebuffers : &opaque = vmIV.allocateCircular([linebufferSize])

    var start = orion.currentTimeInSeconds()
    for i=0,stripsPerCore do

      var [strip] = core*stripsPerCore+i

      var [linebufferBase] = linebuffers -- the image wrappers will mutate linebufferBase each time they allocate a buffer in the start code
      thisLoopStartCode

      for [clock] = startClock, options.height+endClock do
        cstdio.printf("doclock %d\n",clock)
        thisLoopCode
      end

    end
    var endt = orion.currentTimeInSeconds()
  end

end


function orion.terracompiler.allocateImageWrappers(
    kernelGraph, 
    inputImageSymbolMap, 
    outputImageSymbolMap, 
    shifts,
    options)

  assert(orion.IR.isIR(kernelGraph))
  assert(type(shifts)=="table")
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


      if n~=kernelGraph then -- root is just a list of outputs

        -- if this uses any input files, we need to add those too
        n.kernel:S("load"):traverse(
          function(node)
            if type(node.from)=="number" then
              print("inputs","kernelNode",n,"from",node.from)
              inputs[n][node.from] = getInputWrapper(node.from, node.type)
            end
          end)

        -- make the output
        if parentIsOutput(n)~=nil then
          outputs[n] = newImageWrapper( outputImageSymbolMap[parentIsOutput(n)], n.kernel.type, options.width, options.terradebug)

          print("OUT imageWrapper","kernelNode:",n,n:name(),"wrapper",outputs[n])
        else
          outputs[n] = newLineBufferWrapper(
            n:bufferSize(kernelGraph), 
            n.kernel.type,
            downToNearest(options.V, neededStencil(true,kernelGraph,n, shifts):min(1)), -- use the more conservative stencil
            stripWidth(options),
            upToNearest(options.V, neededStencil(true,kernelGraph,n, shifts):max(1)),
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
    taps,
    shifts,
    options)

  if orion.verbose then print("compile") end
  assert(orion.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(inputImages)=="table")
  assert(type(taps)=="table")
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
  local marshalOutputs = {}
  for k,v in kernelGraph:inputs() do 
    table.insert(outputImageSymbolTable, symbol(&opaque)) 
    table.insert(marshalOutputs, quote @stripStorePtr = [outputImageSymbolTable[#outputImageSymbolTable]]; stripStorePtr = stripStorePtr + 1 end)
    marshalBytes = marshalBytes + terralib.sizeof(&opaque)
  end

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

  local threadCode = orion.terracompiler.codegenThread( kernelGraph, inputImages, TapStruct, shifts, options )

  threadCode:printpretty()

  local fin = terra([inputImageSymbolTable], [outputImageSymbolTable], tapsIn : &opaque)
    cstdio.printf("START DR\n")

    var start = orion.currentTimeInSeconds()
    
    var taps : &TapStruct = [&TapStruct](tapsIn)
    var threads : cpthread.pthread_t[options.cores]
    var stripStore : int8[options.cores*marshalBytes]
    
    for l=0, options.looptimes do
      -- launch the kernel for each strip
      
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
          cpthread.pthread_create(&threads[i],nil,threadCode, &stripStore[i*marshalBytes]);
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