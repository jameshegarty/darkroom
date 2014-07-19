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
  assert(darkroom.type.isType(orionType))

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
      end)

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
function LineBufferWrapperFunctions:get(loopid, gather, relX,relY, V, validLeft, validRight)
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
    res = `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.iv[loopid]] + relY*[self:lineWidth()]+ relX),{align=V})
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
      for [i]=0,[V-1] do
                     -- because we expand out the valid region to the largest vector size (to save having to codegen the non-vectorized dangling region), 
                     -- some of the area we compute is garbage, and thus
                     -- will read invalid values (but we will overwrite it later so its ok). Bypass the debug checks on those regions.
                     if [self.posX[loopid]]+i >= validLeft and [self.posX[loopid]]+i < validRight then
                       orionAssert(@([self.ivDebugId[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.id], "incorrect LB Id")
                       orionAssert(@([self.ivDebugY[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.posY[loopid]]+lrelY, "incorrect LB Y") 
                       orionAssert(@([self.ivDebugX[loopid]]+lrelY*[self:lineWidth()]+i+lrelX) == [self.posX[loopid]]+i+lrelX, "incorrect LB X")
                     end

      end
      in res end
  end

  return res
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
  assert(darkroom.type.isType(orionType))
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
function ImageWrapperFunctions:get(loopid, gather, relX, relY, V)
  assert(type(loopid)=="number")
  assert(type(gather)=="boolean")
  assert(type(relX)=="number" or terralib.isquote(relX))
  assert(type(relY)=="number" or terralib.isquote(relY))
  assert(type(V)=="number")

  local expr

  if gather then
    local res = {}
    for i=0,V-1 do table.insert(res, `@([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]] + relY[i]*[self.stride] + relX[i])) ) end
    expr = `vectorof([self.orionType:toTerraType()], res)
  else
    expr = `terralib.attrload([&vector(self.orionType:toTerraType(),V)]([self.data[loopid]] + relY*[self.stride] + relX),{align=V})
  end

  if self.debug then
    return quote
      var this = [self.data[loopid]] + relY*[self.stride] + relX
      
      if (this-[self.basePtr])<0 then cstdio.printf("this:%d start:%d self.data:%d relY:%d stride:%d relX%d\n",
                                                    this,[self.basePtr],[self.data[loopid]],relY,[self.stride],relX) 
      end
      orionAssert( (this-[self.basePtr])>=0,"read before start of array")
      in expr end
  end

  return expr
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
function darkroom.terracompiler.symbol(_type,pointer,vectorN)
  assert(darkroom.type.isType(_type))
  assert(vectorN==nil or type(vectorN)=="number")
  
  return symbol(darkroom.type.toTerraType(_type,pointer,vectorN))
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
  ["&"]=function(lhs,rhs) return `lhs and rhs end,
  ["<<"]=function(lhs,rhs) return `lhs<<rhs end,
  [">>"]=function(lhs,rhs) return `lhs>>rhs end,
  ["min"]=function(lhs,rhs) return `terralib.select(lhs<rhs,lhs,rhs) end,
  ["max"]=function(lhs,rhs) return `terralib.select(lhs>rhs,lhs,rhs) end,
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
  ["-"] = function(expr,ast,V) return `-expr end,
  ["not"] = function(expr,ast,V) return `not expr end,
  ["abs"] = function(expr,ast,V)
    if ast.type==darkroom.type.float(32) then
      return darkroom.terracompiler.vectorizeUnaryPointwise(cmath.fabs,expr,V)
    elseif darkroom.type.isUint(ast.type) then
      -- a uint is always positive
      return expr
    elseif ast.type==darkroom.type.int(32) or 
      ast.type==darkroom.type.int(64) or 
      ast.type==darkroom.type.int(16) then
      return darkroom.terracompiler.vectorizeUnaryPointwise(cstdlib.abs,expr,V)
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["sin"] = function(expr,ast,V)
    if ast.type==darkroom.type.float(32) then
      return darkroom.terracompiler.vectorizeUnaryPointwise(cmath.sin,expr,V)
    else
      assert(false)
    end
  end,
  ["cos"] = function(expr,ast,V)
    if ast.type==darkroom.type.float(32) then
      return darkroom.terracompiler.vectorizeUnaryPointwise(  cmath.cos, expr, V )
    else
      assert(false)
    end
  end,
  ["exp"] = function(expr,ast,V)
    if ast.type==darkroom.type.float(32) then
      return darkroom.terracompiler.vectorizeUnaryPointwise(  cmath.exp, expr, V )
    else
      assert(false)
    end
  end,
  ["print"]= function(expr,ast,V)
    if V>1 then
      assert(false)
    else
      if node.expr.type==darkroom.type.float(32) then
        table.insert(stat,quote cstdio.printf("darkroom.printf:%f\n",expr) end)
      elseif node.expr.type==darkroom.type.uint(8) then
        table.insert(stat,quote cstdio.printf("darkroom.printd:%d\n",expr) end)
      else
        print(darkroom.type.typeToString(node.expr.type))
        assert(false)
      end
   end
  end
}

darkroom.terracompiler.boolBinops={
  ["and"]=function(lhs,rhs) return `lhs and rhs end,
  ["or"]=function(lhs,rhs) return `lhs or rhs end
}

mapreducevarSymbols = {}
function darkroom.terracompiler.codegen(
  inkernel, V, xsymb, ysymb, loopid, stripCount, kernelNode, inputImages, outputs, taps, TapStruct, validLeft, validRight, debug)

  assert(type(loopid)=="number")
  assert(type(stripCount)=="number")
  assert(type(TapStruct)=="table")
  assert(type(debug)=="boolean")

  local res = inkernel:visitEach(
    function(node,args)

      local inputs = {}
      local packedSymbol = {}
      local stat = {}
      local statSeen = {} -- dedup the statements. It's possible a single node is refered to multiple times eg (a+a)
      for k,v in pairs(args) do 
        inputs[k] = args[k][1]
        packedSymbol[k] = args[k][2]
        for kk, vv in pairs(args[k][3]) do
          assert(terralib.isquote(vv))
          if statSeen[vv]==nil then table.insert(stat, vv); statSeen[vv]=1 end
        end
      end

      local finalOut = {}

      -- mapreduce mixes channels is weird ways, so codegen this separately
      if node.kind=="mapreduce" then

        local i = 1
        while node["varid"..i] do -- unused variables
          if mapreducevarSymbols[node["varid"..i]]==nil then
            mapreducevarSymbols[node["varid"..i]] = symbol(int)
          end
          i = i + 1
        end
        
        if node.reduceop=="sum" then

          local finalOut = {}
          local statOut = {}

          for c = 1, node.type:channels() do
            local sum = symbol(darkroom.type.toTerraType(node.type:baseType(),false,V), node.reduceop)

            local out = quote stat; sum = sum + [inputs["expr"][c]] end
          
            if c==node.type:channels() then
              for k,v in pairs(stat) do stat[k] = nil end -- no longer needed in top scope
            end
          
            local i = 1
            while node["varid"..i] do
              out = quote for [mapreducevarSymbols[node["varid"..i]]]=[node["varlow"..i]],[node["varhigh"..i]]+1 do out end end
              i = i + 1
            end

            table.insert(statOut, quote var [sum] = 0; out end)
            table.insert(finalOut, sum)
          end

          local packedSymbol = symbol(darkroom.type.toTerraType(node.type:baseType(),false,V)[node.type:channels()],"pack")
          table.insert(statOut, quote var [packedSymbol] = array(finalOut) end)
          for c=0,node.type:channels()-1 do finalOut[c+1] = `[packedSymbol][c] end
          return {finalOut, `[packedSymbol], statOut}

        elseif node.reduceop=="argmin" then
          -- special case: for argmin, the multiple channels are generated by the one loop
          -- (they are the mapreducevars that yield the smallest value)

          local set = symbol(bool, "set")
          local minValue = symbol(darkroom.type.toTerraType(node.type:baseType(),false,V), node.reduceop.."Value")
          local results = {}
          local declareResults = {}
          local assign = {}
          local assignSelect = {}

          local i = 1
          while node["varid"..i] do
            table.insert(results, symbol(vector(int,V)))
            table.insert(declareResults, quote var [results[#results]] end)
            table.insert(assign, quote [results[#results]] = [mapreducevarSymbols[node["varid"..i]]] end)
            table.insert(assignSelect, quote [results[#results]] = terralib.select( [inputs["expr"][1]] < minValue, [mapreducevarSymbols[node["varid"..i]]], [results[#results]]) end)
            i = i + 1
          end

          -- inner loop
          local out = quote stat; 
            if set==false then
              assign; set=true; minValue = [inputs["expr"][1]];
            else
              assignSelect;
              minValue = terralib.select( [inputs["expr"][1]] < minValue, [inputs["expr"][1]], minValue)
            end
          end

          local i = 1
          while node["varid"..i] do
            out = quote for [mapreducevarSymbols[node["varid"..i]]]=[node["varlow"..i]],[node["varhigh"..i]]+1 do out end end
            i = i + 1
          end

          local statOut = {quote var [set] = false; var [minValue] = 0; declareResults; out end}
          local packedSymbol = symbol(darkroom.type.toTerraType(node.type:baseType(),false,V)[node.type:channels()],"pack")
          
          table.insert(statOut, quote var [packedSymbol] = array(results) end)
          local finalOut = {}
          for c=0,node.type:channels()-1 do finalOut[c+1] = `[packedSymbol][c] end
          return {finalOut, `[packedSymbol], statOut}

        else
          assert(false)
        end
        
      end

      for c=1,node.type:channels() do
        local out
        local resultSymbol = darkroom.terracompiler.symbol(node.type:baseType(), false, V)

        if node.kind=="load" then
          local relX, relY
          if type(node.relX)=="number" then 
            relX = node.relX 
          else
            relX = node.relX:codegen()
          end

          if type(node.relY)=="number" then 
            relY = node.relY 
          else
            relY = node.relY:codegen()
          end

          assert(darkroom.kernelGraph.isKernelGraph(node.from) or type(node.from)=="number")
          out = inputImages[kernelNode][node.from][c]:get(loopid, false, relX, relY,  V, validLeft, validRight);
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
            out = darkroom.terracompiler.numberUnary[node.op](expr,node.expr,V)
          end

        elseif node.kind=="value" then
          local val = node.value

          if type(node.value)=="table" then
            val = node.value[c]
          end

          out = `[darkroom.type.toTerraType(node.type:baseType(),false,V)](val)
        elseif node.kind=="tap" then
          -- kind of a cheap hack to save threading around some state
          local entry
          for k,v in pairs(TapStruct:getentries()) do if v.field==tostring(node.id) then entry=v.field end end
            out = `taps.[entry]
        elseif node.kind=="tapLUTLookup" then
          local index = inputs["index"][1]
          
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

          local ttype = darkroom.type.toTerraType(node.type:baseType(),false, V)

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

        elseif node.kind=="assert" then
          local expr = inputs["expr"][c]

          if darkroom.debug then
            local cond = inputs["cond"][c]
            local printval = inputs["printval"][c]
      
            if node.printval.type==darkroom.type.float(32) then
              for i = 1,V do
              table.insert(stat,quote if cond[i-1]==false then 
                               cstdio.printf("ASSERT FAILED, value %f line %d x:%d y:%d\n",printval[i-1],[node:linenumber()],xsymb,ysymb);
                             cstdlib.exit(1); 
                                      end end)
              end
            elseif node.printval.type==darkroom.type.int(32) then
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
        elseif node.kind=="gather" then
          local inpX = inputs["x"][1] -- should be scalar
          local inpY = inputs["y"][1] -- should be scalar

          assert(node.input.kind=="load")
          assert(darkroom.kernelGraph.isKernelGraph(node.input.from) or type(node.input.from)=="number")

          local relX, relY
          if type(node.input.relX)=="number" then 
            relX = node.input.relX 
          else
            relX = node.input.relX:codegen()
          end
          
          if type(node.input.relY)=="number" then 
            relY = node.input.relY 
          else
            relY = node.input.relY:codegen()
          end

          out = `[inputImages[kernelNode][node.input.from][c]:get(loopid, true, `inpX+relX, `inpY+relY,  V, validLeft, validRight)]

          if debug then
            out = quote
              for i = 0,V do
                if inpX[i] > node.maxX or inpX[i] < -node.maxX then
                  cstdio.printf("error, gathered outside of stencil X %d (max %d)\n",inpX[i], node.maxX)
                  orionAssert(false,"gathered outside of stencil")
                end
                
                if inpY[i] > node.maxY or inpY[i] < -node.maxY then
                  cstdio.printf("error, gathered outside of stencil Y %d (max %d)\n",inpY[i], node.maxY)
                  orionAssert(false,"gathered outside of stencil")
                end
              end
            in out end
          end
        elseif node.kind=="crop" then
          -- just pass through, crop only affects loop iteration space
          out = inputs["expr"][c]
        elseif node.kind=="array" then
          out = inputs["expr"..c][1]
        elseif node.kind=="index" then
          if node.index:eval(1):area()==1 then
            out = inputs["expr"][node.index:eval(1):min(1)+1]
          else
            local cg = node.index:codegen()
            out = `[packedSymbol["expr"]][cg]
          end
        elseif node.kind=="mapreducevar" then
          if mapreducevarSymbols[node.id]==nil then
            mapreducevarSymbols[node.id] = symbol(int,node.variable)
          end
          out = `[mapreducevarSymbols[node.id]]
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
        out = `[darkroom.type.toTerraType(node.type:baseType(),false,V)](out)

        -- only make a statement if necessary
        if node:parentCount(inkernel)==1 then
          finalOut[c] = out
        else
          table.insert(stat,quote var [resultSymbol] = out end)
          finalOut[c] = `[resultSymbol]
        end

      end

      -- if this is an array and we index into it, pack it into a terra array
      local packedSymbol = symbol(darkroom.type.toTerraType(node.type:baseType(),false,V)[node.type:channels()],"pack")
      table.insert(stat,quote var [packedSymbol] = array(finalOut) end)

      for c=0,node.type:channels()-1 do
        finalOut[c+1] = `[packedSymbol][c]
      end
      
      return {finalOut, `[packedSymbol], stat}
    end)

  for k,v in ipairs(res[1]) do assert(terralib.isquote(res[1][k])) end
  
  if darkroom.printstage then
    print("terracompiler.codegen astNodes:",inkernel:S("*"):count()," statements:",#res[3],inkernel:name())
  end

  return res[1],res[3]
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
        s = s:unionWith(node.kernel:stencil(kernelNode):sum(Stencil.new():add(0,node.kernel.shiftY,0)))
      else
        s = s:unionWith(node.kernel:stencil(kernelNode):sum(neededStencil( interior,kernelGraph,node, shifts)))
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
function stripRight(strip, options) return quote var w = (strip+1)*[stripWidth(options)] in terralib.select(w>options.width,options.width,w) end end

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
    options)

  assert(type(shifts)=="table")
  assert(type(options)=="table")

  local x = symbol(int,"x")

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

      local needed, neededStat = memo("needed",needed( kernelGraph, n, strip, shifts, options )) -- clock space
      local neededImageSpace, neededImageSpaceStat = memo("neededImageSpace",shiftRegion( needed, -shifts[n] ))
      local valid, validStat = memo("valid",valid( kernelGraph, n, strip, shifts, options ))
      local validVectorized, validVectorizedStat = memo("validVectorized",vectorizeRegion(valid, options.V)) -- always >= valid to the nearest vector width

      table.insert(loopStartCode,
        quote
          neededStat; neededImageSpaceStat; validStat; validVectorizedStat;
          -- we use image space needed region here b/c These are the actual pixel coords we will write to
          -- since all image accesses use relative coordinates, This doesn't cause problems
          [inputs[n]:declare( loopid, neededImageSpace.left, neededImageSpace.bottom, needed.bottom, core, strip, linebufferBase ) ];
          [outputs[n]:declare( loopid, neededImageSpace.left, neededImageSpace.bottom, needed.bottom, core, strip, linebufferBase ) ];
          
          if options.verbose then
            cstdio.printf("--- %s V %d cores %d core %d shift %d\n",[n.kernel:name()],options.V, options.cores, strip, [shifts[n]])
            cstdio.printf("valid l %d r %d t %d b %d\n",[valid.left],[valid.right],[valid.top],[valid.bottom])
            cstdio.printf("validVectorized l %d r %d t %d b %d\n",[validVectorized.left],[validVectorized.right],[validVectorized.top],[validVectorized.bottom])
            cstdio.printf("needed l %d r %d t %d b %d\n",[needed.left],[needed.right],[needed.top],[needed.bottom])
            cstdio.printf("needed image space l %d r %d t %d b %d\n",[neededImageSpace.left],[neededImageSpace.right],[neededImageSpace.top],[neededImageSpace.bottom])
          end
        end)

      local expr,statements=darkroom.terracompiler.codegen( n.kernel,  options.V, x, clock, loopid, options.stripcount, n, inputs, outputs, taps, TapStruct, valid.left, valid.right, options.debug)

      table.insert(loopCode,
        quote
          if clock >= [needed.bottom] and clock < [needed.top] then
            if clock < [valid.bottom] or clock >= [valid.top]  then
              -- top/bottom row(s) (all boundary)
              -- theoretically we could do some of this vectorized, but it shouldn't really matter

              for [x] = [needed.left], [needed.right] do
                [outputs[n]:set( loopid, boundary(n), 1 )];
                [outputs[n]:next( loopid, 1 )];
                [inputs[n]:next( loopid, 1 )];
              end
              [outputs[n]:nextLine( loopid, `[needed.right]-[needed.left], stripCount)];
              [inputs[n]:nextLine( loopid, `[needed.right]-[needed.left], stripCount)];
            else
              -- interior row(s), mixed boundary and calculated region

              [outputs[n]:next( loopid, `validVectorized.left-needed.left )];
              [inputs[n]:next( loopid, `validVectorized.left-needed.left )];

              for [x] = validVectorized.left, validVectorized.right, options.V do
                statements;
                [outputs[n]:set( loopid, expr, options.V )];
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
function darkroom.terracompiler.codegenThread(kernelGraph, inputs, TapStruct, shifts, options)
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(inputs)=="table")
  assert(type(shifts)=="table")
  assert(type(options)=="table")

  local core = symbol(int, "core")
  local clock = symbol(int, "clock")
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
  local inputs, outputs, linebufferSize = darkroom.terracompiler.allocateImageWrappers(kernelGraph, inputImageSymbolMap, outputImageSymbolMap, shifts, options)

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
    inputs,
    outputs,
    taps,
    TapStruct,
    clock, 
    linebufferBase,
    shifts,
    options)


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

    var [taps] = [&TapStruct](inputArgs+demarshalCount)

    if options.verbose then cstdio.printf("Run Thread %d\n",core) end

    -- allocate line buffer for this thread
    var linebuffers : &opaque = vmIV.allocateCircular([linebufferSize])

    var start = darkroom.currentTimeInSeconds()
    for i=0,stripsPerCore do

      var [strip] = core*stripsPerCore+i

      var [linebufferBase] = linebuffers -- the image wrappers will mutate linebufferBase each time they allocate a buffer in the start code
      thisLoopStartCode

      for [clock] = startClock, options.height+endClock do
        thisLoopCode
      end

    end
    var endt = darkroom.currentTimeInSeconds()
  end

end


function darkroom.terracompiler.allocateImageWrappers(
    kernelGraph, 
    inputImageSymbolMap, 
    outputImageSymbolMap, 
    shifts,
    options)

  assert(darkroom.IR.isIR(kernelGraph))
  assert(type(shifts)=="table")
  assert(type(options)=="table")

  local inputs = {} -- kernelGraphNode->{input wrappers}
  local outputs = {} -- kernelGraphNode->output wrapper
  local linebufferSize = 0

  local function channelPointer(c,ptr,ty)
    return `[&ty](ptr)+[options.width*options.height]*c
  end

  local inputWrappers = {}
  local function getInputWrapper(id,type)
    if inputWrappers[id]==nil then
      inputWrappers[id] = {}
      for c = 1,type:channels() do inputWrappers[id][c] = newImageWrapper( channelPointer(c-1,inputImageSymbolMap[id], type:baseType():toTerraType()), type:baseType(), options.width, options.terradebug ) end
      setmetatable(inputWrappers[id],pointwiseDispatchMT)
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
      setmetatable(inputs[n],pointwiseDispatchMT)

      -- collect the inputs
      n:map("child", function(child,i) 
              inputs[n][child] = outputs[child]
                     end)

      if n~=kernelGraph then -- root is just a list of outputs

        -- if this uses any input files, we need to add those too
        n.kernel:S("load"):traverse(
          function(node)
            if type(node.from)=="number" then
              inputs[n][node.from] = getInputWrapper(node.from, node.type)
            end
          end)

        -- make the output
        if parentIsOutput(n)~=nil then
          outputs[n] = {}
          for c=1,n.kernel.type:channels() do outputs[n][c] = newImageWrapper( channelPointer(c-1,outputImageSymbolMap[parentIsOutput(n)], n.kernel.type:baseType():toTerraType()), n.kernel.type:baseType(), upToNearest(options.V, options.width), options.terradebug) end
          setmetatable(outputs[n],pointwiseDispatchMT)
        else
          outputs[n] = {}

          for c = 1, n.kernel.type:channels() do
            outputs[n][c] = newLineBufferWrapper(
              n:bufferSize(kernelGraph), 
              n.kernel.type:baseType(),
              downToNearest(options.V, neededStencil(true,kernelGraph,n, shifts):min(1)), -- use the more conservative stencil
              stripWidth(options),
              upToNearest(options.V, neededStencil(true,kernelGraph,n, shifts):max(1)),
              options.terradebug)

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
    options)

  if darkroom.verbose then print("compile") end
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
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

  local threadCode = darkroom.terracompiler.codegenThread( kernelGraph, inputImages, TapStruct, shifts, options )
  --threadCode:printpretty(false)

  local fin = terra([inputImageSymbolTable], [outputImageSymbolTable], tapsIn : &opaque)
    var start = darkroom.currentTimeInSeconds()
    
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