cmath = terralib.includec("math.h")
cstdio = terralib.includec("stdio.h")
cassert = terralib.includec("assert.h")
cstdlib = terralib.includec("stdlib.h")
cpthread = terralib.includec("pthread.h")

orion.terracompiler = {}

-----------------------------------
-- the imageWrapper assists the terra codegenerator by wrapping up the data sources
-- (buffer and special input) in a consistant way and providing a nice interface
-- that codegens strength reduced versions of loads etc.

orion.lineBufferWrapper={}
orion.lineBufferWrapper.nextRegister = 0
-- cachedBuffer is used to hold values for cached imageWrappers
orion.lineBufferWrapper.cachedBuffer = {buffer=global(&opaque, nil), linebuffer=global(&opaque, nil), special=global(&opaque, nil)}
-- this is the max stencil size we support with a cached buffer
orion.lineBufferWrapper.cachedBufferMaxWidth = 50
orion.lineBufferWrapper.cachedBufferMaxSize = terralib.sizeof(double)*4*orion.lineBufferWrapper.cachedBufferMaxWidth*orion.lineBufferWrapper.cachedBufferMaxWidth
orion.lineBufferWrapper.cachedBufferMaxCount = 50
orion.lineBufferWrapper.nextCachedId = 0
orion.lineBufferWrapper.linebufferBuffer = global(&opaque, nil)
orion.lineBufferWrapper.linebufferDebugX = global(&int, nil)
orion.lineBufferWrapper.linebufferDebugY = global(&int, nil)
orion.lineBufferWrapper.linebufferDebugId = global(&int, nil)
orion.lineBufferWrapper.maxLinebufferSize = 1024*1024
-- this is the number of strips that we will run _concurrently_
-- the strip is actually mapped mod maxLinebufferStrips
--orion.lineBufferWrapper.maxLinebufferStrips = orion.tune.cores
orion.lineBufferWrapper.nextLinebufferId = 0
orion.lineBufferWrapper.linebufferNextPosition = {} -- rungroup -> next available LB position
-- depending on what coord stuff lands on, growToNearestX may
-- make the linebuffer required larger at runtime than we calculate
-- at compile time (ie left=3 goes to left=0). So pad both sides of the LB
orion.lineBufferWrapper.lbPadding = 4*8

LineBufferWrapperFunctions = {}
LineBufferWrapperMT={__index=LineBufferWrapperFunctions}



-- tab.terraType should be the base type of the data stored in this image
-- ie, if it's a floating point image, tab.terraType should be float
function orion.lineBufferWrapper.new(lines)

  local tab = {}
    assert(type(lines)=="number")

    -- this holds the IVs for the LB.
    -- index 0 is the current location we're pointing at.
    -- index -1 is the previous line (line at index0 -1), etc
    -- the higher indices hold stuff in the past b/c this is
    -- what we will read when we do a stencil read (all stencils have neg indices)

      tab.data = {} -- line -> loop
      tab.base = {}
      tab.baseDebugX = {}
      tab.baseDebugY = {}
      tab.baseDebugId = {}
      tab.dataDebugX = {}
      tab.dataDebugY = {}
      tab.dataDebugId = {}
      tab.posX = {}
      tab.posY = {}


    if tab.gatherPointer==nil then
      tab.gatherPointer = {}
      tab.gatherPointerDebugX = {}
      tab.gatherPointerDebugY = {}
      tab.gatherPointerDebugId = {}
      tab.gatherPointerY = {}
      tab.gatherPointerUsed = {}
    end

    if tab.id==nil then
      tab.id = orion.lineBufferWrapper.nextLinebufferId
      orion.lineBufferWrapper.nextLinebufferId = orion.lineBufferWrapper.nextLinebufferId + 1
    end


  return setmetatable(tab,ImageWrapperMT)
end

-- called by the producer
function LineBufferWrapperFunctions:alloc(stripCount)

    assert(type(stripCount)=="number")

    assert(orion.lineBufferWrapper.maxLinebufferSize % orion.tune.pageSize == 0)

    -- self.linebufferPosition is in bytes
    if self.linebufferPosition == nil then
      local pos = orion.lineBufferWrapper.linebufferNextPosition[self.rungroup] or 0

      -- conservatively round up so that everything is properly aligned
      orion.lineBufferWrapper.linebufferNextPosition[self.rungroup] = upToNearest(
        orion.tune.V*4, pos + 
          terralib.sizeof(self.terraType) * self.lines * self:lineWidth(stripCount)  )

      if orion.lineBufferWrapper.linebufferNextPosition[self.rungroup] >= 
        orion.lineBufferWrapper.maxLinebufferSize then
        print("Exceeded max LB size ",orion.lineBufferWrapper.maxLinebufferSize)
        assert(false)
      end


      self.linebufferPosition = pos
    end

    local res = {}
    table.insert(res,quote 
      if orion.lineBufferWrapper.linebufferBuffer==nil then
        stdlib.posix_memalign( 
          &orion.lineBufferWrapper.linebufferBuffer, 
          orion.tune.pageSize,  
          orion.lineBufferWrapper.maxLinebufferSize*orion.tune.cores)
      end
    end)

    if orion.debug then
      table.insert(res,quote 
                     if orion.lineBufferWrapper.linebufferDebugX==nil then
                       stdlib.posix_memalign( 
                         [&&opaque](&orion.lineBufferWrapper.linebufferDebugX), 
                         orion.tune.pageSize,  
                         orion.lineBufferWrapper.maxLinebufferSize*orion.tune.cores*sizeof(int))
                     end
                     if orion.lineBufferWrapper.linebufferDebugY==nil then
                       stdlib.posix_memalign( 
                         [&&opaque](&orion.lineBufferWrapper.linebufferDebugY), 
                         orion.tune.pageSize,  
                         orion.lineBufferWrapper.maxLinebufferSize*orion.tune.cores*sizeof(int))
                     end
                     if orion.lineBufferWrapper.linebufferDebugId==nil then
                       stdlib.posix_memalign( 
                         [&&opaque](&orion.lineBufferWrapper.linebufferDebugId),
                         orion.tune.pageSize,  
                         orion.lineBufferWrapper.maxLinebufferSize*orion.tune.cores*sizeof(int))
                     end
      end)

    end

    return quote res end
end

-- called by a consumer
function LineBufferWrapperFunctions:init(loopid)
  assert(type(loopid)=="number")

    assert(type(self.id)=="number")



    -- always points to the lowest memory address
    -- so adding Y*linewidth will get to the line you want to gather from
    if self.gatherPointer[loopid]==nil then
      self.gatherPointer[loopid] = symbol(&self.terraType)
      self.gatherPointerDebugX[loopid] = symbol(&int)
      self.gatherPointerDebugY[loopid] = symbol(&int)
      self.gatherPointerDebugId[loopid] = symbol(&int)
      self.gatherPointerY[loopid] = symbol(int) -- relative Y stored in gather pointer
      self.gatherPointerUsed [loopid]= false

    end

    -- base is used when we don't store an IV for the line
    self.base[loopid] = symbol(&self.terraType)


    if orion.debug then
      self.baseDebugX[loopid] = symbol(&int)
      self.baseDebugY[loopid] = symbol(&int)
      self.baseDebugId[loopid] = symbol(&int)

    end

    -- each line has its own IV
    for line = -(self.lines-1),0 do

      if self.data[line][loopid]==nil then
        assert(terralib.types.istype(self.terraType))
        self.data[line][loopid] = symbol(&self.terraType)
        self.used[line][loopid] = false
      end

      -- we don't actually know what stuff is written to at the time we set stuff up (declare variables).
      -- so just assume that everything is written to, it's prob not a huge inefficiency.
      self.used[0][loopid] = true


      if orion.debug then

        if self.dataDebugX[line][loopid]==nil then
          self.dataDebugX[line][loopid] = symbol(&int)
        end

        if self.dataDebugY[line][loopid]==nil then
          self.dataDebugY[line][loopid] = symbol(&int)
        end

        if self.dataDebugId[line][loopid]==nil then
          self.dataDebugId[line][loopid] = symbol(&int)
        end

        self.posX[loopid] = symbol(int)
        self.posY[loopid] = symbol(int)


      
      end
    end

end

function LineBufferWrapperFunctions:declare(loopid)

    local res = {}

    if self.gatherPointerUsed[loopid] then
      table.insert(res, quote
                     var [self.gatherPointer[loopid]] = nil
                     var [self.gatherPointerDebugX[loopid]] = nil
                     var [self.gatherPointerDebugY[loopid]] = nil
                     var [self.gatherPointerDebugId[loopid]] = nil
                     var [self.gatherPointerY[loopid]] = -9999999
      end)
    end

    table.insert(res, quote var [self.base[loopid]] end)

    if orion.debug then
        table.insert(res, quote
                       var [self.baseDebugX[loopid]]
                       var [self.baseDebugY[loopid]]
                       var [self.baseDebugId[loopid]]
      end)
    end

    for line = -(self.lines-1),0 do
      -- meh, doesn't actually matter what we do here b/c
      -- we're going to set position before we do anything

      if self.used[line][loopid] then
      table.insert(res, quote
                     var [self.data[line][loopid]] = [&self.terraType](orion.lineBufferWrapper.linebufferBuffer)
                     
--          orion.lineBufferWrapper.maxLinebufferSize * strip +
--          [orion.lineBufferWrapper.linebufferPosition[self.id]]
        end)
      end

      if orion.debug then
        table.insert(res, quote
                       var [self.dataDebugX[line][loopid]]
                       var [self.dataDebugY[line][loopid]]
                       var [self.dataDebugId[line][loopid]]
                       var [self.posX[loopid]]
                       var [self.posY[loopid]]
        end)
      end

    end
    return quote res end

end

-- the size of this image is the 'valid region'
-- the (x,y) is global space 
-- asserts that this is in the valid region
function LineBufferWrapperFunctions:setPosition( 
    loopid, x, y, 
    core, 
    stripId, 
    stripCount, 
    stripList,
    stripSymbolCache,
    retime)

  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y))
  assert(terralib.isquote(core) or terralib.issymbol(core))
  assert(terralib.isquote(stripId) or terralib.issymbol(stripId))
  assert(type(stripCount)=="number")
  assert(type(stripList)=="table")
  assert(type(stripSymbolCache)=="table")
  assert(type(retime)=="number")

  local res = {}

  assert(terralib.issymbol(stripId))
  
  if self.startPosY==nil then
    -- this is kind of a hack: the only time we set position is in the header of the loop
    -- the first guy to set position is the output. So we just set this variable the first
    -- time it's seen. Then all the inputs are based on this.
    
    -- NOTE that there is only one of these for all the loops. the reason is that this is used
    -- to satisfy cross-loop dependencies. Ie one loop writes at a certain Y, and another
    -- loop has to know where in the LB to consume this from
    
    self.startPosY = symbol(int)
    table.insert(res, quote var [self.startPosY] = y end)
  end

  if orion.debug then
    table.insert(res, quote [self.posX[loopid]] = x; [self.posY[loopid]] = y; end)
  end

    local baseOffset = symbol(int)
    local base = symbol()
    local leftBound = symbol()
    local baseAddX = symbol(int)
    local baseX = symbol()
    local baseY = symbol()
    local baseId = symbol()

    table.insert(res, quote 
                     var [baseOffset] = core * [orion.lineBufferWrapper.maxLinebufferSize] + self.linebufferPosition
                     var [base] = [&uint8](orion.lineBufferWrapper.linebufferBuffer) + baseOffset
                     var [leftBound] = [self.region:stripRuntime(stripId, stripCount, stripList, stripSymbolCache)]:growToNearestX(orion.tune.V).left
                     var [baseAddX] = (x-leftBound+orion.lineBufferWrapper.lbPadding)

                     var [baseX] = orion.lineBufferWrapper.linebufferDebugX + baseOffset
                     var [baseY] = orion.lineBufferWrapper.linebufferDebugY + baseOffset
                     var [baseId] = orion.lineBufferWrapper.linebufferDebugId + baseOffset
          -- base is used when we don't have an IV for this line
          [self.base[loopid]] = [&self.terraType](base) + baseAddX
end)

    if orion.debug then
        table.insert(res, quote
                       [self.baseDebugId[loopid]] = baseId + baseAddX
                       [self.baseDebugY[loopid]] = baseY + baseAddX
                       [self.baseDebugX[loopid]] = baseX + baseAddX
      end)
    end

    if self.gatherPointerUsed[loopid] then

      -- solve for line
      -- 0 = fixedModulus(self.lines-1+line+(y-[self.startPosY]),self.lines)
      -- self.lines-1+line+(y-[self.startPosY]) % self.lines == 0
      table.insert(res, 
        quote 
          for line = -(self.lines-1),0 do
            var l = fixedModulus(self.lines-1+line+(y-[self.startPosY]),self.lines)
            if l==0 then
              [self.gatherPointer[loopid]] = [&self.terraType](base) + baseAddX
              [self.gatherPointerY[loopid]] = line
            end
          end
        end)

      if orion.debug then
        table.insert(res, 
          quote 
            [self.gatherPointerDebugX[loopid]] = baseX + baseAddX
            [self.gatherPointerDebugY[loopid]] = baseY + baseAddX
            [self.gatherPointerDebugId[loopid]] = baseId + baseAddX
          end)
      end
    end

    -- each line has its own IV
    for line = -(self.lines-1),0 do
--      local baseAdd = symbol(int)
      local l = symbol(int)

      if self.used[line][loopid] then
        table.insert(res, 
          quote 
            var [l] = fixedModulus(self.lines-1+line+(y-[self.startPosY])+(retime-self.retime),self.lines)

--            cstdio.printf("SET POS id %d loopid %d line %d startPosY %d y %d l %d\n",self.id,loopid,line,self.startPosY,y,l)
--            cstdio.printf("RT %d %d %d\n",retime,self.retime,self.lines)
            -- we expand this out so that vector size aligned stuff in abs coord space
            -- is vector size aligned in memory
          
            --                     cstdio.printf("SETPOS id:%d loopid:%d l:%d lmod:%d line:%d self.lines:%d y:%d startPosY:%d\n",self.id, loopid, l, l%self.lines, line, self.lines, y, self.startPosY)
            --                     cstdio.printf("x:%d leftBound:%d\n",x,leftBound)

            [self.data[line][loopid]] =  [&self.terraType](base) + l*[self:lineWidth(stripCount)] + baseAddX
        end)


        if orion.debug then
          table.insert(res, 
            quote
              orionAssert(uint64(base) % (orion.tune.V*sizeof([self.terraType]))==0, "lb base not aligned")
              orionAssert(uint64([self:lineWidth(stripCount)]) % orion.tune.V==0, "lb lineWidth not aligned")
              
              var baseAdd = l*[self:lineWidth(stripCount)] + baseAddX
              
              [self.dataDebugX[line][loopid]] = baseX + baseAdd
              [self.dataDebugY[line][loopid]] = baseY + baseAdd
              [self.dataDebugId[line][loopid]] = baseId + baseAdd
              
            end)
        end
      end
end

  return res
end

function LineBufferWrapperFunctions:set( loopid, value, V )
  assert(terralib.isquote(value))
  assert(type(loopid)=="number")
  assert(type(V)=="number")

    local res = {}

    if orion.debug then
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

                       orionAssert(uint64([self.data[0][loopid]]) % (V*sizeof([self.terraType])) == 0,"lb set not aligned")
        end)
      end
    end

    self.used[0][loopid] = true
    table.insert(res,quote 
--                   cstdio.printf("SET %d\n", [self.data[0][loopid]])
--                   cstdio.printf("linebufferBuffer:%d mbs:%d lbp:%d\n",orion.lineBufferWrapper.linebufferBuffer,[orion.lineBufferWrapper.maxLinebufferSize],self.linebufferPosition)
                   @[&vector(self.terraType,V)]([self.data[0][loopid]]) = value 
--                   cstdio.printf("SETD\n")
end)


    return res
end

-- relX and relY should be integer constants relative to
-- the current location
function LineBufferWrapperFunctions:get(loopid, relX,relY, V, retime)
  assert(type(loopid)=="number")
  assert(type(relX)=="number")
  assert(type(relY)=="number")
  assert(type(V)=="number")
  assert(type(retime)=="number")

  local origRelY = relY
  if self.kind=="linebuffer" then
    relY = relY + self.retime - retime
  end

  local debugChecks = {}

    assert(relY<=0)

    if orion.debug then
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
    end

    self.used[relY][loopid] = true

    return quote [debugChecks] in
      terralib.attrload([&vector(self.terraType,V)]([self.data[relY][loopid]] + relX),{align=V})
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

      if orion.debug then
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

    return quote [res];[debugChecks] in vectorof(self.terraType,resTable) end


end

function LineBufferWrapperFunctions:next(loopid,v)
  assert(type(loopid)=="number")
  assert(type(v)=="number")
  

    local res = {}

    if self.gatherPointerUsed[loopid] then
      table.insert(res, quote [self.gatherPointer[loopid]] = [self.gatherPointer[loopid]] + v end)

      if orion.debug then
        table.insert(res, quote [self.gatherPointerDebugX[loopid]] = [self.gatherPointerDebugX[loopid]] + v end)
        table.insert(res, quote [self.gatherPointerDebugY[loopid]] = [self.gatherPointerDebugY[loopid]] + v end)
        table.insert(res, quote [self.gatherPointerDebugId[loopid]] = [self.gatherPointerDebugId[loopid]] + v end)
      end
    end

    for line = -(self.lines-1),0 do

      if self.used[line][loopid] then
        table.insert(res, quote [self.data[line][loopid]] = [self.data[line][loopid]] + v end)

        if orion.debug then
          table.insert(res, quote [self.dataDebugX[line][loopid]] = [self.dataDebugX[line][loopid]] + v end)
          table.insert(res, quote [self.dataDebugY[line][loopid]] = [self.dataDebugY[line][loopid]] + v end)
          table.insert(res, quote [self.dataDebugId[line][loopid]] = [self.dataDebugId[line][loopid]] + v end)
        end
      end
    end

    if orion.debug then
      table.insert(res, quote [self.posX[loopid]] = [self.posX[loopid]] + v end)
    end

    return quote res end

end

-- sub: this is the number of pixels we looped over since last calling nextline
-- basically: the number of times we called nextVector this row * the vector width
function LineBufferWrapperFunctions:nextLine(loopid,  sub, stripCount, y, retime)
  assert(terralib.isquote(sub))
  assert(type(stripCount)=="number")
  assert(terralib.issymbol(y))
  assert(type(retime)=="number")

    local res = {}
    local temp = {}
    local tempDebugX = {}
    local tempDebugY = {}
    local tempDebugId = {}

    if self.gatherPointerUsed[loopid] then
      table.insert(res, quote 
                   [self.gatherPointerY[loopid]] = ([self.gatherPointerY[loopid]] - 1)%self.lines;
                   [self.gatherPointer[loopid]] = [self.gatherPointer[loopid]] - sub
      end)

      if orion.debug then
        table.insert(res, quote 
                   [self.gatherPointerDebugX[loopid]] = [self.gatherPointerDebugX[loopid]] - sub
                   [self.gatherPointerDebugY[loopid]] = [self.gatherPointerDebugY[loopid]] - sub
                   [self.gatherPointerDebugId[loopid]] = [self.gatherPointerDebugId[loopid]] - sub
       end)
     end
    end

    for line = -(self.lines-1),0 do
      temp[line] = symbol(&self.terraType)
      if orion.debug then
        tempDebugX[line] = symbol(&int)
        tempDebugY[line] = symbol(&int)
        tempDebugId[line] = symbol(&int)
      end

      local i = line - 1
      if i < -(self.lines-1) then i =  0 end

      if self.used[line][loopid] then
        table.insert(res, quote var [temp[line]] = [self.data[line][loopid]] - sub end)

        if orion.debug then
          table.insert(res, quote var [tempDebugX[line]] = [self.dataDebugX[line][loopid]] - sub end)
          table.insert(res, quote var [tempDebugY[line]] = [self.dataDebugY[line][loopid]] - sub end)
          table.insert(res, quote var [tempDebugId[line]] = [self.dataDebugId[line][loopid]] - sub end)
        end
      elseif self.used[i][loopid] then

        local baseAdd = symbol(int)

        table.insert(res, quote 
                       var l = fixedModulus(self.lines-1+line+(y-[self.startPosY])+(retime-self.retime),self.lines)
                       var [baseAdd] = l*[self:lineWidth(stripCount)]
                 end)

        table.insert(res, quote var [temp[line]] = [self.base[loopid]] + baseAdd end)

        if orion.debug then
          table.insert(res, quote var [tempDebugX[line]] = [self.baseDebugX[loopid]] + baseAdd end)
          table.insert(res, quote var [tempDebugY[line]] = [self.baseDebugY[loopid]] + baseAdd end)
          table.insert(res, quote var [tempDebugId[line]] = [self.baseDebugId[loopid]] + baseAdd end)
        end

      end
    end

    for line = -(self.lines-1),0 do
      if self.used[line][loopid] then
        local i = line + 1
        if i > 0 then i =  -(self.lines-1) end

        table.insert(res, quote [self.data[line][loopid]] = [temp[i]] end)

        if orion.debug then
          table.insert(res, quote [self.dataDebugX[line][loopid]] = [tempDebugX[i]] end)
          table.insert(res, quote [self.dataDebugY[line][loopid]] = [tempDebugY[i]] end)
          table.insert(res, quote [self.dataDebugId[line][loopid]] = [tempDebugId[i]] end)
        end
      end
    end

    if orion.debug then
      table.insert(res, quote [self.posY[loopid]] = [self.posY[loopid]] + 1 end)
      table.insert(res, quote [self.posX[loopid]] = [self.posX[loopid]] - sub end)
    end

    return quote res end

end


orion.imageWrapper={}

ImageWrapperFunctions = {}
ImageWrapperMT={__index=ImageWrapperFunctions}

function orion.imageWrapper.isImageWrapper(im)
  return getmetatable(im)==ImageWrapperMT
end

-- tab.terraType should be the base type of the data stored in this image
-- ie, if it's a floating point image, tab.terraType should be float
function orion.imageWrapper.new()
  local tab = {data={}}

  return setmetatable(tab,ImageWrapperMT)
end


-- called by the producer
function ImageWrapperFunctions:alloc(stripCount)

    local r = self.register
    local rc = self.refCount
    assert(type(r)=="number")
    assert(type(rc)=="number")

    return `orion.runtime.createRegister(r,rc)

end

-- called by a consumer
function ImageWrapperFunctions:init(loopid)
  assert(type(loopid)=="number")

    if self.data[loopid]==nil then
      assert(terralib.types.istype(self.terraType))
      self.data[loopid] = symbol(&self.terraType)
    end
end

function ImageWrapperFunctions:declare(loopid)
    return quote
      var [self.data[loopid]] = [&self.terraType](orion.runtime.getRegister(self.register))
      end
end


-- the size of this image is the 'valid region'
-- the (x,y) is global space 
-- asserts that this is in the valid region
function ImageWrapperFunctions:setPosition( 
    loopid, x, y, 
    core, 
    stripId, 
    stripCount, 
    stripList,
    stripSymbolCache,
    retime)

  assert(type(loopid)=="number")
  assert(terralib.isquote(x) or terralib.issymbol(x))
  assert(terralib.isquote(y) or terralib.issymbol(y))
  assert(terralib.isquote(core) or terralib.issymbol(core))
  assert(terralib.isquote(stripId) or terralib.issymbol(stripId))
  assert(type(stripCount)=="number")
  assert(type(stripList)=="table")
  assert(type(stripSymbolCache)=="table")
  assert(type(retime)=="number")

  local res = {}


    assert(self.region:growToNearestX(orion.tune.V):getWidth() % orion.tune.V == 0)

    -- we expand out the region so that vector size aligned absolute positions
    -- are vector size aligned in memory
    table.insert(res, quote [self.data[loopid]] =  [&self.terraType](orion.runtime.getRegister(self.register))
                   + (y-[self.region:growToNearestX(orion.tune.V):getBottom()])*[self.region:growToNearestX(orion.tune.V):getWidth()] + 
                   (x-[self.region:growToNearestX(orion.tune.V):getLeft()]) end)

                 -- keep these tests commented out b/c it's actually
                 -- usually ok to set the position out of bounds initially
--[[
                 if orion.debug then
                   table.insert(res, quote 
                                cstdio.printf("lol %d %d\n",y,[self.region:getBottom()])
                                orionAssert((y-[self.region:getBottom()])>=0,"set y out of bound\n") 
                 end)

table.insert(res, quote 
             cstdio.printf("lolx %d %d\n",x,[self.region:getLeft()])
             orionAssert((x-[self.region:getLeft()]) >=0,"set x out of bound\n") 
             end)
end
]]

  return res
end

function ImageWrapperFunctions:set( loopid, value, V )
  assert(terralib.isquote(value))
  assert(type(loopid)=="number")
  assert(type(V)=="number")

    local res = {}
    
    if orion.debug then
      table.insert(res,
                   quote
                     var start = [&self.terraType](orion.runtime.getRegister(self.register))
                     var maxv = orion.runtime.registerSize
                     
                     orionAssert( ([self.data[loopid]]-start)<maxv,"wrote beyond end of array")
                     orionAssert( ([self.data[loopid]]-start)>=0,"wrote before start of array")
                     orionAssert( uint64([self.data[loopid]]) % (V*sizeof([self.terraType])) == 0, "write is not aligned!")
      end)
    end

    if self.consumedInternally==nil or self.consumedInternally==false then
      table.insert(res,quote terralib.attrstore([&vector(self.terraType,V)]([self.data[loopid]]),value,{nontemporal=true}) end)
    else
      table.insert(res,quote terralib.attrstore([&vector(self.terraType,V)]([self.data[loopid]]),value,{}) end)
    end

    return res
end

-- relX and relY should be integer constants relative to
-- the current location
function ImageWrapperFunctions:get(loopid, relX,relY, V, retime)
  assert(type(loopid)=="number")
  assert(type(relX)=="number")
  assert(type(relY)=="number")
  assert(type(V)=="number")
  assert(type(retime)=="number")


  local debugChecks = {}

  if orion.debug then
    local regionWidth = self.region:growToNearestX(orion.tune.V):getWidth()
    table.insert(debugChecks,
                 quote
                 var start =  [&self.terraType](orion.runtime.getRegister(self.register))
                 var maxv = orion.runtime.registerSize
                 var this = [self.data[loopid]] + relY*regionWidth + relX

                 if (this-start)<0 then cstdio.printf("this:%d start:%d self.data:%d relY:%d regionWidth:%d relX%d\n",
                                                      this,start,[self.data[loopid]],relY,regionWidth,relX) 
                   cstdio.printf("reg:%d\n",self.register)
                 end
                 orionAssert( (this-start)>=0,"read before start of array")
                 orionAssert( (this-start)<maxv,"read beyond end of array")
  end)
  end


  local regionWidth = self.region:growToNearestX(orion.tune.V):getWidth()
  return quote [debugChecks] in
                   --var [resultSymbol] = terralib.aligned(@[&vector(self.terraType,V)]([self.data[loopid]] + relY*regionWidth + relX),V)
                   terralib.attrload([&vector(self.terraType,V)]([self.data[loopid]] + relY*regionWidth + relX),{align=V})
                   --    cstdio.printf("%f %f %f %f %d\n",resultSymbol[0],resultSymbol[1],resultSymbol[2],resultSymbol[3], self.data)
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


  if orion.debug then
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
function ImageWrapperFunctions:nextLine(loopid,  sub, stripCount, y, retime)
  assert(terralib.isquote(sub))
  assert(type(stripCount)=="number")
  assert(terralib.issymbol(y))
  assert(type(retime)=="number")


    local a = self.region:growToNearestX(orion.tune.V):getWidth()
    return quote [self.data[loopid]] = [self.data[loopid]] + a - sub end


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
  inkernel, V, xsymb, ysymb, loopid, stripCount, retime)

  assert(type(loopid)=="number")
  assert(type(stripCount)=="number")
  assert(orion.flatIR.isFlatIR(inkernel))
  assert(type(retime)=="number")

  local stat = {}

  inkernel = orion.optimize.CSE(inkernel,{})

  local expr =inkernel:visitEach(
    function(node,inputs)


      local out
      local resultSymbol = orion.terracompiler.symbol(node.type, false, V)

      for k,v in node:children() do
        assert(terralib.isquote(inputs[k]))
      end
      
      if node.kind=="loadConcrete" then
        out = node.from:get(loopid, node.x,node.y,V,retime);
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

-- needed regions are always expanded up to the nearest vector size
function neededInteriorStencil()

end

-- needed regions are always expanded up to the nearest vector size
function neededExteriorStencil()

end

function validStencil()

end

function stripLeft(strip, options)
  return strip*stripWidth(options)
end

-- return interiorValue or exteriorValue depending if this strip's edge is on the exterior of the region we're calculating or not
terra interiorSelectLeft(strip : int, interiorValue : int, exteriorValue : int)
  if strip==0 then return exteriorValue end
  return interiorValue
end

terra interiorSelectRight(strip : int, stripcount : int, interiorValue : int, exteriorValue : int)
  if strip==stripcount-1 then return exteriorValue end
  return interiorValue
end

function needed(strip, options)
  return {left = `stripLeft(strip)+interiorSelectLeft(strip,[neededInteriorStencil():min(1)],[neededExteriorStencil():min(1)]),
          right = `stripRight(strip)+interiorSelectRight(strip,[options.stripcount],[neededInteriorStencil():max(1)],[neededExteriorStencil():max(1)]),
          top = `imageHeight+[neededExteriorStencil(strip):max(2)],
          bottom = `[neededExteriorStencil(strip):min(2)]}
end

function valid()
  return {left = `stripLeft(strip)+interiorSelectLeft(strip,0,[validStencil():min(1)]),
          right = `stripRight(strip)+interiorSelectRight(strip,0,[validStencil():max(1)]),
          top = `imageHeight+[validStencil():max(2)],
          bottom = `[validStencil():min(2)]}
end

-- validVector is always >= vector. We expand out the valid region to 
-- be vector aligned (and also expand the needed regions so that this works).
-- after overcomputing the valid region, we write of it with the boundary info
function validVector()
  return {left = `stripLeft(strip)+interiorSelectLeft(strip,0,[upToNearest(V,validStencil():min(1))]),
          right = `stripRight(strip)+interiorSelectRight(strip,0,[upToNearest(V,validStencil():max(1))]),
          top = `imageHeight+[validStencil():max(2)],
          bottom = `[validStencil():min(2)]}
end

-- codegen all the stuff in the inner loop
function orion.terracompiler.codegenInnerLoop(
    core, 
    strip, 
    kernelGraph, 
    y,
    options)

  assert(type(options)=="table")

  local x = symbol(int)

  local loopStartCode = {}
  local loopCode = {}
  local loopid = 0

  kernelGraph:S("single"):traverse(
    function(n)

      loopid = loopid + 1

      local needed = needed(strip)
      local valid = valid(strip)
      local validVector = validVector(strip) -- always larger than valid to the nearest vector width

      n:initInputImages(loopid);
      n.outputImage:init(loopid);

      local rtY = symbol()
      
      -- we need to call these upfront so that the LBs can remember which IVs were actually used. But after init.
      local expr,statements=orion.terracompiler.codegen( n.kernel,  orion.tune.V, x, rtY, loopid, stripCount, n.retime )

      table.insert(loopStartCode,
                   quote
                     if orion.verbose then cstdio.printf("Run Kernel %d retime %d\n", i, [n.retime]) end
                     
                     var [needed] = [n.neededRegion:stripRuntime(strip, stripCount, stripList, stripSymbolCache)];
                     
                     -- these intersections need to happen at runtime b/c we don't know the width that 
                     -- will be passed in
        var [valid] = [n.validRegion:intersectRuntime(needed)];
        var [validSS] = valid:shrinkToNearestX(orion.tune.V); -- the vectorized sweet spot
        [validSS]:assertXMod(orion.tune.V)

        [n:declareInputImages(i,inputImageTable)];
        [n.outputImage:declare(i)];

        [n.outputImage:setPosition( i, `needed.left,`needed.bottom, core, strip, stripCount, stripList, stripSymbolCache,L["retime"..i])];
        [n:setInputImagePositions( i, `needed.left,`needed.bottom, core, strip, stripCount, stripList, stripSymbolCache,L["retime"..i])];

        if orion.verbose then
          cstdio.printf("valid:\n")
          valid:print()
          cstdio.printf("validSS:\n")
          validSS:print()
          cstdio.printf("needed:\n")
          needed:print()
        end
      end)


      table.insert(loopCode,
      quote
        var [rtY] = y - [L["retime"..i]]
        var bottom = [n.neededRegionUnion:getBottom()]
        var top = [n.neededRegionUnion:getTop()]

        if valid.width>0 then orionAssert(bottom<=valid.bottom, "VB") end
        if needed.width>0 then orionAssert(bottom<=needed.bottom, "NB") end
        if valid.width>0 then orionAssert(top>=valid.top, "VT") end
        if needed.width>0 then orionAssert(top>=needed.top, "NT") end


        if needed.width==0 then
          -- nothing to do!
        elseif valid.width==0 then
          -- we requested a region totally in the boundary

          if rtY >= needed.bottom and rtY < needed.top then
            for [x] = needed.left, needed.right do
              [n.outputImage:set( i, L:boundary(i), 1 )];
              [n.outputImage:next( i, 1 )];
              [n:inputImagesNext( i, 1 )];
            end
            [n.outputImage:nextLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [n:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end

        else
          
          -- top row(s) (all boundary)
          -- theoretically we could do some of this vectorized, but it shouldn't really matter
          if rtY >= needed.bottom and rtY < valid.bottom then
            for [x] = needed.left, needed.right do
              [n.outputImage:set( i, L:boundary(i), 1 )];
              [n.outputImage:next( i, 1 )];
              [n:inputImagesNext( i, 1 )];
            end
            [n.outputImage:nextLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [n:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end
        
          -- interior row(s), mixed boundary and calculated region
          if rtY >= valid.bottom and rtY <valid.top then
            for [x] = needed.left, valid.left do
              [n.outputImage:set(i,L:boundary(i),1)];
              [n.outputImage:next(i,1)];
              [n:inputImagesNext(i,1)];
            end
            

            if validSS.width<=0 or validSS.height<=0 then

              -- sweet spot is empty
              for [x] = valid.left, valid.right do
                statements1;
                [n.outputImage:set(i,expr1,1)];
                [n.outputImage:next(i,1)];
                [n:inputImagesNext(i,1)];
              end

            else
              
              for [x] = valid.left, validSS.left do
                statements1;
                [n.outputImage:set(i,expr1,1)];
                [n.outputImage:next(i,1)];
                [n:inputImagesNext(i,1)];
              end

              for [x] = validSS.left, validSS.right, orion.tune.V do
                statementsV;
                [n.outputImage:set(i,exprV,orion.tune.V)];
                [n.outputImage:next( i, orion.tune.V )];
                [n:inputImagesNext( i, orion.tune.V )];
              end

              for [x] = validSS.right, valid.right do
                statements1;
                [n.outputImage:set(i,expr1,1)];
                [n.outputImage:next( i, 1 )];
                [n:inputImagesNext( i, 1 )];
              end
            end

            for [x] = valid.right, needed.right do
              [n.outputImage:set( i, L:boundary(i), 1 )];
              [n.outputImage:next( i, 1 )];
              [n:inputImagesNext( i, 1 )];
            end
          
            [n.outputImage:nextLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [n:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end
        
          -- last row(s), all boundary
          -- theoretically we could do some of this vectorized, but it shouldn't really matter
          if rtY >= valid.top and rtY < needed.top then
            for [x] = needed.left, needed.right do
              [n.outputImage:set( i, L:boundary(i), 1 )];
              [n.outputImage:next( i, 1 )];
              [n:inputImagesNext( i, 1 )];
            end
            [n.outputImage:nextLine( i,  `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [n:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end
        end   
        --end

      end)
  end)

 return loopStartCode, loopCode
end

-- codegen all the code that runs per thread (and preamble)
function orion.terracompiler.codegenThread(
  kernelGraph, 
  imageWidth, 
  imageHeight,
  options)

  assert(orion.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")
  assert(type(options)=="table")

  local core = symbol(int)
  local y = symbol(int)


  local loopCode = {}
  assert(options.stripcount % options.cores == 0)
  local stripsPerCore = math.floor(options.stripcount/options.cores)

  ------
  local strip = symbol(int)

  local thisLoopStartCode, thisLoopCode = orion.terracompiler.codegenInnerLoop(
    core,
    strip,
    kernelGraph,
    y, 
    options)

  if orion.printstage then
    print("strip list count",#stripList)
  end

  local input = symbol(&opaque)
  local inputDecl = {}
  for k,v in ipairs(inputImageTable) do
    table.insert(inputDecl, quote var [v] = ([&&Image](input))[k] end) -- note the indexing shenanigans
  end

  return terra( [input] ) : &opaque
    var [core] = @([&int](input))
    [inputDecl]
    if orion.verbose then cstdio.printf("Run Loop %d",core) end

    [orion.kernelGraph.outputImageMap(function(n) return n:alloc(stripCount) end)]

    var start = orion.currentTimeInSeconds()
    for i=0,stripsPerCore do

      var [strip] = core*stripsPerCore+i
      stripList

      thisLoopStartCode

      for [y] = 0, imageHeight do
        thisLoopCode
      end

    end
    var endt = orion.currentTimeInSeconds()
  end

end


function orion.terracompiler.allocateImageWrappers(kernelGraph, inputImageSymbolMap)
  assert(orion.IR.isIR(kernelGraph))

  local inputs = {} -- kernelGraphNode->{input wrappers}
  local outputs = {} -- kernelGraphNode->output wrapper

  local inputWrappers = {}
  local function getInputWrapper(inputNode)
    if inputWrappers[inputNode.id]==nil then
      inputWrappers[inputNode.id] = orion.imageWrapper.new(inputImageSymbolMap[inputNode])
    end
    return inputWrappers[inputNode.id]
  end

  local function parentIsOutput(node)
    for v,k in node:parents(kernelGraph) do if v==kernelGraph then return true end end
    return false
  end

  kernelGraph:S("*"):traverse(
    function(n)
      inputs[n] = {}

      -- collect the inputs
      n:map("child", function(child,i) table.insert(inputs[n],child.outputImage) end)

      -- if this uses any input files, we need to add those too
      n.kernel:S("input"):traverse(
        function(node)
          table.insert(inputs[n],getInputWrapper(node))
        end)

      -- make the output
      if parentIsOutput(n) then
        outputs[n] = orion.imageWrapper.new(n.kernel.type)

      else
        outputs[n] = orion.lineBufferWrapper.new(3, n.kernel.type)
      end

    end)

  return inputs, outputs
end

-- this should return a terra function that when called executes the pipeline
function orion.terracompiler.compile(
    kernelGraph, 
    inputImages,
    imageWidth,
    imageHeight,
    options)

  if orion.verbose then print("compile") end
  assert(orion.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(inputImages)=="table")
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")
  assert(type(options)=="table")

  -- make symbols for the input images
  local inputImageSymbolMap = {}   -- maps from bound image id to &Image symbol
  local inputImageSymbolTable = {} -- symbols in order
  for _,v in ipairs(inputImages) do
    assert(v.kind=="crop")
    assert(v.expr.kind=="input")
    assert(type(v.expr.id)=="number")
    inputImageSymbolMap[v.expr.id] = symbol(&Image)
    table.insert(inputImageSymbolTable, inputImageSymbolMap[v.expr.id])
  end

  -- schedule nodes to determine linebuffer size
  local schedule = schedule(kernelGraph)

  -- add the input lists and output to the kernelGraph
  local inputs, outputs = orion.terracompiler.allocateImageWrappers(kernelGraph, inputImageSymbolMap)

  local outputImageSymbolTable = kernelGraph:map("inputImages",function(n) 
                                                   for k,v in pairs(n) do print(k,v) end
                                                   return n.data end)
  for k,v in pairs(outputImageSymbolTable) do print(k,v,terralib.issymbol(v)) end

  local threadCode = orion.terracompiler.codegenThread( 
    kernelGraph, 
    imageWidth, 
    imageHeight,
    options)

  threadCode:printpretty()

  return terra([inputImageSymbolTable], [outputImageSymbolTable])
    var start = orion.currentTimeInSeconds()
    
    var threads : cpthread.pthread_t[orion.tune.cores]
    var stripStore : int[orion.tune.cores]
    
    for i=0, orion.looptimes do
      -- launch the kernel for each strip
      
      if orion.tune.cores==1 then
        -- don't launch a thread to save thread launch overhead
        stripStore[0]=0
        threadCode(&stripStore[0])
      else
        for i=0,orion.tune.cores do
          --                           cstdio.printf("Launch %d\n",i)
          stripStore[i] = i
          cpthread.pthread_create(&threads[i],nil,threadCode, &stripStore[i]);
        end
        for i=0,orion.tune.cores do
          cpthread.pthread_join(threads[i],nil)
          --                             cstdio.printf("join %d\n",i)
        end
      end
    end
    
    if orion.printruntime then
      var len : double = (orion.currentTimeInSeconds()-start)/orion.looptimes
      var bytes :double= 0
      var gbps :double= (bytes/len)/(1024*1024*1024)
      var gb :double= bytes / (1024*1024*1024)
      var lt : int = orion.looptimes
      cstdio.printf("loopTimes:%d avgRuntime:%f GB/s:%f GB:%f\n",lt,len,gbps,gb)
    end
    
  end
end