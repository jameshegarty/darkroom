terralib.require("vector")
--local leak = require "leak"

-- there are two structures that store info about bound images:
--
-- _boundImages holds stuff that's known at compile time. Ex: type
-- must be known at compile time, width/height if it's statically specified etc.
--
-- _boundImagesRuntime holds the stuff that's only know at runtime.
-- ie sizes at runtime.
-- It is initialized with some info from the compile time struct to make sure
-- we don't violate some things at runtime (like that the # of channels must match)
-- that's why it holds w/h/c in both RuntimeImage and the Image that it holds.
-- Image contains the currently active loaded instance, RuntimeImage.w/h/c contain the interface it must match.
-- active: is the image valid?
--
-- _boundImages is 1 indexed, but the boundImageIds are 0 indexed!!!! and _boundImagesRuntime is 0 indexed!
orion._boundImages={}
-- width, height, channels, bits are sizes the image we load in must match.
-- if any of them are 0 it means this parameter is free
struct RuntimeImage { image : Image, width: int, height : int, channels : int, bits : int, floating: bool, isSigned : bool, active:bool }
-- if we save out the pipeline, we need to serialize this, b/c it tells us 
-- what input formats we expect.
orion._boundImagesRuntime = global(Vector(RuntimeImage))

local C = terralib.includecstring [[
#include <sys/time.h>

  double CurrentTimeInSecondsTTTT() {
  struct timeval tv;
  gettimeofday(&tv, 0);
  return tv.tv_sec + tv.tv_usec / 1000000.0;
                                       }

                                   ]]

terra orion.init()
  if orion.verbose then cstdio.printf("orion.init\n") end
  orion._boundImagesRuntime:init()
end

orion.init()

-- convenience function. Loads an image and returns it as an orion function
-- it only makes sense to call this guy at compile time
function orion.load(filename, boundaryCond)
  assert(type(filename)=="string")

  if orion.verbose then print("Load",filename) end

  local terra makeIm( filename : &int8)
    var im : &Image = [&Image](cstdlib.malloc(sizeof(Image)))
    im:initWithFile(filename)

    return im
  end
  
  local im = makeIm(filename)
  
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

  local idast = orion.image(_type,im.width,im.height)
  orion._boundImages[idast.expr.id+1].filename = filename -- for the conv engine
  orion.bindImage(idast.expr.id,im)

  local terra freeIm(im:&Image)
    im:free()
    cstdlib.free(im)
  end
  freeIm(im)

  return idast
end

function orion.loadRaw(filename, w,h,bits,header,flipEndian)
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

function orion.image( imtype, width, height )
  assert(orion.type.isType(imtype))
  assert(type(width)=="number" and width > 0)
  assert(type(height)=="number" and height > 0)

  local res = {}
  res.type = imtype
  res.width = width
  res.height = height

  local channels = orion.type.arrayLength(imtype)
  if channels == 0 then channels=1 end

  local baseImtype = imtype
  if orion.type.isArray(baseImtype) then baseImtype = orion.type.arrayOver(baseImtype) end

  local ttype
  if channels == 1 then
    ttype = orion.type.toTerraType(imtype)
  elseif channels>1 then 
    ttype = orion.type.toTerraType(orion.type.arrayOver(imtype)) 
  end

  local terra addit(width:int, height:int, channels:int, isFloat:bool, isSigned : bool)
    var im : Image
    orion._boundImagesRuntime:push(RuntimeImage { image=im; width=width; height=height; channels=channels; bits=sizeof(ttype)*8; active=false; floating = isFloat, isSigned = isSigned})
  end
  local awidth = width
  local aheight = height
  -- if w/h aren't specified we fill them in with 0 (indicates that's a free parameter)
  if awidth==nil then awidth = 0 end
  if aheight==nil then aheight = 0 end
  addit(awidth,aheight,channels, orion.type.isFloat(baseImtype), orion.type.isInt(baseImtype))


  table.insert(orion._boundImages,res)
  res.ast = orion.ast.new({kind="special", id=#orion._boundImages-1} ):setLinenumber(0):setOffset(0):setFilename("null_special")
  res.ast:setName("cropSpecial"..(#orion._boundImages-1).."Node")

  -- this is kind of a trick: crop from special node will propagate
  res.ast = orion.ast.new({kind="crop", expr=res.ast, mode=orion.cropSame}):setLinenumber(0):setOffset(0):setFilename("null_special")

  return res.ast
end

-- can call this at runtime or compile time
terra orion.bindImage(boundId : int, im : &Image)
  
  orionAssert( boundId < orion._boundImagesRuntime:count() and boundId>=0, "bindImage id out of bound")
  
  -- check if the input image has the right width, height, etc
  if orion._boundImagesRuntime:get(boundId).width>0 then
    orionAssert(orion._boundImagesRuntime:get(boundId).width == im.width, "incorrect bind width")
    orionAssert(orion._boundImagesRuntime:get(boundId).height == im.height, "incorrect bind height")
  end

  orionAssert(orion._boundImagesRuntime:get(boundId).bits == im.bits, "bindImage bits")
  orionAssert(orion._boundImagesRuntime:get(boundId).channels == im.channels, "channels")
  orionAssert(orion._boundImagesRuntime:get(boundId).floating == im.floating, "floating")
  orionAssert(orion._boundImagesRuntime:get(boundId).isSigned == im.isSigned, "bound image should match sign in orion.bindImage" )

  -- was something already bound to this image? need to free it
  if orion._boundImagesRuntime:get(boundId).active then
    orion._boundImagesRuntime:get(boundId).image:free()
  end

  orion._boundImagesRuntime:getPtr(boundId).active = true
  orion._boundImagesRuntime:getPtr(boundId).image = im:deepcopyUnstride()

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
    
    var im : Image
    im:init(w,h,w,c,sizeof(baseTerraType)*8,isFloat,isSigned,[&uint8](data),[&uint8](data))
    
    if orion.verbose then cstdio.printf("Bind Constant %d w %d h %d bits %d\n",boundId, w,h,sizeof(baseTerraType)*8) end
    
    -- was something already bound to this image? need to free it
    if orion._boundImagesRuntime:get(boundId).active then
      orion._boundImagesRuntime:get(boundId).image:free()
    end
    
    orion._boundImagesRuntime:getPtr(boundId).active = true
    orion._boundImagesRuntime:getPtr(boundId).image = im
  end
end

orion.bindConstantFloat32 = makeBindConstant(orion.type.float(32))
orion.bindConstantInt32 = makeBindConstant(orion.type.int(32))
orion.bindConstantUint8 = makeBindConstant(orion.type.uint(8))
orion.bindConstantArray2Float32 = makeBindConstant(orion.type.array(orion.type.float(32),2))
orion.bindConstantArray3Float32 = makeBindConstant(orion.type.array(orion.type.float(32),3))
orion.bindConstantArray3Uint8 = makeBindConstant(orion.type.array(orion.type.uint(8),3))
orion.bindConstantArray2Uint8 = makeBindConstant(orion.type.array(orion.type.uint(8),2))

function orion.constant(ty, width, height, constantValue)
  assert(orion.type.isType(ty))
  assert(type(width)=="number")
  assert(type(height)=="number")
  assert(type(constantValue)=="number")
  
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

orion._tapcount = 0
orion._usedTapNames={}

function orion.tap(ty, name)
  assert(orion.type.isType(ty))
  assert(orion.type.isArray(ty)==false)
  assert(type(name)=="string")

  if orion._usedTapNames[name]~=nil then
    print("Error, tap name ",name," was used twice, which isn't allowed!")
    assert(false)
  end
  orion._usedTapNames[name] = 1

  orion._tapcount = orion._tapcount+1
  return orion.ast.new({kind="tap",type=ty,tapname=name,id=orion._tapcount-1}):setLinenumber(0):setOffset(0):setFilename("null_tap")
end

function orion.setTap(ast,value)
  assert(orion.ast.isAST(ast))

  local terraType = orion.type.toTerraType(ast.type)

  local terra setit(value : terraType)
    var tptr : &terraType = [&terraType]( cstdlib.malloc(sizeof(terraType)) )
    @tptr = value
    orion.runtime.setTap(ast.id, [&int8](tptr))
  end

  setit(value)
end

function orion.getTap(ast)
--  assert(orion.ast.isAST(ast) or orion.convIR.isConvIR(ast))
  local terraType = orion.type.toTerraType(ast.type)

  local terra getit(id:int) : terraType
    var v : &terraType = [&terraType](orion.runtime.getTap(id))
    return @v
  end

  return getit(ast.id)
end

orion._tapLUTcount = 0
function orion.tapLUT(ty, count, name)
  assert(orion.type.isType(ty))
  assert(orion.type.isArray(ty)==false)
  assert(type(count)=="number")
  assert(type(name)=="string")

  orion._tapLUTcount = orion._tapLUTcount+1
  return orion.ast.new({kind="tapLUT",type=ty,count=count,tapname=name,id=orion._tapLUTcount-1}):setLinenumber(0):setOffset(0):setFilename("null_tapLUT")
end

-- even though the LUT will be 0 indexed, value is 1 indexed
-- nice one
function orion.setTapLUT(ast,value)
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

function orion.frontEnd(ast, options)
  assert(type(options)=="table")

  local typedAST = orion.typedAST.astToTypedAST( ast, options.precision=="cpu" )

  if options.platform=="cpu" then
    -- on cpu we turn vector operations into struct of array form
    -- (we eliminate vectors)
    typedAST = orion.optimize.toSOA(typedAST)
  end

  local internalIR,base1,base2 = orion.internalIR.typedASTToInternalIR(typedAST)

  -- optimize
  internalIR = orion.optimize.optimize(internalIR)

  -- determine the set of nodes to make scheduling decisions on
  local scheduleList
  
  if options.schedule=="materializeall" then
    scheduleList = orion.schedule.allNodes(internalIR)
  else
    scheduleList = orion.schedule.allStencilNodes(internalIR)
  end

  scheduleList = orion.schedule.addNecessaryNodes(internalIR, scheduleList)

  local scheduledIR = orion.scheduledIR.internalIRToScheduledIR(internalIR, scheduleList, options.region, base1, base2)

  return scheduledIR, base1, base2
end

function orion.backEndSimulatedAnnealing(scheduledIR, base1, base2, options)
  local bestTime = 100000000
  local bestLoopIR

  local nodeCount = scheduledIR:S("*"):count()

  while 1 do
    -- always make at least 2 rungroups...
    local rungroupCount = math.random(2,nodeCount/4)
    print("RG",rungroupCount)
    local schedule = orion.schedule.initialSeedSchedule(scheduledIR, rungroupCount)

    -- annealing or something
    local bestTimeThisStep

    for i=1,10 do

      collectgarbage("collect")
      collectgarbage("collect")
      print("MEM",collectgarbage("count"))


      local newSchedule = schedule
      if i>1 then
        newSchedule = orion.schedule.annealingStep(scheduledIR,schedule)
        
        -- we can't figure out a way to change this
        if newSchedule==nil then
break
        end
      end

      local loopIR = orion.loopIR.convert(scheduledIR, newSchedule, options.region, base1, base2)

      if loopIR==nil then
        print("impossible schedule")
        -- accept it anyway sometimes
        if math.random()>0.5 then
          schedule = newSchedule
        end
      else
        local stripWidths = orion.schedule.stripWidthBrute( loopIR )
        local stripWidthsBad = orion.schedule.stripWidthHeuristic( loopIR )

        local time

        local perfModel = orion.schedule.computePerfModel( loopIR, stripWidths )
        local perfModelBad = orion.schedule.computePerfModel( loopIR, stripWidthsBad )

        if false then
          local tprog = orion.terracompiler.compile( loopIR, options.region, base1, base2, stripWidths )
          tprog:compile()

          local looptimes = 10
          local terra doit()
            var res = tprog()
            res:free()
            
            var start = C.CurrentTimeInSecondsTTTT()
            
            for i=0,looptimes do
              res = tprog()
              if i<looptimes-1 then -- let the last one leak so we can save it
                res:free()
              end
            end
            var endt = C.CurrentTimeInSecondsTTTT()
            return (endt-start)/looptimes
          end

          time = doit()
        else
          time = perfModel.total
        end

        if time < bestTime then
          bestTime = time
          bestLoopIR = loopIR
        end

        if bestTimeThisStep==nil or 
          time<bestTimeThisStep or
          math.random()>0.5 then
          -- accept it
          schedule = newSchedule
        end

        print("thisTime",time,"modelTime",perfModel.total,"badStripWTime",perfModelBad.total,"bestTime",bestTime)
      end
    end
  end
end

function orion.backEndIter(scheduledIR, base1, base2, options)


   print("Node Count",scheduledIR:S("*"):count())
  print("runtime model fModel heap rungroups avgStripCount")

  assert(options.looptimes==1 or options.looptimes==nil)

  for i=1,100 do
    options.schedule = orion.schedule.randomSchedule(scheduledIR, math.random())
    local i=0
    while orion.schedule.fix(scheduledIR, options.schedule) and i <10 do i = i+1 end
    if i==10 then print("could not fix") end
    options.stripwidth = orion.schedule.stripWidthUniform( options.schedule )

    local function makeScheduleString()
      local scheduleStr = "explicit:"
      for k,v in pairs(options.schedule) do
        scheduleStr = scheduleStr .. k:name() .. "-" .. v.rungroup ..";"
      end
      
      scheduleStr = scheduleStr .. "&"
      for k,v in pairs(options.stripwidth) do
        scheduleStr = scheduleStr .. k .. "-" .. v ..";"
      end
    end

    local rgs = {}
    local avgStripWidth = 0
    for k,v in pairs(options.schedule) do
      if rgs[v.rungroup]==nil then
        avgStripWidth = avgStripWidth + options.stripwidth[v.rungroup]
      end

      rgs[v.rungroup] = 1
    end
    local rgCount = keycount(rgs)
    avgStripWidth = avgStripWidth / rgCount

    local res, perfModel = orion.backEndSimple(scheduledIR, base1, base2, options)

    if res~=nil then

      local outCount = 1
      if scheduledIR.kind=="multiout" then
        outCount = scheduledIR:arraySize("child")
      end
      
      local outVars = {}
      local freeit = {}
      for i=1,outCount do 
        table.insert(outVars,symbol(Image))
        table.insert(freeit,quote [outVars[i]]:free() end)
      end

      local N = 3
      local terra timeit()
        var start = C.CurrentTimeInSecondsTTTT()
        for i=0,N do
          var [outVars] = res()
          freeit
        end
        var endt = C.CurrentTimeInSecondsTTTT()

        return (endt-start)/double(N)
      end

      print(timeit().." "..perfModel.total.." "..perfModel.fast.total.." "..collectgarbage("count").." "..rgCount.." "..avgStripWidth)
    else
      print("invalid")
    end
  end

end

function orion.getScheduleTable(scheduledIR, schedule)
  local tab = {}
  scheduledIR:visitEach(
    function(node)
      local thisRungroup = 1

      if schedule then
        thisRungroup = schedule[node].rungroup
      end

      if node.kind=="toAOS" then
        -- toAOS doesn't require a LB
      elseif node.kind=="single" or
        node.kind=="toSOA" then

        local typesize = 0

        if node.kind=="single" then
          typesize = node.kernel.type:sizeof()
        elseif node.kind=="toSOA" then
          typesize = node.type:sizeof()
        else
          assert(false)
        end

        local t = {__typesize = typesize}

        local consumedExternally = false
        for consumer,_ in node:parents(scheduledIR) do
          if schedule then
            local consumerRungroup = schedule[consumer].rungroup
            if consumerRungroup~=thisRungroup then consumedExternally=true end
          end
        end

        if consumedExternally==false then
          for consumer,_ in node:parents(scheduledIR) do
            
            if consumer.kind=="single" then
              local firstLine = 0
              local lastLine = 0
              
              local s = consumer.kernel:stencil(node)

              if s:area()>0 then
                firstLine = s:min(2)
                lastLine = s:max(2)
              end
              
              t[consumer:name()] = {firstLine = firstLine, lastLine = lastLine}
            elseif consumer.kind=="multiout" then
              -- this leads to a false dependency
            elseif  consumer.kind=="toAOS" then
              -- no stencil
            else
              print(consumer.kind)
              assert(false)
            end
          end
        end

        tab[thisRungroup] = tab[thisRungroup] or {}
        tab[thisRungroup][node:name()] = t
      elseif node.kind=="multiout" then
        print("outputs:")
        for k,v in node:children() do
          print(v:name())
        end
      else
        print(node.kind)
        assert(false)
      end
    end)

  return tab
end

function orion.printSchedule(scheduledIR,schedule)
  local tab = orion.getScheduleTable(scheduledIR,schedule)
  print("{")
  print(table_print(tab))
  print("}")
end

function orion.backEndSimple(scheduledIR, base1, base2, options)
  assert(type(options)=="table")
  assert(type(base1)=="number")
  assert(type(base2)=="number")

  if options.printoptimal then
    orion.printSchedule(scheduledIR)
    print("base1",base1)
    print("base2",base2)
  end

  if orion.verbose or orion.printstage then print("mem",collectgarbage("count")) end
  collectgarbage("collect")
  collectgarbage("collect")
  if orion.verbose or orion.printstage then print("mem",collectgarbage("count")) end

  if orion.verbose or orion.printstage then
    print("schedule nodes",scheduledIR:S("*"):count())
  end

  if options.platform=="cpu" or options.platform == nil then
    
    local fperfS
    local fperfLuaState
    if options.calcPerfModel and options.fastPerfModel then
      fperfS = orion.fperf.start()
      fperfLuaState = fperfS:uploadScheduledIR(scheduledIR)
      if orion.verbose or orion.printstage then print("uploadScheduledIR done") end
    end

    while 1 do
      -- perform scheduling heuristics common to CPU/conv
      local schedule

      if type(options.schedule)=="table" then
        -- explicitly passed
        schedule = options.schedule
      elseif options.schedule:sub(1,6)=="random" then
        schedule = orion.schedule.randomSchedule(scheduledIR,tonumber("0."..options.schedule:sub(7)))
      elseif options.schedule:sub(1,6)=="greedy" then
        schedule = orion.schedule.greedySchedule(scheduledIR,tonumber(options.schedule:sub(7)))
      elseif options.schedule:sub(1,8)=="explicit" then
        local ss = options.schedule:sub(10)
        print(ss)
        local arr = explode("&",ss)
        local scheduleStr = arr[1]
        local stripCountStr = arr[2]
        print(scheduleStr)
        print(stripCountStr)
        local scheduleE = explode(";",scheduleStr)
        local stripCount = explode(";",stripCountStr)

        local scheduleHash = {}
        for k,v in pairs(scheduleE) do
          local s = explode("-",v)
          scheduleHash[s[1]] = tonumber(s[2])
        end

        schedule = {}
        scheduledIR:S("*"):traverse(
          function(n)
            print(n:name())
            assert(scheduleHash[n:name()]~=nil)
            schedule[n] = {rungroup=scheduleHash[n:name()]}
          end)

        options.stripwidth = {}
        for k,v in pairs(stripCount) do
          if v~="" then
            local s = explode("-",v)
            options.stripwidth[tonumber(s[1])] = tonumber(s[2])
          end
        end
      elseif options.schedule=="linebufferall" then
        schedule = orion.schedule.allLB(scheduledIR)
      elseif options.schedule=="default" or options.schedule=="materialize" or options.schedule=="materializeall" then
        schedule = orion.schedule.allMaterialized(scheduledIR)
      elseif options.schedule=="uniform" then
        schedule = orion.schedule.uniformSchedule(scheduledIR)
      else
        assert(false)
      end

      if options.printschedule then
        orion.schedule.writeJSON(scheduledIR, schedule)      
      end

      local loopIR = orion.loopIR.convert(scheduledIR, schedule, options.region, base1, base2)

      if loopIR==nil then
        -- this was an unsatisfiable schedule, loop and try again
        if (type(options.schedule)=="string" and options.schedule:sub(1,6)=="random") then

        else
          --print("Unsatisfiable schedule!")
return nil
        end
      else
        local stripWidths
        local stripWidth
        local stripWidthFastTable
        if type(options.stripwidth)=="table" then
          -- explicitly passed

          -- need to convert keys from rungroup id to loopir
          stripWidths = {}
          loopIR:visitEach(
            function(n)
              if n.kind=="loop" then
                assert(type(options.stripwidth[n.loop.rungroup])=="number")
                stripWidths[n.loop] = options.stripwidth[n.loop.rungroup]
              end
            end)

          stripWidth = -1
          stripWidthFastTable = options.stripwidth
        elseif type(options.stripwidth)=="number" then
          stripWidths = orion.schedule.stripWidthExplicit( loopIR, options.stripwidth )
          stripWidth = options.stripwidth
        elseif options.stripwidth=="default" then
          stripWidths = orion.schedule.stripWidthHeuristic( loopIR )
          stripWidth = orion.tune.cores
        else
          assert(false)
        end

        local perfModel
        if options.calcPerfModel then
          perfModel = orion.schedule.computePerfModel( loopIR, stripWidths )

          if options.fastPerfModel then
            perfModel.fast = {}
            local precision = 0.0000001
            local function checkit(name,a,b)
              if math.abs(a-b) > precision then
                print("check failed ",name,a,b)
                --              assert(false)
              end
            end
            
            
            local fschedule, fstripcounts = orion.fperf.luaToFastSchedule(fperfLuaState,scheduledIR, schedule, stripWidthFastTable)
            local fValid, fTotalTime, fComputeTime, fMainMemoryTime, fMainMemoryTraffic, fLbTime, fBoundaryPixels, fWorkingSet, fLbReadGB, fLbWriteGB, fTotalArea
            
            local st = terralib.currenttimeinseconds()
            local N = 2 -- by default, make this 2, so that we make sure we can run this multiple iter and still get correct results
            for i=1,N do
              fValid, fTotalTime, fComputeTime, fMainMemoryTime, fMainMemoryTraffic, fLbTime, fBoundaryPixels, fWorkingSet, fLbReadGB, fLbWriteGB, fTotalArea = unpackstruct(fperfS:eval(fschedule, stripWidth, fstripcounts))
              perfModel.fast.valid = fValid
              perfModel.fast.computeTime = fComputeTime
              perfModel.fast.mainMemoryTime = fMainMemoryTime
              perfModel.fast.mainMemoryTraffic = fMainMemoryTraffic
              perfModel.fast.lbTime = fLbTime
              perfModel.fast.lbReadGB = fLbReadGB
              perfModel.fast.lbWriteGB = fLbWriteGB
              perfModel.fast.boundaryPixels = fBoundaryPixels
              perfModel.fast.workingSet = fWorkingSet
              perfModel.fast.totalArea = fTotalArea
              perfModel.fast.total = fTotalTime
              
              checkit("mainMemoryTraffic",perfModel.fast.mainMemoryTraffic,perfModel.mainMemoryTraffic) 
              checkit("lbReadGB",perfModel.fast.lbReadGB,perfModel.lbReadGB) 
              checkit("lbWriteGB",perfModel.fast.lbWriteGB,perfModel.lbWriteGB) 
              checkit("boundaryPixels",perfModel.fast.boundaryPixels,perfModel.boundaryPixels) 
              checkit("workingSet",perfModel.fast.workingSet,perfModel.workingSet) 
              checkit("mainMemoryTime",perfModel.fast.mainMemoryTime,perfModel.mainMemoryTime) 
              checkit("lbTime",perfModel.fast.lbTime,perfModel.lbTime) 
              checkit("totalArea", perfModel.fast.totalArea, perfModel.totalArea) 
            end
            local et = terralib.currenttimeinseconds()
            
            -- if we got this far it should be valid
            assert(fValid)
            
            fperfS:free()
            orion.fperf.free()
          end
        end

        if orion.printstage or orion.verbose then print("start compile") end

        if orion.printstage then print("mem",collectgarbage("count")) end
        collectgarbage("collect")
        collectgarbage("collect")
        if orion.printstage then print("mem",collectgarbage("count")) end

        local res = orion.terracompiler.compile( loopIR, options.region, base1, base2, stripWidths, perfModel )

        if orion.printstage or orion.verbose then 
          print("Start terra compile") 
          print("mem",collectgarbage("count"))
        end
        collectgarbage("collect")
        collectgarbage("collect")

        if orion.printstage then
          print("mem",collectgarbage("count"))
        end

        local start = C.CurrentTimeInSecondsTTTT()
        res:compile() -- we may want to do this later (out of this scope), so that more stuff can be GCed?
        local endt = C.CurrentTimeInSecondsTTTT()

        if orion.printstage or orion.verbose then
          print("compile time",(endt-start))
        end

        if orion.printstage or orion.verbose then print("Terra compile done") end
return res, perfModel
      end
    end
  elseif options.platform=="convolution" then
    local convIR = orion.convIR.convert(scheduledIR)
    return orion.convolution.synth(convIR, base1, base2, options)
  else
    assert(false)
  end

end

function orion.compile(imageFunctions, options)
  assert(type(imageFunctions)=="table")

  -- make it so that we can pipe stdout to a file and 
  -- see results before the compile is totally complete
  io.stdout:setvbuf("no") 

  if orion.printstage then
    print("start compile")
  end

  for k,v in ipairs(imageFunctions) do
    if imageFunctions[k]==nil or orion.ast.isAST(imageFunctions[k])==false then
      print("Error, orion.compile expects an array of image functions. Not a single image function.")
    end  
  end

  if #imageFunctions==0 then
    print("Error, orion.compile expects an array of image functions. Not a single image function.")
  end

  -- fill in defaults
  if options==nil then options={} end
  if options.precision==nil then options.precision="cpu" end
  assert(options.precision=="arbitrary" or options.precision=="cpu")
  if options.region==nil then options.region="default" end
  assert(options.region=="default" or options.region=="centered" or options.region=="specify")
  if options.platform==nil then options.platform="cpu" end
  assert(options.platform=="cpu" or options.platform=="convolution")
  if options.schedule==nil then options.schedule="default" end
  assert(options.schedule=="default" or 
         options.schedule=="materializeall" or 
         options.schedule=="materialize" or 
         options.schedule=="linebufferall" or
         options.schedule=="uniform" or
         options.schedule=="brute" or
         options.schedule=="simulatedAnnealing" or
         options.schedule:sub(1,6)=="greedy" or
         options.schedule:sub(1,8)=="explicit" or
         options.schedule:sub(1,6)=="random")
  if options.debug ~= nil then assert(type(options.debug)=="boolean"); orion.debug = options.debug; end
  if options.debugimages ~= nil then assert(type(options.debugimages)=="boolean"); orion.debugimages = options.debugimages; end
  if options.verbose ~= nil then assert(type(options.verbose)=="boolean"); orion.verbose = options.verbose; end
  if options.printruntime ~= nil then assert(type(options.printruntime)=="boolean"); orion.printruntime = options.printruntime; end
  if options.looptimes ~= nil then assert(type(options.looptimes)=="number"); orion.looptimes = options.looptimes; end
  if options.printasm ~=nil then assert(type(options.printasm)=="boolean"); orion.printasm = options.printasm; end
  if options.printloopir ~=nil then assert(type(options.printloopir)=="boolean"); orion.printloopir = options.printloopir; end
  if options.printschedule ~=nil then assert(type(options.printschedule)=="boolean"); end
  if options.printoptimal ~=nil then assert(type(options.printoptimal)=="boolean"); end
  if options.printstage ~=nil then assert(type(options.printstage)=="boolean"); orion.printstage = options.printstage; end
  if options.stripwidth == nil then options.stripwidth="default" end
  assert(type(options.stripwidth)=="number" or options.stripwidth=="default")
  if options.calcPerfModel == nil then options.calcPerfModel=false end
  if options.fastPerfModel == nil then options.fastPerfModel=options.calcPerfModel end
  if options.straighten == nil then options.straighten=false end
  if options.fastmath ~= nil then assert(type(options.fastmath)=="boolean"); orion.fastmath = options.fastmath; end
  if options.ilp ~= nil then assert(type(options.ilp)=="boolean"); orion.ilp = options.ilp; end

  -- do the compile
  local ast = imageFunctions[1]
  
  if #imageFunctions>1 then
    local newnode = {kind="multiout"}
    for k,v in ipairs(imageFunctions) do
      newnode["expr"..k] = v
    end
    ast = orion.ast.new(newnode):setLinenumber(0):setOffset(0):setFilename("null_multiout")
  end

  local scheduledIR, base1, base2 = orion.frontEnd(ast, options)

  local result, rmodel

  if options.schedule=="default" or
    options.schedule:sub(1,6)=="random" or
    options.schedule:sub(1,8)=="explicit" or
    options.schedule:sub(1,6)=="greedy" or
    options.schedule=="linebufferall" or
    options.schedule=="materialize" or 
    options.schedule=="materializeall" then

    result, rmodel = orion.backEndSimple(scheduledIR, base1, base2, options)
  elseif options.schedule=="brute" or
    options.schedule=="simulatedAnnealing" then
--    result, rmodel = orion.backEndSimulatedAnnealing(scheduledIR, base1, base2, options)

    local fperfS = orion.fperf.start()
    local fperfLuaState = fperfS:uploadScheduledIR(scheduledIR)
    local bestSched
    
    if options.schedule=="brute" then
      bestSched = fperfS:bruteForce(fperfLuaState.edgeId)
    elseif options.schedule=="simulatedAnnealing" then
      local invalidEdges = orion.fperf.noteInvalidEdges(fperfLuaState, scheduledIR)
      bestSched = fperfS:simulatedAnnealing(fperfLuaState.edgeId, invalidEdges)
    end

    options.schedule, options.stripwidth = fperfS:extractLuaSchedule(scheduledIR, fperfLuaState, bestSched)
    result, rmodel = orion.backEndSimple(scheduledIR, base1, base2, options)
  elseif options.schedule=="uniform" then
    result, rmodel = orion.backEndIter(scheduledIR, base1, base2, options)
  else
    assert(false)
  end
  
  return result, rmodel
end