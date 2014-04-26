orion._inputCount = 0

function orion.input( imtype )
  assert(orion.type.isType(imtype))

  -- channel == -1 indicates to use all the channels
  -- channel == 1,2, etc indicates to only use that one channel
  local res  = orion.ast.new({kind="input", channel = -1, type = imtype, baseType = imtype, id=orion._inputCount} ):setLinenumber(0):setOffset(0):setFilename("null_special")
  res:setName("cropSpecial"..(orion._inputCount).."Node")
  orion._inputCount = orion._inputCount + 1

  -- this is kind of a trick: crop from special node will propagate
  res = orion.ast.new({kind="crop", expr=res, mode=orion.cropSame}):setLinenumber(0):setOffset(0):setFilename("null_special")

  return res
end

orion._tapcount = 0
function orion.tap(ty)
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

orion._tapLUTcount = 0
function orion.tapLUT(ty, count, name)
  assert(orion.type.isType(ty))
  assert(orion.type.isArray(ty)==false)
  assert(type(count)=="number")
  assert(type(name)=="string")

  orion._tapLUTcount = orion._tapLUTcount+1
  return orion.ast.new({kind="tapLUT",type=ty,count=count,tapname=name,id=orion._tapLUTcount-1}):setLinenumber(0):setOffset(0):setFilename("null_tapLUT")
end

function orion.frontEnd(ast, imageWidth, imageHeight, options)
  assert(type(options)=="table")

  local typedAST = orion.typedAST.astToTypedAST( ast, imageWidth, imageHeight )

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

  local kernelGraph = orion.kernelGraph.internalIRToKernelGraph(internalIR, scheduleList, options.region, base1, base2)

  return kernelGraph, base1, base2
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

function orion.backEnd(
    kernelGraph, 
    base1, 
    base2, 
    inputWidth, 
    inputHeight, 
    inputImages, 
    options)

  assert(type(options)=="table")
  assert(type(base1)=="number")
  assert(type(base2)=="number")
  assert(type(inputImages)=="table")

  if options.printoptimal then
    orion.printSchedule(scheduledIR)
    print("base1",base1)
    print("base2",base2)
  end

  if orion.verbose or orion.printstage then
    print("schedule nodes",scheduledIR:S("*"):count())
  end

  if options.platform=="cpu" or options.platform == nil then
    
    
    if orion.printstage or orion.verbose then print("start compile") end
    
    local res = orion.terracompiler.compile( 
      kernelGraph, 
      options.region, 
      base1, 
      base2, 
      options.stripcount, 
      inputImages, 
      inputWidth, 
      inputHeight )
    
    if orion.printstage or orion.verbose then 
      print("Start terra compile") 
      print("mem",collectgarbage("count"))
    end
    
    local start = orion.currentTimeInSeconds()
    res:compile() -- we may want to do this later (out of this scope), so that more stuff can be GCed?
    local endt = orion.currentTimeInSeconds()
    
    if orion.printstage or orion.verbose then
      print("compile time",(endt-start))
    end

    if orion.printstage or orion.verbose then print("Terra compile done") end
return res

  elseif options.platform=="convolution" then
    local convIR = orion.convIR.convert(scheduledIR)
return orion.convolution.synth(convIR, base1, base2, options)
  else
    assert(false)
  end

end

function orion.compile(inputImageFunctions, outputImageFunctions, tapInputs, inputWidth, inputHeight, options)

  local function checkinput(tab)
    if type(tab)~="table" then
      print("Error, input to orion.compile must be an array of image functions")
    end

    for k,v in ipairs(tab) do
      if tab[k]==nil or orion.ast.isAST(tab[k])==false then
        print("Error, orion.compile expects an array of image functions. Not a single image function.")
      end  
    end
    
    if #tab==0 then
      print("Error, orion.compile expects an array of image functions. Not a single image function.")
    end
  end

  checkinput(inputImageFunctions)
  checkinput(outputImageFunctions)

  assert(type(tapInputs)=="table")
  assert(type(inputWidth)=="number")
  assert(type(inputHeight)=="number")
  assert(type(options)=="table" or options==nil)

  -- make it so that we can pipe stdout to a file and 
  -- see results before the compile is totally complete
  io.stdout:setvbuf("no") 

  if orion.printstage then
    print("start compile")
  end


  -- fill in defaults
  if options==nil then options={} end
  if options.region==nil then options.region="default" end
  assert(options.region=="default" or options.region=="centered" or options.region=="specify")
  if options.platform==nil then options.platform="cpu" end
  assert(options.platform=="cpu" or options.platform=="convolution")
  if options.debug ~= nil then assert(type(options.debug)=="boolean"); orion.debug = options.debug; end
  if options.debugimages ~= nil then assert(type(options.debugimages)=="boolean"); orion.debugimages = options.debugimages; end
  if options.verbose ~= nil then assert(type(options.verbose)=="boolean"); orion.verbose = options.verbose; end
  if options.printruntime ~= nil then assert(type(options.printruntime)=="boolean"); orion.printruntime = options.printruntime; end
  if options.looptimes ~= nil then assert(type(options.looptimes)=="number"); orion.looptimes = options.looptimes; end
  if options.printasm ~=nil then assert(type(options.printasm)=="boolean"); orion.printasm = options.printasm; end
  if options.printschedule ~=nil then assert(type(options.printschedule)=="boolean"); end
  if options.printoptimal ~=nil then assert(type(options.printoptimal)=="boolean"); end
  if options.printstage ~=nil then assert(type(options.printstage)=="boolean"); orion.printstage = options.printstage; end
  if options.stripcount == nil then options.stripcount=orion.tune.cores end
  if options.fastmath ~= nil then assert(type(options.fastmath)=="boolean"); orion.fastmath = options.fastmath; end
  if options.ilp ~= nil then assert(type(options.ilp)=="boolean"); orion.ilp = options.ilp; end

  -- do the compile
  local newnode = {kind="outputs"}
  for k,v in ipairs(outputImageFunctions) do
    newnode["expr"..k] = v
  end
  local ast = orion.ast.new(newnode):setLinenumber(0):setOffset(0):setFilename("null_outputs")

  local scheduledIR, base1, base2 = orion.frontEnd(
    ast, 
    inputWidth, 
    inputHeight, 
    options)

  return orion.backEnd(
    scheduledIR, 
    base1, 
    base2, 
    inputWidth, 
    inputHeight, 
    inputImageFunctions, 
    options)
end