cmath = terralib.includec("math.h")
cstdio = terralib.includec("stdio.h")
cassert = terralib.includec("assert.h")
cstdlib = terralib.includec("stdlib.h")
cpthread = terralib.includec("pthread.h")

orion.terracompiler = {}

terralib.require("terracompiler_codegen")

-- convert a separate R,G,B image into a single RGB image
function orion.terracompiler.toAOS(channels, ty)
  assert(type(channels)=="number")
  assert(terralib.types.istype(ty))

  local symbs = {}
--  local strideSymbs = {}
  -- first C symbols are the pointers to the inputs,
  -- second C symbols are the strides of the inputs
  -- C is the number of channels
  for i=1,channels do table.insert(symbs,symbol(&ty)) end
  for i=1,channels do table.insert(symbs,symbol(int)) end

  local res = symbol(&ty)
  local c = symbol(int)
  local strideSymb = symbol(int)

  local kernel = {}
  local nextLine = {}
  
  for i=1,channels do
    table.insert(kernel, quote if c==(i-1) then @[res] = @[symbs[i]]; [symbs[i]] = [symbs[i]]+1; end end)
    table.insert(nextLine, quote [symbs[i]] = [symbs[i]] - strideSymb + [symbs[channels+i]]; end)
  end

  return 
    terra([strideSymb], height:int, [symbs])
    if orion.verbose then cstdio.printf("TO AOS\n") end
    var [res] = [&ty](cstdlib.malloc(channels*strideSymb*height*sizeof(ty)))
    var ores = res

    for y=0,height do
      for x=0,strideSymb do
        for [c]=0,channels do
          kernel
          res = res + 1
        end
      end
      nextLine
    end


    return ores
    end
end

-- convert from AoS to SoA
function orion.terracompiler.toSOA(channels, ty)
  assert(type(channels)=="number")
  assert(channels>0)
  assert(terralib.types.istype(ty))

  local ptr = &ty
  return 
    terra(width:int,
          height:int,
          index:int,
      from : ptr,
      to : ptr)

      if orion.verbose then cstdio.printf("TO SOA\n") end
      orionAssert(index < channels, "index must be < channels")

      for y=0,height do
        for x=0,width do
          for c=0,channels do
            if c==index then @to=@from; to=to+1; end
            from = from + 1
          end
        end
      end
    end
end

-- result is a scheduledIR graph of terra nodes.
-- terra nodes contain the terra functions for the left, right, middle, and all 
-- strips. Calculate how many strips we need based on the size, and
-- launche the kernels in an order that satisfies dependencies.
function orion.terracompiler.generateExecutable(
  result,
  regionMode,
  outputTypes,
  base1,
  base2,
  stripWidths,
  model)

  assert(orion.scheduledIR.isScheduledIR(result))
  assert(type(regionMode)=="string")
  assert(type(outputTypes)=="table")
  assert(type(base1)=="number")
  assert(type(base2)=="number")
  assert(type(stripWidths)=="table")
  --assert(type(model)=="table") -- model can be nil

  if orion.verbose then print("generateExecutable") end

  -- calculate the largest type we used
  local largestTypeQuote = `0

  assert(keycount(outputTypes)>0)
  for t,_ in pairs(outputTypes) do
    assert(orion.type.isType(t))
    largestTypeQuote = `orion.cropIR.max(sizeof([orion.type.toTerraType(t,false)]),largestTypeQuote)
  end

  -----
  local runKernels = {}
  local setRegisterSize = {}


  result:S("toSOAConcrete"):traverse(
    function(node)
      local boundImage = orion._boundImages[node.special+1]
      local soa = orion.terracompiler.toSOA(orion.type.arrayLength(boundImage.type),node.outputImage.terraType)

      if orion.debug then
        table.insert(runKernels, quote 
                     if orion._boundImagesRuntime:get(node.special).active==false then
                       cstdio.printf("Nothing bound to image id %d\n",node.special)
                       orionAssert(false, "Unbound image")
                     end
      end)
  
end

      table.insert(runKernels, quote 


                     var start = orion.currentTimeInSeconds()
      [node.outputImage:alloc()];
      soa([node.outputImage:width()],
          [node.outputImage:height()],
          [node.index],
          [&node.outputImage.terraType](orion._boundImagesRuntime:get([node.special]).image.data),
          [node.outputImage:getPointer()])
                       var len : double = (orion.currentTimeInSeconds()-start)

                     if orion.printruntime then
                       cstdio.printf("toSOA %s runtime:%f\n",[node:name()],len)
                     end
    end)
    end)

  result:S("terra"):traverse(
    function(node)
      table.insert(setRegisterSize, quote orion.runtime.setRegisterSize([node.loop.outputNeededRegionUnion:growToNearestX(orion.tune.V):getArea()]*sizeof(double)) end)

  local modelTime = 0
  if model then
    assert(type(model.nodes)=="table")
    assert(model.nodes[node.loop:name()]~=nil)
    modelTime = model.nodes[node.loop:name()].time
  end

  table.insert(runKernels, 
                 quote 
                     
                     var start = orion.currentTimeInSeconds()

                     node.preRun()

                     var threads : cpthread.pthread_t[orion.tune.cores]
                     var stripStore : int[orion.tune.cores]

                     for i=0, orion.looptimes do
                       -- launch the kernel for each strip
                       
                       if orion.tune.cores==1 then
                         -- don't launch a thread to save thread launch overhead
                         stripStore[0]=0
                         node.all(&stripStore[0])
                       else
                         for i=0,orion.tune.cores do
--                           cstdio.printf("Launch %d\n",i)
                           stripStore[i] = i
                           cpthread.pthread_create(&threads[i],nil,node.all,&stripStore[i]);
                         end
                         for i=0,orion.tune.cores do
                             cpthread.pthread_join(threads[i],nil)
--                             cstdio.printf("join %d\n",i)
                         end
                       end
                     end

                     node.postRun()

                     if orion.printruntime then
                       var len : double = (orion.currentTimeInSeconds()-start)/orion.looptimes
                       var bytes :double= [node.loop:bytes()]
                       var gbps :double= (bytes/len)/(1024*1024*1024)
                       var gb :double= bytes / (1024*1024*1024)
                       var lt : int = orion.looptimes
                       var mod : float = modelTime
                       var modAccur : float = mod/len
                       cstdio.printf("%s loopTimes:%d avgRuntime:%f GB/s:%f GB:%f model:%f modelFit:%f\n",[node.loop:name()],lt,len,gbps,gb,mod,modAccur)
                     end
                   end)
  end)

  result:S("terra"):traverse(
    function(node)
      if orion.printasm then 
        print("Kernel",node.loop:name())
        node.all:printpretty() 
        node.all:disas()
      end
    end)


  local function returnToAOS(result)
    assert(orion.scheduledIR.isScheduledIR(result.child1))
    assert(result.child1.kind=="index")
    assert(result.child1.child1.kind=="terra")
    local outputImage = result.child1.child1.loop["outputImage"..result.child1.index]
    assert(orion.imageWrapper.isImageWrapper(outputImage))

    local channels = result:arraySize("child")
    local aosfn = orion.terracompiler.toAOS(channels, outputImage.terraType)
    
    local inpList = {}
    local strideList = {}
    
    local ty = outputImage.terraType
    local orionType = outputImage.orionType
    local isFloat = orion.type.isFloat(orionType)
    local isSigned = orion.type.isInt(orionType)

    local releaseList = {}

    result:map("child",
               function(n)
                 assert(n.kind=="index")
                 assert(n.child1.kind=="terra")
                 table.insert(inpList,`[&ty]([n.child1.loop["outputImage"..n.index]:getPointer()]))
                 table.insert(strideList, n.child1.loop["outputImage"..n.index]:widthStored())
                  -- we don't need to call releaseAndKeep here b/c we're making a deep copy ourselves
                 table.insert( releaseList, n.child1.loop["outputImage"..n.index]:release() )
               end)
    
    assert(#inpList == #strideList)
    assert(#inpList == channels)

    -- work around a terra bug?
    local merged={}
    for k,v in pairs(inpList) do table.insert(merged,v) end
    for k,v in pairs(strideList) do table.insert(merged,v) end

    local outputSymb = symbol(Image)
    return outputSymb, quote
                     var start = orion.currentTimeInSeconds()
      var width = [outputImage:width()];
      var height = [outputImage:height()];
      var stride = [outputImage:widthStored()];
      var channels : int = channels
      var bits : int = sizeof(ty)*8
      var data : &opaque = aosfn(stride,height,merged);
      outputSymb:init( width, height, stride, channels, bits, isFloat, isSigned, data, data);
      releaseList
                       var len : double = (orion.currentTimeInSeconds()-start)
      if orion.printruntime then
        cstdio.printf("toAOS channels:%d runtime:%f\n",channels,len)
      end
    end
  end

  local function returnIndex(result)
    assert(orion.scheduledIR.isScheduledIR(result.child1))
    assert(result.child1.kind=="terra")
    assert(orion.imageWrapper.isImageWrapper(result.child1.loop["outputImage"..result.index]))
    local outputSymb = symbol(Image)

    return outputSymb, quote 
      [result.child1.loop["outputImage"..result.index]:toImage( outputSymb )];
      [result.child1.loop["outputImage"..result.index]:releaseAndKeep()] 
  end
  end

  local outputQuote
  local outputSymb

  if result.kind=="toAOS" then
    outputSymb, outputQuote = returnToAOS(result)
  elseif result.kind=="multiout" then
    outputQuote = {}
    outputSymb = {}

    result:map("child",
               function(n,i)

                 local os, oq
                 if n.kind=="toAOS" then
                   os, oq = returnToAOS(n)
                 elseif n.kind=="index" then
                   os, oq = returnIndex(n)
                 else
                   assert(false)
                 end

                 table.insert(outputSymb,os)
                 table.insert(outputQuote,oq)
               end)

    assert(#outputQuote>1)
  elseif result.kind=="index" then
    outputSymb, outputQuote = returnIndex(result)
  else
    assert(false)
  end


  local terra fin()
    setRegisterSize;
    runKernels;

    -- now wrap the result register in an Image object
    var [outputSymb]
    outputQuote

    orionAssert(orion.runtime.activeRegisters()==0, "didn't free all registers!")
    return outputSymb
  end

  return fin
end

-- this should return a terra function that when called executes the pipeline
function orion.terracompiler.compile(rootLoopIR, regionMode, base1, base2, stripWidths, model)
  if orion.verbose then print("compile") end
  assert(orion.scheduledIR.isScheduledIR(rootLoopIR))
  assert(type(regionMode)=="string")
  assert(type(base1)=="number")
  assert(type(base2)=="number")
  assert(type(stripWidths)=="table")
  --assert(type(model)=="table") -- model can be nil

  -- we need to know the largest type we use to size the runtime registers
  local outputTypes = {}

  local result = rootLoopIR:S("loop"):process(
    function(node)
      assert(node.kind=="loop")

      node.loop:map("outputType", 
               function(t)
                 outputTypes[t] = 1
               end)

      -- shallowcopy will grab loop, children
      -- we'll need some settings out of loop to call the kernel (strip width for ex)
      local newNode = node:shallowcopy()
      newNode.kind = "terra"
      newNode.all, newNode.preRun, newNode.postRun = orion.terracompiler.toTerraKernel( node.loop, stripWidths[node.loop] )

      return orion.scheduledIR.new(newNode):copyMetadataFrom(node)
    end)

  result:check()

  return orion.terracompiler.generateExecutable(
    result, 
    regionMode,
    outputTypes,
    base1,
    base2,
    stripWidths,
    model)

end