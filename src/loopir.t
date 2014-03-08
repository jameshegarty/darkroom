cstring = terralib.includec("string.h")

loopIRFunctions={}
setmetatable(loopIRFunctions,{__index=IRFunctions})
loopIRMT={__index=loopIRFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}

orion.loopIR = {}

function loopIRFunctions:boundary(kernelID)
  assert(type(kernelID)=="number")
  assert(orion.type.isType(self["outputType"..kernelID]))
  if self["outputType"..kernelID]==orion.type.bool() then
    return `false
  end

  return `0
end

-- call init on all the inputs
function loopIRFunctions:initInputImages(loopid)
  assert(type(loopid)=="number")

  local res = self:map("inputImage"..loopid.."_",function(arrayItem) 
                           assert(orion.imageWrapper.isImageWrapper(arrayItem))
                           arrayItem:init(loopid)
                                      end)
end

function loopIRFunctions:declareInputImages(loopid)
  assert(type(loopid)=="number")

  local res = self:foldl("inputImage"..loopid.."_",function(tab,arrayItem) 
                           assert(type(tab)=="table")
                           assert(orion.imageWrapper.isImageWrapper(arrayItem))
                           table.insert(tab,arrayItem:declare(loopid)) 
                           return tab
                                      end,{})
  assert(#res==self:arraySize("inputImage"..loopid.."_"))
  return res
end

function loopIRFunctions:setInputImagePositions(loopid,x,y,core,stripId,stripCount, stripList, stripSymbolCache,retime)
  assert(type(loopid)=="number")
  assert(type(stripList)=="table")
  assert(type(stripSymbolCache)=="table")
  assert(type(retime)=="number")

  local res = self:foldl("inputImage"..loopid.."_",function(tab,arrayItem) 
                      appendTable(tab,arrayItem:setPosition(loopid,x,y,core,stripId,stripCount, stripList, stripSymbolCache,retime)) 
                      return tab
                                 end,{})
  return res
end

function loopIRFunctions:inputImagesNext(loopid,v)
  assert(type(loopid)=="number")
  assert(type(v)=="number")
  return self:foldl("inputImage"..loopid.."_",function(tab,arrayItem) 
                      table.insert(tab,arrayItem:next(loopid,v)) 
                      return tab
                                 end,{})
end

function loopIRFunctions:releaseInputImages()
  local res = {}

  -- we actually do want to double free registers!
  -- remember, the refcounts include the references
  -- inside a rungroup. we need to make sure we free those.

  for i=1,self:arraySize("kernel") do
    self:map("inputImage"..i.."_",function(arrayItem) 
               assert(orion.imageWrapper.isImageWrapper(arrayItem))
               local r = arrayItem:release()
               -- r can be nil b/c some image objects don't require to be released
               if r~=nil then 
                 table.insert(res,r) 
               end
                                  end)
  end

  return res
end

function loopIRFunctions:nextInputImagesLine( loopid,  sub, stripCount, y,retime)
  assert(type(loopid)=="number")
  assert(terralib.isquote(sub))
  assert(type(stripCount)=="number")
  assert(type(retime)=="number")

  return self:foldl("inputImage"..loopid.."_",function(tab,arrayItem) 
                      table.insert(tab,arrayItem:nextLine( loopid, sub, stripCount, y,retime)) 
                      return tab
                                 end,{})
end

-- how many GB does this loop read & write? (if everything was linebuffered)
function loopIRFunctions:bytes()

  local res = `0

  self:map("outputImage", function(n) res = `res + [n:bytes()] end)
  
  for i=1,self:arraySize("kernel") do
    self:map("inputImage"..i.."_", function(n) res = `res + [n:bytes()] end)
  end

  return `res
end

function loopIRFunctions:expectedKeycount()
  local kCount = self:arraySize("kernel")
  -- outputNeededRegionUnion, rungroup
  -- each kernel has: kernel, outputNeededRegion, validRegion, outputImage, 
  -- outputType, lbSize, retime, consumedInternally, consumedExternally

  local icount = 0
  for i=1,kCount do icount = icount + self:arraySize("inputImage"..i.."_") end
  return 2 + kCount*9 + icount
end

function loopIRFunctions:checkfn()
  -- notice that loopIR nodes don't have a kind. only one kind of them

  local kCount = self:arraySize("kernel")

  for i=1,kCount do
    assert(orion.cropIR.isCropIR(self["outputNeededRegion"..i]))
    self["outputNeededRegion"..i]:check()
    assert(orion.cropIR.isCropIR(self["validRegion"..i]))
    self["validRegion"..i]:check()
    self["outputImage"..i]:check()
    assert(orion.type.isType(self["outputType"..i]))

    self:map("inputImage"..i.."_",
             function(v) 
               assert(orion.imageWrapper.isImageWrapper(v))
               v:check() 
             end)
  end
  
  assert(type(self.rungroup)=="number")
  assert(orion.cropIR.isCropIR(self.outputNeededRegionUnion))
  self.outputNeededRegionUnion:check()

  self:map("kernel",function(v) assert(orion.flatIR.isFlatIR(v)) end)

  assert(orion.cropIR.isCropIR(self.outputNeededRegionUnion))
end

function loopIRFunctions:printpretty(stripCount)
  if stripCount~=nil then
    assert(type(stripCount)=="number")
    print("strip count:", stripCount)
  end

  local kCount = self:arraySize("kernel")

  print("rungroup:",self.rungroup)

  for i=1,kCount do
    print("kernel"..i..":")
    print("lbSize"..i..":", self["lbSize"..i])
    print("retime"..i..":", self["retime"..i])
    print("consumedInternally"..i..":", self["consumedInternally"..i])
    print("consumedExternally"..i..":", self["consumedExternally"..i])
    print("output needed region:", self["outputNeededRegion"..i]:printprettys())
    print("output valid region:", self["validRegion"..i]:printprettys())

    print("Output Image:")
    self["outputImage"..i]:printpretty()


    local kern = self["kernel"..i]
    kern:printpretty()

    -- just for convenience
    kern:S("special"):traverse(
      function(s)
        print("bound"..s.id.." needed region:",s:neededRegion(self["kernel"..kernelID]):printprettys())
      end)

    print("Input Image(s):")
    self:map("inputImage"..i.."_",function(v) v:printpretty() end)
    
  end


end

-- This will convert all of the 'single' scheduleIR nodes
-- that hold a single kernel, to 'multiple' IR nodes that hold multiple
-- kerneles, based on a 'linebufferGroup' specified in the schedule
function orion.loopIR.singleToMultiple(scheduledIR, schedule)
  local delays, children = orion.schedule.simpleLBSchedule(scheduledIR, schedule)

  if orion.verbose or orion.printstage then
    print("singleToMultiple --------------------")
    print(collectgarbage("count"))
  end

  -- now build up the graph of supernodes based on the delays, children, and outputs
  
  local remap = {} -- old scheduledIR node -> index node that points into supernode
  local rungroupRemap = {} -- rungroup -> supernode
  local outSchedule = {} -- we're changing the graph, so we need to rebuild this
  local startConstruction = {}

  -- this is a function b/c we need to call it recursively.
  -- (need to create child supernodes before parent)
  local function getRungroup(id)
    -- if this supernode already exists, just return it
    if rungroupRemap[id]~=nil then
      return rungroupRemap[id]
    end

    --assert(startConstruction[id]==nil) -- or else we have an infinite loop
    if startConstruction[id]~=nil then
      -- we have an infinite loop
      print("CIRCULAR DEPENDENCY")
      return nil
    end
    
    startConstruction[id] = 1

    -- now build the supernode
    local newnode = {kind="multiple"}

    local idx = 1
    for childRungroup,_ in pairs(children[id]) do
      assert(id~=childRungroup)
      newnode["child"..idx] = getRungroup(childRungroup)
      if newnode["child"..idx]==nil then
return nil
      end
      idx = idx + 1
    end

    -- now copy the kernel etc from each of the single nodes
    if #delays[id]==1 and delays[id][1].scheduledIRNode.kind=="toSOA" then
      -- toSOA nodes can't be fused in yet! They must be in their own rungroup
      -- this is kind of a nasty hack way of keeping track of these nodes...
      -- should refactor
      newnode = delays[id][1].scheduledIRNode
    else
      local idx = 1
      for k,single in ipairs(delays[id]) do
        assert(single.scheduledIRNode.kind=="single")
        -- upload load so that it points to the correct
        -- place in this graph, not the old graph
        local kernel = single.scheduledIRNode.kernel
        kernel = kernel:S("load"):process(
          function(n)
            if n.from.kind=="single" then
              assert(remap[n.from]~=nil)
              local newload = n:shallowcopy()
              newload.from = remap[n.from]
              return orion.internalIR.new(newload):copyMetadataFrom(n)
            elseif n.from.kind=="toSOA" then
              -- we actually don't change the SOA nodes, so these
              -- don't need to change
            else
              assert(false)
            end
          end)
        
        newnode["kernel"..idx] = kernel
        newnode["neededRegion"..idx] = single.scheduledIRNode.neededRegion
        newnode["reason"..idx] = single.scheduledIRNode.reason
        newnode["irNode"..idx] = single.scheduledIRNode.irNode
        newnode["lbSize"..idx] = single.lbSize
        newnode["retime"..idx] = single.retime
        newnode["consumedInternally"..idx] = single.consumedInternally
        newnode["consumedExternally"..idx] = single.consumedExternally
        idx = idx + 1

        -- remap table for the outputs
        -- we have to do this here b/c something in the rungroup may depend on other stuff in the rungroup.
        -- Theoretically kernels in 'delay' array should be sorted so that dependencies are satisfied
        --
        -- NOTE: we're subtly breaking the immutability of the data structure here!!!!!!
        -- technically, it's not possible to make a self loop in an immutable data structure, but we're
        -- doing that here. The only reason this works is that orion.scheduledIR.new doesn't
        -- change the address of its argument - so if we use the (incomplete) 'newnode' here
        -- it will still point to the right thing after we make it immutable. Should maybe
        -- prevent this and rewrite it a more obvious way!!!
        local indexNode = {kind="index", index = k}
        indexNode.child1 = newnode
        indexNode = orion.scheduledIR.new(indexNode):setLinenumber(0):setOffset(0):setFilename("null")
        remap[single.scheduledIRNode] = indexNode
      end
      newnode.rungroup = id
      local nn = orion.scheduledIR.new(newnode):copyMetadataFrom(delays[id][1].scheduledIRNode)
      assert(nn==newnode) -- our trick to make self loops depends on this!
      newnode = nn
    end

    -- fill in the remap tables
    rungroupRemap[id] = newnode


    outSchedule[newnode] = schedule[delays[id][1].scheduledIRNode]

    return newnode
  end

  for rungroup, list in pairs(delays) do
    if getRungroup(rungroup) == nil then
return nil
    end
  end

  -- now process the scheduledIR graph. replace each single node
  -- with an 'index' that points into the graph of supernodes

  -- Theoretically, what this should do is take the few AOS, multiout
  -- etc nodes at the top and make them point into the new graph
  -- we built above. The rest of the graph should just get blown away.
  -- prob a very inefficient way of doing this...
  local output = scheduledIR:S("single"):process(
    function(node, orignode)
      assert(remap[orignode]~=nil)
      return remap[orignode]
    end)

  return output, outSchedule, remap
end

-- basically the way this works is:
-- the input scheduled IR node is converted to a loop scheduled IR Node
-- kind=aggregate scheduled IR nodes hold a graph of N children which will be linebuffered
-- kind=single scheduleIR nodes hold a single kernel which is codegened the same as LB code, but with 0 LBs
-- this produces a graph of kind=single scheduledIR nodes, which contain loopIR in their kernel
function orion.loopIR.convert(scheduledIR, schedule, cropRegion, base1, base2)
  if orion.verbose then print("To Loop IR") end
  assert(type(schedule)=="table")

  -- Fuse everything marked with the same rungroup to a
  -- 'multiple' node, and flatten the kernel list.
  local scheduledIR, schedule = orion.loopIR.singleToMultiple(scheduledIR, schedule)

  -- singleToMultiple failed (mutual dependency)
  if scheduledIR==nil then 
return nil
  end

  scheduledIR:check()

  if orion.verbose or orion.printstage then
    print("AFTER singleToMultiple --------------------")
    print(collectgarbage("count"))
    collectgarbage("collect")
    collectgarbage("collect")
    print(collectgarbage("count"))
  end

  if orion.verbose then
    scheduledIR:printpretty()
  end

  -- the kernel IR nodes held by the scheduledIR don't know 
  -- about the scheduled IR nodes,
  -- so build a map from the old scheduled IR nodes to their
  -- loop node replacement and update the kernel IRs manually
  local scheduleToLoopRemap = {}

  local internalCrops = {}
  scheduledIR:S("*"):traverse(
    function(node)
      if node.kind=="multiple" then
        for i=1,node:arraySize("kernel") do
          if node["consumedInternally"..i] then
            -- notice what's going on here:
            -- if a node is BOTH consumedInternally
            -- and consumedExternally, then it will get written to as if it is a linebuffer!
            -- this means that there will be overwrite (the boundry regions will be written to 
            -- main memory twice). If this is multicore, this might even caused perf
            -- problems due to cache coherence!
            internalCrops[node["irNode"..i]] = 1
          end
        end
      end
    end)

  local q = scheduledIR:S("*")
  local scheduledLoopIR = q:process(
    function(node,origNode)

      if node.kind=="multiple" then

        local loopir = {}

        local newnode = {kind="loop", loop=loopir}
        -- copy over the children list
        node:map("child", function(n,i) newnode["child"..i]=n end)
        scheduleToLoopRemap[origNode] = newnode

        -- collect inputs
        local nextid = node:arraySize("child")+1
        local specialAdded = {}

        -- we need to know the size of the whole loop to generate
        -- this is the union of all the output needed regions
        local outputNeededRegionUnion
        for i=1,node:arraySize("kernel") do
          local bareNeededRegion = node["neededRegion"..i]

          -- get rid of the strip nodes for images that aren't stored
          -- We do this b/c we don't want to force the linebuffered stuff to a certain
          -- strip width - we want to calculate its width based on what is needed
          -- by the actual reified outputs
          bareNeededRegion = bareNeededRegion:S(
            function(n)
              if n.kind=="strip" or n.kind=="intersectionCropBaked" then
                return internalCrops[n.node]~=nil
              end
              return false
            end):process(
            function(n)
              if n.kind=="strip" then
                return n.expr
              else
--                print("HERE")
--                n.node:printpretty()
--                assert(false)
                -- must be an intersectionCropBaked
                -- I don't understand why we have to eliminate these?
                -- it seems like this can only happen if we compose two cropBaked's?
                -- most likely there was another bug that we causing whatever problem this fixed
--                return n.expr2
              end
            end)

          -- we want to make sure the bounds on the output region 
          -- have the vector size as a factor, so that we can do aligned stores
          -- I think we add this annotation here instead of adding it in code generation
          -- just b/c it's easier to always know the exact size we're actually calculating.
          loopir["outputNeededRegion"..i] = bareNeededRegion --:growToNearestX(orion.tune.V)
          if outputNeededRegionUnion==nil then
            outputNeededRegionUnion = loopir["outputNeededRegion"..i]
          else
            outputNeededRegionUnion = outputNeededRegionUnion:unionWith(loopir["outputNeededRegion"..i])
          end
        end


        for i=1,node:arraySize("kernel") do
          local nextidInputImage = 1

          local kernel = node["kernel"..i]:S("special"):process(
            function(n)
              assert(specialAdded[n.id]==nil) -- CSE should guarantee this
              loopir["inputImage"..i.."_"..nextidInputImage] = orion.imageWrapper.special(n.id)
              nextidInputImage = nextidInputImage + 1
              specialAdded[n.id]=1
              
              -- Note that the region here is not actually correct. Special nodes don't
              -- appear as actual nodes in our graph, so outputneededregion doesn't work for them.
              -- so we fake the needed region as just the W,H of the image. But whatever. yolo.
              return orion.internalIR.new({kind="loadConcrete",
                                           type=n.type,
                                           from=orion.imageWrapper.special(n.id), 
                                           x=0,
                                           y=0}):copyMetadataFrom(n)
            end)
          
          kernel = kernel:S("load"):process(
            function(n)
              local lOutput
              
              if n.from.kind=="index" then
                local idx = n.from.index
                local from = n.from.child1

                -- NOTE: we're subtly breaking the immutability of the data structure here!!!!!!
                -- technically, it's not possible to make a self loop in an immutable data structure, but we're
                -- doing that here. The only reason this works is that orion.scheduledIR.new doesn't
                -- change the address of its argument - so if we use the (incomplete) 'newnode' here
                -- it will still point to the right thing after we make it immutable. Should maybe
                -- prevent this and rewrite it a more obvious way!!!

                local lir = scheduleToLoopRemap[from]
                assert(lir)
                lOutput = lir.loop["outputImage"..idx]
                loopir["inputImage"..i.."_"..nextidInputImage] = lOutput
                nextidInputImage = nextidInputImage + 1
              elseif n.from.kind=="toSOA" then
                assert(scheduleToLoopRemap[n.from])
                local lir = scheduleToLoopRemap[n.from]
                lOutput = lir.outputImage
                loopir["inputImage"..i.."_"..nextidInputImage] = lOutput
                nextidInputImage = nextidInputImage + 1
              else
                assert(false)
              end
              
              return orion.internalIR.new({kind="loadConcrete",
                                           type=n.type,
                                           from=lOutput,
                                           x=0,
                                           y=0}):copyMetadataFrom(n)
              
            end)
          
          kernel = kernel:S("gather"):process(
            function(n)
              -- our rules for converting to scheduledIR
              -- should have guaranteed this
              assert(n.input.kind=="loadConcrete")
              
              local gather = n:shallowcopy()
              gather.kind="gatherConcrete"
              gather.from = n.input.from
              gather.input = nil
              
              -- I think, if this isn't the case, there's a translate directly under the input
              -- we don't deal with this yet.
              assert(gather.translate1_input==gather.maxX+gather.translate1_hackBL)
              assert(gather.translate2_input==gather.maxY+gather.translate2_hackBL)
              
              gather.translate1_input = nil
              gather.translate2_input = nil
              gather.scale1_input = nil
              gather.scale2_input = nil
              
              return orion.internalIR.new(gather):copyMetadataFrom(n)            
            end)
          
          kernel:check()

          loopir["kernel"..i] = orion.flatIR.toFlatIR(kernel)
          loopir["outputType"..i] = kernel.type
          loopir["lbSize"..i] = node["lbSize"..i]
          loopir["retime"..i] = node["retime"..i]
          loopir["consumedInternally"..i] = node["consumedInternally"..i]
          loopir["consumedExternally"..i] = node["consumedExternally"..i]          

          local wrapperKind = "buffer"
          if node["consumedInternally"..i] and node["consumedExternally"..i]==false then 
            wrapperKind = "linebuffer" 
          end

          -- theoretically, we should be able to pad this as much as we want and have it still work...
          -- include this whether or not it's a linebuffer - b/c sometimes external images are also
          -- accessed internally
          assert(node["lbSize"..i]>0 or node["consumedExternally"..i])
          local wrapperLines = node["lbSize"..i]

          local newImage = {kind=wrapperKind,
                            lines = wrapperLines,
                            retime = node["retime"..i],
                            consumedInternally = loopir["consumedInternally"..i],
                            rungroup = node:name(),
                            orionType=node["kernel"..i].type,
                            terraType=orion.type.toTerraType(node["kernel"..i].type,false),
                            region=loopir["outputNeededRegion"..i],
                            refCount=0 -- we will patch this up later, after we have found the dependencies of each kernel within this rungroup
          }

          loopir["outputImage"..i] = orion.imageWrapper.new(newImage)
          
          if loopir["kernel"..i].kind=="cropBaked" then
            loopir["validRegion"..i] = loopir["kernel"..i].crop
            loopir["kernel"..i] = loopir["kernel"..i].expr
          else
            loopir["validRegion"..i] = loopir["outputNeededRegion"..i]
          end

        end

        --------------
        loopir.outputNeededRegionUnion = outputNeededRegionUnion
        loopir.rungroup = node.rungroup
        local nl = orion.loopIR.new(newnode.loop):copyMetadataFrom(node)
        assert( nl == newnode.loop) -- our trick to make self loops depends on this!
        newnode.loop = nl -- obviously the prev line implies this is already true, but whatever

        local res = orion.scheduledIR.new(newnode):copyMetadataFrom(node)
        assert(res==newnode) -- our trick to make self loops depends on this!
 
        return res
      elseif node.kind=="toAOS" then
        -- can just keep this node as it is
      elseif node.kind=="multiout" then
        -- can just keep this node as it is
      elseif node.kind=="index" then
        -- theoretically, these should get blown away by their consumer
      elseif node.kind=="toSOA" then
        local newnode = {kind="toSOAConcrete", index = node.index, special = node.special}

        local newImage = {kind="buffer",
                          orionType=node.type,
                          terraType=orion.type.toTerraType(node.type,false),
                          region=orion.cropIR.special(node.special),
                          refCount=origNode:parentCount(scheduledIR)}
        newnode.outputImage = orion.imageWrapper.new(newImage)

        local res = orion.scheduledIR.new(newnode):copyMetadataFrom(node)
        scheduleToLoopRemap[origNode] = res
        return res
      else
        print(node.kind)
        assert(false)
      end

    end)

  -----------------------
  -- determine the number of references to the output images
  -- this is a freakin disaster, there must be a better way to do this

  scheduledLoopIR:S("loop"):traverse(
    function(node)
      local loop = node.loop
      -- now patch up the reference counts
      -- there is almost certainly a better way to do this
      local numInternalDependencies = {}
      for i=1,loop:arraySize("kernel") do
        local j=1
        while loop["inputImage"..i.."_"..j] do
          local inp = loop["inputImage"..i.."_"..j]
          assert(orion.imageWrapper.isImageWrapper(inp))
          if numInternalDependencies[inp]==nil then
            numInternalDependencies[inp] = 1
          else
            numInternalDependencies[inp] = numInternalDependencies[inp]+1
          end
          
          j = j+1
        end
      end
      
      for i=1,loop:arraySize("kernel") do
        local outp = loop["outputImage"..i]
        if type(numInternalDependencies[outp])=="number" then
          outp.refCount = outp.refCount + numInternalDependencies[outp]
        end
        
        for parentNode,_ in node:parents(scheduledLoopIR) do
          assert(orion.scheduledIR.isScheduledIR(parentNode))
          if parentNode.kind=="index" then
            if parentNode.index==i then 
              -- the index node may be consumed by multiple things
              local pc = parentNode:parentCount(scheduledLoopIR)

              -- typically we make 1 index node for each time it's consumed.
              -- however, it's possible for the same index node to get consumed
              -- multiple times by toAOS nodes, in which case we need to 
              -- count the number of parents and use this number
              -- FIX THIS TO MAKE IT LESS MESSY
              outp.refCount = outp.refCount + math.max(pc,1)
            end
          elseif parentNode.kind=="loop" then
            -- need to determine the # of links
            for ii=1,parentNode.loop:arraySize("kernel") do
              parentNode.loop:map("inputImage"..ii.."_", function(n)
                                    if n==outp then outp.refCount = outp.refCount + 1 end
                                                         end)
            end
          else
            print(parentNode.kind)
            assert(false)
          end
        end
      end
    end)

  scheduledLoopIR:check()

  if orion.verbose or orion.printloopir or orion.printstage then
    print("Loop IR -----------------------------------------")
  end

  if orion.verbose or orion.printloopir then
    scheduledLoopIR:printpretty()
  end

  return scheduledLoopIR
end

function orion.loopIR.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,loopIRMT)
end


function orion.loopIR.isLoopIR(ast) return getmetatable(ast)==loopIRMT end

-----------------------------------
-- the imageWrapper assists the terra codegenerator by wrapping up the data sources
-- (buffer and special input) in a consistant way and providing a nice interface
-- that codegens strength reduced versions of loads etc.

orion.imageWrapper={}
orion.imageWrapper.nextRegister = 0
-- cachedBuffer is used to hold values for cached imageWrappers
orion.imageWrapper.cachedBuffer = {buffer=global(&opaque, nil), linebuffer=global(&opaque, nil), special=global(&opaque, nil)}
-- this is the max stencil size we support with a cached buffer
orion.imageWrapper.cachedBufferMaxWidth = 50
orion.imageWrapper.cachedBufferMaxSize = terralib.sizeof(double)*orion.tune.V*orion.imageWrapper.cachedBufferMaxWidth*orion.imageWrapper.cachedBufferMaxWidth
orion.imageWrapper.cachedBufferMaxCount = 50
orion.imageWrapper.nextCachedId = 0
orion.imageWrapper.linebufferBuffer = global(&opaque, nil)
orion.imageWrapper.linebufferDebugX = global(&int, nil)
orion.imageWrapper.linebufferDebugY = global(&int, nil)
orion.imageWrapper.linebufferDebugId = global(&int, nil)
orion.imageWrapper.maxLinebufferSize = orion.tune.maxLinebufferSize
-- this is the number of strips that we will run _concurrently_
-- the strip is actually mapped mod maxLinebufferStrips
--orion.imageWrapper.maxLinebufferStrips = orion.tune.cores
orion.imageWrapper.nextLinebufferId = 0
orion.imageWrapper.linebufferNextPosition = {} -- rungroup -> next available LB position
-- depending on what coord stuff lands on, growToNearestX may
-- make the linebuffer required larger at runtime than we calculate
-- at compile time (ie left=3 goes to left=0). So pad both sides of the LB
orion.imageWrapper.lbPadding = orion.tune.V*8

ImageWrapperFunctions = {}
ImageWrapperMT={__index=ImageWrapperFunctions}

function orion.imageWrapper.isImageWrapper(im)
  return getmetatable(im)==ImageWrapperMT
end

-- tab.terraType should be the base type of the data stored in this image
-- ie, if it's a floating point image, tab.terraType should be float
function orion.imageWrapper.new(tab)
  assert(type(tab)=="table")

  if tab.kind=="buffer" then
    if tab.data==nil then
      tab.data = {}
    end
    
    if tab.register==nil then
      tab.register = orion.imageWrapper.nextRegister
      orion.imageWrapper.nextRegister = orion.imageWrapper.nextRegister + 1
    end

    if tab.region~=nil then
      if tab.region:getArea()==0 then
        print(tab.region:filename(), tab.region:linenumber())
        assert(false)
      end
    end

  elseif tab.kind=="special" then
  elseif tab.kind=="cached" then
    if tab.id==nil then
      tab.id = orion.imageWrapper.nextCachedId
      orion.imageWrapper.nextCachedId = orion.imageWrapper.nextCachedId + 1
      if orion.imageWrapper.nextCachedId>=orion.imageWrapper.cachedBufferMaxCount then
        orion.imageWrapper.nextCachedId = 0
      end
    else
--      assert(tab.id<orion.imageWrapper.cachedBufferMaxCount)
    end
  elseif tab.kind=="linebuffer" then
    assert(type(tab.lines)=="number")
    assert(type(tab.retime)=="number")

    -- this holds the IVs for the LB.
    -- index 0 is the current location we're pointing at.
    -- index -1 is the previous line (line at index0 -1), etc
    -- the higher indices hold stuff in the past b/c this is
    -- what we will read when we do a stencil read (all stencils have neg indices)

    if tab.data==nil then
      tab.data = {} -- line -> loop
      tab.used = {}
      tab.base = {}
      tab.baseDebugX = {}
      tab.baseDebugY = {}
      tab.baseDebugId = {}
      tab.dataDebugX = {}
      tab.dataDebugY = {}
      tab.dataDebugId = {}
      tab.posX = {}
      tab.posY = {}

      for i=-(tab.lines-1),0 do
        tab.data[i] = {}
        tab.used[i] = {}
        tab.base[i] = {}
        tab.baseDebugX[i] = {}
        tab.baseDebugY[i] = {}
        tab.baseDebugId[i] = {}
        tab.dataDebugX[i] = {}
        tab.dataDebugY[i] = {}
        tab.dataDebugId[i] = {}
      end
    end

    if tab.gatherPointer==nil then
      tab.gatherPointer = {}
      tab.gatherPointerDebugX = {}
      tab.gatherPointerDebugY = {}
      tab.gatherPointerDebugId = {}
      tab.gatherPointerY = {}
      tab.gatherPointerUsed = {}
    end

    if tab.id==nil then
      tab.id = orion.imageWrapper.nextLinebufferId
      orion.imageWrapper.nextLinebufferId = orion.imageWrapper.nextLinebufferId + 1
    end

  else
    print(tab.kind)
    assert(false)
  end

  local res = setmetatable(tab,ImageWrapperMT)
  res:check()

  return res
end

orion.imageWrapper._specialCache = setmetatable({}, {__mode="k"})
function orion.imageWrapper.special(id)
  assert(type(id)=="number")

  if orion.imageWrapper._specialCache[id]==nil then
    assert(orion._boundImages[id+1])
    local ty = orion.type.toTerraType(orion._boundImages[id+1].type,false)
    
    local newim = {kind="special", 
                   orionType = orion._boundImages[id+1].type,
                   terraType=ty,
                   id=id,
                   data={},
                   region=orion.cropIR.special(id)}
    
    orion.imageWrapper._specialCache[id] = orion.imageWrapper.new(newim)
  end

  return orion.imageWrapper._specialCache[id]
end

function ImageWrapperFunctions:name()
  if self.kind=="special" then
    return "special"..self.id
  elseif self.kind=="buffer" then
    return "buffer"..self.register
  elseif self.kind=="linebuffer" then
    return "linebuffer"
  else
    assert(false)
  end
end

-- return a quote that calculates the size of this image in GB
function ImageWrapperFunctions:bytes()
  return self.region:getArea()*terralib.sizeof(self.terraType)
end

function ImageWrapperFunctions:lineWidth(stripCount)
  -- it's actually ok if this is a buffer... this thing might be both consumedInternally and consumedExternally
  --assert(self.kind=="linebuffer")
  assert(type(stripCount)=="number")
  local lineWidth = self.region:growToNearestX(orion.tune.V):getMaxWidth(stripCount) + orion.imageWrapper.lbPadding*2
  return lineWidth
end

function ImageWrapperFunctions:lbBytes(stripCount)
  -- it's actually ok if this is a buffer... this thing might be both consumedInternally and consumedExternally
  --assert(self.kind=="linebuffer")
  assert(type(stripCount)=="number")
  assert(type(self.lines)=="number")
  return self:lineWidth(stripCount)*self.lines*terralib.sizeof(self.terraType)
end

function ImageWrapperFunctions:recomputeBytes(stripCount)
  assert(type(stripCount)=="number")
  return self.region:getRecomputeArea(stripCount)*terralib.sizeof(self.terraType)
end

function ImageWrapperFunctions:area()
  return self.region:getArea()
end

-- this is the area including recompute regions
-- double counted (which results from stripping).
-- if no strips, this is the same as :area()
function ImageWrapperFunctions:recomputeArea(stripCount)
  assert(type(stripCount)=="number")
  return self.region:getRecomputeArea(stripCount)
end

function ImageWrapperFunctions:recomputeMaxWidth(stripCount)
  assert(type(stripCount)=="number")
  return self.region:getMaxWidth(stripCount)
end

function ImageWrapperFunctions:printpretty(stripCount)


  print("\tregion:",self.region:printprettys())
  print("\tkind:", self.kind)

  if self.kind=="special" then
    print("\tspecial",self.id)
  elseif self.kind=="linebuffer" then
    print("\tid:",self.id)
    print("\tlines:",self.lines)
    print("\tretime:",self.retime)

    if stripCount~=nil then
      assert(type(stripCount)=="number")
      print("\tallocatedStripWidth:", self:lineWidth(stripCount))
      print("\tsize:",self:lbBytes(stripCount))
    end
  elseif self.kind=="buffer" then
    print("\tbuffer register:",self.register,"refCount:",self.refCount)
  else
    assert(false)
  end

  --print("\tdata:",self.data)
  print("\tterra type:",self.terraType)
  print("\torion type:",self.orionType:str())
end

function ImageWrapperFunctions:check()

  assert(terralib.types.istype(self.terraType))
  assert(orion.type.isType(self.orionType))

  if self.kind~="cached" then
    assert(type(self.data)=="table") -- the symbol
    assert(orion.cropIR.isCropIR(self.region))
  end

  if self.kind=="special" then
    assert(type(self.id)=="number")
  elseif self.kind=="buffer" then
    assert(type(self.register)=="number")
    assert(type(self.refCount)=="number")
  elseif self.kind=="linebuffer" then
    assert(type(self.rungroup)=="string")
    assert(type(self.id)=="number")
  elseif self.kind=="cached" then
  else
    assert(false)
  end

end

-- called by the producer
function ImageWrapperFunctions:alloc(stripCount)

  if self.kind=="special" then
    -- we should never be producing a special
    assert(false)
  elseif self.kind=="buffer" then
    local r = self.register
    local rc = self.refCount
    assert(type(r)=="number")
    assert(type(rc)=="number")

    return `orion.runtime.createRegister(r,rc)
  elseif self.kind=="linebuffer" then
    assert(type(stripCount)=="number")

    assert(orion.imageWrapper.maxLinebufferSize % orion.tune.pageSize == 0)

    -- self.linebufferPosition is in bytes
    if self.linebufferPosition == nil then
      local pos = orion.imageWrapper.linebufferNextPosition[self.rungroup] or 0

      -- conservatively round up so that everything is properly aligned
      orion.imageWrapper.linebufferNextPosition[self.rungroup] = upToNearest(
        orion.tune.V*4, pos + 
          terralib.sizeof(self.terraType) * self.lines * self:lineWidth(stripCount)  )

      if orion.imageWrapper.linebufferNextPosition[self.rungroup] >= 
        orion.imageWrapper.maxLinebufferSize then
        print("Exceeded max LB size ",orion.imageWrapper.maxLinebufferSize)
        assert(false)
      end


      self.linebufferPosition = pos
    end

    local res = {}
    table.insert(res,quote 
      if orion.imageWrapper.linebufferBuffer==nil then
        stdlib.posix_memalign( 
          &orion.imageWrapper.linebufferBuffer, 
          orion.tune.pageSize,  
          orion.imageWrapper.maxLinebufferSize*orion.tune.cores)
      end
    end)

    if orion.debug then
      table.insert(res,quote 
                     if orion.imageWrapper.linebufferDebugX==nil then
                       stdlib.posix_memalign( 
                         [&&opaque](&orion.imageWrapper.linebufferDebugX), 
                         orion.tune.pageSize,  
                         orion.imageWrapper.maxLinebufferSize*orion.tune.cores*sizeof(int))
                     end
                     if orion.imageWrapper.linebufferDebugY==nil then
                       stdlib.posix_memalign( 
                         [&&opaque](&orion.imageWrapper.linebufferDebugY), 
                         orion.tune.pageSize,  
                         orion.imageWrapper.maxLinebufferSize*orion.tune.cores*sizeof(int))
                     end
                     if orion.imageWrapper.linebufferDebugId==nil then
                       stdlib.posix_memalign( 
                         [&&opaque](&orion.imageWrapper.linebufferDebugId),
                         orion.tune.pageSize,  
                         orion.imageWrapper.maxLinebufferSize*orion.tune.cores*sizeof(int))
                     end
      end)

    end

    return quote res end
  elseif self.kind=="cached" then
    local res = {}
    for k,v in pairs({"buffer","linebuffer","special"}) do
      local size = orion.imageWrapper.cachedBufferMaxCount*orion.imageWrapper.cachedBufferMaxSize
      table.insert(res,quote 
                     if [orion.imageWrapper.cachedBuffer[v]]==nil then
                       stdlib.posix_memalign( 
                         &[orion.imageWrapper.cachedBuffer[v]], 
                         orion.tune.pageSize,  
                         size)

                       cstring.memset([orion.imageWrapper.cachedBuffer[v]],0,size)
                     end
    end)
end

    return quote res end
  else
    assert(false)
  end

end

-- called by a consumer
function ImageWrapperFunctions:init(loopid)
  assert(type(loopid)=="number")


  if self.kind=="special" then
    if self.data[loopid]==nil then
      assert(terralib.types.istype(self.terraType))
      self.data[loopid] = symbol(&self.terraType)
    end


  elseif self.kind=="buffer" then
    if self.data[loopid]==nil then
      assert(terralib.types.istype(self.terraType))
      self.data[loopid] = symbol(&self.terraType)
    end

  elseif self.kind=="linebuffer" then
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

  else
    assert(false)
  end
end

function ImageWrapperFunctions:declare(loopid)
  if self.kind=="special" then
    return quote
      var [self.data[loopid]] = [&self.terraType](orion._boundImagesRuntime:get(self.id).image.data)
    end
  elseif self.kind=="buffer" then
    return quote
      var [self.data[loopid]] = [&self.terraType](orion.runtime.getRegister(self.register))
      end
  elseif self.kind=="linebuffer" then
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
                     var [self.data[line][loopid]] = [&self.terraType](orion.imageWrapper.linebufferBuffer)
                     
--          orion.imageWrapper.maxLinebufferSize * strip +
--          [orion.imageWrapper.linebufferPosition[self.id]]
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
  else
    assert(false)

  end

end

-- called by the consumer
-- free 1 reference
function ImageWrapperFunctions:release()
  if self.kind=="buffer" then
    return quote orion.runtime.releaseRegister(self.register) end
  else
    -- actually allow this for convenience
    --assert(false)
  end
end


function ImageWrapperFunctions:releaseAndKeep()
  if self.kind=="buffer" then
    return quote orion.runtime.releaseAndKeep(self.register) end
  else
    assert(false)
  end
end

function ImageWrapperFunctions:close()
  assert(self.kind=="cached")

  return quote
    var startAddr = [&uint8]([orion.imageWrapper.cachedBuffer[self.shadowKind]])+[self.id%orion.imageWrapper.cachedBufferMaxCount]*orion.imageWrapper.cachedBufferMaxSize

    for i=0,orion.imageWrapper.cachedBufferMaxWidth*orion.imageWrapper.cachedBufferMaxWidth do
      var addr = [&self.terraType](startAddr)+i
      @addr = @[&self.terraType](startAddr)
    end
  end
end

-- a % b
-- stupid C mod doesn't treat negative numbers as you'd hope
terra fixedModulus(a : int,b : int)
  while a < 0 do a = a+b end
  return a % b
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

  if self.kind=="special" then

    if orion.debug then
      table.insert(res,
                   quote 
                     if orion._boundImagesRuntime:get(self.id).active==false then
                       cstdio.printf("Nothing bound to image id %d\n",self.id)
                       orionAssert(false, "Unbound image")
                     end

    end)
end

    table.insert(res,quote [self.data[loopid]] = [&self.terraType](orion._boundImagesRuntime:get(self.id).image.data)
                   + (y-[self.region:getBottom()])*[self.region:getWidth()] + 
                   (x-[self.region:getLeft()]) end)

  elseif self.kind=="buffer" then

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
  elseif self.kind=="linebuffer" then
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
                     var [baseOffset] = core * [orion.imageWrapper.maxLinebufferSize] + self.linebufferPosition
                     var [base] = [&uint8](orion.imageWrapper.linebufferBuffer) + baseOffset
                     var [leftBound] = [self.region:stripRuntime(stripId, stripCount, stripList, stripSymbolCache)]:growToNearestX(orion.tune.V).left
                     var [baseAddX] = (x-leftBound+orion.imageWrapper.lbPadding)

                     var [baseX] = orion.imageWrapper.linebufferDebugX + baseOffset
                     var [baseY] = orion.imageWrapper.linebufferDebugY + baseOffset
                     var [baseId] = orion.imageWrapper.linebufferDebugId + baseOffset
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
  else
    assert(false)
  end

  return res
end

function ImageWrapperFunctions:set( loopid, value, V )
  assert(terralib.isquote(value))
  assert(type(loopid)=="number")
  assert(type(V)=="number")

  if self.kind=="buffer" then
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
  elseif self.kind=="linebuffer" then
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
--                   cstdio.printf("linebufferBuffer:%d mbs:%d lbp:%d\n",orion.imageWrapper.linebufferBuffer,[orion.imageWrapper.maxLinebufferSize],self.linebufferPosition)
                   @[&vector(self.terraType,V)]([self.data[0][loopid]]) = value 
--                   cstdio.printf("SETD\n")
end)


    return res
  elseif self.kind=="cached" then
    local res = {}
    local addr = symbol(&uint8)

    table.insert(res,quote 
                   var [addr] = [&uint8]([orion.imageWrapper.cachedBuffer[self.shadowKind]])+([self.id%orion.imageWrapper.cachedBufferMaxCount])*orion.imageWrapper.cachedBufferMaxSize
end)

    if orion.debug then
      table.insert(res,quote 
                     orionAssert(uint64(addr) % (V*sizeof([self.terraType])) == 0,"cache set not aligned")
    end)
    end

    table.insert(res,quote 
--                   orionAssert(uint64(orion.imageWrapper.cachedBuffer) % (V*sizeof([self.terraType])) == 0,"CB not aligned")
--                   cstdio.printf("%d\n",orion.imageWrapper.cachedBufferMaxSize)
--
--  @[&vector(self.terraType,V)]([addr]) = value 
                   terralib.attrstore([&vector(self.terraType,V)]([addr]),value,{isvolatile=true})
end)

    return res
  else
   assert(false)
  end
end

-- relX and relY should be integer constants relative to
-- the current location
function ImageWrapperFunctions:get(loopid, relX,relY, V, retime)
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

  if orion.debug and self.kind=="buffer" then
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
  elseif orion.debug and self.kind=="special" then
    
    local regionWidth = self.region:getWidth()
    table.insert(debugChecks,
                 quote
                   var start = [&self.terraType](orion._boundImagesRuntime:get(self.id).image.data)
                   var w = orion._boundImagesRuntime:get(self.id).image.width
                   var h = orion._boundImagesRuntime:get(self.id).image.height
                   var b = (orion._boundImagesRuntime:get(self.id).image.bits)/8
                   var maxv = w*h*b

                   var this = [self.data[loopid]] + relY*regionWidth + relX

                   orionAssert(w==regionWidth,"regionWidth doesn't match")

                   if (this-start)>=maxv then
                     cstdio.printf("this:%d start:%d self.data:%d relY:%d regionWidth:%d relX%d\n",
                                   this,start,[self.data[loopid]],relY,regionWidth,relX)
                     cstdio.printf("maxv: %d, %d %d %d\n",maxv,w,h,b)
                   end

                   orionAssert( (this-start)>=0,"read before start of special array")
                   orionAssert( (this-start)<maxv,"read beyond end of special array")
                 end)
  elseif orion.debug and self.kind=="linebuffer" then
  elseif orion.debug and self.kind=="cached" then
  elseif orion.debug then
    assert(false)
  end

  if self.kind=="special" then
    local regionWidth = self.region:getWidth()
    
    return quote [debugChecks] in
                   --var [resultSymbol] = terralib.aligned(@[&vector(self.terraType,V)]([self.data[loopid]] + relY*regionWidth + relX),V)
                   terralib.attrload([&vector(self.terraType,V)]([self.data[loopid]] + relY*regionWidth + relX),{align=V})
                   --    cstdio.printf("%f %f %f %f %d\n",resultSymbol[0],resultSymbol[1],resultSymbol[2],resultSymbol[3], self.data)
    end
  elseif self.kind=="buffer" then
  local regionWidth = self.region:growToNearestX(orion.tune.V):getWidth()
  return quote [debugChecks] in
                   --var [resultSymbol] = terralib.aligned(@[&vector(self.terraType,V)]([self.data[loopid]] + relY*regionWidth + relX),V)
                   terralib.attrload([&vector(self.terraType,V)]([self.data[loopid]] + relY*regionWidth + relX),{align=V})
                   --    cstdio.printf("%f %f %f %f %d\n",resultSymbol[0],resultSymbol[1],resultSymbol[2],resultSymbol[3], self.data)
    end

  elseif  self.kind=="linebuffer" then
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
  elseif self.kind=="cached" then
    assert(relX<=0)
    assert(relY<=0)

    return quote
      [debugChecks]
                   var startAddr = [&uint8]([orion.imageWrapper.cachedBuffer[self.shadowKind]])+[self.id%orion.imageWrapper.cachedBufferMaxCount]*orion.imageWrapper.cachedBufferMaxSize
                   var addr = [&self.terraType](startAddr)+([(-relY)%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(-relX)%orion.imageWrapper.cachedBufferMaxWidth]
                   in
                   terralib.attrload([&vector(self.terraType,V)](addr),{align=V, isvolatile=true})
                   end
                   
  else
    assert(false)
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

--  local origY = relY
--  if self.kind=="linebuffer" then
--    relY = `relY + [self.retime-retime]
--  end

  local debugChecks = {}

  if self.kind~="cached" then
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
  end

  if orion.debug and self.kind=="buffer" then
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
  elseif orion.debug and self.kind=="special" then
    local regionWidth = self.region:getWidth()

    table.insert(debugChecks,
      quote
        var start = [&self.terraType](orion._boundImagesRuntime:get(self.id).image.data)
        var w = orion._boundImagesRuntime:get(self.id).image.width
        var h = orion._boundImagesRuntime:get(self.id).image.height
        var b = (orion._boundImagesRuntime:get(self.id).image.bits)/8
        var maxv = w*h*b
        
        for i=0,V do
          var this = [self.data[loopid]] + relY[i]*regionWidth + relX[i] + i
          
          if (this-start)>=maxv then
            cstdio.printf("this:%d start:%d self.data:%d relY:%d regionWidth:%d relX%d\n",
                          this,start,[self.data[loopid]],relY,regionWidth,relX)
            cstdio.printf("maxv: %d, %d %d %d\n",maxv,w,h,b)
          end
          
          orionAssert( (this-start)>=0,"gather before start of special array")
          orionAssert( (this-start)<maxv,"gather beyond end of special array")
        end
      end)
  elseif orion.debug and self.kind=="linebuffer" then

  elseif orion.debug and self.kind=="cached" then

  elseif orion.debug then
    assert(false)
  end

  if self.kind=="buffer" or self.kind=="special" then
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

  elseif self.kind=="linebuffer" then
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

  elseif self.kind=="cached" then
    assert(blX<0)
    assert(blY<0)
    assert(trX>blX)
    assert(trY>blY)

    return quote [debugChecks]
      var startAddr = [&uint8]([orion.imageWrapper.cachedBuffer[self.shadowKind]])+[self.id%orion.imageWrapper.cachedBufferMaxCount]*orion.imageWrapper.cachedBufferMaxSize
      --  cstdio.printf("%d %d %d %d %d\n",blX,blY,trX,trY,-cstdlib.rand()%blX)
      
      var addr1 = [&self.terraType](startAddr)+([(blY+(cstdlib.rand()%(blY-trY)))%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(blX+(cstdlib.rand()%(blX-trX)))%orion.imageWrapper.cachedBufferMaxWidth]
      var addr2 = [&self.terraType](startAddr)+([(blY+(cstdlib.rand()%(blY-trY)))%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(blX+(cstdlib.rand()%(blX-trX)))%orion.imageWrapper.cachedBufferMaxWidth]
      var addr3 = [&self.terraType](startAddr)+([(blY+(cstdlib.rand()%(blY-trY)))%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(blX+(cstdlib.rand()%(blX-trX)))%orion.imageWrapper.cachedBufferMaxWidth]
      var addr4 = [&self.terraType](startAddr)+([(blY+(cstdlib.rand()%(blY-trY)))%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(blX+(cstdlib.rand()%(blX-trX)))%orion.imageWrapper.cachedBufferMaxWidth]
      
      --                   var addr2 = [&self.terraType](startAddr)+([(-trY)%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(-blX)%orion.imageWrapper.cachedBufferMaxWidth]
--                   var addr3 = [&self.terraType](startAddr)+([(-blY)%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(-trX)%orion.imageWrapper.cachedBufferMaxWidth]
--                   var addr4 = [&self.terraType](startAddr)+([(-trY)%orion.imageWrapper.cachedBufferMaxWidth])*orion.imageWrapper.cachedBufferMaxWidth+[(-trX)%orion.imageWrapper.cachedBufferMaxWidth]
--                   var [resultSymbol] = terralib.attrload([&vector(self.terraType,V)](addr),{align=V})
      in
        vectorof(self.terraType,@addr1,@addr2,@addr3,@addr4)
      end

  else
    assert(false)
  end

end

function ImageWrapperFunctions:next(loopid,v)
  assert(type(loopid)=="number")
  assert(type(v)=="number")
  
  if self.kind=="buffer" or self.kind=="special" then
    return quote [self.data[loopid]] = [self.data[loopid]] + v end
  elseif self.kind=="linebuffer" then
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
  else
    assert(false)
  end
end

-- sub: this is the number of pixels we looped over since last calling nextline
-- basically: the number of times we called nextVector this row * the vector width
function ImageWrapperFunctions:nextLine(loopid,  sub, stripCount, y, retime)
  assert(terralib.isquote(sub))
  assert(type(stripCount)=="number")
  assert(terralib.issymbol(y))
  assert(type(retime)=="number")

  if self.kind=="special" then
    local a = self.region:getWidth()
    return quote [self.data[loopid]] = [self.data[loopid]] + a - sub end
  elseif self.kind=="buffer" then
    local a = self.region:growToNearestX(orion.tune.V):getWidth()
    return quote [self.data[loopid]] = [self.data[loopid]] + a - sub end
  elseif self.kind=="linebuffer" then
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
  else
    assert(false)
  end

end

-- this is the width of the image region we're calculating
function ImageWrapperFunctions:width()
  return self.region:getWidth()
end

-- this is the width of the region we're storing (image width + padding)
function ImageWrapperFunctions:widthStored()
  return self.region:growToNearestX(orion.tune.V):getWidth()
end

-- get a pointer to the valid data
function ImageWrapperFunctions:getPointer()
  if self.kind=="buffer" then
    return `[&self.terraType](orion.runtime.getRegister([self.register]))+([self.region:getLeft()]-[self.region:growToNearestX(orion.tune.V):getLeft()])
  else
    print(self.kind)
    assert(false)
  end

end

-- get a pointer to the underlying data store (valid region + padding region)
function ImageWrapperFunctions:getPointerStored()
  if self.kind=="buffer" then
    return `orion.runtime.getRegister([self.register])
  else
    assert(false)
  end

end

function ImageWrapperFunctions:height()
  return self.region:getHeight()
end

-- returns a quote that initializes Image 'outputImage' with 
-- data from this wrapped image
function ImageWrapperFunctions:toImage(outputImage)
  assert(terralib.issymbol(outputImage))

  local floating = orion.type.isFloat(self.orionType)
  local isSigned = orion.type.isInt(self.orionType)

  return quote
    var width = [self:width()];
    var stride = [self:widthStored()];
    var height = [self:height()];
    if orion.verbose then cstdio.printf("toImage %d %d %d\n",width,stride,height); end
    var channels : int = 1;
    var bits : int = sizeof(self.terraType)*8;
    var data  = [self:getPointer()];
    var dataPtr  = [self:getPointerStored()];
    outputImage:init( width, height, stride, channels, bits, floating, isSigned, data, dataPtr );
  end

end