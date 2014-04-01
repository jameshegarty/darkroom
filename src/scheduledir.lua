scheduledIRFunctions={}
setmetatable(scheduledIRFunctions,{__index=IRFunctions})
scheduledIRMT={__index=scheduledIRFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}

-- this is supposed to be an IR that makes it easy to manipulate scheduling decisions.
-- this is common to Conv engine and CPU/GPU
-- your scheduling heuristics will operate on this IR

orion.scheduledIR = {}

function scheduledIRFunctions:init()
  setmetatable(self,nil)
  orion.scheduledIR.new(self)
end

function scheduledIRFunctions:irType()
  return "scheduledIR"
end

function scheduledIRFunctions:makeNewName()
  return "scheduledIRNode"
end

function scheduledIRFunctions:isARoot(root)
  if self:parentCount(root) == 0 then
    return true
  end

  return false
end

function scheduledIRFunctions:expectedKeycount()
  local childCount = 0
  while self["child"..(childCount+1)] do 
    childCount = childCount+1 
  end

  if self.kind=="single" then
    return 5+childCount -- kind, neededRegion, reason, kernel, irNode
  elseif self.kind=="multiple" then
    local kernelCount = self:arraySize("kernel")
    -- kind, rungroup
    -- each kernel has: neededRegion, reason, kernel, retime, lbSize, 
    -- consumedInternally, consumedExternally, irNode
    return 2+kernelCount*8+childCount 
  elseif self.kind=="index" then
    assert(childCount==1)
    return 3 -- kind, child, index
  elseif self.kind=="loop" then
    return 2+childCount -- kind, loop
  elseif self.kind=="conv" then
    return 1+childCount+self:arraySize("conv") -- kind, conv
  elseif self.kind=="terra" then
    return 5+childCount -- kind, loop, all, preRun, postRun
  elseif self.kind=="toAOS" then
    return 1+childCount 
  elseif self.kind=="multiout" then
    return 1+childCount
  elseif self.kind=="toSOA" then
    return 4 -- kind, type, index, special
  elseif self.kind=="toSOAConcrete" then
    return 4 -- kind, outputImage, index, special
  else
    print(self.kind)
    assert(false)
  end


end

function scheduledIRFunctions:checkfn()

  local childCount = 0
  while self["child"..(childCount+1)] do 
    if self.kind=="loop" then
      local child = self["child"..(childCount+1)]
      assert(orion.scheduledIR.isScheduledIR(child))
      assert(child.kind=="loop" or child.kind=="toSOAConcrete")
    end
    
    childCount = childCount+1 
  end

  if self.kind=="single" then
    assert(orion.internalIR.isInternalIR(self.kernel))
    self.kernel:check()
    assert(orion.cropIR.isCropIR(self.neededRegion))
    self.neededRegion:check()
    assert(type(self.reason)=="string")
  elseif self.kind=="multiple" then
    assert(type(self.rungroup)=="number")
    local kernelCount = self:arraySize("kernel")
    assert(kernelCount == self:arraySize("retime"))
    assert(kernelCount == self:arraySize("lbSize"))
    assert(kernelCount == self:arraySize("reason"))
    assert(kernelCount == self:arraySize("neededRegion"))

    assert(kernelCount>0)

    self:map("kernel",function(n)
               assert(orion.internalIR.isInternalIR(n))
               n:check()
                      end)

    self:map("neededRegion",function(n)
               assert(orion.cropIR.isCropIR(n))
               n:check()
                      end)

    self:map("reason",function(n)
               assert(type(n)=="string")
                            end)

    self:map("retime",function(n)
               assert(type(n)=="number")
                            end)

    self:map("lbSize",function(n)
               assert(type(n)=="number")
                            end)

  elseif self.kind=="index" then
    assert(type(self.index)=="number")
  elseif self.kind=="loop" then
    assert(orion.loopIR.isLoopIR(self.loop))
    self.loop:check()
  elseif self.kind=="conv" then
    self:map("conv",
      function(c) 
        assert(orion.convIR.isConvIR(c)) 
        c:check()
      end)

  elseif self.kind=="terra" then
    assert(terra.isfunction(self.all))
    assert(terra.isfunction(self.preRun))
    assert(terra.isfunction(self.postRun))
    assert(orion.loopIR.isLoopIR(self.loop))
    self.loop:check()
  elseif self.kind=="multiout" then
  elseif self.kind=="toAOS" then
  elseif self.kind=="toSOA" then
    assert(type(self.index)=="number")
    assert(type(self.special)=="number")
    assert(orion.type.isType(self.type))
  elseif self.kind=="toSOAConcrete" then
    assert(orion.imageWrapper.isImageWrapper(self.outputImage))
    assert(type(self.index)=="number")
    assert(type(self.special)=="number")
  else
    print(self.kind)
    assert(false)
  end

end

function scheduledIRFunctions:root()
  if self.kind=="single" then
    return self.kernel
  else
    assert(false)
  end
end

function scheduledIRFunctions:printpretty(stripWidths)

  self:S("*"):traverse(function(node)
    print("----------------")
    print("name:",node:name())
    print("kind:",node.kind)
    print("table",node)
    print("Input ScheduledIR Nodes:")
    local i=1
    while node["child"..i] do print(node["child"..i]:name()); i=i+1 end

    if node.kind=="multiout" then
      print("multiout")
    elseif node.kind=="toAOS" then
      print("to AOS node")
    elseif node.kind=="toSOA" or node.kind=="toSOAConcrete" then
      print("to SOA node, special"..node.special.." index "..node.index)
    elseif node.kind=="conv" then
      print("conv kernels",node:arraySize("conv"))
      node:map("conv",
        function(n)
          n.kernel:printpretty()
        end)
    else
      print("kernel:")
      if node.kind=="loop" then
        print("name:",node.loop:name())
        if stripWidths~=nil then
          node.loop:printpretty(stripWidths[node.loop])
        else
          node.loop:printpretty()
        end
      elseif node.kind=="single" then
        print("reason:",node.reason) -- why did we decide to materialize this?
        print(node.kernel)
        node.kernel:printpretty()
      elseif node.kind=="multiple" then
        local idx = node:arraySize("kernel")
        print("kernelCount",idx)
        for i=1,idx do
          print("kernel "..i..":")
          print(node["reason"..i])
          print(node["kernel"..i])
          node["kernel"..i]:printpretty()
        end
      elseif node.kind=="index" then
        print("index "..node.index.." "..node.child1:name())
      else
        print(node.kind)
        assert(false)
      end
    end
  end)
end

-- scheduleNodes is a list of nodes in the internalIR that will
-- becomed scheduledIR nodes (basically, they are the nodes that are
-- interesting to manipulate when we are making scheduling decisions)
function orion.scheduledIR.internalIRToScheduledIR(internalIR, scheduleNodes, cropRegion, base1, base2)
  assert(orion.internalIR.isInternalIR(internalIR))
  assert(type(scheduleNodes)=="table")

  if orion.verbose or orion.printstage then 
    print("To Scheduled IR", keycount(scheduleNodes), internalIR:S("*"):count()) 
  end

  -- root has to be in the list
  assert(scheduleNodes[internalIR])

  -- translation from the new kernel we generate to the original version
  local origKernel = {}

  -- We do this with a traverse instead of a process b/c we're converting
  -- from one type of AST to another
  local scheduledIR = internalIR:S(
    function(node) 
      return scheduleNodes[node]~=nil
    end)
  :traverse(
    function(node,args)
      if node.kind=="toAOS" then
        local newnode = {kind="toAOS"}
        node:map("expr", 
                 function(n,i)
                   -- find the scheduledIR node that corresponds to this entry
                   local snode
                   for _,child in pairs(args) do
                     if n==origKernel[child.kernel] then snode = child end
                   end
                   -- All inputs to AOS must be materialized!!!!!
                   assert(snode~=nil)
                   newnode["child"..i] = snode
                 end)

        local nn = orion.scheduledIR.new(newnode):copyMetadataFrom(node)
        origKernel[nn] = node

        return nn
      elseif node.kind=="multiout" then
        local newnode = {kind="multiout"}
        node:map("expr", 
                 function(n,i)
                   -- find the scheduledIR node that corresponds to this entry
                   local snode
                   for _,child in pairs(args) do
                     if n==origKernel[child.kernel] then snode = child end
                     if n==origKernel[child] then snode = child end
                   end
                   -- All inputs to multiout must be materialized!!!!!
                   assert(snode~=nil)
                   newnode["child"..i] = snode
                 end)

        -- this should be root, otherwise we need to assign to origKernel
        assert(node==internalIR) 

        return orion.scheduledIR.new(newnode):copyMetadataFrom(node)
      elseif node.kind=="toSOA" then
        local newnode = node:shallowcopy()
        local nn =  orion.scheduledIR.new(newnode):copyMetadataFrom(node)
        origKernel[nn] = node
        return nn
      else
        local newnode = {kind="single"}
        
        -- go through and replace all the references to input nodes with loads
        local childCount = 1

        local query = node:F(
          function(n) -- figure out if this node was replaced by a scheduledIR node
            local res = false
            for _,child in pairs(args) do
              res = res or (n==origKernel[child.kernel])
            end
            return res or n.kind=="toSOA" end)
        assert(query:count()==#args)

        local kernel = query:process(
          function(n,origNode)
            local child
            
            for _,pchild in pairs(args) do
              if origNode==origKernel[pchild.kernel] or
                (pchild.kind=="toSOA" and origNode==origKernel[pchild]) then 
                child = pchild 
              end
            end
            assert(child~=nil)
            
            newnode["child"..childCount] = child
            childCount = childCount+1
            
            local res = orion.internalIR.new({kind="load",from=child,type=n.type}):copyMetadataFrom(n)
            return res
          end)

        newnode.kernel = kernel
        newnode.reason = scheduleNodes[node]
        newnode.irNode = node
        if newnode.reason==nil then newnode.reason="parent required materialization?" end

        local nr = node:neededRegion(internalIR,cropRegion,base1,base2,scheduleNodes)
--        local cc = nr:optimize(cropRegion,base1,base2)
        newnode.neededRegion = nr:copyMetadataFrom(node)
        origKernel[kernel] = node
        return orion.scheduledIR.new(newnode):copyMetadataFrom(node)
      end
  end)
  -- our algorithm returns a list of nodes
  -- the root should be a list of 1
  assert(#scheduledIR==1)
  scheduledIR = scheduledIR[1]

  scheduledIR:check()

  if orion.verbose or orion.printstage then
    print("Scheduled IR Done --------------------------------------")
  end

  if orion.verbose then
    scheduledIR:printpretty()
    scheduledIR:printprettyJSON("debug/scheduledir.json")
  end

  return scheduledIR
end

function orion.scheduledIR.isScheduledIR(ast) return getmetatable(ast)==scheduledIRMT end

function orion.scheduledIR.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,scheduledIRMT)
end
