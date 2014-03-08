local cstdlib = terralib.includec("stdlib.h")
local cstdio = terralib.includec("stdio.h")


-------------
struct UFNode { parent : &UFNode, id:int } -- id is the id for the multinodes

terra UFNode:init()
  self.parent = self
  self.id = -1
end

tUF = {}

terra tUF.makeSet()
  var out = [&UFNode](cstdlib.malloc(sizeof(UFNode)))
  out.parent = out
  out.id=-1
  return out
end

terra tUF.find(x : &UFNode) : &UFNode
  if x.parent == x then
    return x
  end

  return tUF.find(x.parent)
end

terra tUF.tunion(x : &UFNode ,y : &UFNode)
  var xRoot = tUF.find(x)
  var yRoot = tUF.find(y)
  xRoot.parent = yRoot
  xRoot.id = -1
  yRoot.id = -1
end

-------------
struct Node { id:int,
              isSingle : bool, -- as in, it's not a AOS or multiout
              time:double, 
              rungroup : &UFNode, 
              inputs : &&Node, 
              inputEdgeId : &int, -- the edge id corresponding with the input list
              inputCount:int, 
              consumers : &&Node, 
              consumerCount : int, 
              consumerEdgeId : &int,
              -- note that special nodes are directly consumed by a crop node, so they always end up on their own. 
              -- So there can only ever be one special in a Node
              specialReadBytes : double, -- bytes we read from special images
              specialReadWidthBytes : double, --  width of the special node we're reading in bytes
              -- we don't bother adjusting boundaryPixels to account
              -- for the overlap due to strips, so this is always the same
              -- no matter what schedule
              boundaryPixels : double, 
              maxWidth : {&bool, int} -> double,  -- max strip width of output
              lineWidth : {&bool, int} -> double,
              recomputeArea : {&bool, int} -> double, -- recompute area of output needed region
              negOverhang : int, 
              delay : int,
              lastDelay : int, -- largest delay of a consumer of this node
              bytes : int, -- byte size of output type of this node
              consumedInternally : bool,
              consumedExternally : bool}

terra Node:free()
  cstdlib.free(self.rungroup)
  cstdlib.free(self.inputs)
  cstdlib.free(self.inputEdgeId)
  cstdlib.free(self.consumers)
end

struct Multinode { id : int,
                   time : double,
                   computeTime : double, -- total compute time
                   totalArea : double, -- number of pixels computed
                   inputs : &bool,
                   inputsCounted : &bool, -- prevent double counting the inputs
                   boundaryPixels : double,
                   lbTime : double,
                   discovered : bool, -- for the dfs. init to false
                   explored : bool, -- for the dfs. init to false
                   written : bool,
                   mainMemoryTime : double,
                   mainMemoryTrafficGB : double,
                   lbWriteBytes : double,
                   lbReadBytes : double,
                   workingSet : double, -- in bytes
                   stripCount : int
                 }


orion.fperf = {}
orion.fperf.verbose = orion.verbose
orion.fperf.tune = {}
orion.fperf.tune.perfModelWriteBW = global(&double)
orion.fperf.tune.perfModelLbBW = global(&double)
function makeBWTable(luaTab, terraTab)
  assert(type(luaTab)=="table")

  local startValue, endValue, step = orion.tune.findRange(luaTab)
  local count = ((endValue-startValue)/step)+1

  local terra alloc()
    terraTab = [&double](cstdlib.malloc(count*sizeof(double)))
  end
  alloc()

  local terra set(stripWidth: int, bw:double)
    var index = (stripWidth-startValue)/step
    terraTab[index] = bw
  end

  for k,v in pairs(luaTab) do
    set(k,v)
  end

  return terra(stripWidth:int) : double
    if stripWidth > endValue then 
      return terraTab[count-1] 
    end

    if stripWidth < startValue then 
      return terraTab[0]
    end

    var swi = stripWidth - startValue
    swi = swi - (swi % step)
    
    return terraTab[swi/step]
         end
end

orion.fperf.tune.writeBW = makeBWTable(orion.tune.perfModelWriteBW[orion.tune.cores],orion.fperf.tune.perfModelWriteBW)
orion.fperf.tune.lbBW = makeBWTable(orion.tune.perfModelLBBW[orion.tune.cores], orion.fperf.tune.perfModelLbBW)

struct FperfState {
  nodes : &Node,
  nodeCount : int,
  multinodes : &Multinode,
  bestMultinodes : &Multinode, -- multinodes with the best strip count
  multinodeCount : int,
  lbSchedule : &bool, -- are the nodes fused?
  lbScheduleTrue : &bool, -- all not fused
  dfsStack : &&Multinode,
  dfsStackInput : &int}

terra FperfState:free()
  cstdlib.free(self.nodes)
  cstdlib.free(self.multinodes)
  cstdlib.free(self.bestMultinodes)
end

terra Multinode:init(S:&FperfState, rungroupId : int)
--  self.kernelCount = 0
  self.id = rungroupId
  self.boundaryPixels = 0
  self.discovered = false
  self.explored = false
  self.written = false
  self.time = 0
  self.computeTime = 0
  self.totalArea = 0
  self.mainMemoryTime = 0
  self.mainMemoryTrafficGB = 0
  self.lbTime = 0
  self.workingSet = 0
  self.lbWriteBytes = 0
  self.lbReadBytes = 0
  self.stripCount = -1

  for i=0,S.nodeCount do
    self.inputs[i] = false
    self.inputsCounted[i] = false
  end

end

terra Multinode:copyFrom(S:&FperfState, other : &Multinode)
  self.id = other.id
  self.boundaryPixels = other.boundaryPixels
  self.discovered = other.discovered
  self.explored = other.explored
  self.written = other.written
  self.time = other.time
  self.computeTime = other.computeTime
  self.totalArea = other.totalArea
  self.mainMemoryTime = other.mainMemoryTime
  self.mainMemoryTrafficGB = other.mainMemoryTrafficGB
  self.lbTime = other.lbTime
  self.workingSet = other.workingSet
  self.lbWriteBytes = other.lbWriteBytes
  self.lbReadBytes = other.lbReadBytes
  self.stripCount = other.stripCount

  for i=0,S.nodeCount do
    self.inputs[i] = other.inputs[i]
    self.inputsCounted[i] = other.inputsCounted[i]
  end

end

terra Multinode:alloc(S:&FperfState)
  self.inputs = [&bool](cstdlib.malloc(S.nodeCount*sizeof(bool)))
  self.inputsCounted = [&bool](cstdlib.malloc(S.nodeCount*sizeof(bool)))
end

terra orion.fperf.start()
  var fps = [&FperfState](cstdlib.malloc(sizeof(FperfState)))
  return fps
end

terra FperfState:allocateNodes(count:int)
  self.nodeCount = count
  self.nodes = [&Node](cstdlib.malloc(count*sizeof(Node)))

  -- obv we don't know how many multinodes there are until we have a
  -- schedule. So just allocate this conservatively
  self.multinodes = [&Multinode](cstdlib.malloc(count*sizeof(Multinode)))
  self.bestMultinodes = [&Multinode](cstdlib.malloc(count*sizeof(Multinode)))
  self.multinodeCount = 0

  self.lbSchedule = [&bool](cstdlib.malloc(count*sizeof(bool)))
  self.lbScheduleTrue = [&bool](cstdlib.malloc(count*sizeof(bool)))

  -- init the memory for each multinode
  -- do this conservatively
  for i=0,count do
    self.multinodes[i]:alloc(self)
    self.bestMultinodes[i]:alloc(self)
    self.lbSchedule[i] = false
    self.lbScheduleTrue[i] = true
  end

  self.dfsStack = [&&Multinode](cstdlib.malloc(count*sizeof([&Multinode])))
  self.dfsStackInput = [&int](cstdlib.malloc(count*sizeof(int)))

end

terra Node:init(fperfs : &FperfState)
  self.delay = 0
  self.lastDelay = 0
  self.rungroup:init()
end

terra FperfState:addNode(
    id:int, 
    isSingle:bool,
    inputCount:int, 
    consumerCount:int, 
    time:double, 
    negOverhang : int,
    byteSize : int,
    boundaryPixels : double,
    specialReadBytes : double,
    specialReadWidthBytes : double)

  self.nodes[id].id = id
  self.nodes[id].isSingle = isSingle
  self.nodes[id].time = time
  self.nodes[id].inputs = [&&Node](cstdlib.malloc(inputCount*sizeof([&Node])))
  self.nodes[id].inputEdgeId = [&int](cstdlib.malloc(inputCount*sizeof(int)))
  self.nodes[id].inputCount = inputCount
  self.nodes[id].consumers = [&&Node](cstdlib.malloc(consumerCount*sizeof([&Node])))
  self.nodes[id].consumerEdgeId = [&int](cstdlib.malloc(consumerCount*sizeof(int)))
  self.nodes[id].consumerCount = consumerCount
  self.nodes[id].rungroup = tUF.makeSet()
  self.nodes[id].delay = 0
  self.nodes[id].lastDelay = 0
  self.nodes[id].negOverhang = negOverhang
  self.nodes[id].bytes = byteSize
  self.nodes[id].boundaryPixels = boundaryPixels
--  cstdio.printf("USRB %d %f\n",id,specialReadBytes)
  self.nodes[id].specialReadBytes = specialReadBytes
  self.nodes[id].specialReadWidthBytes = specialReadWidthBytes
end

terra FperfState:setNodeFunctions(
    id:int, 
    maxWidth : {&bool, int} -> double,
    lineWidth : {&bool, int} -> double,
    recomputeArea : {&bool, int} -> double)

  self.nodes[id].maxWidth = maxWidth
  self.nodes[id].lineWidth = lineWidth
  self.nodes[id].recomputeArea = recomputeArea
end    
terra FperfState:addInput(nodeId:int, inputIndex:int, inputId:int, edgeId:int)
  self.nodes[nodeId].inputs[inputIndex] = &self.nodes[inputId]
  self.nodes[nodeId].inputEdgeId[inputIndex] = edgeId
end

terra FperfState:addConsumer(nodeId:int, consumerIndex:int, consumerId:int, edgeId:int)
  self.nodes[nodeId].consumers[consumerIndex] = &self.nodes[consumerId]
  self.nodes[nodeId].consumerEdgeId[consumerIndex] = edgeId
end

-- keep the GC from deleting these too early
orion.fperf._maxWidthFunctionCache = setmetatable({},{__mode="k"})
orion.fperf._lineWidthFunctionCache = setmetatable({},{__mode="k"})
orion.fperf._recomputeAreaFunctionCache = setmetatable({},{__mode="k"})

function orion.fperf.free()
  orion.fperf._maxWidthFunctionCache = setmetatable({},{__mode="k"})
  orion.fperf._lineWidthFunctionCache = setmetatable({},{__mode="k"})
  orion.fperf._recomputeAreaFunctionCache = setmetatable({},{__mode="k"})
end

function FperfState.methods:uploadScheduledIR(scheduledIR)

  if orion.printstage then
    print("uploadScheduledIR")
  end

  -- we name the nodes and edges with an int index
  -- this will allow us to send schedules back and forth between
  -- lua and terra
  local LuaState = {nodeId=0,
                    LNodeNames={}, -- scheduledIRNode -> number
                    edgeId=0,
                    LEdgeNames={} -- scheduledIRNodeConsumer -> scheduledIRNodeInput -> number
  }

  self:allocateNodes(scheduledIR:S("*"):count())

  local outputImages = {}

  local internalIRNodeNames = {}
  scheduledIR:S("*"):traverse(
    function(node)
      LuaState.LNodeNames[node] = LuaState.nodeId

      if node.kind=="single" then

        internalIRNodeNames[node.irNode] = LuaState.nodeId

        -- unfortunately, we need to do this correctly (thread the images through for real)
        -- b/c if not the bogus values will potentially cause crashes
        outputImages[node] = orion.imageWrapper.new(
              {kind="buffer",
               orionType=node.kernel.type,
               terraType=node.kernel.type:toTerraType(),
               region=orion.cropIR.explicit(0,0,100,100),
               refCount=0})

        if node.kernel.type~=orion.type.float(32) then
          print("WARNING: node output isn't float32",node:name(), node.kernel.type:str())
        end

        local kernel = node.kernel:S("load"):process(
          function(n)
            assert(outputImages[n.from]~=nil)
            return orion.internalIR.new({kind="loadConcrete",
                                         type = n.type,
                                         from = outputImages[n.from],
                                         x=0,y=0}):copyMetadataFrom(n)
                                         
                                         
          end)

        -- special images aren't listed in the children,
        -- but we do need to account for their BW
        local specialList = {}

        kernel = kernel:S("special"):process(
          function(n)
            local w = orion._boundImages[n.id+1].width
            local h = orion._boundImages[n.id+1].height
            local s = terralib.sizeof(orion._boundImages[n.id+1].type:toTerraType())
            specialList[n.id] = {w*s,w*h*s}
--            print("SS",specialList[n.id])

            local lo = orion.imageWrapper.new(
              {kind="special",
               id = n.id,
               data = symbol(int),
               orionType=n.type,
               terraType=n.type:toTerraType(),
               region=orion.cropIR.empty(),
               refCount=0})

            return orion.internalIR.new({kind="loadConcrete",
                                         type = n.type,
                                         from = lo,
                                         x=0,y=0}):copyMetadataFrom(n)
          end)

        kernel = kernel:S("gather"):process(
          function(n)
            assert(n.input.kind=="loadConcrete")
            local gather = n:shallowcopy()
            gather.kind="gatherConcrete"
            gather.from = n.input.from
            gather.input = nil
            return orion.internalIR.new(gather):copyMetadataFrom(n)
          end)

        local validRegionArea
        
        if kernel.kind=="cropBaked" then
          validRegionArea = kernel.crop:getArea()
          kernel = kernel.expr
        else
          validRegionArea = node.neededRegion:getArea()
        end

        local boundaryPixels = node.neededRegion:getArea()- validRegionArea
        assert(kernel:S("cropBaked"):count()==0)

        local kernel = orion.flatIR.toFlatIR(kernel)
        local time = kernel:time(outputImages[node])

        local specialReadBytes = 0
        local specialReadWidth = 0
        local specialReadCount = 0

        for _,s in pairs(specialList) do
          specialReadBytes = specialReadBytes + s[2]
          specialReadWidth = specialReadWidth + s[1]
          specialReadCount = specialReadCount + 1
        end

--        print("nct",terralib.sizeof(node.kernel.type:toTerraType()))
--        node.kernel.type:print()

        self:addNode(
          LuaState.nodeId, 
          true,
          node:childrenCount(),
          node:parentCount(scheduledIR),
          time,
          node.kernel:stencil():negOverhang(2),
          terralib.sizeof(node.kernel.type:toTerraType()),
          boundaryPixels,
          specialReadBytes,
          specialReadWidth)

      elseif node.kind=="toAOS" or
        node.kind=="toSOA" or
        node.kind=="multiout" then

        local orionType
        local terraType
        local byteSize

        if node.kind=="toAOS" then
          assert(node.child1.kind=="single")
          orionType = orion.type.array(node.child1.kernel.type, node:childrenCount())
          terraType = orionType:toTerraType()
          byteSize = terralib.sizeof(terraType)
        elseif node.kind=="toSOA" then
          assert(orion.type.isArray(node.type)==false)
          orionType = node.type
          terraType = orionType:toTerraType()
          byteSize = terralib.sizeof(terraType)
        else
          -- multiout
          -- the types don't matter - we should never actually consume them
          orionType = orion.type.bool()
          terraType = bool
          byteSize = 0
        end

        local specialReadBytes = 0
        local specialReadWidth = 0

        if node.kind=="toSOA" then
          -- we will say that a toSOA node reads all channels. This is
          -- sort of true
          local w = orion._boundImages[node.special+1].width
          local h = orion._boundImages[node.special+1].height
          local s = terralib.sizeof(orion._boundImages[node.special+1].type:toTerraType())
          specialReadBytes = w*h*s
          specialReadWidth = w*s
        end

        -- we need to create this here b/c other nodes will consume it
        outputImages[node] = orion.imageWrapper.new(
              {kind="buffer",
               orionType=orionType,
               terraType=terraType,
               region=orion.cropIR.explicit(0,0,100,100),
               refCount=0})

        self:addNode(
          LuaState.nodeId, 
          false,
          node:childrenCount(),
          node:parentCount(scheduledIR),
          0,
          0,
          byteSize,
          0,
          specialReadBytes,
          specialReadWidth)

      else
        print(node.kind)
        assert(false)
      end

      assert(LuaState.LEdgeNames[node]==nil)
      LuaState.LEdgeNames[node] = {}
      
      local i=0
      for k,v in node:children() do
--        assert(LuaState.LEdgeNames[node][v]==nil)
        if LuaState.LEdgeNames[node][v]==nil then
          LuaState.LEdgeNames[node][v] = LuaState.edgeId
          LuaState.edgeId = LuaState.edgeId+1
        else
--          print("double edge")
--          assert(false)
        end

        self:addInput(LuaState.nodeId,i,LuaState.LNodeNames[v],LuaState.LEdgeNames[node][v])
        i=i+1
      end

      LuaState.nodeId = LuaState.nodeId + 1
    end)

  if orion.printstage then print("stage2") end
  -- this need to come later b/c we need to fill in the
  -- names for everybody first
  scheduledIR:S("*"):traverse(
    function(node)

      local i=0
      for parentNode,k in node:parents(scheduledIR) do
        self:addConsumer(LuaState.LNodeNames[node],i,LuaState.LNodeNames[parentNode], LuaState.LEdgeNames[parentNode][node])
        i=i+1
      end


      if node.kind=="toSOA" or
        node.kind=="toAOS" or
        node.kind=="multiout" then

        local lbSchedule = symbol(&bool)
        local stripCount = symbol(int)
        
        local area
        local width
        if node.kind=="toAOS" then
          local tarea, twidth

          for k,v in node:children() do
            assert(v.kind=="single")

            if tarea == nil then
              tarea = v.neededRegion:getArea()
            else
              assert(tarea == v.neededRegion:getArea())
            end

            if twidth == nil then
              twidth = v.neededRegion:getWidth()
            else
              assert(twidth == v.neededRegion:getWidth())
            end

          end

          area = tarea
          width = twidth
        elseif node.kind=="toSOA" then
          width = orion._boundImages[node.special+1].width
          local h = orion._boundImages[node.special+1].height
          area = width*h
        else
          area = 0
          width = 0
        end
        
        local terra maxWidth([lbSchedule], [stripCount]) : double
          return width
        end
        orion.fperf._maxWidthFunctionCache[node] = maxWidth
        
        -- lineWidth should never be used
        -- b/c it shouldn't be possible to fuse in toSOA or toAOS
        local terra lineWidth([lbSchedule], [stripCount]) : double
          orionAssert(false,"LW")
        end
        orion.fperf._lineWidthFunctionCache[node] = lineWidth
        
        local terra recomputeArea([lbSchedule], [stripCount]) : double
          return area
        end
        orion.fperf._recomputeAreaFunctionCache[node] = recomputeArea
        
        self:setNodeFunctions(
          LuaState.LNodeNames[node],
          maxWidth:getpointer(),
          lineWidth:getpointer(),
          recomputeArea:getpointer())
        
      elseif node.kind=="single" then
        -----------------
        local lbSchedule = symbol(&bool)
        local crop = node.neededRegion:S("strip"):process(
          function(n)
            assert(internalIRNodeNames[n.node]~=nil)
            local nodeId = internalIRNodeNames[n.node]
            return n.expr:stripOptional(lbSchedule, nodeId)
          end)
        
        assert(crop:S("strip"):count()==0)

        -------------------------------
        local stripList={}
        local stripId = symbol(int)
        local stripCount = symbol(int)
        local stripSymbolCache={[stripCount]={[stripId]={}}}
        
        local res = crop:stripRuntime(stripId, stripCount, stripList, stripSymbolCache)
        local terra maxWidth([lbSchedule], [stripCount]) : double
          var stripsPerCore = orion.cropIR.stripsPerCoreTerra(stripCount)
          
          var maxWidth : double = 0
          for [stripId]=0,stripsPerCore*orion.tune.cores do
            stripList
            if res.width>maxWidth then maxWidth = res.width end
          end
          
          return maxWidth
        end
        orion.fperf._maxWidthFunctionCache[node] = maxWidth
        -----------------
        local terra lineWidth([lbSchedule], [stripCount]) : double
          var stripsPerCore = orion.cropIR.stripsPerCoreTerra(stripCount)
          
          var maxWidth : double = 0
          for [stripId]=0,stripsPerCore*orion.tune.cores do
            stripList
            var w = res:growToNearestX(orion.tune.V).width
            if w>maxWidth then maxWidth = w end
          end
          
          return maxWidth
        end
        orion.fperf._lineWidthFunctionCache[node] = lineWidth
        ------------------------------
        local stripList={}
        local stripId = symbol(int)
        local stripCount = symbol(int)
        local stripSymbolCache={[stripCount]={[stripId]={}}}
        
        local res = crop:stripRuntime(stripId, stripCount, stripList, stripSymbolCache)

        -- we need to check if there are any active strip nodes
        local skip = symbol(bool)
        local skiplist = {}
        crop:S("stripOptional"):traverse(
          function(n)
            table.insert(skiplist,quote if lbSchedule[n.nodeId]==false then skip=false end end)
          end)

        local terra recomputeArea([lbSchedule], [stripCount]) : double

          var [skip] = true
          skiplist

          if skip then
            return [crop:getArea()]
          else

            var stripsPerCore = orion.cropIR.stripsPerCoreTerra(stripCount)

            var totalArea : double = 0
            for [stripId]=0,stripsPerCore*orion.tune.cores do
              stripList
              totalArea = totalArea + res:area()
            end
            
            return totalArea
          end
        end
        orion.fperf._recomputeAreaFunctionCache[node] = recomputeArea
        -------------------------------
        
        self:setNodeFunctions(
          LuaState.LNodeNames[node],
          maxWidth:getpointer(),
          lineWidth:getpointer(),
          recomputeArea:getpointer())
      else
        print(node.kind)
        assert(false)
      end

    end)

  return LuaState
end

function orion.fperf.luaToFastSchedule(LuaState, scheduledIR, schedule, stripCounts)

  if orion.printstage then
    print("luaToFastSchedule")
  end

  local terra makeSchedule(edgeCount:int, nodeCount:int)
    var wasSet = [&bool](cstdlib.malloc(sizeof(bool)*edgeCount))
    for i=0,edgeCount do
      wasSet[i] = false
    end

    return cstdlib.malloc(sizeof(bool)*edgeCount), cstdlib.malloc(sizeof(int)*nodeCount), wasSet
  end

  local terra setEdge(schedule:&bool,id:int, fused:bool, ws:&bool)

    orionAssert(id<[LuaState.edgeId],"ideid")

    if ws[id] then
      orionAssert(schedule[id]==fused,"should match")
    else
      schedule[id]=fused
    end

    ws[id] = true
  end

  local terra setStripCount(sc : &int, id:int, tsc : int)
    sc[id] = tsc
  end

  local sched,sc,ws = unpackstruct(makeSchedule(LuaState.edgeId, LuaState.nodeId))

  scheduledIR:S("*"):traverse(
    function(node)
      if node.kind=="single" or
        node.kind=="toAOS" or
        node.kind=="toSOA" or
        node.kind=="multiout" then

        if stripCounts~=nil then
          local stripCount = stripCounts[schedule[node].rungroup]
          assert(type(stripCount)=="number")
          setStripCount(sc,LuaState.LNodeNames[node], stripCount)
        end

        for k,v in node:children() do
          assert(type(schedule[node].rungroup)=="number")
          assert(type(schedule[v].rungroup)=="number")
          local fused = schedule[node].rungroup==schedule[v].rungroup
          assert(type(LuaState.LEdgeNames[node][v])=="number")

          if node.kind=="toAOS" or node.kind=="multiout" or node.kind=="toSOA" then
            assert(fused==false)
          end

          setEdge(sched, LuaState.LEdgeNames[node][v], fused, ws)
        end
      else
        assert(false)
      end
    end)

  local terra check(ws:&bool)
    for i=0,[LuaState.edgeId] do
      orionAssert(ws[i],"ws")
    end
  end

  check(ws)

  if orion.printstage then
    print("luaToFastSchedule done")
  end

  return sched, sc
end

terra FperfState:fixup(schedule : &bool) 
  var anyChanges : bool = false

  -- copypasta
  for i=0,self.nodeCount do
    var n = &self.nodes[i]
    n:init(self)
    orionAssert(tUF.find(n.rungroup).id==-1,"rg-1a")

    for inputIndex=0,n.inputCount do
      if schedule[n.inputEdgeId[inputIndex]] then

        -- can't fuse single and AOS nodes
        if n.isSingle ~= n.inputs[inputIndex].isSingle then
          schedule[n.inputEdgeId[inputIndex]] = false
anyChanges = true
        else
          -- this edge is fused
          tUF.tunion(n.rungroup, n.inputs[inputIndex].rungroup)
          orionAssert(tUF.find(n.rungroup).id==-1,"rg-1c")
        end
      end
    end

    orionAssert(tUF.find(n.rungroup).id==-1,"rg-1")
  end

  self.multinodeCount = 0
  for nodeIndex=0,self.nodeCount do
    var n = &self.nodes[nodeIndex]
    var rg = tUF.find(n.rungroup)

    -- name the rungroup if it isn't already
    if rg.id<0 then 
      rg.id = self.multinodeCount;
      var mn = &self.multinodes[rg.id]
      mn:init(self, rg.id)
      self.multinodeCount = self.multinodeCount + 1;
    end
  end

  for nodeIndex=0,self.nodeCount do
    var n = &self.nodes[nodeIndex]
    var rg = tUF.find(n.rungroup)
    var mn = &self.multinodes[rg.id]

    for inputId=0,n.inputCount do
      var input : &Node = n.inputs[inputId]
      var inputRungroup = tUF.find(input.rungroup)

      -- check that if the input is in the same rungroup, it's fused
      if inputRungroup==rg and schedule[n.inputEdgeId[inputId]]==false then
        schedule[n.inputEdgeId[inputId]] = true
anyChanges = true
      end
    end
  end

  return anyChanges
end

-- if stripCount is 0, search over strip counts and choose the best
terra FperfState:eval(schedule : &bool, requestedStripCount : int, stripCounts : &int)
  orionAssert(requestedStripCount>=0 or requestedStripCount==-1,"stripCount<0")

  -- pass 1: find the rungroups
  -- nodes are in a topological order
  for i=0,self.nodeCount do
    var n = &self.nodes[i]
    n:init(self)
    orionAssert(tUF.find(n.rungroup).id==-1,"rg-1a")

    for inputIndex=0,n.inputCount do
      if schedule[n.inputEdgeId[inputIndex]] then

        -- can't fuse single and AOS nodes
        if n.isSingle ~= n.inputs[inputIndex].isSingle then
return false,0,0,0,0,0,0,0,0,0,0
        end

        -- this edge is fused
        tUF.tunion(n.rungroup, n.inputs[inputIndex].rungroup)
        orionAssert(tUF.find(n.rungroup).id==-1,"rg-1c")

        -- calculate the delay
        var oDelay = n.inputs[inputIndex].delay+n.negOverhang
        if oDelay > n.delay then n.delay = oDelay end
      end
    end

    for inputIndex=0,n.inputCount do
      if schedule[n.inputEdgeId[inputIndex]] then
        -- inform input of the delay we chose
        var input = n.inputs[inputIndex]
        if n.delay > input.lastDelay then
          input.lastDelay = n.delay
        end

      end
    end

    orionAssert(tUF.find(n.rungroup).id==-1,"rg-1")
  end

  -- pass 2: build multinodes
  self.multinodeCount = 0
  for nodeIndex=0,self.nodeCount do
    var n = &self.nodes[nodeIndex]
    var rg = tUF.find(n.rungroup)

    -- name the rungroup if it isn't already
    if rg.id<0 then 
      rg.id = self.multinodeCount;
      self.multinodeCount = self.multinodeCount + 1;
    end

    -- add up memory time for outputs
    n.consumedInternally = false
    n.consumedExternally = false
    self.lbSchedule[nodeIndex] = false

    for consumerId=0,n.consumerCount do
      if schedule[n.consumerEdgeId[consumerId]] then
        -- fused: must be consumed internally
        -- remember: we check that all dependencies in a rungroup are fused
        n.consumedInternally = true
        self.lbSchedule[nodeIndex] = true
      else
        -- not fused: must be consumed externally.
        -- remember: we check that all dependencies in a rungroup are fused
        n.consumedExternally = true
      end
    end

    -- it must be consumed externally, or why would be we calculating it? (prob a root or something)
    if n.consumerCount==0 then 
      n.consumedExternally = true 
    end

    orionAssert(n.consumedInternally or n.consumedExternally, "ci or ce")

    if n.consumedInternally and n.consumedExternally then
--      cstdio.printf("ROFLCASE %d\n",n.id)
    end

  end

  -- pass 3
  var stripCountI = requestedStripCount
  var endStripCount = requestedStripCount

  if requestedStripCount==0 then
    stripCountI = orion.tune.cores
    endStripCount = 256

    for mnIndex = 0, self.multinodeCount do
      var mn = &self.bestMultinodes[mnIndex]
      mn:init(self, mnIndex)
      mn.time = 10000000
    end
  end

  while stripCountI <= endStripCount do

    var stripCount = stripCountI -- restrict scope

    if stripCountI==-1 then
      stripCountI = 0
    else
      stripCountI = stripCountI*2
    end

    for mnIndex = 0, self.multinodeCount do
      var mn = &self.multinodes[mnIndex]
      mn:init(self, mnIndex)
    end

    for nodeIndex=0,self.nodeCount do
      var n = &self.nodes[nodeIndex]
      var rg = tUF.find(n.rungroup)
      var mn = &self.multinodes[rg.id]
      
      if requestedStripCount == -1 then
        stripCount = stripCounts[nodeIndex]
      end

      mn.boundaryPixels = mn.boundaryPixels + n.boundaryPixels
      
      -- don't count compute time of AOS/multiout
      if n.isSingle then
        mn.computeTime = mn.computeTime + n.recomputeArea(self.lbSchedule, stripCount)*n.time
        mn.totalArea = mn.totalArea + n.recomputeArea(self.lbSchedule, stripCount)
      end
      
      -- if this thing consumed a special node, we need to account for it
      -- note that this BW is bogus - but we don't actually account for this correctly in the
      -- other perf model so whatever
      
      var mainMemoryTrafficGBSpecial = double(n.specialReadBytes) / double(1024*1024*1024)
      mn.mainMemoryTrafficGB = mn.mainMemoryTrafficGB + mainMemoryTrafficGBSpecial
      var stime = (mainMemoryTrafficGBSpecial/orion.fperf.tune.writeBW(n.specialReadWidthBytes))
      mn.mainMemoryTime = mn.mainMemoryTime + stime
      
      if orion.fperf.verbose and mainMemoryTrafficGBSpecial>0 then
        cstdio.printf("mmts %f readBytes %f typeBytes %d width %f time %e\n",
                      mainMemoryTrafficGBSpecial,
                      n.specialReadBytes,
                      n.bytes,
                      n.specialReadWidthBytes,
                      stime)
      end
      
      if n.consumedExternally then
        orionAssert(mn.inputsCounted[n.id]==false, "WT")

        if n.isSingle then -- _DO_ double count stuff for toAOS nodes
          -- aos can consume the same thing twice and we want to count this as two reads to match the other model (but this is prob wrong)
          mn.inputsCounted[n.id] = true
        end

        var rcarea :double = n.recomputeArea(self.lbSchedule, stripCount)
        var mainMemoryTrafficGB = double(rcarea*n.bytes) / double(1024*1024*1024)
        
        var nWidth = n.maxWidth(self.lbSchedule, stripCount)
        
        var nStripBW = orion.fperf.tune.writeBW(nWidth*n.bytes)
        
        mn.mainMemoryTrafficGB = mn.mainMemoryTrafficGB + mainMemoryTrafficGB
        var mtime = (mainMemoryTrafficGB/nStripBW)
        mn.mainMemoryTime = mn.mainMemoryTime + mtime
        
        if orion.fperf.verbose then
          cstdio.printf("mmt %e area %f bytes %d mn %d n %d time %e width %f\n",
                        mainMemoryTrafficGB,
                        rcarea,
                        n.bytes,
                        mn.id,
                        n.id,
                        mtime,
                        nWidth*n.bytes)
        end
        
        if n.consumedInternally then
          -- if this is consumedInternally, then we think it will stay in cache when we consume it,
          -- so we need to count this towards the working set
          
          var lbLines = n.lastDelay - n.delay + 1
          orionAssert(lbLines>=0,"lbl")
          var w = n.lineWidth(self.lbSchedule, stripCount) + orion.imageWrapper.lbPadding*2
          mn.workingSet = mn.workingSet + w*lbLines*n.bytes
        end
      elseif n.consumedInternally and n.consumedExternally==false then
        var lbLines = n.lastDelay - n.delay + 1
        orionAssert(lbLines>=0,"lbll")
        
        var w = n.lineWidth(self.lbSchedule, stripCount) + orion.imageWrapper.lbPadding*2
        mn.workingSet = mn.workingSet + w*lbLines*n.bytes
        mn.lbWriteBytes = mn.lbWriteBytes + n.recomputeArea(self.lbSchedule, stripCount)*n.bytes
      else
        orionAssert(false,"waffle")
      end
      
      for inputId=0,n.inputCount do
        var input : &Node = n.inputs[inputId]
        
        var inputRungroup = tUF.find(input.rungroup)
        
        -- check that if the input is in the same rungroup, it's fused
        -- (simple scheduling error)
        if inputRungroup==rg and schedule[n.inputEdgeId[inputId]]==false then
        -- invalid schedule
return false,0,0,0,0,0,0,0,0,0,0
        end
        
        -- if this is a different RG, add this as an input to the multinode
        if inputRungroup~=rg then
          orionAssert(inputRungroup.id>=0,"inputRungroup.id<0")
          mn.inputs[inputRungroup.id] = true
        end
        
        -- add up the memory time for this node
        if inputRungroup~=rg then
          if mn.inputsCounted[input.id]==false then
            if n.isSingle then -- _DO_ double count stuff for toAOS nodes
              -- aos can consume the same thing twice and we want to count this as two reads to match the other model (but this is prob wrong)
              mn.inputsCounted[input.id] = true
            end

            orionAssert(self.lbSchedule[input.id]==false or input.consumedInternally, "fused?")
            var oldScheduleVal = self.lbSchedule[input.id]
            self.lbSchedule[input.id] = true
            -- this is an external input, need to count it
            
            var sz = input.recomputeArea(self.lbSchedule, stripCount)
            var mainMemoryTrafficGB = double(sz*input.bytes) / double(1024*1024*1024)
            mn.mainMemoryTrafficGB = mn.mainMemoryTrafficGB + mainMemoryTrafficGB
            
            var width = input.maxWidth(self.lbSchedule, stripCount)
            
            if n.isSingle==false then
              -- AOS doesn't strip its inputs
              width = input.maxWidth(self.lbScheduleTrue, stripCount)
            end
            
            var stripBW = orion.fperf.tune.writeBW(width*input.bytes)
            
            var mtime = (mainMemoryTrafficGB/stripBW)
            mn.mainMemoryTime = mn.mainMemoryTime + mtime
            
            if orion.fperf.verbose then
              cstdio.printf("mm5 %f sz %f bytes %d mn %d n %d time %e width %f maxWidth %f\n",
                            mainMemoryTrafficGB,
                            sz,
                            input.bytes,
                            mn.id,
                            input.id,
                            mtime,
                            width*input.bytes,
                            width)
            end
            
            self.lbSchedule[input.id] = oldScheduleVal
          else
            -- This is an image that we're consuming externally. So it's possible
            -- that it's consumedInternally and consumedExternally
            var sz = input.recomputeArea(self.lbSchedule, stripCount)*input.bytes
            var ci = 0
            if input.consumedInternally then ci=1 end
            
            if orion.fperf.verbose then
              cstdio.printf("LBa %f %d\n",sz,ci)
            end
            
            mn.lbReadBytes = mn.lbReadBytes + sz
            
          end
        elseif inputRungroup==rg then
          -- this is fused
          
          if input.consumedExternally==false then
            -- must be consumed internally
            var sz = input.recomputeArea(self.lbSchedule, stripCount)*input.bytes
            
            if orion.fperf.verbose then
              cstdio.printf("LBb %f\n",sz)
            end
            
            mn.lbReadBytes = mn.lbReadBytes + sz
          else
            -- must be consumedExternally
            orionAssert(input.consumedInternally,"should be ci")
            var sz = input.recomputeArea(self.lbSchedule, stripCount)*input.bytes
            
            if orion.fperf.verbose then
              cstdio.printf("LBbb %f\n",sz)
            end
            
            mn.lbReadBytes = mn.lbReadBytes + sz
            -- already accounted for?
            --orionAssert(false,"RR")
          end
        end
      end
    end

    for multinodeIndex = 0, self.multinodeCount do
      var mn = &self.multinodes[multinodeIndex]
      mn.stripCount = stripCount

      mn.lbTime = (mn.lbWriteBytes+mn.lbReadBytes)/(1024*1024*1024*orion.fperf.tune.lbBW(mn.workingSet))

      -- the total perf model
      var coeffComputeTime = [orion.tune.coeff[orion.tune.cores].computeTime]
      var coeffBoundaryPixels = [orion.tune.coeff[orion.tune.cores].boundaryPixels]
      var coeffStripCount = [orion.tune.coeff[orion.tune.cores].stripCount]
      var coeffMainMemoryTime = [orion.tune.coeff[orion.tune.cores].mainMemoryTime]
      var coeffLbTime = [orion.tune.coeff[orion.tune.cores].lbTime]

      var computeTime = coeffComputeTime*mn.computeTime+coeffBoundaryPixels*mn.boundaryPixels+coeffStripCount*mn.stripCount
      var memTime = coeffMainMemoryTime*mn.mainMemoryTime+coeffLbTime*mn.lbTime
      if memTime > computeTime then
        mn.time = memTime
      else
        mn.time = computeTime
      end

      if requestedStripCount == 0 then
        -- fill in the best array
        var mnb = &self.bestMultinodes[multinodeIndex]
        if mnb.time > mn.time then
          mnb:copyFrom(self,mn)
--          cstdio.printf("BETTER TIME %d %d\n",multinodeIndex,stripCount)
        else
--          cstdio.printf("WORSE TIME %f < %f\n",mnb.time,mn.time)
        end
      end
    end

  end


  -- pass 4: check multinodes for cycles, calc model
  -- we check cycles by doing a DFS
  -- by defn DFS goes as deep as possible before popping off
  -- the stack, so if there's a cycle it will be found
  var dfsStack = self.dfsStack
  var dfsStackInput = self.dfsStackInput
  var stackPos = 0
  var multinodeList = self.multinodes
  if requestedStripCount==0 then
    multinodeList = self.bestMultinodes
  end

  -- push root on stack
  -- by defn the last multinode in the topological sort is the root
  -- not that we traverse stuff from root to children in pass 2,
  -- so, root is at index 0
  dfsStack[0] = &multinodeList[self.multinodeCount-1]
  dfsStack[0].discovered = true
  dfsStackInput[0] = 0
  
  var totalTime : double = 0
  var lbTime : double = 0
  var totalArea : double = 0
  var lbWriteTime : double = 0
  var lbReadTime : double = 0
  var lbWriteGB : double = 0
  var lbReadGB : double = 0
  var mainMemoryTime : double = 0
  var mainMemoryTrafficGB : double = 0
  var computeTime : double = 0
  var boundaryPixels : double = 0
  var workingSet : double = 0

  ::nextFrame::
  while stackPos>=0 do -- while there's still nodes on the stack

    -- add up some stuff for the perf model
    -- this should only happen once per multinode!
    if dfsStack[stackPos].written==false then
      var mn = dfsStack[stackPos]

      lbTime = lbTime + mn.lbTime
      totalArea = totalArea + mn.totalArea
      lbWriteTime = lbWriteTime + double(mn.lbWriteBytes)/double(1024*1024*1024*orion.fperf.tune.lbBW(0))
      lbReadTime = lbReadTime + double(mn.lbReadBytes)/double(1024*1024*1024*orion.fperf.tune.lbBW(mn.workingSet))
      lbWriteGB = lbWriteGB + double(mn.lbWriteBytes)/double(1024*1024*1024)
      lbReadGB = lbReadGB + double(mn.lbReadBytes)/double(1024*1024*1024)
      workingSet = workingSet + mn.workingSet
      mainMemoryTime = mainMemoryTime + mn.mainMemoryTime
      mainMemoryTrafficGB = mainMemoryTrafficGB + mn.mainMemoryTrafficGB
      computeTime = computeTime + mn.computeTime
      boundaryPixels = boundaryPixels + mn.boundaryPixels
      totalTime = totalTime + mn.time
      dfsStack[stackPos].written = true
    end

    -- if there's still stuff to explore on the top
    while dfsStackInput[stackPos] < self.multinodeCount do
      var input = &multinodeList[dfsStackInput[stackPos]]
      
      if dfsStack[stackPos].inputs[dfsStackInput[stackPos]] and
        input.explored==false then

        if input.discovered then
          -- found a back edge!
--          cstdio.printf("BackEdge\n")
return false,0,0,0,0,0,0,0,0,0,0
        end

        -- this is a valid edge, put on stack and explore it
        stackPos = stackPos+1
        input.discovered = true
        dfsStack[stackPos] = input
        dfsStackInput[stackPos] = 0
        goto nextFrame
      end
      dfsStackInput[stackPos] = dfsStackInput[stackPos] + 1
    end

    -- we're done with this node, pop
    dfsStack[stackPos].explored = true
    stackPos = stackPos-1
  end

--  cstdio.printf("DONE\n")
--  cstdio.printf("%f %f %f %f %f %f\n",computeTime, mainMemoryTime, mainMemoryTrafficGB, lbTime, boundaryPixels, workingSet)
  return true, totalTime, computeTime, mainMemoryTime, mainMemoryTrafficGB, lbTime, boundaryPixels, workingSet, lbReadGB, lbWriteGB, totalArea
end

terra orion.fperf.numToSchedule(schedule : &bool, number:uint64, edgeCount : int)
  var scheduleCount : uint64 = uint64(1) << edgeCount

  orionAssert(number < scheduleCount,"n<ec")
  
  var mask : uint64 = 1

--  cstdio.printf("N %d ",number)

  for i=0,edgeCount do
    var b = (number and mask) ~= 0
    schedule[i] = b
    
    if b then
--      cstdio.printf("T")
    else
--      cstdio.printf("F")
    end

    mask = mask << 1
  end

--  cstdio.printf("\n")
end

terra orion.fperf.scheduleToNumber(schedule : &bool, edgeCount : int) : uint64
  cstdio.printf("Edges %d\n",edgeCount)
  orionAssert(edgeCount < 64,"too many edges")

  var bit : uint64 = 1
  var number : uint64 = 0

--  cstdio.printf("ST\n")

  for i=0,edgeCount do
    if schedule[i] then
      number = number or bit
--      cstdio.printf("T")
--      cstdio.printf("%d\n",bit)
--      cstdio.printf("%d\n",number)
    else
--      cstdio.printf("F")
    end
    bit = bit << 1
  end

--  cstdio.printf("\n%d\n",number)

  return number
end

-- brute force try all schedules
terra FperfState:bruteForce(edgeCount : int)
  var schedule = [&bool](cstdlib.malloc(sizeof(bool)*edgeCount))

  -- if we have more than 64 edges, we could never brute force this anyway
  orionAssert(edgeCount < 63,"<63")

  cstdio.printf("start brute %d %d\n",edgeCount,orion.tune.cores)

  var validCount = 0
  var bestTime : double = 10000000
  var bestSched : uint64 = 0
  var worstTime : double = 0
  var worstSched : uint64 = 0

  var scheduleCount : uint64 = uint64(1) << edgeCount
  var tick = scheduleCount / 1000
  orionAssert(tick>0, "tick<=0")

  var prog = 0
  for i=0, scheduleCount do
    
    if i%tick==0 then
      cstdio.printf("%d/1000\n",prog)
      prog = prog + 1
    end

    orion.fperf.numToSchedule(schedule,i,edgeCount)
    var valid, time,a,b,c,d,e,f,g,h,j = self:eval(schedule, 0, nil)
    if valid then
      if time < bestTime then bestTime = time; bestSched = i end
      if time > worstTime then worstTime = time; worstSched = i end
      validCount = validCount + 1
    end

  end

  cstdlib.free(schedule)

  cstdio.printf("Valid %d / %d, %d edges \n",validCount,1 << edgeCount, edgeCount)
  cstdio.printf("best %f worst %f \n",bestTime, worstTime)
  cstdio.printf("best sched %d\n",bestSched)
  cstdio.printf("worst sched %d\n",worstSched)

  return bestSched
--  return worstSched
end

-- fastSchedule is an array of bools
function FperfState.methods:extractLuaSchedule(scheduledIR, luaState, fastSchedule)
  assert(type(luaState)=="table")

  self:eval(fastSchedule,0,nil)

  local terra getRungroup(nodeIndex:int)
    var n = &self.nodes[nodeIndex]
    var rg = tUF.find(n.rungroup)
    return rg.id
  end

  local terra getStripCount(nodeIndex:int)
    var mn = &self.bestMultinodes[nodeIndex]
    return mn.stripCount
  end

  local rschedule = {}
  scheduledIR:S("*"):traverse(
    function(ast)
      local nodeId = luaState.LNodeNames[ast]
      rschedule[ast] = {rungroup = getRungroup(nodeId)}
    end)

  local stripCounts = {}
  for i=0, self.multinodeCount-1 do
    stripCounts[i] = getStripCount(i)
    print("SC",stripCounts[i])
  end

  return rschedule, stripCounts
end

randHack = terralib.includecstring [[
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <time.h>

float randf(){
return (float)rand()/(float)RAND_MAX;
             }

void printuint64(uint64_t i){
  printf("%" PRIu64 "\n",i);
}

void randSeed(){
srand(time(0));
               }

]]

cmath = terralib.includec("math.h")

local terra copySched(src:&bool,dst:&bool, edgeCount:int)
  for i=0,edgeCount do dst[i]=src[i]; end
end

local terra printSched(src:&bool, edgeCount:int)
  for i=0,edgeCount do 
    if src[i] then
      cstdio.printf("T")
    else
      cstdio.printf("F")
    end
  end
  cstdio.printf("\n")
end

terra FperfState:simulatedAnnealing(edgeCount : int, invalidEdges : &bool)
  var currentSched = [&bool](cstdlib.malloc(sizeof(bool)*edgeCount))
  var fixedSched = [&bool](cstdlib.malloc(sizeof(bool)*edgeCount))

  randHack.randSeed()

  -- random start condition
  for i=0,edgeCount do
    currentSched[i] = (cstdlib.rand() % 2 == 0) and (invalidEdges[i]==false) -- random
    currentSched[i] = (invalidEdges[i]==false) -- lb all 
  end

  cstdio.printf("SA iter accept time alpha bestTime\n")

  var bestTime : double = 10000000
  var bestSched = [&bool](cstdlib.malloc(sizeof(bool)*edgeCount))
  var currentTime : double = 1000000

  var N = 1000
  var i=0
  
  while i<N or (bestTime > 100 and i < 10000) do
    var flipIndex = 0

    if i>0 then
      for flipCount = 0,1 do
        while true do
          flipIndex = cstdlib.rand() % edgeCount
          
          if invalidEdges[flipIndex]==false then
            currentSched[flipIndex] = not currentSched[flipIndex]
--            cstdio.printf("flip %d\n", flipIndex)
            break
          end
        end
      end

      copySched(currentSched, fixedSched, edgeCount)
      while self:fixup(fixedSched) do 
--        cstdio.printf("fixup\n") 
      end
    end

    var valid, time,a,b,c,d,e,f,g,h,j = self:eval(fixedSched, 0, nil)
    if valid==false then
      time = 100000
    end

    var T = 0.01*(1-float(i)/float(N))
--    var alpha = currentTime/(time*T)
    var rnd : float= randHack.randf()
    var alpha = cmath.exp(-(time-currentTime)/T)
    if time<=currentTime or alpha>randHack.randf() then
        -- accept
--        cstdio.printf("Accept time %f alpha %f rnd %f i %d bestTime %f\n",time,alpha,rnd,i,bestTime)
        cstdio.printf("SA %d true %f %f %f\n",i,time,alpha,bestTime)
        currentTime = time
--        currentSched = orion.fperf.scheduleToNumber(schedule, edgeCount)
        if time < bestTime then 
          bestTime = time 
          --bestSched = currentSched
          copySched(fixedSched,bestSched,edgeCount)
        end
      else
        -- reject
--        cstdio.printf("Reject alpha %f time %f\n",alpha,time)
        cstdio.printf("SA %d false %f %f %f\n",i,time,alpha,bestTime)
        currentSched[flipIndex] = not currentSched[flipIndex]
      end

    i = i + 1
  end

  cstdio.printf("Best Time %f\n", bestTime)
  printSched(bestSched,edgeCount)
--  cstdio.printf("invalid %d\n",orion.fperf.scheduleToNumber(invalidEdges,edgeCount))
--  randHack.printuint64(bestSched)

  return bestSched
end

-- returns a bool array of edges that must be false (AOS etc)
function orion.fperf.noteInvalidEdges(LuaState, scheduledIR)
  local terra makeSchedule(count:int)
    var out = [&bool](cstdlib.malloc(sizeof(bool)*count))
    for i=0,count do
      out[i] = false
    end

    return out
  end

  local sched = makeSchedule(LuaState.edgeId)

  local terra setEdge(schedule:&bool,id:int, fused:bool)
    orionAssert(id<[LuaState.edgeId],"ideid")
    schedule[id]=fused
  end

  scheduledIR:S("*"):traverse(
    function(node)
      if node.kind=="toAOS" or
        node.kind=="toSOA" or
        node.kind=="multiout" then

        for k,v in node:children() do
          local edgeId = LuaState.LEdgeNames[node][v]
          assert(type(edgeId)=="number")
          setEdge(sched,edgeId,true)
        end

      elseif node.kind=="single" then
        for k,v in node:children() do
          local edgeId = LuaState.LEdgeNames[node][v]
          assert(type(edgeId)=="number")

          if v.kind=="toAOS" or
            v.kind=="toSOA" or
            v.kind=="multiout" then
            setEdge(sched,edgeId,true)
          end
        end

      else
        assert(false)
      end
    end)

  return sched
end
