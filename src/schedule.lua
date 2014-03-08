-- this is various scheduling heuristics that operate on the scheduled IR

orion.schedule = {}
orion.schedule.verbose = orion.verbose

UF = {}

-- this is really inefficient, but whatever
function UF.MakeSet()
  local s = {}
  s.parent = s
  return s
end
 
function UF.Find(x)
  assert(type(x)=="table")
  if x.parent == x then
    return x
  else
    return UF.Find(x.parent)
  end
end
 
function UF.Union(x, y)
  assert(type(x)=="table")
  assert(type(y)=="table")

  local xRoot = UF.Find(x)
  local yRoot = UF.Find(y)
  xRoot.parent = yRoot
end
--------------------
function orion.schedule.stripWidthHeuristic(scheduledLoopIR)
  assert(orion.scheduledIR.isScheduledIR(scheduledLoopIR))

  local stripWidths = {}

  scheduledLoopIR:visitEach(
    function(n)
      if n.kind=="loop" then
        stripWidths[n.loop] = orion.tune.cores
      end
    end)

  return stripWidths
end

function orion.schedule.stripWidthExplicit(scheduledLoopIR, width)
  assert(orion.scheduledIR.isScheduledIR(scheduledLoopIR))

  local stripWidths = {}
  scheduledLoopIR:visitEach(
    function(n)
      if n.kind=="loop" then
        stripWidths[n.loop] = width
      end
    end)

  return stripWidths
end

function orion.schedule.stripWidthUniform( schedule )
  local out = {}

  for k,v in pairs(schedule) do
    assert(type(v.rungroup)=="number")
    out[v.rungroup] = orion.tune.cores*math.pow(2,math.random(0,7))
--    out[v.rungroup] = orion.tune.cores
  end

  return out
end

function orion.schedule.stripWidthBrute(loopIR)

  local stripWidths = {}
  local bestStripWidths = {}
  local bestStripTimes = {}

  for w=1,10 do
    loopIR:visitEach(
      function(n)
        if n.kind=="loop" then
          stripWidths[n.loop] = w
        end
      end)

    local perfModel = orion.schedule.computePerfModel( loopIR, stripWidths )

    loopIR:visitEach(
      function(n)
        if n.kind=="loop" then
          local t = perfModel.nodes[n.loop:name()].time
          if bestStripWidths[n.loop]==nil or
            t < bestStripTimes[n.loop] then
            bestStripWidths[n.loop] = w
            bestStripTimes[n.loop] = t
          end
        end
      end)
    
  end

  return bestStripWidths
end

function orion.schedule.writeJSON(scheduledIR, schedule)
  print("{")
  print("  \"nodes\":[")

  local nodes = {}
  local nodeIndex = {}
  scheduledIR:S("*"):traverse(
    function(ast)
      table.insert(nodes,ast)
      nodeIndex[ast] = #nodes-1
      local comma = ""
      if ast~=scheduledIR then comma="," end
      print("      {\"name\":\""..ast:name().."\",\"group\":"..schedule[ast].rungroup..",\"kind\":\""..ast.kind.."\"}"..comma)

    end)

  print("  ],")
  print("  \"links\":[")
  
  for k,v in pairs(nodes) do
    local i=1
    local cc =  v:childrenCount()
    for kk,vv in v:children() do
      local comma = ""
      
      if (k==#nodes and i==cc)==false then
        comma = ","
      end
      i = i+1
      print("    {\"source\":"..nodeIndex[vv]..",\"target\":"..(k-1)..",\"value\":1}"..comma)
    end
  end
  print("  ]")
  print("}")
end

---------------------------------------------------
-- examine the scheduledIR ndoes that were determined to
-- be interesting and decide how to execute them (fused, materialized, LBed)

-- materialize all intermediates
function orion.schedule.allMaterialized(inast)
  assert(orion.scheduledIR.isScheduledIR(inast))
  local schedule={}
  local rungroup = 0
  inast:S("*"):traverse(function(ast) 
                          schedule[ast]={rungroup=rungroup}; 
                          rungroup = rungroup+1 end)
  return schedule
end

function orion.schedule.allLB(inast)
  assert(orion.scheduledIR.isScheduledIR(inast))
  local schedule={}
  local rungroup = 1

  -- build a UF. we don't want to linebuffer disjoint sets
  -- ex: if a toAOS node holds two trees, even though we want
  -- to linebuffer everything we can't linebuffer the two
  -- tree together... fusing in toAOS nodes isn't supported.
  local uf = {}
  inast:S("*"):traverse(function(ast)
                          uf[ast] = UF.MakeSet()
                          if ast.kind=="single" then 
                            for k,v in ast:children() do
                              -- can't fuse in AOS, etc yet
                              if v.kind=="single" then
                                UF.Union(uf[ast],uf[v])
                              end
                            end
                          end
                        end)

  local ufToRG = {}
  inast:S("*"):traverse(function(ast)
                          local ufnode = UF.Find(uf[ast])

                          if ufToRG[ufnode]==nil then 
                            ufToRG[ufnode] = rungroup
                            rungroup = rungroup + 1
                          end

                          schedule[ast]={rungroup=ufToRG[ufnode]};
                        end)
  return schedule
end

function orion.schedule.fix(scheduledIR, schedule)
  -- build up depencency graph for rungroups

--  print("FIX")

  local graph = {}
  scheduledIR:S("*"):traverse(
    function(n)
      graph[schedule[n].rungroup] = graph[schedule[n].rungroup] or {}
      for k,v in n:children() do
        if schedule[n].rungroup ~= schedule[v].rungroup then
          graph[schedule[n].rungroup][schedule[v].rungroup] = 1
        end
      end
    end)

  local seen = {}
  local seenStack = {}
  local function dfs(rungroup) 
    assert(type(rungroup)=="number")

--    print("start",rungroup)
    if seen[rungroup] then
      -- we have a circular dependency
--      print("CIRCULAR")
      table.insert(seenStack,rungroup)
      return true
    end
    
    seen[rungroup] = 1
    table.insert(seenStack,rungroup)

    for k,_ in pairs(graph[rungroup]) do
      if dfs(k) then
        return true
      end
    end

--    print("end",rungroup)
    seen[rungroup] = nil
    seenStack[#seenStack] = nil
    return false
  end

  local circular = dfs(schedule[scheduledIR].rungroup)

  if circular then
    -- merge all the rungroups in seen
    local stopat = seenStack[#seenStack]
    local i = #seenStack-1
    while seenStack[i]~=stopat do
--      print("merge",seenStack[i],stopat)
      for k,v in pairs(schedule) do
        if v.rungroup==seenStack[i] then
          schedule[k].rungroup = stopat
        end
      end
      i = i-1
    end

    return true
  end

  return false
end

-- prob is the probability of fusing (0 to 1)
function orion.schedule.randomSchedule(inast, prob)
   assert(type(prob)=="number")
  assert(orion.scheduledIR.isScheduledIR(inast))
  local schedule={}
  local rungroup = 0
  inast:S("*"):traverse(
    function(ast) 
      if ast.kind=="single" then
        schedule[ast]={rungroup=rungroup};
        rungroup = rungroup+1 
        
        
        -- with a certain probability, fuse with parent
        for k,v in ast:children() do
          if math.random()<prob and v.kind=="single" then
            assert(type(schedule[v].rungroup)=="number")
            schedule[ast].rungroup = schedule[v].rungroup
          end
        end
        
      else -- AOS/SOA etc
        schedule[ast]={rungroup=rungroup}; 
        rungroup = rungroup+1 
      end
      
    end)

  return schedule
end

function orion.schedule.chooseRandomNeighbor(root,ast,schedule)
  assert(orion.scheduledIR.isScheduledIR(root))
  assert(orion.scheduledIR.isScheduledIR(ast))
  assert(type(schedule)=="table")

  -- find which rungroups this node neighbors
  local rgSet = {}

  for k,v in ast:children() do
    if schedule[v] and v.kind=="single" then
      rgSet[schedule[v].rungroup] = 1
    end
  end
  
  for v,k in ast:parents(root) do
    if schedule[v] and v.kind=="single" then
      rgSet[schedule[v].rungroup] = 1
    end
  end
  
  -- assign it randomly to one of them
  local rgList = {}
  for k,_ in pairs(rgSet) do table.insert(rgList,k) end          
  if #rgList==0 then 
--    unAssigned = unAssigned+1
    return nil
  else
--    schedule[ast] = {rungroup = rgList[math.random(#rgList)]}
    return rgList[math.random(#rgList)]
  end

end

function orion.schedule.initialSeedSchedule(inast,rungroupCount)
  local q = inast:S("*")

  -- number the nodes
  local numberedNodes = {}
  q:traverse(function(ast) table.insert(numberedNodes,ast) end)

  local chosenNodes = {}
  for i=1,rungroupCount do
    local n = math.random(rungroupCount)
    -- make sure we don't choose the same number twice
    while chosenNodes[n]~=nil do
      n = math.random(rungroupCount)
    end
    chosenNodes[n] = 1
  end

  local schedule = {}

  -- assign these nodes to a RG
  for k,_ in pairs(chosenNodes) do
    schedule[numberedNodes[k]] = {rungroup=k}
  end

  -- grow the seeds
  local unAssigned

  repeat
    unAssigned = 0

    q:traverse(
      function(ast)
        if schedule[ast]==nil then
          local rg = orion.schedule.chooseRandomNeighbor(inast,ast,schedule)
          if rg==nil then
            unAssigned = unAssigned + 1
          else
            schedule[ast] = {rungroup=rg}
          end

        end
      end)

  until unAssigned==0

  return schedule
end

function orion.schedule.annealingStep(inast, schedule)
  assert(orion.scheduledIR.isScheduledIR(inast))
  assert(type(schedule)=="table")

  local newSchedule = {}
  for k,v in pairs(schedule) do
    newSchedule[k] = {rungroup=v.rungroup}
  end

  -- find all of the nodes on boundaries between nodes
  local candidateNodes = inast:S(
    function(ast)
      if ast.kind=="single" then
        for k,v in ast:children() do
          if v.kind=="single" and schedule[v].rungroup~=schedule[ast].rungroup then
            return true
          end
        end
        for n,k in ast:parents(inast) do
          if n.kind=="single" and schedule[n].rungroup~=schedule[ast].rungroup then
            return true
          end
        end
      end

      return false
    end)

  -- this can happen for ex if all boundaries between groups are toAOS nodes or
  -- other things we don't track
  if candidateNodes:count()==0 then
return nil
  end

  -- choose which to flip
  for i=1,2 do
--    assert(candidateNodes:count()>0)
    local choice = math.random(candidateNodes:count())
--  print("c",choice, candidateNodes:count())
    for k,v in ipairs(candidateNodes) do print(k,v) end
    local chosenNode = candidateNodes.list[choice]
    
    local rg = orion.schedule.chooseRandomNeighbor(inast,chosenNode,schedule)
    assert(rg)
    newSchedule[chosenNode].rungroup = rg
  end

  return newSchedule
end

function orion.schedule.uniformSchedule(inast)
  local nodeCount = inast:S("*"):count()

  local q = inast:S("*")

  while 1 do
    local rungroupCount = math.random(nodeCount)

    local schedule = {}

    -- assign rungroups randomly
    q:traverse(
      function(ast)
        local rg = math.random(rungroupCount)
        schedule[ast] = {rungroup=rg}
      end)

    -- make sure we aren't fusing in aos,soa,multi
    local invalid = false
    q:traverse(
      function(ast)
        if ast.kind=="toAOS" or ast.kind=="multiout" then
          for k,v in ast:children() do
            if schedule[ast].rungroup == schedule[v].rungroup then
              invalid = true
            end
          end
        elseif ast.kind=="single" then
          for k,v in ast:children() do
            if (v.kind=="toAOS" or v.kind=="toSOA") and
              schedule[ast].rungroup == schedule[v].rungroup then
              invalid = true
            end
          end
        else
          assert(false)
        end

      end)

    -- rungroups can't be disjoint
    -- we could run this all the way through synthesis,
    -- but let's just test this here and only return
    -- potentially valid schedules.

    -- note: this will not filter out schedules that
    -- result in self loops! That is checked later

    if invalid==false and orion.schedule.isDisjoint(inast,schedule)==false then
      return schedule
    end

  end

end

function orion.schedule.greedySchedule(inast, maxCount)
   assert(type(maxCount)=="number")
  assert(orion.scheduledIR.isScheduledIR(inast))

  local schedule={}
  local rungroups = {} -- rungroup -> list of ast nodes in this rungroup
  local rungroup = 0

  inast:S("*"):traverse(
    function(ast) 
      if ast.kind=="single" then
        
        -- try and see if we can fuse with a child
        for k,v in ast:children() do
          if v.kind=="single" and #rungroups[schedule[v].rungroup]<maxCount then
            --assert(type(schedule[v].rungroup)=="number")
            schedule[ast]={rungroup = schedule[v].rungroup}
            table.insert( rungroups[schedule[v].rungroup], ast)
            break
          end
        end
        
        -- couldn't merge with an existing rungroup
        if schedule[ast]==nil then
          schedule[ast]={rungroup=rungroup};
          rungroups[rungroup] = rungroups[rungroup] or {}
          table.insert(rungroups[rungroup], ast)
          rungroup = rungroup+1 
        end
        
      else -- AOS/SOA etc
        schedule[ast]={rungroup=rungroup}; 
        rungroups[rungroup] = rungroups[rungroup] or {}
        table.insert(rungroups[rungroup], ast)
        rungroup = rungroup+1 
      end
      
    end)

  return schedule
end

---------------------------------------------------
-- these functions decide which internalIR nodes to turn
-- into scheduledIR nodes. Later, we will examine this
-- set of nodes and decide how to execute them.

-- find all the nodes in the internalIR that are accessed as a stencil
-- and return them as a list
function orion.schedule.allStencilNodes(internalIR)
  assert(orion.internalIR.isInternalIR(internalIR))


  local reason = {}

  local thelist = internalIR:S(
    function(node)
      assert(reason[node]==nil)
      reason[node]=1
      local parentCount = node:parentCount(internalIR)

      if parentCount==0 then
        -- root node, should be in the list
        assert(node==internalIR)
        reason[node] = "root"
        return true
      elseif node.kind=="cropBaked" then
        -- for codegen simplicity, we materialize all cropped nodes
        reason[node] = "cropBaked"
        return true
      elseif node.kind=="tap" or
        node.kind=="value" then
        -- these guys are almost certainly accessed as a stencil, but aren't particularly interesting
        return false
      elseif parentCount>1 then
        -- determine if it's being accessed as a stencil
        if node:absoluteTranslationCount(internalIR)>1 then
          reason[node] = "stencil"
          assert(false) -- I'm not convinced this happens
          return true
        end
      end
    end):getList()

  -- invert the list
  local invList = {}
  for k,v in pairs(thelist) do
    invList[v] = reason[v]
  end
  
  return invList
end

function orion.schedule.allNodes(internalIR)

  assert(orion.internalIR.isInternalIR(internalIR))

  local query = internalIR:S("*")
  local thelist = query:getList()

  -- invert the list
  local invList = {}
  for k,v in pairs(thelist) do
    invList[v] = "allmat"
  end
  
  return invList
end

-- the codegen has some hard constraints about what it can do - for
-- example it must materialize inputs to gather. Intead of force schedules
-- to follow these rules, schedules can be whatever, and we enforce the constraints here.

function orion.schedule.addNecessaryNodes(internalIR,scheduleNodes)
  internalIR:S("*"):traverse(
    function(node)
      -- all children of AOS/multiout must be materialized
      local parentRequiresMaterialization = false
      for n,k in node:parents(internalIR) do 
        if n.kind=="toAOS" or n.kind=="multiout" then 
          parentRequiresMaterialization = true 
        end

        if n.kind=="gather" and k=="input" then
          parentRequiresMaterialization = true 
        end
      end

      if node.kind=="toAOS" or 
        parentRequiresMaterialization or 
        node.kind=="toSOA" or
        node.kind=="multiout" then

        scheduleNodes[node] = "necessary"
      end
    end)

  return scheduleNodes
end

function orion.schedule.isDisjoint(scheduledIR, schedule)
  -- check that the rungroups are contiguous
  -- we need a union-find data structure here

  local q = scheduledIR:S("single")

  -- build the union-find
  local uf = {} -- scheduleIRNode -> UF node
  q:traverse(
    function(node)
      uf[node] = UF.MakeSet()
      for k,v in node:children() do
        if schedule[v].rungroup==schedule[node].rungroup then
          if type(uf[v])~="table" then
            print(v.kind)
            assert(false)
          end

          UF.Union(uf[node],uf[v])
        end
      end
    end)

  -- check that rungroups are contiguous
  -- the idea is, we check to see if a rungroup
  -- maps to multiple UF groups. this would mean that
  -- it maps to a non-contiguous region
  local disjoint = false
  local rungroupToUfnode = {} -- rungroup id -> UF node
  q:traverse(
    function(node)
      local rg = schedule[node].rungroup
      if rungroupToUfnode[rg]==nil then
        rungroupToUfnode[rg] = UF.Find(uf[node])
      else
--        assert(rungroupToUfnode[rg] == UF.Find(uf[node]))
        if UF.Find(rungroupToUfnode[rg]) ~= UF.Find(uf[node]) then
          disjoint=true
        end
      end
    end)

  return disjoint
end

-- Input:
--   schedule: scheduledIRNode -> schedule table
--   schedule table contains rungroup, stripWidth

-- Output: delays, children
--
-- delays: rungroup -> array of {scheduledIRNode, delay} pairs
-- The pairs are sorted by the delay (shortest delay first).
-- The reason they need to be sorted is that the consumer uses the line produced
-- by the producer in the same iteration that it's computed. (if not we 
-- would need to store an extra line in the LB). So producers need to come before
-- consumers. 
-- I _think_ that if two kernels have the same delay than the order doesn't matter
-- we disallow pointwise kernels, so if one dependes on another they should have 
-- a different delay
--
-- children: rungroup -> child rungroup id -> 1
-- children is a map of rungroup ids. We don't actually care which output
-- of the rungroup is used... This is only necessary to 
-- produce an ordering on the rungroups that satisfies the dependencies.
--
-- I _think_ that based on our constraints that the rungroups are
-- contiguous, and that the scheduledIR graph is a DAG
-- that this will also be a DAG

if orion.ilp then
  package.path = package.path .. ";../optimal/?.t"
  terralib.require("main")
end

function orion.schedule.simpleLBSchedule(scheduledIR, schedule)

  assert(orion.schedule.isDisjoint(scheduledIR, schedule)==false)

  local ilpR = {}
  if orion.ilp then
    local stap = orion.getScheduleTable(scheduledIR,schedule)

    for k,v in pairs(stap) do
      ilpR[k] = solveschedule(v,false)
    end
  end

  -- calculate delays.
  -- leaf node for a rungroup get 0. and inputs are recorded as children
  -- others get max delay of children
  -- Probably very inefficient!

  local delays = {} -- rungroup -> {{scheduledIRNode, delay}}
  local children = {} -- rungroup -> children -> 1. children are rungroups
  local remap = {} -- scheduledIRNode -> index into delays

  scheduledIR:S("single"):traverse(
    function(node)
      assert(type(schedule[node])=="table")
      local thisRungroup = schedule[node].rungroup
      assert(type(thisRungroup)=="number")
      children[thisRungroup] = children[thisRungroup] or {}

      -- the delay of this node is max {input with longest delay + required stencil}
      for k,v in node:children() do
        local childRungroup = schedule[v].rungroup
        assert(type(childRungroup)=="number")
        if childRungroup~=thisRungroup then
          -- this is an input to the rungroup, record it
          children[thisRungroup][childRungroup] = 1
        end
      end

      -- is this node totally internal?
      local consumedExternally = false
      local consumedInternally = false
      for parentNode,_ in node:parents(scheduledIR) do
        if schedule[parentNode].rungroup~=thisRungroup then
          consumedExternally = true
        else
          consumedInternally = true
        end
      end

      local retime
      
      if orion.ilp  then
        retime = ilpR[thisRungroup][node:name()]

        
        local consumedExternally = false
        local PC = 0
        for parentNode,_ in node:parents(scheduledIR) do
          if parentNode.kind=="single" and schedule[parentNode].rungroup==thisRungroup  then
            print("PC",parentNode:name())
            PC = PC + 1
          end

          if schedule[parentNode].rungroup~=thisRungroup then 
            consumedExternally = true
          end
        end
        
        local CC = 0
        for ch,_ in node:children() do
          if ch.kind=="single" and schedule[ch].rungroup==thisRungroup then 
            CC = CC+1 
          end
        end

        if (consumedExternally or (PC==0 and CC==0)) and retime==nil then
          -- a constant
          retime = 0
        end
        
      else
        retime = 0
      end

      assert(retime~=nil)

      -- it must be consumed externally, or why would be we calculating it? (prob a root or something)
      if node:parentCount(scheduledIR)==0 then consumedExternally = true end

      -- at least one of these must be true!
      assert(consumedExternally or consumedInternally)
      
      delays[thisRungroup] = delays[thisRungroup] or {}
      local delayNode = {scheduledIRNode = node, consumedInternally=consumedInternally, 
                         consumedExternally = consumedExternally, retime = retime}
      table.insert(delays[thisRungroup],delayNode)
      remap[node] = #delays[thisRungroup]
    end)

  -- calc stencil size now that retimings are known
  scheduledIR:S("single"):traverse(
    function(node)
      local thisRungroup = schedule[node].rungroup
      
      local theMax = 0
      
      -- max{-F_e+d_c for all edges e attached to consumers c}
      for consumer,_ in node:parents(scheduledIR) do
        if consumer.kind=="single" and
          schedule[consumer].rungroup==thisRungroup then
          local s = consumer.kernel:stencil(node)
          if s:area()>0 then
            local smin = -s:min(2) + delays[thisRungroup][remap[consumer]].retime
            if smin > theMax then theMax = smin end
          end
        end
      end
      
      
      if delays[thisRungroup][remap[node]].consumedExternally==false then
        delays[thisRungroup][remap[node]].lbSize = -delays[thisRungroup][remap[node]].retime + 1 + theMax 
      else
        delays[thisRungroup][remap[node]].lbSize = 0
      end
    end)

  scheduledIR:S("toSOA"):traverse(
    function(node)
      local thisRungroup = schedule[node].rungroup
      assert(type(thisRungroup)=="number")
      -- toSOA nodes can't be fused in yet! They must be on their own rungroup
      assert(delays[thisRungroup]==nil)

      children[thisRungroup] = {}
      delays[thisRungroup] = {}
      table.insert( delays[thisRungroup], {scheduledIRNode = node, delay=0})
    end)

  return delays, children
end

function orion.schedule.computePerfModel(scheduledLoopIR, stripWidths)
  assert(orion.scheduledIR.isScheduledIR(scheduledLoopIR))
  assert(type(stripWidths)=="table")

  if orion.schedule.verbose or orion.printstage then 
    print("Calculate perf model")
  end

  local res = {}
  res.nodes = {}
  res.compute = {}
  res.latency = {}
  res.memory = {}

  scheduledLoopIR:S("*"):traverse(
    function(snode)

      if snode.kind=="loop" then

        local stripWidth = stripWidths[snode.loop]
        assert(type(stripWidth)=="number")

        local node = snode.loop

        if orion.schedule.verbose then 
          print("node",node:name())
        end
        
        local kCount = node:arraySize("kernel")
        
        -- now find all the images we're reading or writing to main memory
        -- the reason this is a hash is b/c we don't want to double count
        -- stuff that's both consumedExternally and consumedInterally -
        -- everything that is read from images with these settings should remain in cache!
        -- NOTE that in this case we overwrite the image to main memory on the boundries
        -- to make sure this is true! (this is why we count the region as the write region and not the 
        -- read region! the write region should be the union of all the reads or our codegen is fucked!)
        --
        -- notice that the assumption that the input will stay in cache isn't totally true -> it may be accessed
        -- at different delays. Technically we should track this difference in delay and add it to the working set
        local mainMemoryImages = {} -- imageWrapperObject -> {region,typeSize,name,consumedInterally, consumedExternally}
        local workingSet = 0
        local lbWriteBytes = 0
        local lbReadBytes = 0
        local boundaryPixels = 0

        for i=1,kCount do
          local bpix = (node["outputNeededRegion"..i]:getArea() - node["validRegion"..i]:getArea())
          assert(bpix>=0)
          boundaryPixels = boundaryPixels + bpix

          if node["outputImage"..i].kind=="buffer" then
            assert(node["consumedExternally"..i])

            local ci=""

            if node["consumedInternally"..i] then
              -- if this is consumedInternally, then we think it will stay in cache when we consume it,
              -- so we need to count this towards the working set
              workingSet = workingSet + node["outputImage"..i]:lbBytes(stripWidth)
              ci="CI"
            end

            -- outputs that are consumedExternally are streamed out to main memory
            assert(mainMemoryImages[node["outputImage"..i]]==nil) -- images should be written before they're read
            mainMemoryImages[node["outputImage"..i]]={region=node["outputImage"..i].region, 
                                                      typeSize=terralib.sizeof(node["outputImage"..i].terraType),
                                                      name="buffer"..ci,
                                                      consumedInternally = node["consumedInternally"..i],
                                                      consumedExternally = node["consumedExternally"..i]}

          elseif node["outputImage"..i].kind=="linebuffer" then
             local thisSize = node["outputImage"..i]:lbBytes(stripWidth)
             print("WS",thisSize, terralib.sizeof(node["outputImage"..i].terraType),node["outputImage"..i]:lineWidth(stripWidth),node["outputImage"..i].lines,node["kernel"..i]:name())
             workingSet = workingSet + thisSize
             lbWriteBytes = lbWriteBytes + node["outputImage"..i]:recomputeArea(stripWidth)*terralib.sizeof(node["outputImage"..i].terraType)
           else
              assert(false)
           end

          local doubles = {}
          node:map("inputImage"..i.."_",
                   function(v) 
                     if doubles[v]~=nil then
                       -- this shouldn't happen - fperf doesn't support it
                       assert(false)
                     else
                       doubles[v] = 1
                     end
                   end)

          node:map("inputImage"..i.."_",
                   function(v) 
                     -- did this input NOT get linebuffered?
                     if v.kind=="buffer" or v.kind=="special" then

                       -- as mentioned above, if this is consumedExternally, use its region size on write, not on read,
                       -- to account for the overwrite correctly
                       if mainMemoryImages[v]==nil then
                         -- in this case, image is from another rungroup (not this rungroup)
                         -- if it was from _this_ rungroup, it would have already been accounted for above

                          -- so, if this is an input from _another_ rungroup, remove the strip node at the
                          -- top of its crop region to account for overfetch
                          local r = v.region
                          if r.kind=="strip" then
                            r = r.expr
                          end

                          mainMemoryImages[v]={region=r,typeSize=terralib.sizeof(v.terraType),name=v.kind.."Ext"}
                        else
                          --assert(false)
                          -- this branch deals with two cases:
                          -- 1) the image was both consumedExternally and consumedInternally
                          -- (we already added it to the outputs above, but it's showing up again in the input list)
                          -- 2) we're consuming an image from another rungroup twice
                          --
                          --  in these cases we should  count this towards
                          -- linebuffer BW, working set (above), b/c we're not double counting this towards main mem (read + write)
                          -- BW on the assumption that it will stay in cache. We're only counting it to main
                          -- memory bw on the write.
                          local sz = v:recomputeArea(stripWidth)*terralib.sizeof(v.terraType)
                          lbReadBytes = lbReadBytes + sz

                          assert(v.kind~="special")

                          if orion.schedule.verbose then
                            print("LBa",sz,mainMemoryImages[v].consumedInternally, mainMemoryImages[v].consumedExternally, v,mainMemoryImages[v].name,'kernel',i)
                            v:printpretty()
                            --v.region:printpretty()
                          end

                        end
                     elseif v.kind=="linebuffer" then
                       local sz = v:recomputeArea(stripWidth)*terralib.sizeof(v.terraType)
                       lbReadBytes = lbReadBytes + sz

                       if orion.schedule.verbose then
                         print("LBb",sz)
                       end

                       -- notice that we DO NOT add this to the working set a second time. We don't double-store
                       -- the stuff in the LB, even if we consume it twice.
                     else
                        print(v.kind)
                        assert(false)
                     end
                   end)
        end

        local lbTime = (lbWriteBytes+lbReadBytes)/(1024*1024*1024*orion.tune.lbBW(workingSet))
        local lbWriteTime = (lbWriteBytes)/(1024*1024*1024*orion.tune.lbBW(0))
        local lbReadTime = (lbReadBytes)/(1024*1024*1024*orion.tune.lbBW(workingSet))


        -- now sum up the main memory traffic
        local mainMemoryTime = 0 -- in seconds
        local totalMainMemoryTrafficGB = 0 -- in GB

        for k,v in pairs(mainMemoryImages) do
          local mainMemoryTrafficGB = (v.region:getRecomputeArea(stripWidth)*v.typeSize) / (1024*1024*1024)

          totalMainMemoryTrafficGB = totalMainMemoryTrafficGB + mainMemoryTrafficGB

          local width  = v.region:getMaxWidth(stripWidth)
          local stripBW = orion.tune.writeBW(width*v.typeSize)

          local mtime = (mainMemoryTrafficGB / stripBW)
          mainMemoryTime = mainMemoryTime + mtime

          if orion.schedule.verbose then
            print("MMT",
                  mainMemoryTrafficGB,
                  "recomputeArea",
                  v.region:getRecomputeArea(stripWidth),
                  "typeSize",
                  v.typeSize,
                  "stripWidth",
                  stripWidth,
                  v.name,
                  "time",
                  mtime,
                  "width",
                  width*v.typeSize,
                 "maxWith",
                 width,
                 "image",
                 k)
          end
        end

        -- calculate compute time
        local computeTime = 0 -- in seconds
        local totalArea = 0 -- # of pixels computed
        local idealArea = 0 -- if there was no overcompute

        for i=1,kCount do
          local time = node["kernel"..i]:time(node["outputImage"..i])

          local area = node["outputImage"..i]:area()
          local rcarea = node["outputImage"..i]:recomputeArea(stripWidth)
          assert(rcarea == node["outputNeededRegion"..i]:getRecomputeArea(stripWidth))
          assert(rcarea>=area)

          totalArea = totalArea + rcarea
          idealArea = idealArea + area

          computeTime = computeTime + time*rcarea
        end

        res.nodes[node:name()] = {totalMainMemoryTrafficGB = totalMainMemoryTrafficGB, 
                                  totalArea = totalArea,
                                  idealArea = idealArea,
                                  computeTime=computeTime, 
                                  lbTime = lbTime,
                                  mainMemoryTime = mainMemoryTime, 
                                  workingSet=workingSet,
                                  boundaryPixels = boundaryPixels,
                                  lbReadBytes = lbReadBytes,
                                  lbWriteBytes = lbWriteBytes}

        local coeff = orion.tune.coeff[orion.tune.cores]
        local modMem = mainMemoryTime*coeff.mainMemoryTime + lbTime*coeff.lbTime
        local modComp = computeTime*coeff.computeTime+boundaryPixels*coeff.boundaryPixels+stripWidth*coeff.stripCount
        if modMem > modComp then
           res.nodes[node:name()].time=modMem
           res.nodes[node:name()].computeBound=false
        else
           res.nodes[node:name()].time=modComp
           res.nodes[node:name()].computeBound=true
        end

      elseif snode.kind=="index" then

      elseif snode.kind=="toAOS" or
        snode.kind=="multiout" then


        local function handleToAOS(v)
          assert(v.kind=="toAOS")

          local mmt = 0
          local mainMemoryTime = 0

          assert(v.child1.kind=="index")
          assert(v.child1.child1.kind=="loop")
          local img = v.child1.child1.loop["outputImage"..v.child1.index]
          local typeSize = terralib.sizeof(img.terraType)
          local sz = (img.region:getArea()*typeSize)/(1024*1024*1024)
          mmt = mmt+sz*v:childrenCount()
          
          local w = img.region:getWidth()*typeSize
          local stripBW = orion.tune.writeBW(w)
          local mtime = (sz/stripBW)
          mainMemoryTime = mainMemoryTime + mtime*v:childrenCount()
          


          -- account for the fact that we both read and write this data
          -- don't bother recording writes of multiout
          if snode.kind=="toAOS" then
             if orion.schedule.verbose then
                print("MMT aos input ",sz,"time",mtime,"width",w,snode.kind)
             end

            local cc = v:childrenCount()
            local mtime = (sz*cc)/orion.tune.writeBW(w*cc)
            mainMemoryTime = mainMemoryTime+mtime

            if orion.schedule.verbose then
              print("toAOS out traffic ",mmt," time ",mtime,"width",w*cc)
            end

            return mmt*2, mainMemoryTime
          elseif snode.kind=="multiout" then
            local cc = v:childrenCount()
            local mtime = (sz*cc)/orion.tune.writeBW(w*cc)

            if orion.schedule.verbose then
              print("toAOS multiout traffic ",mmt," time ",mtime,"width",w*cc)
            end

            return mmt, mtime
          end


        end

        local mmt, mainMemoryTime

        if snode.kind=="multiout" then
          mmt = 0
          mainMemoryTime = 0

          for k,v in snode:children() do
            if v.kind=="toAOS" then
              local a, b = handleToAOS(v)
              mmt = mmt + a
              mainMemoryTime = mainMemoryTime + b
            elseif v.kind=="index" then
              assert(v.child1.kind=="loop")
              local img = v.child1.loop["outputImage"..v.index]
              local typeSize = terralib.sizeof(img.terraType)
              local sz = (img.region:getArea()*typeSize)/(1024*1024*1024)
              mmt = mmt+sz

              local w = img.region:getWidth()*typeSize
              local stripBW = orion.tune.writeBW(w)
              local mtime = (sz/stripBW)
              mainMemoryTime = mainMemoryTime + mtime

            else
              assert(false)
            end
          end
        elseif snode.kind=="toAOS" then
          mmt, mainMemoryTime = handleToAOS(snode)
        end

        res.nodes[snode:name()] = {time=mainMemoryTime,
                                   totalMainMemoryTrafficGB = mmt,
                                  totalArea = 0,
                                  idealArea = 0,
                                  computeTime=0, 
                                  lbTime = 0,
                                  mainMemoryTime = mainMemoryTime, 
                                  workingSet=0,
                                  boundaryPixels = 0,
                                  lbReadBytes = 0,
                                  lbWriteBytes = 0,
                                  computeBound = false}


--        assert(false)
      elseif snode.kind=="toSOAConcrete" then
--        assert(false)
        -- we will say that a toSOA node reads all channels. This is
        -- sort of true

        local w = orion._boundImages[snode.special+1].width
        local h = orion._boundImages[snode.special+1].height
        local tt = orion._boundImages[snode.special+1].type
        assert(orion.type.isArray(tt))
        local s = terralib.sizeof(tt:toTerraType())
        local ss = terralib.sizeof(orion.type.arrayOver(tt):toTerraType())

        -- remember, we need to account for both the read and the write
        local mmta = (w*h*s)/(1024*1024*1024) -- read
        local mmtb = (w*h*ss)/(1024*1024*1024) -- write

        local mmt = mmta + mmtb
        local mainMemoryTimea = mmta/orion.tune.writeBW(w*s)
        local mainMemoryTimeb = mmtb/orion.tune.writeBW(w*ss)
        local mainMemoryTime = mainMemoryTimea + mainMemoryTimeb

        if orion.schedule.verbose then
          print("toSOA read",mmta,"time",mainMemoryTimea)
          print("toSOA write",mmtb,"time",mainMemoryTimeb)
        end

        res.nodes[snode:name()] = {time=mainMemoryTime,
                                   totalMainMemoryTrafficGB = mmt, 
                                  totalArea = 0,
                                  idealArea = 0,
                                  computeTime=0, 
                                  lbTime = 0,
                                  mainMemoryTime = mainMemoryTime, 
                                  workingSet=0,
                                  boundaryPixels = 0,
                                  lbReadBytes = 0,
                                  lbWriteBytes = 0,
                                  computeBound = false}

        
      else
        print(snode.kind)
        assert(false)
      end

    end)
  
  local tot = 0
  local kernelCount = 0
  local computeBoundCount = 0
  res.totalArea = 0
  res.idealArea = 0
  res.lbTime = 0
  res.workingSet = 0
  res.mainMemoryTime = 0
  res.mainMemoryTraffic = 0
  res.computeTime = 0
  res.lbReadGB = 0
  res.lbWriteGB = 0
  res.boundaryPixels = 0
  for _,v in pairs(res.nodes) do
    tot = tot + v.time
    kernelCount = kernelCount+1
    res.computeTime = res.computeTime + v.computeTime
    res.totalArea = res.totalArea + v.totalArea
    res.boundaryPixels = res.boundaryPixels + v.boundaryPixels
    res.lbTime = res.lbTime + v.lbTime
    res.idealArea = res.idealArea + v.idealArea
    res.workingSet = res.workingSet + v.workingSet
    res.mainMemoryTime = res.mainMemoryTime + v.mainMemoryTime
    res.mainMemoryTraffic = res.mainMemoryTraffic + v.totalMainMemoryTrafficGB
    res.lbReadGB = res.lbReadGB + v.lbReadBytes
    res.lbWriteGB = res.lbWriteGB + v.lbWriteBytes
    if v.computeBound then computeBoundCount = computeBoundCount + 1 end
  end
  res.lbReadGB = res.lbReadGB/(1024*1024*1024)
  res.lbWriteGB = res.lbWriteGB/(1024*1024*1024)
  res.total = tot
  res.kernelCount = kernelCount
  res.computeBoundCount = computeBoundCount

  if orion.printstage then
    print("compute perf model done")
  end

  return res
end
