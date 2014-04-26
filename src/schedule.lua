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

