local function checkPrim(n)
  if n.kind=="binop" or n.kind=="load" or n.kind=="crop" or n.kind=="value" or n.kind=="cast" or n.kind=="position" or n.kind=="unary" or n.kind=="select" or n.kind=="delay" then

  elseif n.kind=="array" or n.kind=="vectorSelect" or n.kind=="index" then
    -- an exception?
  else
    print(n.kind)
    assert(false)
  end
end

Depth = 25
Width = 30
MaxFanout = 6
AcceptBad = 0.5
T = 0.5
RandomLayout = true
Effort = 20

local function generateRandomRouting()
  local R = {}
  local d = Depth
  while d>=0 do
    R[d] = {}
    for w=0,Width-1 do
      R[d][w] = {inputs={},parents={},inputsMap={},parentsMap={},placed={}}
      if d<Depth then

        -- choose 5 random inputs
        local seen = {}
        for i=1,MaxFanout do
          
          local n
          if RandomLayout then
            n = math.random(Width)-1
            while seen[n]~=nil do n = math.random(Width)-1 end
          else
            local skip = {0,1,4,math.floor(Width/3), math.floor(Width/2)}
            n = (w+skip[i]) % Width
          end
          
          table.insert(R[d+1][n].parents, R[d][w])
          assert(R[d+1][n].parentsMap[R[d][w]]==nil)
          R[d+1][n].parentsMap[R[d][w]]=1
          table.insert(R[d][w].inputs, R[d+1][n])
          seen[n]=1
        end
        
        assert(#R[d][w].inputs==MaxFanout)
      end
    end
    d = d - 1
  end

  local rFanin = {}
  local rFanout = {}
  for d=0,Depth do
    for w=0,Width-1 do
      local fi = #R[d][w].inputs
      local fo = #R[d][w].parents
      rFanin[fi] = rFanin[fi] or 0
      rFanin[fi] = rFanin[fi] + 1
      rFanout[fo] = rFanout[fo] or 0
      rFanout[fo] = rFanout[fo] + 1
    end
  end
  
  print("FANIN")
  for k,v in pairs(rFanin) do print(k,v) end
  print("FANOUT")
  for k,v in pairs(rFanout) do print(k,v) end

  return R
end

local __chan = {}
local function chan(n,chan)
  assert(darkroom.typedAST.isTypedAST(n))
  assert(type(chan)=="number")
  __chan[n] = __chan[n] or {}
  __chan[n][chan] = __chan[n][chan] or {n=n,chan=chan}
  return __chan[n][chan]
end

local function findInout(node, inout, R, d, w, input)
  assert(darkroom.typedAST.isTypedAST(node.orig))
  assert(darkroom.typedAST.isTypedAST(inout.orig))
  assert(type(d)=="number")
  assert(type(w)=="number")

  local s = R[d][w].inputs
  if input==false then s = R[d][w].parents end

  for k,v in pairs(s) do
    if v.placed[inout]~=nil then return true end
  end

  return false
end

local function evalPlacement(node, R, w)
  assert(darkroom.typedAST.isTypedAST(node.orig))
  assert(w>=0 and w<Width)

  local benefit = 0

  if R[node.depth][w].placed[node]~=nil then return -1000 end  
  if keycount(R[node.depth][w].placed)==0 then benefit = benefit + 1 end

  for _,nn in pairs(node.inputs) do
    if findInout(node,nn,R,node.depth,w,true) then benefit = benefit + 100 end
  end

  for _,nn in pairs(node.outputs) do
    if findInout(node,nn,R,node.depth,w,false) then benefit = benefit + 100 end
  end

  return benefit
end

__depthCache = {}
local function depth(n, root)
  assert(darkroom.typedAST.isTypedAST(n))

  __depthCache[root] = __depthCache[root] or {}
  __depthCache[root][n] = 0
  
  for v,_ in n:parents(root) do
    local d = depth(v,root)+1
    if d>__depthCache[root][n] then __depthCache[root][n] = d end
  end

  return __depthCache[root][n]
end


local function bestPlaceInRow(node,R)
  assert(darkroom.typedAST.isTypedAST(node.orig))

  local bestPlace = 0
  local bestPlacements = {}
  
  for w=0,Width-1 do
    local benefit = evalPlacement(node,R,w)
    
    if benefit>bestPlace then
      bestPlacements={w}
      bestPlace=benefit
    elseif benefit==bestPlace then
      table.insert(bestPlacements, w)
    end
  end
  
  -- choose arbitrarily amoung options
  assert(#bestPlacements>0)
  local w = bestPlacements[math.random(#bestPlacements)]
  
  return w
end

local function placeNode(node, R)
  local w = bestPlaceInRow(node,R)
  R[node.depth][w].placed[node] = 1
end

local function placeGroups(groups, max, root, R)
  local g = max
  while g>=0 do
    if groups[g]~=nil then
      for _,v in pairs(groups[g]) do
        placeNode(v,R)
      end
    end
    g = g-1
  end
end

local function inputChan(thisnode, inputnode)
  assert(type(thisnode.chan)=="number")
  assert(darkroom.typedAST.isTypedAST(inputnode))

  if thisnode.n.type:channels()==inputnode.type:channels() then
    return chan(inputnode, thisnode.chan), inputnode, thisnode.chan
  elseif thisnode.n.type:channels()>1 and inputnode.type:channels()==1 then
    return chan(inputnode, 1), inputnode, 1
  elseif thisnode.n.kind=="index" then
    assert(thisnode.n.index.kind=="value")
    return chan(inputnode, thisnode.n.index.value+1), inputnode, thisnode.n.index.value+1
  else
    print("input",inputnode.type:channels(), inputnode.kind)
    print("this",thisnode.n.type:channels(),thisnode.n.kind)
    print(thisnode.n:linenumber())
    assert(false)
  end
end

local function parentChan(thisnode, parentnode)
  assert(type(thisnode.chan)=="number")
  assert(darkroom.typedAST.isTypedAST(parentnode))

  if thisnode.n.type:channels()==parentnode.type:channels() then
    return chan(parentnode, thisnode.chan), parentnode, thisnode.chan
  elseif parentnode.type:channels()>1 and thisnode.n.type:channels()==1 then
    return chan(parentnode, 1), parentnode, 1
  elseif parentnode.kind=="index" then
    return chan(parentnode,1), parentnode, 1
  else
    print("parent",parentnode.type:channels(), parentnode.kind)
    print("this",thisnode.n.type:channels(),thisnode.n.kind)
    assert(false)
  end
end

local function countInout(n,R,d,w, placedSet)
  assert(darkroom.typedAST.isTypedAST(n.orig))
  assert(type(d)=="number")
  assert(type(w)=="number")
  assert(placedSet==nil or type(placedSet)=="table")

  local cnt = 0
  local pcnt = 0
  local totalValid = 0

  for _,nn in pairs(n.inputs) do
    if placedSet==nil or placedSet[nn] then
      local b = findInout(n,nn,R,d,w,true)
      if b then cnt=cnt+1 end
      totalValid = totalValid + 1
    end
  end
  
  for _,nn in pairs(n.outputs) do
    if placedSet==nil or placedSet[nn] then
      local b = findInout(n,nn,R,d,w,false)
      if b then cnt=cnt+1; pcnt = pcnt + 1 end
      totalValid = totalValid + 1
    end
  end

  return cnt, pcnt, totalValid
end

local function verify(R)
  local placedCells = 0
  local doublePlaced = 0
  local empty = 0

  local missingInout = 0
  local correctInout = 0
  local partialInout = 0

  for d=0,Depth do
    for w=0,Width-1 do
      local pcount = 0

      for n,_ in pairs(R[d][w].placed) do
        local ioc, pcnt = countInout(n,R,d,w)
        local cnt = #n.inputs+#n.outputs

        if ioc==cnt then correctInout = correctInout + 1 end
        if ioc>0 and ioc<cnt then partialInout = partialInout + 1 
          print("PARTIAL","kind",n.orig.kind,ioc,"inpcnt",ioc-pcnt,"parentcnt",pcnt,"expectedinp",#n.inputs,"expectedp",#n.outputs)
          print("line",n.orig:linenumber())
        end
        if ioc==0 then missingInout = missingInout + 1 
        print("MISSING","inputcnt",#n.inputs,"outputcnt",#n.outputs,n.orig.kind) end

        pcount = pcount + 1
      end
      
      if pcount==0 then empty = empty+1 end
      if pcount>1 then doublePlaced = doublePlaced+1 end
      if pcount==1 then placedCells = placedCells+1 end

    end
  end

  print("Total Cells:",Depth*Width)
  print("Empty Cells:", empty)
  print("Cells w/1 placement:", placedCells)
  print("Cells w/>1 placement:", doublePlaced)
  print("missingInout:", missingInout)
  print("partialInout:", partialInout)
  print("correctInout:", correctInout)
end

local function printR(R)
  for d=0,Depth do
    for w=0,Width-1 do
      local c = keycount(R[d][w].placed)

      if c==0 then 
        io.write(".")
      else
        if true then
          local c = 0
          for n,_ in pairs(R[d][w].placed) do
            local cc,_,tv = countInout(n,R,d,w)
            c = c+ (tv-cc)
          end
          io.write(math.min(9,c))
        else
          io.write(c)
        end
      end
    end
    io.write("\n")
  end
end

local function clearPlacement(R)
  for k,v in pairs(R) do
    for kk,vv in pairs(v) do
      R[k][kk].placed = {}
    end
  end
end

local function reduceFanout(kernel)
  assert(darkroom.typedAST.isTypedAST(kernel))

  local rfnodes = {}
  local res = kernel:visitEach(
    function(n,inputs)
      local res = n:shallowcopy()
      for k,v in n:inputs() do
        res[k] = inputs[k]
      end

      if n:parentCount(kernel)>=ReduceFanout then
        local newcount = math.ceil(n:parentCount(kernel)/ReduceFanout)
        local per = math.ceil(n:parentCount(kernel)/newcount)
        rfnodes[n]={at=1,per=per}
      end
    end)

  print("REDUCE FANOUT",kernel:S("*"):count()," to ",res:S("*"):count())
  return res
end

local function retime(kernel)
  assert(darkroom.typedAST.isTypedAST(kernel))
  
  local rt = {}
  local function getrt(node,delay)
    assert(darkroom.typedAST.isTypedAST(node))
    assert(delay>=0)
    if delay==0 then return node end

    rt[node] = rt[node] or {}
    
    if rt[node][delay]==nil then
      rt[node][delay] = darkroom.typedAST.new({kind="delay",expr=getrt(node,delay-1),type=node.type}):copyMetadataFrom(node)
    end

    return rt[node][delay]
  end

  local res = kernel:visitEach(
    function(n,inputs)
      local res = n:shallowcopy()
      for k,v in n:inputs() do
        local d = depth(v,kernel)-depth(n,kernel)-1
        res[k] = getrt(inputs[k],d)
        assert(res[k].type==inputs[k].type)
        assert(res[k].type==v.type)
      end
      return darkroom.typedAST.new(res):copyMetadataFrom(n)
    end)
  
  res:visitEach(
    function(n)
      for k,v in n:inputs() do
        assert(depth(v,res)==depth(n,res)+1)
      end
    end)

  return res
end

local function rowcost(R, d, placedSet)
  assert(d >=0 and d <= Depth)
  assert(placedSet==nil or type(placedSet)=="table")

  local totalCost = 0
  local cost = {}
  local nonzero = {}
  for w=0,Width-1 do

    for n,_ in pairs(R[d][w].placed) do
      assert(n.depth==d)
--      local cnt = #n.inputs+#n.outputs
      local ioc, pcnt, totalCnt = countInout(n,R,d,w, placedSet)

      if cost[w]~=nil then 
        cost[w] = cost[w]+100
      else
        cost[w] = 0
      end

      cost[w] = cost[w] + (totalCnt-ioc)
      totalCost = totalCost + cost[w]
      if cost[w]>0 then table.insert(nonzero,w) end
    end
  end
  return totalCost, cost, nonzero
end

local function allcost(R, placedSet)
  assert(placedSet==nil or type(placedSet)=="table")

  local totalCost = 0
  local cost = {}
  local nonzero = {}
  for d=0,Depth do
    local tc, _
    tc, _, nonzero[d] = rowcost(R,d, placedSet)
    totalCost = totalCost + tc
    cost[d] = tc
  end
  return totalCost, cost, nonzero
end

local function swap(R, d, w1, p1, w2, p2)
  assert(type(d)=="number")
  assert(type(w1)=="number")
  assert(type(w2)=="number")

  if p1~=nil then
    R[d][w1].placed[p1] = nil
    R[d][w2].placed[p1] = 1
  end

  if p2~=nil then
    R[d][w2].placed[p2] = nil
    R[d][w1].placed[p2] = 1
  end
end

local function chooseRandom(R,nonzeroCache)

  local d, w1, p1, w2, p2
  while w1==nil do
    d = math.random(Depth+1)-1
    if #nonzeroCache[d]>0 then
      w1 = nonzeroCache[d][math.random(#nonzeroCache[d])]
      
      local s = math.random(keycount(R[d][w1].placed))
      local i=1
      for n,_ in pairs(R[d][w1].placed) do
        if i==s then p1 = n end
        i=i+1
      end
      assert(p1~=nil)
    end
  end


  if math.random()>AcceptBad then
    w2 = bestPlaceInRow(p1,R,d)
  else
    while w2==nil do
      local tw = math.random(Width)-1
      if tw~=w1 then
        w2=tw
      end
    end
  end

  local s = math.random(keycount(R[d][w2].placed))
  local i=1
  for n,_ in pairs(R[d][w2].placed) do
    if i==s then p2 = n end
    i = i + 1
  end
  
  assert(type(p1)=="table" or type(p2)=="table")
  
  return d, w1, p1, w2, p2
end

local function evalCostDelta(R,costCache, d, placedSet)
  assert(type(d)=="number")

  local da = math.max(d-1,0)
  local db = math.min(Depth,d+1)

  local oldCost = 0
  for d=da,db do oldCost = oldCost + costCache[d] end

  local newCost = 0
  local newCostCache = {}
  local newNonzeroCache = {}
  for d=da,db do 
    local tc, cc, nz = rowcost(R,d, placedSet)
    newCost = newCost + tc
    newCostCache[d] = tc
    newNonzeroCache[d] = nz
  end

  return newCost,oldCost, newCostCache, newNonzeroCache
end

AnnealVerbose = false
local function anneal(R, placedSet, maxRounds)
  assert(placedSet==nil or type(placedSet)=="table")

  local totalCost, costCache, nonzeroCache = allcost(R, placedSet)
  local bestCost = 1000000000
  print("Total Cost", totalCost)
  
  local i=0
  local round = 0
  while totalCost>0 and (maxRounds==nil or round<maxRounds) do
    local d,w1,p1,w2,p2 = chooseRandom(R,nonzeroCache)

    local w1orig = {}
    for k,v in pairs(R[d][w1].placed) do w1orig[k] = v end
    local w2orig = {}
    for k,v in pairs(R[d][w2].placed) do w2orig[k] = v end

    swap(R,d,w1,p1,w2,p2)
    local newCost,oldCost, cc, nz = evalCostDelta(R,costCache,d, placedSet)

--    if costDelta<0 or math.random()<AcceptBad then
    if newCost<oldCost or math.random()<math.exp(-(newCost-oldCost)/T) then
      -- accept
      totalCost = totalCost + (newCost-oldCost)
      for k,v in pairs(cc) do
        costCache[k] = cc[k]
        nonzeroCache[k] = nz[k]
      end
      if AnnealVerbose then io.write("A") end
    else
      R[d][w1].placed={}
      for k,v in pairs(w1orig) do R[d][w1].placed[k] = v end
      R[d][w2].placed={}
      for k,v in pairs(w2orig) do R[d][w2].placed[k] = v end

      if AnnealVerbose then io.write("R") end
    end

    i = i + 1
    if i==100 then
      if AnnealVerbose then print("\ncost",totalCost,"round",round,"best",bestCost) end

      local realcost = allcost(R, placedSet)
      assert(realcost == totalCost)
      i=0
      round = round + 1
    end

    if totalCost<bestCost then bestCost = totalCost end
  end

  return totalCost, bestCost
end

local function flatten(kernel)
  local roots = {}

  local totalCount = 0
  local toflat = {}
  kernel:visitEach(
    function(n)
      checkPrim(n)

      for c=1, n.type:channels() do
        toflat[n] = toflat[n] or {}
        toflat[n][c] = {inputs={}, outputs={}, chan=c, orig=n, depth = depth(n,kernel)}
        totalCount = totalCount + 1
        if n.kind=="array" and false then
          -- special case arrays
          assert(n["expr"..c].type:channels()==1)
          local inp = n["expr"..c]
          assert(toflat[inp]~=nil)
          assert(toflat[inp][1]~=nil)
          table.insert(toflat[n][c].inputs,toflat[inp][1])
          table.insert(toflat[inp][1].outputs, toflat[n][c])
        else
          for k,v in n:inputs() do
            local _,inp,chan = inputChan(chan(n,c),v)
            assert(toflat[inp][chan]~=nil)
            table.insert(toflat[n][c].inputs,toflat[inp][chan])
            table.insert(toflat[inp][chan].outputs, toflat[n][c])
          end
        end

--        for v,_ in n:parents(kernel) do
--          local _,pnt,chan = parentChan(chan(n,c),v)
--          table.insert(toflat[n][c].outputs,{pnt,chan})
--        end

        if n:parentCount(kernel)==0 then
          table.insert(roots,toflat[n][c])
        end
       end      
    end)

--  for k,v in pairs(toflat) do
--    for kk,vv in pairs(v) do
--      for kkk,vvv in pairs(toflat[k][kk].outputs) do
--        assert(toflat[vvv[1]][vvv[2]]~=nil)
--        toflat[k][kk].outputs[kkk] = toflat[vvv[1]][vvv[2]]
--      end
--    end
--  end

  for k,v in pairs(toflat) do
    for kk,node in pairs(v) do
      for k,v in pairs(node.inputs) do
        local idx
        for kk,vv in pairs(v.outputs) do
          if vv==node then idx=kk end
        end
        assert(idx~=nil)
      end

      for k,v in pairs(node.outputs) do
        local idx
        for kk,vv in pairs(v.inputs) do
          if vv==node then idx=kk end
        end
        assert(idx~=nil)
      end
    end
  end

  return roots, totalCount
end

local function visitEach(Q,f)
  local seen = {}
  while #Q>0 do
    f(Q[1])
    for _,v in pairs(Q[1].inputs) do if seen[v]==nil then Q[#Q+1] = v; seen[v]=1 end end
    table.remove(Q,1)
  end
end

local function place(kernel, R)
  kernel:visitEach(
    function(node)
      clearPlacement(R)
      local maxConstraints = 0
      local constraintGroups = {}

      if node.kernel~=nil then
        print("\n\nPlace",node:name(),"cnt",node.kernel)
        local kernel = retime(node.kernel)
        kernel = flatten(kernel)

        visitEach(kernel,
          function(n)

            if true then
              constraintGroups[0] = constraintGroups[0] or {}
              table.insert(constraintGroups[0], n)  
            else
              local nconst = n:inputCount()*n:parentCount(kernel)
              if nconst > maxConstraints then maxConstraints = nconst;print("setMC", nconst, n.kind, n:inputCount(), n:parentCount(kernel)) end
              for c=1, n.type:channels() do
                constraintGroups[nconst] = constraintGroups[nconst] or {}
                table.insert(constraintGroups[nconst], chan(n,c))
              end
            end
          end)

        print("maxConstraints",maxConstraints)

        placeGroups(constraintGroups, maxConstraints, kernel, R)
        verify(R)
        printR(R, kernel)
        anneal(R)
        verify(R, kernel)
      end
    end)
end

local function treeify(node)
  print("TREEIFY")
  assert(#node.outputs>1)

  local a = {inputs={}, outputs={}, chan=node.chan, depth=node.depth, orig=node.orig}
  local b = {inputs={}, outputs={}, chan=node.chan, depth=node.depth, orig=node.orig}

  for k,v in pairs(node.inputs) do
    a.inputs[k] = v
    b.inputs[k] = v

    local idx
    for kk,vv in pairs(v.outputs) do
      if vv==node then idx=kk end
    end
    assert(idx~=nil)
    table.remove(v.outputs,idx)
    table.insert(v.outputs, a)
    table.insert(v.outputs, b)
  end

  for k,v in pairs(node.outputs) do
    local idx
    for kk,vv in pairs(v.inputs) do
      if vv==node then idx=kk end
    end
    assert(idx~=nil)
    table.remove(v.inputs,idx)

    if k>(#node.outputs/2) then
      table.insert(b.outputs, v)
      table.insert(v.inputs,b)
    else
      table.insert(a.outputs, v)
      table.insert(v.inputs,a)
    end
  end

  return {a,b}
end

local function placeSequential(kernel, R)
  kernel:visitEach(
    function(node)
      clearPlacement(R)

      if node.kernel~=nil then
        print("\n\nPlace Sequential",node:name(),"cnt",node.kernel)
        local kernel = retime(node.kernel)
        local Q, totalCnt = flatten(kernel)
        local placedCnt = 1
        local seen = {}
        local placed = {}

        while #Q>0 do

--          if (cost>0 and #Q[1].outputs>4) or (math.random()<0.0 and #Q[1].outputs>1) then
          if #Q[1].outputs>4 then
            local newnodes = treeify(Q[1])
            table.remove(Q,1)
            totalCnt = totalCnt + #newnodes -1
            for k,v in pairs(newnodes) do
              table.insert(Q,1,v)
            end
          else
            print("Place "..placedCnt.."/"..totalCnt)
            placeNode(Q[1],R)
            placed[Q[1]] = 1
            placedCnt = placedCnt + 1
            local cost, best = anneal(R,placed, Effort)
            print("BEst",best)
            printR(R)

            for _,v in pairs(Q[1].inputs) do if seen[v]==nil then Q[#Q+1] = v; seen[v]=1 end end
            table.remove(Q,1)
          end
        end

        verify(R)
        local L = AnnealVerbose
        AnnealVerbose = true
        anneal(R)
        AnnealVerbose = L
      end
    end)
end

local function hist(kernel,stat)
  local fanoutTotal = {}
  kernel:visitEach(
    function(node)
      local fanout = {}
      print(stat,node:name())
      if node.kernel~=nil then
        node.kernel:visitEach(
          function(n)
            checkPrim(n)
            local pc
            local count = n.type:channels()
            if stat=="fanin" then 
              pc = n:inputCount() 
              if n.kind=="array" then pc=1 end
            elseif stat=="depth" then
              pc = depth(n,node.kernel)
            elseif stat=="fanout" then
              pc =  n:parentCount(node.kernel)
            elseif stat=="bytes" then
              pc = n.type:baseType():sizeof()
            elseif stat=="muldepth" then
              pc = depth(n,node.kernel)
              if n.kind=="binop" and n.op=="*" then count=1 else count=0 end
            elseif stat=="adddepth" then
              pc = depth(n,node.kernel)
              if n.kind=="binop" and n.op=="+" then count=1 else count=0 end
            else
              assert(false)
            end
            fanout[pc] = fanout[pc] or 0
            fanout[pc] = fanout[pc]+count
            fanoutTotal[pc] = fanoutTotal[pc] or 0
            fanoutTotal[pc] = fanoutTotal[pc]+count
          end)
      end
      for k,v in pairs(fanout) do
        print(k..", "..v)
      end
    end)

  print(stat,"total")
  for k,v in pairs(fanoutTotal) do
    print(k..", "..v)
  end
end

local origCompile = darkroom.compile
function darkroom.compile(inputImageFunctions, outputImageFunctions, tapInputs, inputWidth, inputHeight, options)
  if options==nil then options={} end

  options.callbackScheduledKernelGraph = function(node) 
    hist(node,"fanout"); hist(node,"fanin"); hist(node,"depth") 
    hist(node,"bytes")
    hist(node,"muldepth")
    hist(node,"adddepth")
    local R = generateRandomRouting()
--    place(node,R)
    placeSequential(node,R)
  end
  local res =  origCompile(inputImageFunctions, outputImageFunctions, tapInputs, inputWidth, inputHeight, options)

  return res
end