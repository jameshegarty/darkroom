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
Width = 40
MaxFanout = 5
AcceptBad = 0.5
T = 0.5
RandomLayout = true

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

local function placeNode(node, root, R)
  local w = bestPlaceInRow(node,R)
  R[node.depth][w].placed[node] = 1
end

local function placeGroups(groups, max, root, R)
  local g = max
  while g>=0 do
    if groups[g]~=nil then
      for _,v in pairs(groups[g]) do
        placeNode(v,root,R)
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

local function countInout(n,R,d,w)
  assert(darkroom.typedAST.isTypedAST(n.orig))

  local cnt = 0
  local pcnt = 0

  for _,nn in pairs(n.inputs) do
    local b = findInout(n,nn,R,d,w,true)
    if b then cnt=cnt+1 end
  end
  
  for _,nn in pairs(n.outputs) do
    local b = findInout(n,nn,R,d,w,false)
    if b then cnt=cnt+1; pcnt = pcnt + 1 end
  end

  return cnt, pcnt
end

local function verify(R, root)
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
        local ioc, pcnt = countInout(n,R,d,w,root)
        local cnt = #n.inputs+#n.outputs

        if ioc==cnt then correctInout = correctInout + 1 end
        if ioc>0 and ioc<cnt then partialInout = partialInout + 1 
          print("PARTIAL","kind",n.orig.kind,ioc,"inpcnt",ioc-pcnt,"parentcnt",pcnt,"expectedinp",#n.inputs,"expectedp",#n.outputs)
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

local function printR(R,root)
  for d=0,Depth do
    for w=0,Width-1 do
      local c = keycount(R[d][w].placed)

      if false then
        c = 0
        for n,_ in pairs(R[d][w].placed) do
          local ioc = countInout(n,R,d,w,root)
          local cnt = n.n:inputCount()+n.n:parentCount(root)
          if ioc==0 then c=c+1 end
        end
      end

      if c==0 then 
        io.write(".")
      else
        io.write(c)
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

local function rowcost(R, kernel, d)
  assert(d >=0 and d <= Depth)

  local totalCost = 0
  local cost = {}
  local nonzero = {}
  for w=0,Width-1 do

    for n,_ in pairs(R[d][w].placed) do
      local cnt = #n.inputs+#n.outputs
      local ioc, pcnt = countInout(n,R,d,w,kernel)

      if cost[w]~=nil then 
        cost[w] = cost[w]+100
      else
        cost[w] = 0
      end

      cost[w] = cost[w] + (cnt-ioc)
      totalCost = totalCost + cost[w]
      if cost[w]>0 then table.insert(nonzero,w) end
    end
  end
  return totalCost, cost, nonzero
end

local function allcost(R, kernel)
  local totalCost = 0
  local cost = {}
  local nonzero = {}
  for d=0,Depth do
    local tc, _
    tc, _, nonzero[d] = rowcost(R,kernel,d)
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

local function evalCostDelta(R,kernel,costCache, d)
  assert(type(d)=="number")
  local da = math.max(d-1,0)
  local db = math.min(Depth,d+1)

  local oldCost = 0
  for d=da,db do oldCost = oldCost + costCache[d] end

  local newCost = 0
  local newCostCache = {}
  local newNonzeroCache = {}
  for d=da,db do 
    local tc, cc, nz = rowcost(R,kernel,d)
    newCost = newCost + tc
    newCostCache[d] = tc
    newNonzeroCache[d] = nz
  end

  return newCost,oldCost, newCostCache, newNonzeroCache
end

local function anneal(R, kernel)
  local totalCost, costCache, nonzeroCache = allcost(R,kernel)
  local bestCost = 1000000000
  print("Total Cost", totalCost)
  
  local i=0
  local round = 0
  while totalCost>0 do
    local d,w1,p1,w2,p2 = chooseRandom(R,nonzeroCache,kernel)

    local w1orig = {}
    for k,v in pairs(R[d][w1].placed) do w1orig[k] = v end
    local w2orig = {}
    for k,v in pairs(R[d][w2].placed) do w2orig[k] = v end

    swap(R,d,w1,p1,w2,p2)
    local newCost,oldCost, cc, nz = evalCostDelta(R,kernel,costCache,d)

--    if costDelta<0 or math.random()<AcceptBad then
    if newCost<oldCost or math.random()<math.exp(-(newCost-oldCost)/T) then
      -- accept
      totalCost = totalCost + (newCost-oldCost)
      for k,v in pairs(cc) do
        costCache[k] = cc[k]
        nonzeroCache[k] = nz[k]
      end
      io.write("A")
    else
      R[d][w1].placed={}
      for k,v in pairs(w1orig) do R[d][w1].placed[k] = v end
      R[d][w2].placed={}
      for k,v in pairs(w2orig) do R[d][w2].placed[k] = v end

      io.write("R")
    end

    i = i + 1
    if i==100 then
      print("\ncost",totalCost,"round",round,"best",bestCost)

      local realcost = allcost(R,kernel)
      assert(realcost == totalCost)
      i=0
      round = round + 1
    end

    if totalCost<bestCost then bestCost = totalCost end
  end
end

local function flatten(kernel)
  local roots = {}

  local toflat = {}
  kernel:visitEach(
    function(n)
      checkPrim(n)

      for c=1, n.type:channels() do
        toflat[n] = toflat[n] or {}
        toflat[n][c] = {inputs={}, outputs={}, chan=c, orig=n, depth = depth(n,kernel)}
        for k,v in n:inputs() do
          local _,inp,chan = inputChan(chan(n,c),v)
          assert(toflat[inp][chan]~=nil)
          table.insert(toflat[n][c].inputs,toflat[inp][chan])
        end

        for v,_ in n:parents(kernel) do
          local _,pnt,chan = parentChan(chan(n,c),v)
          table.insert(toflat[n][c].outputs,{pnt,chan})
        end

        if n:parentCount(kernel)==0 then
          table.insert(roots,toflat[n][c])
        end
       end      
    end)

  for k,v in pairs(toflat) do
    for kk,vv in pairs(v) do
      for kkk,vvv in pairs(toflat[k][kk].outputs) do
        assert(toflat[vvv[1]][vvv[2]]~=nil)
        toflat[k][kk].outputs[kkk] = toflat[vvv[1]][vvv[2]]
      end
    end
  end

  return roots
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
        verify(R, kernel)
        printR(R, kernel)
        anneal(R,kernel)
        verify(R, kernel)
      end
    end)
end

local function placeSequential(kernel, R)
  kernel:visitEach(
    function(node)
      clearPlacement(R)

      if node.kernel~=nil then
        print("\n\nPlace Sequential",node:name(),"cnt",node.kernel)
        local kernel = retime(node.kernel)

        kernel:S("*"):processReverse(
          function(n)
            
          end)
      end
    end)
end

local function findFirstFree(d,target,octopied)
  octopied[d] = octopied[d] or {}
  while octopied[d][target]~=nil do target=target+1 end
  return target
end

local function simpleLinearPlace(node, root, cache, octopied)
  assert(darkroom.typedAST.isTypedAST(node))

  if cache[node]==nil then
    local d = depth(node,root)
    
    local target = findFirstFree(d,0,octopied)
    for n,_ in node:parents(root) do
      local t = simpleLinearPlace(n,root,cache,octopied)
      if t>target then target = findFirstFree(d,t,octopied) end
    end

    cache[node] = target
    assert(octopied[d][target]==nil)
    octopied[d][target] = node
  end

  return cache[node]
end

local function verifyLinear(kernel,cache)
  local dists = {}
  kernel:visitEach(
    function(n)
      local l = cache[n]
      local maxl = 0
      for k,v in n:inputs() do
        local ll = cache[v]
        assert(ll>=l)
        if ll>maxl then maxl = ll end
      end
      dists[maxl] = dists[maxl] or 0
      dists[maxl] = dists[maxl] + 1
    end)

  print("DISTS")
  for k,v in pairs(dists) do
    print(k,v)
  end
end

local function drawLinear(octopied)
  local cnt = 0
  for d=0,#octopied do
    local max = keycount(octopied[d])
    for w=0,max do
      if octopied[d][w]~=nil then
        io.write("X")
        cnt = cnt + 1
      else
        io.write(".")
      end
    end
    io.write("\n")
  end
  print("total count",cnt)
end

local function linearplace(kernel, R)
  kernel:visitEach(
    function(node)
      if node.kernel~=nil then

        local kernel = retime(node.kernel)

        print("\n\nPlace",node:name(),"nodecount",kernel:S("*"):count())

        local linearPlaceCache = {}
        local octopied = {}

        kernel:visitEach(
          function(n)
            checkPrim(n)
            simpleLinearPlace(n,kernel, linearPlaceCache, octopied)
          end)

        verifyLinear(kernel,linearPlaceCache)
        drawLinear(octopied)
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
    place(node,R)
--    linearplace(node)
  end
  local res =  origCompile(inputImageFunctions, outputImageFunctions, tapInputs, inputWidth, inputHeight, options)

  return res
end