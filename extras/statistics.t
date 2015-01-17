local function checkPrim(n)
  if n.kind=="binop" or n.kind=="load" or n.kind=="crop" or n.kind=="value" or n.kind=="cast" or n.kind=="position" or n.kind=="unary" or n.kind=="select" or n.kind=="delay" then

  elseif n.kind=="array" or n.kind=="vectorSelect" or n.kind=="index" then
    -- an exception?
  else
    print(n.kind)
    assert(false)
  end
end

Depth = 18
Width = 20
MaxFanout = 20
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
          local n = math.random(Width)-1
          while seen[n]~=nil do n = math.random(Width)-1 end

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
  local s = R[d][w].inputs
  if input==false then s = R[d][w].parents end

  for k,v in pairs(s) do
    if v.placed[inout]~=nil then return true end
  end

  return false
end

local function evalPlacement(node, root, R, d, w)
  local benefit = 0

  if R[d][w].placed[node]~=nil then return -1000 end  
  if keycount(R[d][w].placed)==0 then benefit = benefit + 1 end

  for _,nn in node.n:inputs() do
    if findInout(node,chan(nn,node.chan),R,d,w,true) then benefit = benefit + 100 end
  end

  for nn,_ in node.n:parents(root) do
    if findInout(node,chan(nn,node.chan),R,d,w,false) then benefit = benefit + 100 end
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


local function placeNode(node, root, R)
  local d = depth(node.n, root)
  
  local count = math.ceil(node.n:parentCount(root)/MaxFanout)
  for c=1,count do
    local bestPlace = 0
    local bestPlacements = {}

    for w=0,Width-1 do
      local benefit = evalPlacement(node,root,R,d,w)

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

    R[d][w].placed[node] = 1
  end
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

local function countInout(n,R,d,w,root)
  local cnt = 0
  for k,nn in n.n:inputs() do
    local b = findInout(n,chan(nn,n.chan),R,d,w,true)
    if b then cnt=cnt+1 end
--    if b==false then allInout=false end
  end
  
  for nn,_ in n.n:parents(root) do
    local b = findInout(n,chan(nn,n.chan),R,d,w,false)
    if b then cnt=cnt+1 end
--    if b then anyInout=true end
--    if b==false then allInout=false end
  end

  return cnt
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
        --local anyInout = false
        --local allInout = true
        local ioc = countInout(n,R,d,w,root)
        local cnt = n.n:inputCount()+n.n:parentCount(root)

        if ioc==cnt then correctInout = correctInout + 1 end
        if ioc>0 and ioc<cnt then partialInout = partialInout + 1 end
        if ioc==0 then missingInout = missingInout + 1 
        print("MISSING",n.n:inputCount(),n.n:parentCount(root),n.n.kind) end

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

local function place(kernel, R)
  kernel:visitEach(
    function(node)
      clearPlacement(R)
      local maxConstraints = 0
      local constraintGroups = {}

      if node.kernel~=nil then
        print("\n\nPlace",node:name())
        local kernel = retime(node.kernel)
        kernel:visitEach(
          function(n)
            checkPrim(n)
            local nconst = n:inputCount()*n:parentCount(kernel)
            if nconst > maxConstraints then maxConstraints = nconst;print("setMC", nconst, n.kind, n:inputCount(), n:parentCount(kernel)) end
            for c=1, n.type:channels() do
              constraintGroups[nconst] = constraintGroups[nconst] or {}
              table.insert(constraintGroups[nconst], chan(n,c))
            end
          end)

        print("maxConstraints",maxConstraints)

        placeGroups(constraintGroups, maxConstraints, kernel, R)
        verify(R, kernel)
        printR(R)
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
    linearplace(node)
  end
  local res =  origCompile(inputImageFunctions, outputImageFunctions, tapInputs, inputWidth, inputHeight, options)

  return res
end