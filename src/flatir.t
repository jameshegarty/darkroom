-- this is basically the same as internal IR, but it's flattened out
-- so that each node corresponds to exactly one line in the executable code
-- i.e. if a load from an image is accessed as a stencil, you
-- get one load node for each stencil that it is accessed by

flatIRFunctions={}
setmetatable(flatIRFunctions,{__index=IRFunctions})
flatIRMT={__index=flatIRFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}

orion.flatIR = {}

function flatIRFunctions:irType()
  return "flatIR"
end

function flatIRFunctions:init()
  setmetatable(self,nil)
  orion.flatIR.new(self)
end


-- we take in the real outputImage so that we can produce
-- and store valid results (some programs may check
-- results with asserts etc, we don't want them to fail)
orion.flatIR._timeCache = setmetatable({}, {__mode="k"})
function flatIRFunctions:time(outputImage)
  assert(orion.imageWrapper.isImageWrapper(outputImage))

  if orion.flatIR._timeCache[self]==nil then
  -- take the flatir, and replace all its image reads with
  -- read guaranteed to hit in cache
  local fastVersion = self:S(
    function(n) 
      return n.kind=="loadConcrete" or n.kind=="gatherConcrete"
    end):process(
    function(node)
      local nn = node:shallowcopy()
      local id = node.from.id
      if node.from.kind=="buffer" then id=node.from.register end
      nn.from = orion.imageWrapper.new(
        { kind="cached", 
          shadowKind = node.from.kind,
          id = id,
          orionType = node.from.orionType,
          terraType = node.from.terraType})

      local res = orion.flatIR.new(nn):copyMetadataFrom(node)

      return res
    end)

  -- make sure we didn't miss anything
  fastVersion:S("*"):traverse(
    function(node)
      for k,v in pairs(node) do
        if orion.imageWrapper.isImageWrapper(v) then
--          print(k,node.kind,v.kind)
          assert(v.kind=="cached")
        end
      end
    end)

  -- JIT it
  local x = symbol(int)
  local y = symbol(int)
  local expr,statements=orion.terracompiler.codegen( fastVersion, orion.tune.V, x, y, 0, orion.tune.cores,0 )


  local tt = self.type:toTerraType()

  local oid = outputImage.id
  if outputImage.kind=="buffer" then oid = outputImage.register end

  local output = orion.imageWrapper.new(
    { kind="cached", 
      shadowKind = outputImage.kind,
      id = oid,
      orionType = self.type,
      terraType = tt})


  -- this is kind of hacky. :alloc() for cached
  -- image wrappers isn't attached to a particular
  -- imageWrapper - we just need to call it on
  -- _any_ imageWrapper
  local function allocImageWrapper()
    return output:alloc()
  end

--  local area = 128*128
--  local area = 512*512
--  local area = 1024*1024
--  local area = 32*32

  local terra runit()
    [allocImageWrapper()]
    var [x]
    var [y]

    var start = orion.currentTimeInSeconds()
    
    var area : double = 0
    while true do
      for i=0,[1024*128] do
        statements;
        [output:set(0,expr,orion.tune.V)]
      end
      area = area+1024*128
      var endt = orion.currentTimeInSeconds()

      if (endt-start)>0.02 then break end
    end

    var endt = orion.currentTimeInSeconds()
    orionAssert(endt>start,"lolwtf")
    [output:close()]

--    cstdio.printf("cnt%d\n",cnt)
--    return (endt-start)/(orion.tune.V*orion.tune.cores*area)
    return (endt-start)/(area)
--    return (endt-start)/area
  end
--  runit:printpretty()
--  runit:disas()

  orion.flatIR._timeCache[self] = runit()
  end
  return orion.flatIR._timeCache[self]
end

-- we need to make sure that (3+4)(x+a,y+b) isn't
-- flattened into multiple values (for multiple a,b). actually maybe
-- this should happen higher up in the optimizer.
-- translates of constants are always 0,
-- this fn should just the the stupid thing and
-- assume its input was CSEd correctly
function orion.flatIR._convert(internalIR,x,y,z,mem)
  assert(orion.internalIR.isInternalIR(internalIR))

  if mem[x]==nil then mem[x]={} end
  if mem[x][y]==nil then mem[x][y]={} end
  if mem[x][y][z]==nil then mem[x][y][z]={} end

  if mem[x][y][z][internalIR]==nil then
    local res = internalIR:shallowcopy()

    if internalIR:childrenCount()==0 or internalIR.kind=="gatherConcrete" then
      -- this guy is a leaf
      if internalIR.kind=="loadConcrete" then
        res.x = x
        res.y = y
      elseif internalIR.kind=="special" then
        -- this is for the convolution engine only
        -- shouldn't happen on the cpu
        res.kind = "specialConv"
        res.x = x
        res.y = y
      elseif internalIR.kind=="load" then
        -- this is for the convolution engine only
        -- shouldn't happen on the cpu
        res.kind = "loadConv"
        res.x = x
        res.y = y
        res.index  = 1
      elseif internalIR.kind=="value" then
      elseif internalIR.kind=="tap" then
      elseif internalIR.kind=="position" then
        -- this probably doesn't work for some really subtle reason!
        local tmp = orion.flatIR.new(res):copyMetadataFrom(internalIR)
        res = {kind="binop", lhs=tmp, type = tmp.type, op="+"}

        if internalIR.coord=="x" then
          res.rhs = orion.flatIR.new({kind="value",value=x,type=tmp.type}):copyMetadataFrom(internalIR)
        elseif internalIR.coord=="y" then
          res.rhs = orion.flatIR.new({kind="value",value=y,type=tmp.type}):copyMetadataFrom(internalIR)
        else
          assert(false)
        end
      elseif internalIR.kind=="gatherConcrete" then
        local xexpr = orion.flatIR._convert(internalIR.x, x+res.translate1_x, y+res.translate2_x, 0, mem)
        local yexpr = orion.flatIR._convert(internalIR.y, y+res.translate1_y, y+res.translate2_y, 0, mem)

        local curX = orion.flatIR.new({kind="value",value=x+(internalIR.translate1_hackTR-internalIR.maxX), type=xexpr.type}):copyMetadataFrom(internalIR)
        local curY = orion.flatIR.new({kind="value",value=y+(internalIR.translate2_hackTR-internalIR.maxY), type=yexpr.type}):copyMetadataFrom(internalIR)

        --xexpr = orion.flatIR.new({kind="value",value=0, type=xexpr.type}):copyMetadataFrom(internalIR)
        --yexpr = orion.flatIR.new({kind="value",value=0, type=yexpr.type}):copyMetadataFrom(internalIR)

        res.x = orion.flatIR.new({kind="binop", lhs=xexpr, rhs=curX, type = xexpr.type, op="+"}):copyMetadataFrom(internalIR)
        res.y = orion.flatIR.new({kind="binop", lhs=yexpr, rhs=curY, type = yexpr.type, op="+"}):copyMetadataFrom(internalIR)

        -- we just throw this away, but its needed to get the checkfn to pass
        res.hackBL = orion.flatIR._convert(internalIR.hackBL, x+res.translate1_hackBL, y+res.translate2_hackBL, 0, mem)
        res.hackTR = orion.flatIR._convert(internalIR.hackTR, x+res.translate1_hackTR, y+res.translate2_hackTR, 0, mem)
      else
        print(internalIR.kind)
        assert(false)
      end
    else
      for k,v in internalIR:children() do
        assert(res["scale1_"..k]==1)
        assert(res["scale2_"..k]==1)
        res[k] = orion.flatIR._convert(v,x+res["translate1_"..k],y+res["translate2_"..k],0,mem)
        res["scale1_"..k]=nil
        res["scale2_"..k]=nil
        res["translate1_"..k]=nil
        res["translate2_"..k]=nil
      end
    end

    mem[x][y][z][internalIR] = orion.flatIR.new(res):copyMetadataFrom(internalIR)
  end

  return mem[x][y][z][internalIR]  
end

function orion.flatIR.toFlatIR(internalIR)
  assert(orion.internalIR.isInternalIR(internalIR))

  if orion.printstage or orion.verbose then
    print("toFlatIR")
  end

  local res = orion.flatIR._convert(internalIR,0,0,0,{})
  res:check()

  return res
end

function orion.flatIR.isFlatIR(ast) 
  return getmetatable(ast)==flatIRMT 
end

function orion.flatIR.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,flatIRMT)
end
