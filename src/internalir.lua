internalIRFunctions={}
setmetatable(internalIRFunctions,{__index=IRFunctions})
internalIRMT={__index=internalIRFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}


-- basically the same as the AST generated from the user's code, but
-- cleaned up and de-sugared.
--
-- translates are converted to canonical format.
-- switch converted to selects

orion.internalIR = {}


function orion.internalIR._toInternalIR(ast, visited)
  assert(type(visited)=="table")
  
  if visited[ast]==nil then
    
    -- chains of transforms should have been eliminated  
    assert(ast.kind~="transformBaked")
    
    local newnode = ast:shallowcopy()
    
    local highestTranslate1 = -10000000
    local highestTranslate2 = -10000000
    local base1,base2
    
    for k,v in ast:children() do
      if v.kind=="transform" then
        assert(false)
      elseif v.kind=="transformBaked" then
        newnode[k],base1,base2=orion.internalIR._toInternalIR(v.expr, visited)
        newnode["translate1_"..k]=v.translate1+base1
        newnode["translate2_"..k]=v.translate2+base2
        newnode["scale1_"..k]=v.scale1
        newnode["scale2_"..k]=v.scale2
      else
        newnode[k],base1,base2=orion.internalIR._toInternalIR(v, visited)
        newnode["translate1_"..k]=base1
        newnode["translate2_"..k]=base2
        newnode["scale1_"..k]=1
        newnode["scale2_"..k]=1
      end
      
      if newnode["translate1_"..k]>highestTranslate1 then highestTranslate1 = newnode["translate1_"..k] end
      if newnode["translate2_"..k]>highestTranslate2 then highestTranslate2 = newnode["translate2_"..k] end
    end
    
    for k,v in ast:children() do
      newnode["translate1_"..k] = newnode["translate1_"..k]-highestTranslate1
      newnode["translate2_"..k] = newnode["translate2_"..k]-highestTranslate2
    end
    
    -- leaf nodes have a base of 0,0
    if ast:childrenCount() == 0 then
      highestTranslate1=0
      highestTranslate2=0
    end
    
    if newnode.kind=="cropBaked" then
      newnode.crop = newnode.crop:translate( highestTranslate1, highestTranslate2 )
    end
    
    visited[ast]={orion.internalIR.new(newnode):copyMetadataFrom(ast), highestTranslate1, highestTranslate2}
  end

  return visited[ast][1], visited[ast][2], visited[ast][3]

end

function orion.internalIR.typedASTToInternalIR(typedAST)

  -- collapse chains of transforms
  typedAST = typedAST:S("transformBaked"):process(
    function(n)
      if n.expr.kind=="transformBaked" then

        assert(n.expr.scale1==1) -- NYI
        assert(n.expr.scale2==1)
        assert(n.scale1==1)
        assert(n.scale2==1)

        -- now compose the transforms
        local newnode = n:shallowcopy()
        newnode.translate1 = newnode.translate1 + n.expr.translate1
        newnode.translate2 = newnode.translate2 + n.expr.translate2
        newnode.expr = n.expr.expr
        return orion.typedAST.new(newnode):copyMetadataFrom(n)
      end
    end)

  -- now move transforms inline
  local res,base1,base2 = orion.internalIR._toInternalIR(typedAST,{})

  res:check()

  if orion.verbose or orion.printstage then
    print("Conversion to InternalIR Done -------------------")
  end

  if orion.verbose then
    res:printpretty()
    print("base",base1,base2)
  end

  return res,base1,base2
end

function orion.internalIR.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,internalIRMT)
end

-- kind of a hack - so that the IR library can shallowcopy and then
-- modify an ast node without having to know its exact type
function internalIRFunctions:init()
  setmetatable(self,nil)
  orion.internalIR.new(self)
end


orion.internalIR._absoluteTranslationCache = setmetatable({}, {__mode="k"})
function internalIRFunctions:absoluteTranslationList(root)
  if orion.internalIR._absoluteTranslationCache[root]==nil then
    orion.internalIR._absoluteTranslationCache[root] = {}
  end
  
  if orion.internalIR._absoluteTranslationCache[root][self]==nil then
    orion.internalIR.buildAbsoluteTranslationCache(root,self)
  end

  assert(orion.internalIR._absoluteTranslationCache[root][self]~=nil)
  
  return orion.internalIR._absoluteTranslationCache[root][self]
end

function internalIRFunctions:absoluteTranslationCount(root)
  return #(self:absoluteTranslationList(root))
end

function orion.internalIR.buildAbsoluteTranslationCache(root,node)
  assert(orion.internalIR.isInternalIR(node))
  
  if orion.internalIR._absoluteTranslationCache[root][node]==nil then
    orion.internalIR._absoluteTranslationCache[root][node]={}
  end

  local transList = {}

  for parentNode,childKey in node:parents(root) do
    local plist = parentNode:absoluteTranslationList(root)
    
    local tx = parentNode["translate1_"..childKey]
    local ty = parentNode["translate2_"..childKey]
    
    for k,v in pairs(plist) do
      local x = v[1]+tx
      local y = v[2]+ty
      transList[x] = transList[x] or {}
      transList[x][y] = 1
    end
  end    

  for x,v in pairs(transList) do
    for y,_ in pairs(v) do 
      assert(type(x)=="number")
      assert(type(y)=="number")
      table.insert(orion.internalIR._absoluteTranslationCache[root][node],{x,y})
    end
  end
end

function orion.internalIR.isInternalIR(ast) return getmetatable(ast)==internalIRMT end

-- returns the stencil with (0,0,0) at the origin
-- if input isn't null, only calculate stencil for this input (a scheduledIR node)
function internalIRFunctions:calculateStencil(input)

  if self.kind=="binop" then
    return self.lhs:stencil(input)
    :translate(self.translate1_lhs,self.translate2_lhs,0)
    :unionWith(self.rhs:stencil(input):translate(self.translate1_rhs,self.translate2_rhs,0))
  elseif self.kind=="multibinop" then
    local res = Stencil.new()

    for i=1,self:arraySize("lhs") do
      res = res:unionWith(self["lhs"..i]:stencil(input):translate(self["translate1_lhs"..i], self["translate2_lhs"..i],0))
    end

    for i=1,self:arraySize("rhs") do
      res = res:unionWith(self["rhs"..i]:stencil(input):translate(self["translate1_rhs"..i], self["translate2_rhs"..i],0))
    end

    return res
  elseif self.kind=="multiunary" then
    local res = Stencil.new()

    for i=1,self:arraySize("expr") do
      res = res:unionWith(self["expr"..i]:stencil(input):translate(self["translate1_expr"..i], self["translate2_expr"..i],0))
    end

    return res
  elseif self.kind=="unary" then
    return self.expr:stencil(input):translate(self.translate1_expr,self.translate2_expr,0)
  elseif self.kind=="assert" then
    return self.cond:stencil(input):translate(self.translate1_cond,self.translate2_cond,0)
    :unionWith(self.expr:stencil(input):translate(self.translate1_expr,self.translate2_expr,0))
  elseif self.kind=="cast" then
    return self.expr:stencil(input):translate(self.translate1_expr,self.translate2_expr,0)
  elseif self.kind=="select" or self.kind=="vectorSelect" then
    return self.cond:stencil(input)
    :translate(self.translate1_cond,self.translate2_cond,0)
    :unionWith(self.a:stencil(input)
           :translate(self.translate1_a,self.translate2_a,0) 
           :unionWith(self.b:stencil(input)
                  :translate(self.translate1_b,self.translate2_b,0)))
  elseif self.kind=="position" or self.kind=="tap" or self.kind=="value" then
    return Stencil.new()
  elseif self.kind=="tapLUTLookup" then
    return self.index:stencil(input):translate(self.translate1_index,self.translate2_index,0)
  elseif self.kind=="special" then
    --if input~=nil then assert(false) end
    return Stencil.new():add(0,0,0)
  elseif self.kind=="load" then
    assert(orion.scheduledIR.isScheduledIR(self.from))
    local s = Stencil.new()
    if input==nil or input==self.from then s = s:add(0,0,0) end
    return s
  elseif self.kind=="gather" then
    --if input~=nil then assert(false) end
    assert(self.input.kind=="load")
    assert(orion.scheduledIR.isScheduledIR(self.input.from))

    if input~=nil and self.input.from~=input then
      return Stencil.new()
    else
      local g = self.input:stencil(input)
      g = g:unionWith(self.hackBL:stencil(input):translate(self.translate1_hackBL, self.translate2_hackBL,0))
      g = g:unionWith(self.hackTR:stencil(input):translate(self.translate1_hackTR, self.translate2_hackTR,0))
      return g:unionWith(self.x:stencil(input)):unionWith(self.y:stencil(input))
    end
  elseif self.kind=="array" then
    local exprsize = self:arraySize("expr")

    local s = Stencil.new()
    s = s:unionWith(Stencil.new())
    for i=1,exprsize do
      s = s:unionWith(self["expr"..i]:stencil(input):translate(
                        self["translate1_expr"..i],
                        self["translate2_expr"..i],
                        0))
    end

    return s
  elseif self.kind=="reduce" then
    local s = Stencil.new()
    local i=1
    while self["expr"..i] do
      s = s:unionWith(self["expr"..i]:stencil(input):translate(self["translate1_expr"..i],self["translate2_expr"..i],0))
      i=i+1
    end
    return s
  elseif self.kind=="index" then
    return self.expr:stencil(input)
  elseif self.kind=="cropBaked" then
    return self.expr:stencil(input)
  end

  print(self.kind)
  assert(false)
end

orion.internalIR._stencilCache = setmetatable({}, {__mode="k"})

-- remember: the stencil is a property of the children of a node, not its parents
-- so we don't need to pass this function a root. This function calculates the stencil 
-- that this node needs to access the leaf nodes (special, load)
function internalIRFunctions:stencil(input)
  local index = input
  if index==nil then index = 0 end

  if orion.internalIR._stencilCache[self]==nil then
    orion.internalIR._stencilCache[self] = setmetatable({}, {__mode="k"})
  end

  if orion.internalIR._stencilCache[self][index] == nil then
    orion.internalIR._stencilCache[self][index] = self:calculateStencil(input)
  end

  return orion.internalIR._stencilCache[self][index]
end

function internalIRFunctions:calcBase(root)
  
end

-- confusingly, we consider some nodes of the graph to be a 'root'
-- even if they aren't the root of the tree. This is because the real
-- root might be a 'multioutput' node (=> the tree has multiple actual roots)
function internalIRFunctions:isARoot(root)

  if self:parentCount(root)==0 then
    assert(self==root)
    return true
  end

  local isRoot = false -- is this ever a root?
  local allRoot = true -- is this always a root?

  for parentNode,childKey in self:parents(root) do
    if parentNode.kind~="toAOS" and parentNode.kind~="multiout" then
      allRoot = false
    end

    if parentNode.kind=="toAOS" or parentNode.kind=="multiout" then
      isRoot = true
    end
  end

  return allRoot
  --if isRoot then assert(allRoot) end
  --return isRoot
end

orion.internalIR._neededRegionCache=setmetatable({}, {__mode="k"})
function internalIRFunctions:neededRegion(root, regionMode, base1, base2, scheduleNodes)
  assert(orion.internalIR.isInternalIR(root))
  assert(type(regionMode)=="string")
  assert(type(base1)=="number")
  assert(type(base2)=="number")
  assert(type(scheduleNodes)=="table")

  if orion.internalIR._neededRegionCache[root]==nil then
    orion.internalIR._neededRegionCache[root]=setmetatable({}, {__mode="kv"})
  end

  if orion.internalIR._neededRegionCache[root][self]==nil then
    local neededRegion
    
    if self:isARoot(root) then
      if self.kind=="cropBaked" then
        neededRegion = orion.cropIR.output( self.crop, regionMode, base1, base2 ):setLinenumber(0):setOffset(0)
      else
        neededRegion = orion.cropIR.infinite():setLinenumber(0):setOffset(0)
      end

    else

      for parentNode,childKey in self:parents(root) do
        local tX = parentNode["translate1_"..childKey]
        local tY = parentNode["translate2_"..childKey]
        local sX = parentNode["scale1_"..childKey]
        local sY = parentNode["scale2_"..childKey]
        
        local parentNR

        if parentNode.kind=="cropBaked" then
          -- the reason we special case cropBaked's here is because
          -- if we want to get the needed region of a cropBaked,
          -- (ie run this fn on a node that's a cropBaked node)
          -- we want to get the needed region that results from tracing
          -- down from its parents, not itself.
          --
          -- remember, the area outside of the crop (valid region) is 
          -- filled with boundary conditions, so we don't want to count
          -- the boundary region as part of the needed region
          --
          -- the subtle thing here is that this code is calculating
          -- the needed region implied by the parent on its children, 
          -- _NOT_ the  needed region of the parent itself. These may 
          -- be different!
          parentNR = parentNode.crop

          -- we need to intersect this crop with its needed region.
          -- This crop may be infinite for example... which is 
          -- technically a no-op if it has a noninfinite crop above it
          local parentNodeNR = parentNode:neededRegion(root,regionMode,base1,base2, scheduleNodes)
          assert(tX==0 and tY==0) -- I don't think I did this right
          --parentNodeNR = parentNodeNR:translate(tX,tY):copyMetadataFrom(parentNR)
          parentNR = parentNR:intersectionCropBaked(parentNodeNR,self)
        elseif parentNode.kind=="toAOS" or parentNode.kind=="multiout" then
          -- if all parents are of this type, this node was considered a root
          -- so this means that it has at least one other parent that's a regular node
          parentNR = orion.cropIR.empty():copyMetadataFrom(parentNode)
          assert(self:parentCount(root)>1)
        else
          parentNR = parentNode:neededRegion(root,regionMode,base1,base2, scheduleNodes)
          parentNR = parentNR:translate(tX,tY):copyMetadataFrom(parentNR)
        end

        -- accumulate the needed region
        if neededRegion==nil then
          neededRegion = parentNR
        else
          neededRegion = neededRegion:unionWith(parentNR):copyMetadataFrom(neededRegion)
        end

      end
    end

    -- schedule nodes may be stripped, so we need to note this
    if scheduleNodes~=nil and scheduleNodes[self] then
      neededRegion = neededRegion:strip(self)
    end

    orion.internalIR._neededRegionCache[root][self] = neededRegion
  end

  return orion.internalIR._neededRegionCache[root][self]
end

