

function astFunctions:expectedKeycount()
  local baseSize = 1 -- kind

  if self.kind=="func" then
    return baseSize+self:arraySize("identifier")+self:arraySize("arg")
  elseif self.kind=="binop" then
    return baseSize+3
  elseif self.kind=="unary" then
    return baseSize+2
  elseif self.kind=="transform" then
    return baseSize+1+self:arraySize("arg")
  elseif self.kind=="apply" then
    return baseSize+1+self:arraySize("arg")
  elseif self.kind=="value" then
    return baseSize+1
  elseif self.kind=="select" or self.kind=="vectorSelect" then
    return baseSize+3
  elseif self.kind=="cast" then
    return baseSize+2
  elseif self.kind=="array" then
    local exprsize = self:arraySize("expr")
    return baseSize+exprsize
  elseif self.kind=="assert" then
    return baseSize+3
  elseif self.kind=="type" then
    return baseSize+1
  elseif self.kind=="special" then
    return baseSize+1
  elseif self.kind=="position" then
    return baseSize+1
  elseif self.kind=="mapreduce" then
    return baseSize+self:arraySize("varname")*3+2
  elseif self.kind=="mapreducevar" then
    return baseSize+3
  elseif self.kind=="letvar" then
    return baseSize+1 -- kind,variable
  elseif self.kind=="tap" then
    return 4
  elseif self.kind=="tapLUT" then
    return 5 -- kind, type, count, id, tapname
  elseif self.kind=="tapLUTLookup" then
    return 6 -- kind, type, count, id, index, tapname
  elseif self.kind=="lua" then
    return baseSize+1
  elseif self.kind=="reduce" then
    return baseSize+1+self:arraySize("expr")
  elseif self.kind=="crop" then
    assert(orion.type.isCropMode(self.mode))

    if self.mode==orion.cropExplicit then
      return baseSize+2+4
    else
      return baseSize+2
    end

  elseif self.kind=="let" then
    local cnt = self:arraySize("expr")
    return baseSize+cnt*2+1
  elseif self.kind=="index" then
    return baseSize+2
  elseif self.kind=="multiout" then
    return 1+self:arraySize("expr")
  elseif self.kind=="switch" then
    return 1+self:arraySize("expr")+self:arraySize("val")+2 -- controlExpr, default
  elseif self.kind=="gather" then
    return 9 -- kind, input,x,y,maxX,maxY,clamp,hackBL, hackTR
  else
    print(self.kind)
    assert(false)
  end

end

function astFunctions:checkfn()
  assert(type(self.kind)=="string")
  
  if self.kind=="func" then

    local i = 1
    while self["identifier"..i] do
      local v = self["identifier"..i]
      -- the identifier can potentially contain expressions we haven't parsed yet
      assert(type(v)=="string" or type(v)=="number" or (type(v)=="table" and orion.ast.isAST(v)))
      i = i + 1
    end

    local j = 1
    while self["arg"..j] do
      assert(getmetatable(self["arg"..j])==getmetatable(self))
      j=j+1
    end

  elseif self.kind=="binop" then
    assert(getmetatable(self.lhs)==getmetatable(self))
    assert(getmetatable(self.rhs)==getmetatable(self))
    assert(type(self.op)=="string")
  elseif self.kind=="unary" then
    assert(type(self.op)=="string")
    assert(getmetatable(self.expr)==getmetatable(self))
  elseif self.kind=="transform" then
    assert(getmetatable(self.expr)==getmetatable(self))

    local i=1
    while self["arg"..i] do
      local v = self["arg"..i]
      assert(getmetatable(v)==getmetatable(self))
      i=i+1
    end
  elseif self.kind=="apply" then
    assert(getmetatable(self.expr)==getmetatable(self))

    local i=1
    while self["arg"..i] do
      local v = self["arg"..i]
      assert(getmetatable(v)==getmetatable(self))
      i=i+1
    end

  elseif self.kind=="value" then
    assert(type(self.value)=="number" or type(self.value)=="boolean" or type(self.value)=="table")

    if type(self.value)=="table" then
      for k,v in pairs(self.value) do
        assert(type(k)=="number")
        assert(type(v)=="number" or type(v)=="boolean")
      end
    end

    if self.type~=nil then
      if orion.type.isNumber(self.type) then assert(type(self.value)=="number") end
      if orion.type.isBool(self.type) then assert(type(self.value)=="boolean") end
      if orion.type.isArray(self.type) then assert(type(self.value)=="table") end
    end

  elseif self.kind=="select" or self.kind=="vectorSelect" then
    assert(getmetatable(self.cond)==getmetatable(self))
    assert(getmetatable(self.a)==getmetatable(self))
    assert(getmetatable(self.b)==getmetatable(self))
  elseif self.kind=="cast" then
    assert(getmetatable(self.type)==getmetatable(self))
    assert(self.type.kind=="type")
    assert(getmetatable(self.expr)==getmetatable(self))
    assert(self:childrenCount()==2)
  elseif self.kind=="array" then
    local exprsize = self:arraySize("expr")
    self:map("expr", function(n) 
               assert(getmetatable(n)==getmetatable(self))
                     end)

  elseif self.kind=="asserttype" then
    -- asserts that _expr has type _type
    -- _type is either a type (it will do an exact match),
    -- or "float", "int", "uint", "bool", "number" which will match multiple types
    -- a noop (unless the assert fails). returns _expr
    assert(getmetatable(self.expr)==getmetatable(self))
    assert(orion.type.isType(self.type))

  elseif self.kind=="assert" then
    -- asserts that cond is true at runtime. cond must yield a bool
    -- a noop (unless the assert fails). returns expr
    assert(getmetatable(self.expr)==getmetatable(self))
    assert(getmetatable(self.printval)==getmetatable(self))
    assert(getmetatable(self.cond)==getmetatable(self))
  elseif self.kind=="call" then
    assert(type(_args)=="table")
    assert(terralib.isfunction(_tfunc))
    assert(orion.type.isType(_type))
  elseif self.kind=="type" then
    -- returns the type of _expr
    assert(orion.type.isType(self.type))

  elseif self.kind=="special" then
    assert(self:childrenCount()==0)
    assert(type(self.id)=="number")
  elseif self.kind=="position" then
    assert(type(self.coord)=="string")
    assert(self.coord=="x" or self.coord=="y" or self.coord=="z")
  elseif self.kind=="mapreduce" then

    local i=1
    while self["varname"..i]~=nil do
      assert(type(self["varname"..i])=="string")
      assert(orion.ast.isAST(self["varlow"..i]))
      assert(orion.ast.isAST(self["varhigh"..i]))
      i=i+1
    end

    assert(type(self.reduceop)=="string")
    assert(getmetatable(self.expr)==getmetatable(self))
  elseif self.kind=="mapreducevar" then
    assert(type(self.variable)=="string")
    assert(orion.ast.isAST(self.low))
    assert(orion.ast.isAST(self.high))
  elseif self.kind=="letvar" then
    assert(type(self.variable)=="string")
  elseif self.kind=="tap" then
    assert(type(self.id)=="number")
    assert(type(self.tapname)=="string")
    assert(orion.type.isType(self.type))
  elseif self.kind=="tapLUT" then
    assert(type(self.id)=="number")
    assert(type(self.count)=="number")
    assert(type(self.tapname)=="string")
    assert(orion.type.isType(self.type))
  elseif self.kind=="tapLUTLookup" then
    assert(type(self.id)=="number")
    assert(type(self.count)=="number")
    assert(orion.type.isType(self.type))
  elseif self.kind=="lua" then
    assert(type(self.expr)=="function")
  elseif self.kind=="reduce" then
    assert(self.op=="sum" or self.op=="min" or self.op=="max")
    
    assert(self.expr2~=nil) -- better at least have 2 things we're reducing

    local i = 1
    while self["expr"..i] do
      assert(getmetatable(self["expr"..i])==getmetatable(self))
      i=i+1
    end
  elseif self.kind=="crop" then
    assert(orion.type.isCropMode(self.mode))

    if self.mode==orion.cropExplicit then
      assert(type(self.x)=="number")
      assert(type(self.y)=="number")
      assert(type(self.w)=="number")
      assert(self.w>0)
      assert(type(self.h)=="number")
      assert(self.h>0)
    else
    end

    assert(getmetatable(self.expr)==getmetatable(self))
  elseif self.kind=="let" then
    local cnt = self:arraySize("expr")
    self:map("expr", function(n)    assert(getmetatable(n)==getmetatable(self))  end)
    self:map("exprname", function(n) assert(type(n)=="string") end)
    assert(getmetatable(self.res)==getmetatable(self))

  elseif self.kind=="index" then
    assert(getmetatable(self.expr)==getmetatable(self))
    -- orion arrays are 0 indexed
    assert(getmetatable(self.index)==getmetatable(self))
  elseif self.kind=="multiout" then
    self:map("expr",function(n) 
               if getmetatable(n)~=getmetatable(self) then
                 print("RR")
                 n:printpretty()
               end
               assert(getmetatable(n)==getmetatable(self)) end)
  elseif self.kind=="switch" then
    assert(getmetatable(self)==getmetatable(self.controlExpr))
    assert(getmetatable(self)==getmetatable(self.default))

    self:map("expr",function(n,idx) 
               assert(getmetatable(n)==getmetatable(self))
               assert(getmetatable(self["val"..idx])==getmetatable(self)) end)
  elseif self.kind=="gather" then
    assert(getmetatable(self)==getmetatable(self.input))
    assert(getmetatable(self)==getmetatable(self.x))
    assert(getmetatable(self)==getmetatable(self.y))
    assert(getmetatable(self)==getmetatable(self.hackBL))
    assert(getmetatable(self)==getmetatable(self.hackTR))
    assert(type(self.maxX)=="number")
    assert(type(self.maxY)=="number")
    assert(type(self.clamp)=="boolean")
  else
    print(self.kind)
    assert(false)
  end

end

function orion.ast.check(node,options)
  return astFunctions.check(node,options)
end

function astFunctions:printprettys()

  local out

  if self.kind=="func" then
    local i=1
    out=""
    while self["identifier"..i] do
      out = out..self["identifier"..i]
      if self["identifier"..(i+1)] then out=out.."." end
      i=i+1
    end

    if self.arg1~=nil then
      out = out.."("
      local i=1
      while self["arg"..i] do
        out = out..self["arg"..i]:printprettys()
        if self["arg"..(i+1)] then out=out.."," end
        i=i+1
      end
      out = out..")"
    end

  elseif self.kind=="binop" then
    out="("..self.lhs:printprettys()..")"..self.op.."("..self.rhs:printprettys()..")"
  elseif self.kind=="unary" then
    out=self.op.."("..self.expr:printprettys()..")"
  elseif self.kind=="value" then
    out=tostring(self.value)
  elseif self.kind=="special" then
    out="_special_"..self.id
  elseif self.kind=="mapreduce" then
    local vars,i = "",1
    while self["varname"..i] do
      vars = vars.."_mr_"..self["varname"..i].."="..self["varlow"..i]:printprettys()..","..self["varhigh"..i]:printprettys().." "
      i=i+1
    end
    out="map "..vars.." reduce("..self.reduceop..") "..self.expr:printprettys().." end"
  elseif self.kind=="mapreducevar" then
    out="_mr_"..self.variable.."["..self.low:printprettys().." to "..self.high:printprettys().."]"
  elseif self.kind=="letvar" then
    out="_letvar_"..self.variable
  elseif self.kind=="reduce" then
    out = "reduce()"
  elseif self.kind=="position" then
    out="_pos"..self.coord
  elseif self.kind=="tap" then
    out="_tap_"..self.tapname
  elseif self.kind=="tapLUT" then
    out="_tapLUT_"..self.tapname
  elseif self.kind=="tapLUTLookup" then
    out="_tapLUT_"..self.tapname.."["..self.index:printprettys().."]"
  elseif self.kind=="transform" then
    out= self.expr:printprettys()
    if self.arg1~=nil then
      out = out.."("
      local i=1
      while self["arg"..i] do
        out = out..self["arg"..i]:printprettys()..","
        i=i+1
      end
      out = out..")"
    end

  elseif self.kind=="select" or self.kind=="vectorSelect" then
    out="if "..self.cond:printprettys().." then "..self.a:printprettys().." else "..self.b:printprettys().." end"
  elseif self.kind=="lua" then
    out = "luaexpr"
  elseif self.kind=="crop" then
    out = "crop("..self.expr:printprettys()..","..self.mode.name

    if self.mode==orion.cropExplicit then
      out = out.."("..self.x..","..self.y..","..self.w..","..self.h..")"
    end

    out = out..")"
  elseif self.kind=="cast" then
    out = "cast("..self.expr:printprettys()..","..self.type:printprettys()..")"
  elseif self.kind=="type" then
    out = self.type:str()
  elseif self.kind=="let" then
    out = "let \n"
    
    local cnt = 1
    while self["expr"..cnt] do
      out = out .. self["exprname"..cnt] .. " = " .. self["expr"..cnt]:printprettys() .. "\n"
      cnt = cnt + 1
    end

    out = out .. "in " .. self.res:printprettys()
  elseif self.kind=="array" then
    out = "{"
    local cnt = 1
    while self["expr"..cnt] do
      out = out .. self["expr"..cnt]:printprettys() .. ","
      cnt = cnt + 1
    end
    out = out .. "}"
  elseif self.kind=="assert" then
    out = "assert("..self.expr:printprettys()..","..self.printval:printprettys()..","..self.cond:printprettys()..")"
  elseif self.kind=="multiout" then
    out = "multiout("

    local cnt = 1
    while self["expr"..cnt] do
      out = out .. self["expr"..cnt]:printprettys() .. ","
      cnt = cnt + 1
    end

    out = out .. ")"
  elseif self.kind=="switch" then
    out = "switch "..self.controlExpr:printprettys().."\n"
    self:map("expr",function(n,idx)
               out = out .. self["val"..idx]:printprettys() .. " => "..n:printprettys().."\n"
                    end)
    out = out.."default => "..self.default:printprettys().."\n"
    out = out.."end"
  elseif self.kind=="gather" then
    out = "gather(\n"..self.input:printprettys()..",\n"
    out = out..self.x:printprettys()..",\n"
    out = out..self.y:printprettys()..",\n"
    out = out..tostring(self.maxX)..", "..tostring(self.maxY)..", "..tostring(self.clamp)..")"
  elseif self.kind=="index" then
    out = self.expr:printprettys().."["..self.index:printprettys().."]"
  else
    print(self.kind)
    assert(false)
  end

  return out
end

function astFunctions:printpretty()
  print(self:printprettys())
end



function internalIRFunctions:irType()
  return "internalIR"
end

function internalIRFunctions:expectedKeycount(flatir)


  if self.kind=="load" then
    return 3 -- kind, type, from
  elseif self.kind=="loadConv" then
    return 6 -- kind, type, from, x,y, index
  elseif self.kind=="specialConv" then
    return 4 -- kind, id, x,y
  elseif self.kind=="loadConcrete" then
    return 5 -- kind,type, x,y, from
  elseif self.kind=="gatherConcrete" then
    return 10+self:childrenCount()*4 -- kind, type, x,y, maxX, maxY, clamp, from, hackBL, hackTR
  end

  local transCnt = self:childrenCount()*4
  if flatir~=nil then transCnt=0 end

  return typedASTFunctions.expectedKeycount(self)+transCnt
end

function internalIRFunctions:checkfn(flatir)
  
  if self.kind=="transform" or self.kind=="transformBaked" then
    assert(false)
  elseif self.kind=="load" then
    -- load from another scheduled IR node
    assert( orion.scheduledIR.isScheduledIR(self.from) )
  elseif self.kind=="loadConv" then
    -- load from another scheduled IR node
    assert( orion.scheduledIR.isScheduledIR(self.from) )
    assert(type(self.x)=="number")
    assert(type(self.y)=="number")
    assert(type(self.index)=="number")
    assert(orion.type.isType(self.type))
  elseif self.kind=="loadConcrete" then
    -- load from an image wrapper
    assert(orion.imageWrapper.isImageWrapper(self.from))
    self.from:check()
    assert(type(self.x)=="number")
    assert(type(self.y)=="number")
  elseif self.kind=="specialConv" then
    assert(type(self.id)=="number")
    assert(type(self.x)=="number")
    assert(type(self.y)=="number")
  elseif self.kind=="gatherConcrete" then
    assert(orion.imageWrapper.isImageWrapper(self.from))
    self.from:check()
    assert(getmetatable(self.x)==getmetatable(self))
    assert(getmetatable(self.y)==getmetatable(self))
    assert(getmetatable(self.hackBL)==getmetatable(self))
    assert(getmetatable(self.hackTR)==getmetatable(self))
    assert(type(self.maxX)=="number")
    assert(type(self.maxY)=="number")
    assert(type(self.clamp)=="boolean")
  else
    typedASTFunctions.checkfn(self)
  end

  if flatir==nil then
    local highestTranslate1 = -10000000
    local highestTranslate2 = -10000000
    
    for k,v in self:children() do
      assert(type(self["translate1_"..k])=="number")
      assert(type(self["translate2_"..k])=="number")
      assert(type(self["scale1_"..k])=="number")
      assert(type(self["scale2_"..k])=="number")
      if self["translate1_"..k]>highestTranslate1 then highestTranslate1 = self["translate1_"..k] end
      if self["translate2_"..k]>highestTranslate2 then highestTranslate2 = self["translate2_"..k] end
    end
    
    if self:childrenCount() > 0 then
      assert(highestTranslate1==0)
      assert(highestTranslate2==0)
    end
  end

end


function internalIRFunctions:printprettys(root,parent,key,assignments)
  return orion.internalIR.printprettys(self,root,parent,key,assignments)
end


function orion.internalIR.printprettys(self,root,parent,key,assignments)
  assert(orion.IR.isIR(root))
  assert(orion.IR.isIR(parent) or parent==nil)
  assert(type(key)=="string")
  assert(type(assignments)=="table")

  local out
  if self.kind=="load" then
    out = "load_"..self.from:name()
  elseif self.kind=="loadConv" then
    out = "loadConv_"..self.x.."_"..self.y.."_"..self.from:name().."["..self.index.."]"
  elseif self.kind=="loadConcrete" then
    out = "loadConcrete_"..self.x.."_"..self.y.."_"..self.from:name()
  elseif self.kind=="specialConv" then
    out = "specialConv_"..self.id.."_"..self.x.."_"..self.y
  elseif self.kind=="gatherConcrete" then
    out = "gatherConcrete(\n"..self.from:name()..",\n"
    out = out..self.x:printprettys(root,self,"x",assignments)..",\n"
    out = out..self.y:printprettys(root,self,"y",assignments)..",\n"
    out = out..tostring(self.maxX)..", "..tostring(self.maxY)..", "..tostring(self.clamp)..")"
  else
    out = orion.typedAST.printprettys(self,root,parent,key,assignments)
  end

  if parent~=nil and orion.flatIR.isFlatIR(parent)==false then
    out = out.."{"..parent["translate1_"..key]..","..parent["translate2_"..key]..","
    out = out..parent["scale1_"..key]..","..parent["scale2_"..key].."}" 
  end

--  if self:parentCount(root)>1 then
--    assignments[self:name()] = out
--    return self:name()
--  end

  return out
end

function internalIRFunctions:printpretty()
  orion.internalIR.printpretty(self)
end

function orion.internalIR.printpretty(self)
  local assignments = {}
  local out = self:printprettys(self,nil,"",assignments)
  for k,v in pairs(assignments) do print(k,"=",v) end
  print(out)
end

function typedASTFunctions:expectedKeycount()
  local baseSize = 2

  if self.kind=="mapreduce" or 
    self.kind=="mapreducevar" or 
    self.kind=="transform" or
    self.kind=="crop" then
    assert(false) -- should have been eliminated
  elseif self.kind=="transformBaked" then
    return baseSize+1+self:arraySize("translate")*2
  elseif self.kind=="cropBaked" then
    return baseSize+2
  elseif self.kind=="cast" then
    return baseSize+1
  elseif self.kind=="toAOS" then
    local cnt = self:arraySize("expr")
    return baseSize+cnt
  elseif self.kind=="multibinop" then
    local lhscnt = self:arraySize("lhs")
    local rhscnt = self:arraySize("rhs")
    return baseSize+1+lhscnt+rhscnt
  elseif self.kind=="multiunary" then
    local exprcnt = self:arraySize("expr")
    return baseSize+1+exprcnt
  elseif self.kind=="toSOA" then
    return 4 -- kind, special, index, type
  elseif self.kind=="tap" then
    return 4
  elseif self.kind=="tapLUTLookup" then
    return 6 -- kind, type, index, count, id, tapname
  end

  return astFunctions.expectedKeycount(self)+1 -- type
end

function typedASTFunctions:checkfn()

  assert(orion.type.isType(self.type))

  if self.kind=="mapreduce" or 
    self.kind=="mapreducevar" or 
    self.kind=="transform" or
    self.kind=="crop" then
    assert(false) -- should have been eliminated
  elseif self.kind=="transformBaked" then
    
    local i=1
    while self["translate"..i] or i<3 do -- we at least need coords for x,y
      assert(type(self["translate"..i])=="number")
      assert(type(self["scale"..i])=="number")
      i=i+1
    end

    assert(getmetatable(self.expr)==getmetatable(self))
  elseif self.kind=="toSOA" then
    assert(type(self.special)=="number")
    assert(type(self.index)=="number")
  elseif self.kind=="cropBaked" then
    assert(getmetatable(self.expr)==getmetatable(self))
    assert(orion.cropIR.isCropIR(self.crop))
    self.crop:check()
  elseif self.kind=="cast" then
    assert(orion.type.isType(self.type))
    assert(getmetatable(self.expr)==getmetatable(self))
    assert(self:childrenCount()==1)
  elseif self.kind=="index" then
    assert(getmetatable(self.expr)==getmetatable(self))
    assert(type(self.index)=="number")
  elseif self.kind=="toAOS" then
    -- converts N inputs to array of structs form
    local cnt = self:arraySize("expr")
    self:map("expr",function(n) assert(getmetatable(n)==getmetatable(self)) end)
  elseif self.kind=="multibinop" then
    -- a binop that takes multiple inputs for the lhs, rhs
    -- ex: dot product. These are vector operations that mix channels
    -- that have been devectorized

    local lhscnt = self:arraySize("lhs")
    local rhscnt = self:arraySize("rhs")
    self:map("lhs",function(n) assert(getmetatable(n)==getmetatable(self)) end)
    self:map("rhs",function(n) assert(getmetatable(n)==getmetatable(self)) end)
    assert(type(self.op)=="string")
  elseif self.kind=="multiunary" then
    -- a unary that takes multiple inputs for the expr
    -- ex: arrayAnd. These are vector operations that mix channels
    -- that have been devectorized

    local exprcnt = self:arraySize("expr")
    self:map("expr",function(n) assert(getmetatable(n)==getmetatable(self)) end)
    assert(type(self.op)=="string")
  else
    astFunctions.checkfn(self)
  end

end

function orion.typedAST.check(node,options)
  return typedASTFunctions.check(node,options)
end

function flatIRFunctions:printprettys(root,parent,key,assignments)
  return orion.internalIR.printprettys(self,root,parent,key,assignments)
end

function flatIRFunctions:printpretty()
  orion.internalIR.printpretty(self)
end

function flatIRFunctions:expectedKeycount()
  internalIRFunctions.expectedKeycount(self,true)
end

function flatIRFunctions:check()
  internalIRFunctions.checkfn(self,true)
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
