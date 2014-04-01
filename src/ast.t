-- AST generated from user input

astFunctions={}
setmetatable(astFunctions,{__index=IRFunctions})
astMT={__index=astFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
  end,
  __call = function(table, compilerOptions)
    local func = orion.compile({table},compilerOptions)
    return func()
  end
}

orion.ast={}

orion.ast.kinds={["func"] = 1, ["binop"]=1, ["unary"]=1, ["value"]=1, ["block"]=1, 
                 ["assignment"]=1, ["select"]=1, ["cast"]=1, ["index"]=1,["array"]=1,["tap"]=1,["apply"]=1,
			   ["position"]=1, ["call"]=1, ["special"]=1, ["mapreduce"]=1,    ["mapFuture"]=1, ["single"]=1, ["multiple"]=1,["loadConcrete"]=1,["strip"]=1,["stripOptional"]=1,
         ["mapreducevar"]=1, ["letvar"]=1, ["transform"]=1,["transformBaked"]=1,["load"]=1, ["lua"]=1, ["let"]=1, ["single"]=1, ["gatherConcrete"]=1,["cropBaked"]=1,
         ["loop"]=1,["specialConv"]=1,["loadConv"]=1,["grow"]=1,["gather"]=1,["switch"]=1,["equals"]=1,["intersection"]=1,["union"]=1,["terra"]=1,["toSOAConcrete"]=1, ["toSOA"]=1,["toAOS"]=1}


function astFunctions:irType()
  return "ast"
end

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

function astFunctions:save(filename,compilerOptions)
  local func = orion.compile({self},compilerOptions)
  print("Call",compilerOptions)
  local out = func()
  local terra dosave(im: &Image, filename : &int8)
    im:save(filename)
  end

  dosave(out,filename)
  --out:save(filename)
end

function astFunctions:saveRaw(filename,bits)
  local func = orion.compile({self})
  print("Call")
  local out = func()
  local terra dosave(im: &Image, filename : &int8, bits:int)
    im:saveRaw(filename,bits)
  end

  dosave(out,filename,bits)
  --out:save(filename)
end

function astFunctions:_cparam(key)
  if self.kind~="crop" or self.expr.kind~="special" then
    orion.error("could not determine "..key.." - not an input fn")
  end

  local id = self.expr.id

  if type(orion._boundImages[id+1][key])~="number" then
    orion.error("could not determine "..key.." - wasn't specified at compile time")
  end

  return orion._boundImages[id+1][key]
end

function astFunctions:width()
  return self:_cparam("width")
end

function astFunctions:height()
  return self:_cparam("height")
end

function astFunctions:id()
  if self.kind~="crop" or self.expr.kind~="special" then
    orion.error("could not determine "..key.." - not an input fn")
  end

  assert(type(self.expr.id)=="number")
  return self.expr.id
end

function orion.ast.isKind(ast, kind)
  return getmetatable(ast)==astMT and ast.kind==kind
end

function orion.ast.isAST(ast) return getmetatable(ast)==astMT end

-- kind of a hack - so that the IR library can shallowcopy and then
-- modify an ast node without having to know its exact type
function astFunctions:init()
  setmetatable(self,nil)
  orion.ast.new(self)
end

function orion.ast.new(tab)
  assert(type(tab)=="table")
  assert(getmetatable(tab)==nil)
  orion.IR.new(tab)
  return setmetatable(tab,astMT)
end

-- 
function astFunctions:localEnvironment(root,envROOT)
  local env
  if root == self then
    env = {}
    -- need to do a deep copy b/c we don't want to mess up envRoot
    --    for k,v in pairs(envROOT) do env[k]=v end
    env= envROOT
  else
    -- if this is false, it violates lexical scope?
    assert( self:parentCount(root)==1 )
    for parentNode, key in self:parents(root) do
      env = parentNode:localEnvironment(root, envROOT)
    end
  end


  -- if this node added stuff to the lexical environment, modify env
  if self.kind=="mapreduce" then
    self:map("varname", 
             function(n,i)
               print("ADD",n)
               env[n] = orion.ast.new({kind="mapreducevar",
                                       variable=n,
                                       low=self["varlow"..i],
                                       high=self["varhigh"..i]}):copyMetadataFrom(self)
             end)
  elseif self.kind=="let" then
    self:map("expr",
             function(n,i)
               env[self["exprname"..i]] = orion.ast.new({kind="func",
                                                         identifier1 = self["exprname"..i]}):copyMetadataFrom(self)
             end)
  end


  return env

end