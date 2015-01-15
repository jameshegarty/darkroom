-- AST generated from user input

astFunctions={}
setmetatable(astFunctions,{__index=IRFunctions})
astMT={__index=astFunctions, 
  __newindex = function(table, key, value)
                    darkroom.error("Attempt to modify ast node")
  end,
  __call = function(table, compilerOptions)
    local func = darkroom.compile({table},compilerOptions)
    return func()
  end
}

darkroom.ast={}


function astFunctions:irType()
  return "ast"
end


function darkroom.ast.isKind(ast, kind)
  return getmetatable(ast)==astMT and ast.kind==kind
end

function darkroom.ast.isAST(ast) return getmetatable(ast)==astMT end

-- kind of a hack - so that the IR library can shallowcopy and then
-- modify an ast node without having to know its exact type
function astFunctions:init()
  setmetatable(self,nil)
  darkroom.ast.new(self)
end

function darkroom.ast.new(tab)
  assert(type(tab)=="table")
  assert(getmetatable(tab)==nil)
  darkroom.IR.new(tab)
  return setmetatable(tab,astMT)
end

function astFunctions:optimize()
  local res = self:S("*"):process(
    function(n)
      if n.kind=="binop" and n.op=="+" and n.lhs.kind=="value" and n.lhs.value==0 then
        return n.rhs
      elseif n.kind=="binop" and n.op=="+" and n.rhs.kind=="value" and n.rhs.value==0 then
        return n.lhs
      elseif n.kind=="binop" and n.op=="-" and n.lhs.kind=="value" and n.lhs.value==0 and n.rhs.kind=="value" then
        local r = n.rhs:shallowcopy()
        r.value=0-n.rhs.value
        return darkroom.ast.new(r):copyMetadataFrom(n.rhs)
      elseif n.kind=="binop" and n.op=="*" and n.rhs.kind=="value" and n.rhs.value==0 then
        return n.rhs
      elseif n.kind=="binop" and n.op=="*" and n.lhs.kind=="value" and n.lhs.value==0 then
        return n.lhs
      elseif n.kind=="binop" and n.op=="/" and n.lhs.kind=="value" and n.lhs.value==0 then
        return n.lhs
      end
    end)

  return res
end

function astFunctions:eval(dim, irRoot)
  assert(type(dim)=="number")
  assert(darkroom.IR.isIR(irRoot))

  if self.kind=="value" then
    assert(type(self.value)=="number")
    return Stencil.new():addDim(dim, self.value)
  elseif self.kind=="unary" and self.op=="-" then
    return self.expr:eval(dim, irRoot):flipDim(dim)
  elseif self.kind=="mapreducevar" then
    local l = irRoot:lookup(self.mapreduceNode)["varlow"..self.id]
    local h = irRoot:lookup(self.mapreduceNode)["varhigh"..self.id]
    if darkroom.ast.isAST(l) then l=l:eval(dim, irRoot); assert(l:area()==1); l = l:min(dim) end
    if darkroom.ast.isAST(h) then h=h:eval(dim, irRoot); assert(h:area()==1); h = h:min(dim) end
    assert(type(l)=="number")
    assert(type(h)=="number")
    return Stencil.new():addDim(dim, l):addDim(dim, h)
  elseif self.kind=="iterationvar" then
    local n = irRoot:lookup(self.iterateNode)
    local l = n.iterationSpaceLow
    local h = n.iterationSpaceHigh
    if darkroom.ast.isAST(l) then l=l:eval(dim, irRoot); assert(l:area()==1); l = l:min(dim) end
    if darkroom.ast.isAST(h) then h=h:eval(dim, irRoot); assert(h:area()==1); h = h:min(dim) end
    assert(type(l)=="number")
    assert(type(h)=="number")
    return Stencil.new():addDim(dim, l):addDim(dim, h)
  elseif self.kind=="binop" and self.op=="+" then
    return self.lhs:eval(dim, irRoot):sum(self.rhs:eval(dim, irRoot))
  elseif self.kind=="binop" and self.op=="-" then
    return self.lhs:eval(dim, irRoot):sum(self.rhs:eval(dim, irRoot):flipDim(dim))
  elseif self.kind=="binop" and self.op=="*" then
    return self.lhs:eval(dim, irRoot):product(self.rhs:eval(dim, irRoot))
  else
    print("internal error, couldn't statically evaluate ", self.kind)
    assert(false)
  end
end

function astFunctions:codegen()
  if self.kind=="value" then
    return `[self.value]
  elseif self.kind=="binop" and self.op=="+" then
    return `[self.lhs:codegen()]+[self.rhs:codegen()]
  elseif self.kind=="binop" and self.op=="-" then
    return `[self.lhs:codegen()]-[self.rhs:codegen()]
  elseif self.kind=="binop" and self.op=="*" then
    return `[self.lhs:codegen()]*[self.rhs:codegen()]
  elseif self.kind=="unary" and self.op=="-" then
    return `-[self.expr:codegen()]
  elseif self.kind=="mapreducevar" then
    if mapreducevarSymbols[self.mapreduceNode]==nil then
      mapreducevarSymbols[self.mapreduceNode] = symbol(int,self.variable)
    end
    return `[mapreducevarSymbols[self.mapreduceNode]]
  else
    print("internal error, couldn't codegen ast ", self.kind)
    assert(false)
  end

end

function astFunctions:localEnvironment( root, envROOT, resolveVars)
  assert(type(resolveVars)=="function")

  local env
  if root == self then
    env =  envROOT
  else
    -- if this is false, it violates lexical scope?
    assert( self:parentCount(root)==1 )
    for parentNode, key in self:parents(root) do
      env = parentNode:localEnvironment(root, envROOT, resolveVars)
    end
  end

  -- if this node added stuff to the lexical environment, modify env
  if self.kind=="mapreduce" then
    self:map("varname", 
             function(n,i)
               env[n] = self["varnode"..i]
             end)
  elseif self.kind=="let" then
    self:map("expr",
             function(n,i)
               env[self["exprname"..i]] = darkroom.ast.new({kind="func",
                                                         identifier1 = self["exprname"..i]}):copyMetadataFrom(self)
             end)
  end

  return env
end