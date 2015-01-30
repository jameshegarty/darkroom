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