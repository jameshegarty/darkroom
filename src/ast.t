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


function astFunctions:irType()
  return "ast"
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