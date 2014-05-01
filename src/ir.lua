-- this is the base class for all our compiler IR

orion.IR = {}

IRFunctions = {}

-- prototype for ast query object
local astQuery={}
local astQueryMT = {__index=astQuery}

function astQuery:check()
  if orion.debug==false then return end
  -- let's check that each ast only appears once in the list
  local listcheck={}

  for _,v in ipairs(self.list) do
    assert(orion.IR.isIR(v))
    assert(listcheck[v]==nil)
    assert(self.inList[v]~=nil)
    listcheck[v]=1

    local parentCount = 0
    for _,_ in v:parents(self.ast) do parentCount=parentCount+1 end

    if parentCount<1 then
      -- no parents, this has better be self.ast, and had better be the last thing we do
      -- (or the first thing we do, if we're going in reverse)
      assert(v==self.ast)
    end
  end
end

-- func(AST to alter, original AST)
-- forward order: children are visited before parents
-- reverse order: parents are visited before children
function astQuery:process(func, reverse)
  assert(type(func)=="function")
  self:check()

  if #self.list==0 then
return self.ast
  end

  local starti = 1
  local endi = #self.list
  local stepi = 1

  if reverse then
    starti = #self.list
    endi = 1
    stepi = -1
  end

  local replacedNodes = {} -- ast in original tree -> new ast node
  local readyNodes = {} -- ast in original tree -> new ast node

  -- update the parents. Basically, walk the tree up until we hit something else
  -- that's in the list, then stop. Keep track of what we've changed already
  -- in 'replacedNodes' - this will keep us from replacing it multiple times
  -- on reconvergence, and make sure we keep propagating these changes up to the top
  
  -- a node can be in one of 2 states:
  -- replaced (replacedNodes array): user function has been run on this node,
  --      and we're totally done with it. It's in its final state.
  -- ready (readyNodes array): all inputs are either replaced or affected==false.
  --      the node has been updated to reflect its new set of children.
  --      we can now run the user's function on it and transition it to replaced.
  --      parents of nodes in the query list can only be marked ready
  --      if the node in the query list has been replaced.
  -- ready is a subset of replaced.
  
  local function prop(oldast)
    if replacedNodes[oldast] then
 return
    end

    -- first, decide if we're ready to replace this node yet
    local changed = false -- did it change at all?
    for k,v in oldast:inputs() do
      if self.affected[v] and replacedNodes[v]==nil then 
        -- not ready to replace it yet - some of its children haven't yet been processed
return
      end
      assert(readyNodes[v] or self.affected[v]==nil)
      if v~=replacedNodes[v] then changed=true end
    end
    
    assert(self.affected[oldast]) -- make sure we didn't mess up affected calculation
    
    -- build the node to send to the user
    if changed then
      local newNode = {}
      for k,v in pairs(oldast) do  newNode[k]=replacedNodes[v] or v  end
      setmetatable(newNode,getmetatable(oldast))
      newNode:init()
      newNode:copyMetadataFrom(oldast)

      readyNodes[oldast] = newNode

      if self.inList[oldast]==nil then
        replacedNodes[oldast] = newNode
      end

    else
      -- nothing actually changed, but we want to mark this node as ready
      readyNodes[oldast] = oldast
      if self.inList[oldast]==nil then
        replacedNodes[oldast] = oldast
      end
    end
    
    -- now propagate this as far as we can
    if self.inList[oldast]==nil then
      for p,_ in oldast:parents(self.ast) do prop(p) end 
    end
  end

  -- mark the nodes in the list with no affected inputs as ready
  for i=starti,endi,stepi do
    local ready = true
    for k,v in self.list[i]:inputs() do
      if self.affected[v]~=nil then ready=false end
    end

    if ready then readyNodes[self.list[i]]=self.list[i] end
  end

  for i=starti,endi,stepi do

    local thisast = readyNodes[self.list[i]]
    assert(thisast)

    local newAST = func( thisast, self.list[i] )

    -- in this case, user actually passed us a new AST, make sure it's legit
    if newAST~=nil and newAST~=thisast then
      assert(getmetatable(newAST)==getmetatable(self.ast))
      -- can't replace stuff if we're visiting parents before children. crazy stuff will happen
      assert(reverse==false or reverse==nil)
    end

    replacedNodes[self.list[i]] = newAST or thisast

    for p,_ in self.list[i]:parents(self.ast) do prop(p) end

  end

  assert(readyNodes[self.ast])

  -- we fill in the replaced table even if we didn't actually change anything
  local root = replacedNodes[self.ast]
  assert(root)

  -- we don't support running process multiple times, so break this query
  self.ast = nil
  self.list = nil

  return root
end

-- func can return multiple values
-- func(node,arglist): where node is the node to perform your operation on
-- arglist is a table with all the results of calling func on the children
-- deduplicated. keys in this table have undefined behavior
function astQuery:traverse(func)
  local results = {}
  self:check()

  for i=1,#self.list do
    -- figure out the arg list
    local inargsInv={}

    -- should rewrite this in a more efficient way lol!
    local seen = {}
    local function search(node)
      if seen[node]==nil then
        seen[node] = 1
        if self.inList[node] then
          assert(results[node]~=nil)
          for k,v in pairs(results[node]) do inargsInv[v] = 1 end
        else
          for k,v in node:inputs() do search(v) end
        end
      else
        seen[node] = 1
      end
    end
    for k,v in self.list[i]:inputs() do search(v) end

    local inargs = {}
    for k,v in pairs(inargsInv) do table.insert(inargs,k) end

    -- call the func
    local result = pack(func(self.list[i], inargs))
    results[self.list[i]]=result
  end

  return results[self.ast]
end

-- this basically calls func on each node in this IR _exactly_ once
-- func should return 1 value
-- func(node,argList) : node is the node we're currently looking at
-- arglist: key=key name of child, value = result of calling func on this child
--[=[
function IRFunctions:visitEach(func)
  local seen = {}
  local expanded = {}

  local visitList = {}
  local stack = {}
  stack[1] = {self}
  print("AA")
  print(debug.traceback())
  while #stack>0 do
    -- need to add all children of everything in this list to the stack
    -- do a DFS
    local cur = #stack
    local next = #stack+1

    if #stack[cur]==0 then
      stack[cur]=nil
    else

      local nextNode = stack[cur][#stack[cur]]
      if expanded[nextNode] then
        -- we must have already added its children
        if seen[nextNode]==nil then
          seen[nextNode] = 1
          table.insert( visitList, nextNode )
        end
        
        stack[cur][#stack[cur]] = nil
      elseif seen[nextNode]==nil then
        stack[next] = {}
        for _,c in nextNode:inputs() do
          table.insert( stack[next], c )
        end
        expanded[nextNode]=1
        
        if #stack[next]==0 then
          seen[nextNode] = 1
          table.insert( visitList, nextNode )
          stack[cur][#stack[cur]] = nil
          stack[next]=nil
        end
      else
        stack[cur][#stack[cur]] = nil
      end

    end
    
  end
  print("BB")
  assert(#visitList == self:S("*"):count())
  seen = {}
  stack = nil
  local value = {}

  for _,node in ipairs(visitList) do

    local argList = {}
    for k,v in node:inputs() do
      assert(seen[v])
      argList[k]=value[v]
    end

    value[node] = func(node,argList)
    seen[node] = 1
  end

  return value[self]
end]=]

function IRFunctions:visitEach(func)
  local seen = {}
  local value = {}

  local function trav(node)
    if seen[node]~=nil then return value[node] end

    local argList = {}
    for k,v in node:inputs() do
      argList[k]=trav(v)
    end

    value[node] = func(node,argList)
    seen[node] = 1
    return value[node]
  end

  return trav(self)
end

-- number of nodes returned by the query
function astQuery:count()
  return #self.list  
end

function astQuery:getList()
  return self.list
end

-- visit all parents of a child before visiting that child
function astQuery:processReverse(func)
  return self:process(func,true)
end

-- this figures out what nodes in the DAG starting at root are _potentially_
-- affected by a query. Essentially: all parents of nodes in the query
-- list are potentially affected.
function orion.IR.buildAffected(root,list)
  local affected={}

  local function prop(ast)
    if affected[ast]==nil then
      affected[ast]=1
      for p,_ in ast:parents(root) do
        prop(p)
      end
    end
  end

  for _,v in pairs(list) do prop(v) end

  return affected
end

-- query string to bool
-- if query has multiple items, the queries are ANDed together
function IRFunctions:matches(query)
  if query==nil then return false end

  if type(query)=="function" then
    -- just run it
    return query(self)
  end

  if type(query)~="string" then
    orion.error("Bad query type")  
  end

  if query=="*" then 
    return true
  end

  local st = explode(" ", query)

  for _,v in pairs(st) do

    local exploded = explode("=",v)

    if self.kind==v then

    elseif #exploded==2 then
      if self[exploded[1]]~=exploded[2] then return false end      
    else
      return false      
    end
  end

  return true
end


-- remember, we're doing a topological sort here. Our AST is actually a DAG, and
-- we only want to add each node to the list once.
-- the 'list' returned has the property that for each node, all it's children
-- come after it in the list. (we can traverse the list in reverse, to get the
-- property that each node comes before all its parents)

-- the question is: when we have a node with multiple parents, and we replace it,
-- what kind of behavior do we want?
-- a) each of the parent nodes gets the new ast (the replacement). The 
--    pointers on all of them gets updated when we replace the node.
-- b) we visit each path from the multiple parents to the common node, and allow
--    each path to replace the parent differently. (only one pointer is updated for each replacement)
-- Based on my knowledge of compilers, I don't think (b) is ever going to be particulary useful,
-- so for now we'll just support (a)

-- list: an array of ASTs in topological order
-- visited : astPointer->1
local function Sinternal(self, query, stopQuery, list, visited)
  assert(orion.IR.isIR(self))

  if visited[self]==nil then
    -- mark as visited
    visited[self]=1
    
    for k,v in self:inputs() do

      if v:matches(stopQuery) then
        -- do nothing. this is a stop node
      else
        Sinternal(v, query,stopQuery,list,visited)
      end
    end

    -- if the root is the type we're looking for, add it to the list
    if self:matches(query) then table.insert(list,self) end
  end
end

-- this function performs a query on the ast, and returns an ast query object, 
-- which stores a list of pointers to the found nodes
-- 
-- query = query string for the nodes we're looking for
-- stopQuery = query string for nodes we stop at (we won't consider this node or its children)
function IRFunctions:S(query, stopQuery)
  local list = {}
  Sinternal( self, query, stopQuery, list, {})
  local inList = {}
  for k,v in pairs(list) do inList[v]=k end
  return setmetatable({ast=self, 
                       list=list, 
                       inList=inList, 
                       affected=orion.IR.buildAffected(self,list)},astQueryMT)
end

-- this performs a query on the ast, where we pass ast to queryFunc, and if queryFunc
-- returns true, than that sub-ast A is included in the list. However, none of 
-- A's children are included
-- 
-- MISCONCEPTION: because none of A's children are included, it actually doesn't matter
-- whether we use process or processReverse (this returns a cut in the graph)
--
-- FALSE: actually, I think it's possible there are sub-asts A and B, where A,B are
-- children of the root, but B is also a child of A. Then, it matters whether
-- we use process or processReverse. We make no guarantee that the sub-asts are 
-- independent
--
-- queryFunc :: ast -> bool
function IRFunctions:F(queryFunc)
  assert(type(queryFunc)=="function")

  local list = {}
  Finternal( self, queryFunc, list, {})

  local inList = {}
  for k,v in pairs(list) do inList[v]=k end

  -- a horrible hack: F doesn't do a correct topological sort, so use S
  -- that does and fix up F. Fix!
  local q = self:S("*")
  local flist = {}
  for _,v in pairs(q.list) do if inList[v] then table.insert(flist,v) end end
  assert(#list==#flist)
  list = flist

  return setmetatable({ast=self, list=list, inList=inList, affected=orion.IR.buildAffected(self,list)},astQueryMT)
end

function Finternal(self, queryFunc, list, visited)
  assert(type(queryFunc)=="function")
  assert(orion.IR.isIR(self))

  if visited[self] == nil then
    -- mark as visited
    visited[self]={}

    -- if the root is the type we're looking for, add it to the list
    if queryFunc(self) then 
      table.insert(list,self)
    else

      for k,v in self:inputs() do
        Finternal( v, queryFunc, list, visited )
      end
    end
  end
end

-- we store arrays in ASTs in a wacky way 
-- [key..N] where N=1...length
-- this maps over these keys.
-- func(value,index)
function IRFunctions:map(key, func)
  assert(type(key)=="string")
  -- make sure we're not storing this 0 indexed
  assert(self[key.."0"]==nil)

  local res = {}
  local index=1
  while self[key..index] do
    local fres = func(self[key..index],index)
    if fres~=nil then table.insert(res,fres) end
    index = index + 1
  end

  return res
end

function IRFunctions:foldl(key, func, v)
  assert(type(key)=="string")
  -- make sure we're not storing this 0 indexed
  assert(self[key.."0"]==nil)

  local index=1
  while self[key..index] do
    v = func(v,self[key..index])
    index = index + 1
  end
  
  return v
end

-- we store arrays in ASTs in a wacky way 
function IRFunctions:arraySize(key)
  assert(type(key)=="string")
    -- make sure we're not storing this 0 indexed
  assert(self[key..0]==nil)

  local index=1
  while self[key..index] do
    index = index+1
  end

  return index-1
end

-- this basically only copies the actual entires of the table,
-- but doesn't copy children/parents, so 
-- you still have to call orion.ast.new on it
function IRFunctions:shallowcopy()
  local newir = {}

  for k,v in pairs(self) do
    newir[k]=v
  end

  return newir
end

-- generic IR check code - check stuff that should apply to all IRs (parent and child validity for ex)
function orion.IR.check(node)

  -- check that table wasn't modified
  assert(type(orion.IR._original[node])=="table")
  for k,v in pairs(orion.IR._original[node]) do
    assert(node[k]==v)
  end

  for k,v in pairs(node) do
    assert(orion.IR._original[node][k]==v)
  end

  -- check that some necessary metadata is set
  assert(type(node:linenumber())=="number")
  assert(type(node:offset())=="number")
end

-- like pairs(), but only return the k,v pairs where the v is another IR node
-- by defn, this function only returns v's that have the same metatable as self
-- note that this can return the same value under different keys!
function IRFunctions:inputs()
  local f, s, var = pairs(self)

  local function filteredF(s,var)
    while 1 do
      local k,v = f(s,var)
      var = k
      if k==nil then return nil,nil end
      if orion.IR.isIR(v) and getmetatable(self)==getmetatable(v) then return k,v end
    end
  end

  return filteredF,s,var
end

orion.IR._parentsCache=setmetatable({}, {__mode="k"})
function orion.IR.buildParentCache(root)
  assert(orion.IR._parentsCache[root]==nil)
  orion.IR._parentsCache[root]=setmetatable({}, {__mode="k"})
  orion.IR._parentsCache[root][root]=setmetatable({}, {__mode="k"})

  local visited={}
  local function build(node)
    if visited[node]==nil then
      for k,child in node:inputs() do
        if orion.IR._parentsCache[root][child]==nil then
          orion.IR._parentsCache[root][child]={}
        end

        -- notice that this is a multimap: node can occur
        -- multiple times with different keys
        table.insert(orion.IR._parentsCache[root][child],setmetatable({node,k},{__mode="v"}))
        build(child)
      end
      visited[node]=1
    end
  end
  build(root)
end

-- this node's set of parents is always relative to some root
-- returns k,v in this order:  parentNode, key in parentNode to access self
-- NOTE: there may be multiple keys per parentNode! A node can hold
-- the same AST multiple times with different keys.
function IRFunctions:parents(root)
  assert(orion.IR.isIR(root))

  if orion.IR._parentsCache[root]==nil then
    -- need to build cache
    orion.IR.buildParentCache(root)
  end

  -- maybe this node isn't reachable from the root?
  assert(type(orion.IR._parentsCache[root][self])=="table")

  -- fixme: I probably didn't implement this correctly

  local list = orion.IR._parentsCache[root][self]

  local function filteredF(s,var)
    assert(type(s)=="table")
    assert(type(s.i)=="number")

    if s.i>#list then return nil,nil end
    s.i = s.i+1
    return list[s.i-1][1],list[s.i-1][2]
  end

  return filteredF,{i=1},1

end

-- count the number of keys in this node
function IRFunctions:keyCount()
  local kc=0
  for k,v in pairs(self) do
    kc = kc+1
  end
  return kc
end

function IRFunctions:inputCount()
  local cc=0
  for k,v in self:inputs() do cc=cc+1 end
  return cc
end

function IRFunctions:parentCount(root)
  local pc=0
  for k,v in self:parents(root) do pc=pc+1 end
  return pc
end

function IRFunctions:maxDepth()

  local seen = {}


  local function trav(node)
    if seen[node] then return seen[node] end
    local res = 0

    for k,v in node:inputs() do
      res = math.max(res, trav(v)+1)
    end

    seen[node]=res
    return seen[node]
  end

  return trav(self)
end

orion.IR._original=setmetatable({}, {__mode="k"})

function orion.IR.new(node)
  assert(getmetatable(node)==nil)

  orion.IR._original[node]=setmetatable({}, {__mode="kv"})
  for k,v in pairs(node) do
    orion.IR._original[node][k]=v
  end

end

function orion.IR.isIR(v)
  local mt = getmetatable(v)
  if type(mt)~="table" then return false end
  if type(mt.__index)~="table" then return false end
  local mmt = getmetatable(mt.__index)
  if type(mmt)~="table" then return false end

  local t = mmt.__index

  return t==IRFunctions
end

function IRFunctions:equals(other)
  if getmetatable(self)~=getmetatable(other) then return false end

  for k,v in pairs(self) do if self[k]~=other[k] then return false end end
  for k,v in pairs(other) do if self[k]~=other[k] then return false end end

  return true
end

----
-- various bits of metadata. Each IR node should only contain stuff that's relevant
-- to the CSE algorithm. Various other bits that don't matter are stored off to the side.

function IRFunctions:copyMetadataFrom(otherIR)
  assert(orion.IR.isIR(otherIR))

  local baseName = otherIR:baseName()
  if baseName~=nil then self:setName(baseName) end

  self:setLinenumber(otherIR:linenumber())
  self:setOffset(otherIR:offset())
  self:setFilename(otherIR:filename())
  return self
end

orion.IR._filenames=setmetatable({}, {__mode="k"})
function IRFunctions:setFilename(n)
  assert(type(n)=="string")
  if orion.IR._filenames[getmetatable(self)] == nil then orion.IR._filenames[getmetatable(self)]=setmetatable({},{__mode="k"}) end
  orion.IR._filenames[getmetatable(self)][self] = n
  return self
end

function IRFunctions:filename()
  assert(type(orion.IR._filenames[getmetatable(self)])=="table")

  if type(orion.IR._filenames[getmetatable(self)][self])~="string" then
    print("filename missing",self.kind)
    self:printpretty()
  end
  
  assert(type(orion.IR._filenames[getmetatable(self)][self])=="string")
  return orion.IR._filenames[getmetatable(self)][self]
end

orion.IR._linenumbers=setmetatable({}, {__mode="k"})
function IRFunctions:setLinenumber(n)
  assert(type(n)=="number")
  if orion.IR._linenumbers[getmetatable(self)] == nil then orion.IR._linenumbers[getmetatable(self)]=setmetatable({},{__mode="k"}) end
  orion.IR._linenumbers[getmetatable(self)][self] = n
  return self
end

function IRFunctions:linenumber()
  assert(type(orion.IR._linenumbers[getmetatable(self)])=="table")

  if type(orion.IR._linenumbers[getmetatable(self)][self])~="number" then
    print("Line number missing")
    self:printpretty()
  end
  
  assert(type(orion.IR._linenumbers[getmetatable(self)][self])=="number")
  return orion.IR._linenumbers[getmetatable(self)][self]
end

orion.IR._offsets=setmetatable({}, {__mode="k"})
function IRFunctions:setOffset(n)
  assert(type(n)=="number")
  if orion.IR._offsets[getmetatable(self)] == nil then orion.IR._offsets[getmetatable(self)]=setmetatable({},{__mode="k"}) end
  orion.IR._offsets[getmetatable(self)][self] = n
  return self
end

function IRFunctions:offset()
  assert(type(orion.IR._offsets[getmetatable(self)])=="table")
  assert(type(orion.IR._offsets[getmetatable(self)][self])=="number")
  return orion.IR._offsets[getmetatable(self)][self]
end

orion.IR._names=setmetatable({}, {__mode="k"})  -- IR node -> string
orion.IR._basenames=setmetatable({}, {__mode="k"})  -- IR node -> string. the name we requested. may be duplicated!
orion.IR._usedNames=setmetatable({}, {__mode="k"})  -- string -> count of number of times it's been used
function IRFunctions:name()
  if orion.IR._names[getmetatable(self)] == nil then orion.IR._names[getmetatable(self)]=setmetatable({},{__mode="k"}) end
  if orion.IR._basenames[getmetatable(self)] == nil then orion.IR._basenames[getmetatable(self)]=setmetatable({},{__mode="k"}) end
  if orion.IR._usedNames[getmetatable(self)] == nil then orion.IR._usedNames[getmetatable(self)]=setmetatable({},{__mode="k"}) end

  if orion.IR._names[getmetatable(self)][self]==nil then
    -- we need to generate a new name
    local newname = self:makeNewName()
    self:setName(newname)
  end

  if type(orion.IR._names[getmetatable(self)][self])=="string" then
    -- we already assigned this guy a name
return orion.IR._names[getmetatable(self)][self]
  end

  assert(false)
end

-- may return nil!
function IRFunctions:baseName()
  if orion.IR._basenames[getmetatable(self)] == nil then orion.IR._basenames[getmetatable(self)]=setmetatable({},{__mode="k"}) end

  return orion.IR._basenames[getmetatable(self)][self]
end

function IRFunctions:setName(newname)
  assert(type(newname)=="string")

  if orion.IR._names[getmetatable(self)] == nil then orion.IR._names[getmetatable(self)]=setmetatable({},{__mode="k"}) end
  if orion.IR._basenames[getmetatable(self)] == nil then orion.IR._basenames[getmetatable(self)]=setmetatable({},{__mode="k"}) end
  if orion.IR._usedNames[getmetatable(self)] == nil then orion.IR._usedNames[getmetatable(self)]=setmetatable({},{__mode="k"}) end

  orion.IR._basenames[getmetatable(self)][self] = newname

  -- not used yet. can just return it
  if orion.IR._usedNames[getmetatable(self)][newname]==nil then
    orion.IR._usedNames[getmetatable(self)][newname] = 1
    orion.IR._names[getmetatable(self)][self]=newname
return newname
  end

  -- need to add some extra junk to the end
  assert(type(orion.IR._usedNames[getmetatable(self)][newname])=="number")
  orion.IR._usedNames[getmetatable(self)][newname] = orion.IR._usedNames[getmetatable(self)][newname] + 1
  newname = newname.."_"..orion.IR._usedNames[getmetatable(self)][newname]
  assert(orion.IR._usedNames[getmetatable(self)][newname]==nil) -- dno why this would happen
  orion.IR._usedNames[getmetatable(self)][newname] = 1
  orion.IR._names[getmetatable(self)][self]=newname

  return newname

end

-- use whatever clever algorithm you want here
function IRFunctions:makeNewName()
  return "unnamed"
end