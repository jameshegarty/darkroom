local cstdio = terralib.includec("stdio.h")
local cstdlib = terralib.includec("stdlib.h")

-- a hack to have assert print a traceback
local oldassert = assert
function assert(x)
  if x==false then print(debug.traceback()) end
  oldassert(x)
end

orion={}

setmetatable(orion,{
  __index = function(tab,key)
--    print("INDEX",key)
    local stype = orion.type.stringToType(key)

    -- autogenerate the cast macros
    if stype then
    elseif key:sub(1,5)=="array" then
      local subkey = key:sub(6)
      local arrLen = subkey:match("%d+")
      local s, e = subkey:find("%d+")
      stype = orion.type.stringToType(subkey:sub(e+1))

      if stype then
        stype = orion.type.array(stype,tonumber(arrLen))
      else
        return nil
      end
    end

    tab[key] = function( thisast, expr )
      assert(orion.ast.isAST(expr))
      local typeNode = orion.ast.new({kind="type",type=stype}):copyMetadataFrom(thisast)
      return orion.ast.new({kind="cast",expr=expr,type=typeNode}):copyMetadataFrom(thisast)
    end
    return tab[key]

  end})

terralib.require("util")

orion.boolops = {["or"]=1,["and"]=1} -- bool -> bool -> bool
orion.cmpops = {["=="]=1,["~="]=1,["<"]=1,[">"]=1,["<="]=1,[">="]=1} -- number -> number -> bool
orion.binops = {["|"]=1,["^"]=1,["&"]=1,["<<"]=1,[">>"]=1,["+"]=1,["-"]=1,["%"]=1,["*"]=1,["/"]=1}
-- these binops only work on ints
orion.intbinops = {["<<"]=1,[">>"]=1,["&"]=1,["|"]=1,["^"]=1}
-- ! does a logical not in C, use 'not' instead
-- ~ does a bitwise not in C
orion.unops = {["not"]=1,["-"]=1}
appendSet(orion.binops,orion.boolops)
appendSet(orion.binops,orion.cmpops)
orion.keyremap = {r=0,g=1,b=2, x=0, y=1, z=2}
orion.dimToCoord={[1]="x",[2]="y",[3]="z"}

orion.cropSame = {name="same"}
orion.cropGrow = {name="grow"}
orion.cropShrink = {name="shrink"}
orion.cropExplicit = {name="explicit"}
orion.cropNone = {name="none"}
orion.defaultCrop = orion.cropSame

-- for debugging. Set these using compile options, not here!
orion.verbose = false -- write out a ton of crap
orion.printasm = false -- print out the asm of compiled kernels
orion.printstage = false -- print the stage of compile we're in (debug slow compiles)
orion.debug = false -- enable asserts & checks in generated code & writing of intermediates
orion.debugimages = false -- enables debug images
orion.printruntime = false
orion.looptimes = 1 -- number of times to run each kernel in terra. Used to get timing data. Usually should be 1!
orion.fastmath = false -- extra math optimizations that I'm not sure preserve values. Most important for conv engine
orion.ilp = false
orion.unroll = false

local Ctmp = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <inttypes.h>

  double CurrentTimeInSeconds() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + tv.tv_usec / 1000000.0;
                                 }

                                   ]]

orion.currentTimeInSeconds = Ctmp.CurrentTimeInSeconds

terra orionAssert(cond : bool, str : &int8)
  if cond==false then
    cstdio.printf("ASSERTT fail %s\n", str)
    cstdlib.exit(1)
  end
end


function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end

terralib.require("types")
terralib.require("tune")
if isModuleAvailable("tuneThisMachine") then terralib.require("tuneThisMachine") end
require "stencil"
require "conv"
require "optimizations"
require "ir"
terralib.require("ast")
terralib.require("imageWrapper")
require "typedAST"
terralib.require("convir")
require "internalir"
require "kernelgraph"
terralib.require("image")
require "schedule"
terralib.require("flatir")
terralib.require("api")
terralib.require("cropir")
terralib.require("terracompiler")
-- terra compiler gets loaded at the end of the file after everything else


----------------------
-- these are macros, which will become ops in the orion langauge
-- orion macros are passed the AST of the macro itself as the first argument

function orion.pow( thisast, base, exp)
  assert(orion.ast.isAST(base))
  assert(orion.ast.isAST(exp))
  return orion.ast.new({kind="binop",op="pow",lhs=base,rhs=exp}):copyMetadataFrom(thisast)
end

function orion.min( thisast, ...)
  assert(orion.ast.isAST(thisast))
  local args = {...}
  local n = {kind="reduce",op="min"}
  for k,v in ipairs(args) do
    assert(orion.ast.isAST(v))
    n["expr"..k] = v
  end

  return orion.ast.new(n):copyMetadataFrom(thisast)
end

function orion.max( thisast, ...)
  assert(orion.ast.isAST(thisast))
  local args = {...}
  local n = {kind="reduce",op="max"}
  for k,v in ipairs(args) do
    assert(orion.ast.isAST(v))
    n["expr"..k] = v
  end

  return orion.ast.new(n):copyMetadataFrom(thisast)
end

function orion.sum( thisast, ... )
  assert(orion.ast.isAST(thisast))
  local args = {...}
  local n = {kind="reduce",op="sum"}
  for k,v in ipairs(args) do
    assert(orion.ast.isAST(v))
    n["expr"..k] = v
  end

  return orion.ast.new(n):copyMetadataFrom(thisast)
end

function orion.dot( thisast, v1,v2)
  assert(orion.ast.isAST(v1))
  assert(orion.ast.isAST(v2))
  return orion.ast.new({kind="binop",op="dot",lhs=v1,rhs=v2}):copyMetadataFrom(thisast)
end

function orion.floor( thisast, expr)
  assert(orion.ast.isAST(expr))
  return orion.ast.new({kind="unary",op="floor",expr=expr}):copyMetadataFrom(thisast)
end

function orion.abs( thisast, expr)
  assert(orion.ast.isAST(expr))
  return orion.ast.new({kind="unary",op="abs",expr=expr}):copyMetadataFrom(thisast)
end

function orion.cos( thisast, expr)
  assert(orion.ast.isAST(expr))
  return orion.ast.new({kind="unary",op="cos",expr=expr}):copyMetadataFrom(thisast)
end

function orion.sin( thisast, expr)
  assert(orion.ast.isAST(expr))
  return orion.ast.new({kind="unary",op="sin",expr=expr}):copyMetadataFrom(thisast)
end

function orion.exp( thisast, expr)
  assert(orion.ast.isAST(expr))
  return orion.ast.new({kind="unary",op="exp",expr=expr}):copyMetadataFrom(thisast)
end

function orion.arrayAnd( thisast, expr)
  assert(orion.ast.isAST(expr))
  return orion.ast.new({kind="unary",op="arrayAnd",expr=expr}):copyMetadataFrom(thisast)
end

function orion.vectorSelect( thisast, cond, a, b)
  assert(orion.ast.isAST(cond))
  assert(orion.ast.isAST(a))
  assert(orion.ast.isAST(b))
  return orion.ast.new({kind="vectorSelect",cond=cond,a=a,b=b}):copyMetadataFrom(thisast)
end


-- if cond if false, prints linenumber, printval
-- if cond is true, returns expr
function orion.assert( thisast, expr, printval, cond )
  assert(orion.ast.isAST(expr))
  assert(orion.ast.isAST(printval))
  assert(orion.ast.isAST(cond))

  return orion.ast.new({kind="assert",expr=expr,printval=printval,cond=cond}):copyMetadataFrom(thisast)
end

function orion.print( thisast, exp)
  return orion.ast.unary("print",exp)
end

-- input: the input image to gather from
-- x,y = offsets from current (x,y). ie should be small values, 0,1,2 etc
-- maxX, maxY maximum area to gather from. so x=[-maxX,maxX], y=[-maxY,maxY]
-- clamp: what to do when we go outside of maxX, maxY? clamp to that
--        window, or raise an assert?
function orion.gather( thisast, input,x,y,maxXV,maxYV,clamp)
  assert(orion.ast.isAST(input))
  assert(orion.ast.isAST(x))
  assert(orion.ast.isAST(y))

  assert(orion.ast.isAST(maxXV))
  assert(maxXV.kind=="value")
  local maxX = maxXV.value
  assert(type(maxX)=="number")

  assert(orion.ast.isAST(maxYV))
  assert(maxYV.kind=="value")
  local maxY = maxYV.value
  assert(type(maxY)=="number")

  assert(orion.ast.isAST(clamp))
  assert(clamp.kind=="value")
  clamp = clamp.value
  assert(type(clamp)=="boolean")

  local negmaxXV = maxXV:shallowcopy()
  negmaxXV.value = -negmaxXV.value
  negmaxXV = orion.ast.new(negmaxXV):copyMetadataFrom(maxXV)

  local negmaxYV = maxYV:shallowcopy()
  negmaxYV.value = -negmaxYV.value
  negmaxYV = orion.ast.new(negmaxYV):copyMetadataFrom(maxXV)

  local px = orion.ast.new({kind="position",coord="x"}):copyMetadataFrom(thisast)
  local py = orion.ast.new({kind="position",coord="y"}):copyMetadataFrom(thisast)

  local a = orion.ast.new({kind="binop", lhs=px, rhs=negmaxXV, op="+"}):copyMetadataFrom(thisast)
  local b = orion.ast.new({kind="binop", lhs=py, rhs=negmaxYV, op="+"}):copyMetadataFrom(thisast)
  local hackBL = orion.ast.new({kind="transform",expr = input, arg1 = a, arg2 = b}):copyMetadataFrom(thisast)

  local c = orion.ast.new({kind="binop", lhs=px, rhs=maxXV, op="+"}):copyMetadataFrom(thisast)
  local d = orion.ast.new({kind="binop", lhs=py, rhs=maxYV, op="+"}):copyMetadataFrom(thisast)
  local hackTR = orion.ast.new({kind="transform",expr = input, arg1 = c, arg2 = d}):copyMetadataFrom(thisast)

  return orion.ast.new({kind="gather",
                        input=input, 
                        hackBL=hackBL, 
                        hackTR=hackTR,
                        x=x,
                        y=y,
                        maxX=maxX,
                        maxY=maxY,
                        clamp=clamp}):copyMetadataFrom(thisast)
end

---------------------------------

orion._currentBlock = ""
orion.error=function(msg,line,char,filename)
  if line==nil then
    print(msg)
  else
    if filename==nil then filename = "" end
    print(filename.." line "..line.." "..msg)
  end

  --print(orion._currentBlock)
  local code = explode("\n",orion._currentBlock)
  for i=1,#code do
    print(code[i])
    if i==line and char~=nil then 
      local bar = ""
      for j=1,(char-3) do bar = bar .. "-" end
      print(bar.."^")
    end
  end

  print(debug.traceback())

  os.exit()
end

function orion.warning(str, line, offset,filename)
  print("Warning: "..str,"line",line,"offset",offset,"filename",filename)
end

function orion.remapSubkey(subkey)
  if type(subkey)=="string" then
    if orion.keyremap[subkey]==nil then
      print("Error, subkey "..subkey.." doesn't map to an array index")
      os.exit()
    end

    return orion.keyremap[subkey]

  elseif type(subkey)=="number" then
    return subkey
  end

  orion.error("Error, subkey has bad type "..type(subkey))

  return nil
end


-- convert the luavalue to an orion type
-- implements behavior of dropping a unquoted lua value in Orion
-- ast is either a func node or a lua node that yielded this value
function orion.quoteToOrion(luavalue, ast)
  assert(orion.ast.isAST(ast))
  assert(ast.kind=="func" or ast.kind=="lua")


  if orion.ast.isAST(luavalue) then
    return luavalue
  elseif (type(luavalue)=="number") or (type(luavalue)=="boolean") then
    if ast:arraySize("arg")>0 then
      orion.warning("You are translating / scaling a constant - this is probably not what you wanted to do.")
    end

    return orion.ast.new({kind="value",value=luavalue}):copyMetadataFrom(ast)
  elseif type(luavalue)=="table" and orion.type.isType(luavalue) then
    return orion.ast.new({kind="type",type=luavalue}):copyMetadataFrom(ast)
  elseif type(luavalue)=="table" then
    for k,v in pairs(luavalue) do
      if type(k)~="number" then 
        if ast.identifier~=nil then print(table.concat(ast.identifier)) end
        orion.error("When converting a lua table to an orion type, all keys must be numeric",ast:linenumber(), ast:offset())
      end
    end

    local newnode = {kind="array"}

    for i=1,#luavalue do
      if type(luavalue[i])~="number" then
        orion.error("tables inserted into orion must contain all numbers", ast:linenumber(), ast:offset(),ast:filename())
      end

      newnode["expr"..i] = orion.ast.new({kind="value",value=luavalue[i]}):copyMetadataFrom(ast)
    end

    return orion.ast.new(newnode):copyMetadataFrom(ast)

  elseif(type(luavalue)=="function") then
    assert(ast.kind=="func")
    return orion.ast.new({kind="macro",func=luavalue}):copyMetadataFrom(ast)
  end
  
  orion.error("Type "..type(luavalue).." can't be converted to an orion type!", ast:linenumber(), ast:offset(), ast:filename())

  return nil
end

-- take a 'func' ast node, resolve its identifier, and plop in
-- the ast node for its value. This is either:
-- (a) a lua variable, which becomes a const
-- (b) a lua variable, which contains an ast (and generate translate/scale as needed)
function orion.resolveIdentifiers(ast,xvar,yvar,zvar,env)
  assert(ast.kind=="func")
  assert(type(env)=="table")
  
  local i=1
  local identifier={}
  while ast["identifier"..i] do
    local v = ast["identifier"..i]
    if orion.ast.isAST(v) then
      if v.kind=="value" then
        assert(type(v.value)=="string" or type(v.value)=="number")
        identifier[k] = v.value
      else
        orion.error("key into lua variable or array must be constant")
      end
    else
      assert(type(v)=="string" or type(v)=="number")
      identifier[i]=ast["identifier"..i]
    end
    i=i+1
  end

  local targetTable,key = orion.util.identifierToVariable(env,identifier)
  
  if #identifier==1 and (identifier[1]==xvar) then
    -- check if this is the position argument first
    if ast.arg1~=nil then orion.error("You can't offset a position var!",ast.line,ast.char);os.exit() end
    return orion.ast.new({kind="position",coord="x"}):copyMetadataFrom(ast)
  elseif #identifier==1 and (identifier[1]==yvar) then
    if ast.arg1~=nil then orion.error("You can't offset a position var!",ast.line,ast.char);os.exit() end
    return orion.ast.new({kind="position",coord="y"}):copyMetadataFrom(ast)
  elseif #identifier==1 and (identifier[1]==zvar) then
    if ast.arg1~=nil then orion.error("You can't offset a position var!",ast.line,ast.char);os.exit() end
    return orion.ast.new({kind="position",coord="z"}):copyMetadataFrom(ast)
  elseif targetTable==nil then
    orion.error("Fatal error, variable ".. table.concat(identifier,".") .." is undefined!",ast.line,ast.char)
    os.exit()
  elseif (Strict~=nil and targetTable==_G and Strict.isDeclared(key)==false) or  -- have to do this to make Strict.lua not trigger
         targetTable[key]==nil then

    -- note that, unlike when we're finding a variable to write to, when
    -- we're resolving an existing variable targetTable[key] must be defined

    -- check if we're dealing with the case where we're looking up whatever.whatever.r or whatever.whatever[3]
    -- ie we have an array type we're indexing into

    if #identifier>1 then
      local subident = {}
      for i=1,#identifier-1 do subident[i]=identifier[i] end
      local targetTable,key = orion.util.identifierToVariable(env,subident)
      
      -- this shouldn't be possible if we got this far
      assert(targetTable~=nil)
      assert(targetTable[key]~=nil)

      if orion.ast.isAST(targetTable[key]) then
        local subkey = identifier[#identifier]
        return orion.ast.index(targetTable[key], orion.remapSubkey(subkey) )
      end
    end

    orion.error("Fatal error, variable ".. table.concat(identifier,".") .." is nil!",ast:linenumber(),ast.char)
    os.exit()
  end

  if orion.ast.isAST(targetTable[key]) then
    return targetTable[key]
  end
  
  -- it's a lua value. convert it to an ast
  return orion.quoteToOrion(targetTable[key], ast)

end



---------------------------------------------------------
local Parser = terralib.require("parsing")
local lang = {}

lang.value = function(p)

    if p:nextif("true") then
      return orion.ast.new({kind="value",value=true}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    elseif p:nextif("false") then
      return orion.ast.new({kind="value",value=false}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    elseif p:matches(p.number) then
      return orion.ast.new({kind="value",value=p:next().value}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    elseif p:matches(p.name) then
      return p:func()
    else
      p:errorexpected("value")
    end

    return nil
end

----------------------------------------------------------
lang.expr = Parser.Pratt():prefix(Parser.default,function(p)
    return p:value()
  end)

  -- prescedence for binary operators. based on c++ precedence
  local precedence = {}
  precedence["or"] = 10
  precedence["and"] = 11
  precedence["|"] = 12
  precedence["^"] = 13
  precedence["&"] = 14
  precedence["=="] = 20
  precedence["~="] = 20
  precedence["<"] = 21
  precedence[">"] = 21
  precedence[">="] = 21
  precedence["<="] = 21
  precedence["<<"] = 30
  precedence[">>"] = 30
  precedence["+"] = 40
  precedence["-"] = 40
  precedence["%"] = 50
  precedence["*"] = 50
  precedence["/"] = 50


  for op,_ in pairs(orion.binops) do
    lang.expr = lang.expr:infix(op,precedence[op],function(p,lhs)
      local op = p:next().type
      local rhs = p:expr(precedence[op])
      return orion.ast.new({kind="binop", op=op, lhs=lhs, rhs=rhs}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    end)
  end

  for op,_ in pairs(orion.unops) do
    lang.expr = lang.expr:prefix(op,
      function(p)
        local op = p:next().type
        local expr = p:expr(90)
        return orion.ast.new({kind="unary",op=op,expr = expr}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
      end)
  end

  -- postfix
  lang.expr = lang.expr:infix("[", 100, 
    function(p,lhs)
      p:expect("[")
      local idx = p:expr()
      p:expect("]")
      return orion.ast.new({kind="index",index=idx,expr = lhs}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    end)

  lang.expr = lang.expr:infix("(", 100, 
    function(p,lhs)
      p:expect("(")

      local args = {}
      table.insert(args,p:expr())
      
      while p:nextif(",") do
        table.insert(args,p:expr())
      end

      p:expect(")")

      local newnode = {kind="apply",expr=lhs}
      for k,v in pairs(args) do newnode["arg"..k]=v end
      return orion.ast.new(newnode):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    end)

  lang.expr = lang.expr:prefix("(",function(p)
    p:expect("(")
    local expr = p:expr()
    p:expect(")")
    return expr
  end)
  :prefix("{",function(p)
    local ar = {kind="array"}
    local exprcnt = 1

    p:expect("{")
    
    repeat
      ar["expr"..exprcnt] = p:expr()
      exprcnt = exprcnt + 1
    until p:nextif(",")==false

    p:expect("}")

    return orion.ast.new(ar):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)
  :prefix("[",function(p)
    p:expect("[")
    local expr = p:luaexpr()
    p:expect("]")
    return orion.ast.new({kind="lua",expr = expr}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)
  :prefix("if",function(p)
    p:expect("if")
    local condexpr = p:expr()
    p:expect("then")
    local a = p:expr()
    p:expect("else")
    local b = p:expr()
    p:expect("end")
    return orion.ast.new({kind="select",cond=condexpr,a=a,b=b}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)
  :prefix("switch",function(p)
    local switchexpr = {kind="switch"}

    p:expect("switch")

    switchexpr.controlExpr = p:expr()
    
    local cnt = 1
    while p:matches("default")==false do
      p:expect("case")
      switchexpr["val"..cnt] = p:expr()
      p:expect("->")
      switchexpr["expr"..cnt] = p:expr()
      p:nextif(";") -- optionally accept semicolon
      cnt = cnt + 1
    end

    p:expect("default")
    p:expect("->")
    switchexpr.default = p:expr()
    p:nextif(";") -- optionally accept semicolon

    p:expect("end")

    return orion.ast.new(switchexpr):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)
  :prefix("let",function(p)
    local letast = {kind="let"}
    local exprcnt = 1

    local seen = {}

    p:expect("let")
    
    repeat
      local name = p:expect(p.name).value

      if seen[name]~=nil then
        p:error("name '"..name.."' is a duplicate, which isn't allowed")
      end
      seen[name] = 1

      local ty
      if p:nextif(":") then
        ty = p:typeexpr()
      end

      p:expect("=")

      local expr = p:expr()

      if ty~=nil then
        expr = orion.ast.new({kind="cast",expr=expr,type=ty}):copyMetadataFrom(expr)
      end

      p:nextif(";") -- accept a semicolon at the end if the user added one

      letast["exprname"..exprcnt]=name
      letast["expr"..exprcnt]=expr
      exprcnt = exprcnt+1

    until p:nextif("in")

    letast.res = p:expr()

    return orion.ast.new(letast):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)
  :prefix("map",function(p)
    p:expect("map")

    local vars={}
    while p:matches(p.name) do
      local name = p:expect(p.name).value
      p:expect("=")
      local low = p:expr()
      p:expect(",")
      local high = p:expr()
      if vars[name]~=nil then p:error("Can't use the same name twice") end
      table.insert(vars,{name,low,high})
    end

    p:expect("reduce")
    p:expect("(")
    local reduceop = p:expect(p.name).value
    p:expect(")")

    local expr = p:expr()
    
    p:expect("end")

    local newnode = {kind="mapreduce",expr=expr,reduceop=reduceop}
    for k,v in ipairs(vars) do newnode["varname"..k]=v[1];newnode["varlow"..k]=v[2];newnode["varhigh"..k]=v[3]; end
    return orion.ast.new(newnode):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)

------------------------------------------------------------
lang.identifier = function(p)

  local ident = {}
  table.insert(ident,p:expect(p.name).value)
  p:ref(ident[1])
  
  -- parse a chain: lol.wtf.bbq    
  -- if people want to use the [] index operator, force them to
  -- use an explicit escape
  while p:nextif(".") do
    table.insert(ident,p:expect(p.name).value)
  end
  
  return ident
end

------------------------------------------------------------
lang.type = function(p)

  local ty

  if p:matches(p.name) then
    -- this is kind of messed up
    -- we special case stuff like 'float32' etc
    -- so if you use a name (for ex a local variable)
    -- instead, it will think it's not a valid type.
    -- if you want to do this, use an escape
    local name = p:expect(p.name).value
    local arrs
    if p:nextif("[") then
      arrs = p:expect(p.number).value
      p:expect("]")
    end
    
    ty = orion.type.stringToType(name)
    
    if ty==nil then
      p:error("Unknown type "..name)
    else
      if arrs~=nil then
        ty = orion.type.array(ty,arrs)
      end
    end

    assert(orion.type.isType(ty))

    -- wrap the type in a type ast node. This is stupid.
    -- the reason we need to do this is b/c the IR library
    -- won't let us patch tables into the AST. So the
    -- result of an escape AST node is a type AST node,
    -- not a type itself

    return orion.ast.new({kind="type",type=ty}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end

  -- escapes and other expr that evaluate to a type
  return p:expr()
end

lang.typeexpr = function(p)

  local cropMode, type, x, y, w, h

  repeat
    if p:matches(p.name) then
      local name = p:cur().value

      if name=="cropNone" then
        cropMode = orion.cropNone
        p:next()
      elseif name=="cropGrow" then
        cropMode = orion.cropGrow
        p:next()
      elseif name=="cropShrink" then
        cropMode = orion.cropShrink
        p:next()
      elseif name=="cropSame" then
        cropMode = orion.cropSame
        p:next()
      elseif name=="crop" then
        cropMode = orion.cropExplicit
        
        p:next()
        p:expect("(")
        
        x = p:expect(p.number).value
        p:expect(",")
        y = p:expect(p.number).value
        p:expect(",")
        w = p:expect(p.number).value
        if w<=0 then p:error("crop width must be > 0") end
        p:expect(",")
        h = p:expect(p.number).value
        if h<=0 then p:error("crop height must be > 0") end
        
        p:expect(")")
      else
        type = p:type() --orion.type.stringToType(name)
      end
    else
      -- could be an escape for exx
      type = p:type() --orion.type.stringToType(name)
    end
  until p:nextif(",") == false 

  return type, cropMode, x, y, w ,h
end

------------------------------------------------------------
lang.func = function(p)
  local ident = p:identifier()
  
  local newnode = {kind="func"}
  for k,v in ipairs(ident) do newnode["identifier"..k]=v end
  return orion.ast.new(newnode):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
end

  -----------------------------------------------------------
lang.imageFunction = function(p)

    p:expect("im")

    local ident

    -- functions can be anonymous
    if p:matches(p.name) then
      ident = p:identifier()
    end

    local xvar = nil
    local yvar = nil
    local zvar = nil


    p:expect("(")
    xvar = p:expect(p.name).value
    p:expect(",")
    yvar = p:expect(p.name).value
    p:expect(")")

    local castType, cropMode, x, y, w, h
    if p:nextif(":") then castType, cropMode,x,y,w,h = p:typeexpr() end

    local rvalue = p:expr()
    if castType~=nil then rvalue=orion.ast.new({kind="cast",expr=rvalue,type=castType}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename) end

    p:expect("end")

    -- insert the crop
    if cropMode==nil then cropMode=orion.defaultCrop end
    if cropMode == orion.cropExplicit then
      rvalue = orion.ast.new({kind="crop", mode=cropMode, expr=rvalue, x=x,y=y,w=w,h=h}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    elseif cropMode~=orion.cropNone then
      rvalue = orion.ast.new({kind="crop", mode=cropMode, expr=rvalue}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    end

    local chosenName
    if ident==nil then
      local cleanFilename = rvalue:filename():gsub("%.","")
      cleanFilename = cleanFilename:gsub([[%/]],"")
      cleanFilename = cleanFilename:gsub([[%-]],"")
      cleanFilename = cleanFilename:sub(#cleanFilename-10)

      chosenName = "lambda_"..cleanFilename.."_line"..rvalue:linenumber()
    else
      assert(type(ident)=="table")
      chosenName = table.concat(ident,".")
    end

    rvalue:S("*"):process(
      function(n)
        n:setName(chosenName)
      end)

    -- this isn't really an ast node, just a table that holds info about this im func
    return {identifier=ident,xvar=xvar,yvar=yvar,zvar=zvar,
            rvalue=rvalue, linenumber=p:cur().linenumber, offset=p:cur().offset, filename = p:cur().filename}
end

function orion.compileTimeProcess(imfunc, envfn)

  -- first, check that the users AST isn't totally messed up
  if orion.verbose then 
    print("start process, node count:",imfunc.rvalue:S("*"):count(),imfunc.rvalue:name()) 
  end

  imfunc.rvalue:check()

  local env = envfn()
  local rvalue = imfunc.rvalue

  -- this is kind of subtle. It's extremely important that we don't process the AST exponentially here,
  -- b/c that will mess us up for the rest of the compile.
  -- Something that could cause this for ex, would be first evaluating escapes, and then desugaring let statements.
  -- What would happen then, is any ASTs in the escape would recieve let statement processing, and then we would do the let
  -- statement processing again here. So, for stuff like mapreduce, let, we just replace the variables with placeholders,
  -- and do the desugaring later in toTypedAST, once we have the whole graph.
  -- so it's important that we do everything here in one pass.
  rvalue=rvalue:S("mapreduce"):process(
    function(inp)
      local newNode = inp:shallowcopy()
      
      local i,vars,low,high = 1,{},{},{}
      while inp["varname"..i] do 
        vars[inp["varname"..i]]=1;
        low[inp["varname"..i]]=inp["varlow"..i];
        high[inp["varname"..i]]=inp["varhigh"..i];
        i=i+1 
      end
      
      newNode.expr = inp.expr:S("func"):process(
        function(fin)
          if fin.identifier1~=nil and fin.identifier2==nil and vars[fin.identifier1]~=nil then
            return orion.ast.new({kind="mapreducevar",variable=fin.identifier1, low=low[fin.identifier1], high=high[fin.identifier1]}):copyMetadataFrom(fin)
          end
          return fin
        end)
      newNode = orion.ast.new(newNode):copyMetadataFrom(inp)
      newNode:check()
      
      return newNode
    end)
  
  -- desugar let statements

  local function removeLet( expr, namemap )
    return expr:S("func"):process(
      function(n)
        if n:arraySize("identifier")==1 and namemap[n.identifier1]~=nil then
          if n:arraySize("arg")>0 then
            orion.error("You can't index into a named expression in a let statement",n:linenumber(),n:offset())
          end
          return namemap[n.identifier1]
        end
        return n
      end)
  end

  rvalue = rvalue:S("let"):process(
    function(n)
      local newNode = n:shallowcopy()
      local cnt = 1
      local namemap = {}

      while n["expr"..cnt] do
        newNode["expr"..cnt] = removeLet( n["expr"..cnt], namemap)
        namemap[n["exprname"..cnt]] = orion.ast.new({kind="letvar",variable = n["exprname"..cnt]}):copyMetadataFrom(n)
        cnt = cnt + 1
      end

      newNode.res = removeLet( n.res, namemap )
      return orion.ast.new(newNode):copyMetadataFrom(n)
    end)

  if orion.verbose then
    print("Size preresolve:",rvalue:S("*"):count(),rvalue:name())
  end

  rvalue = rvalue:S(
    function(n) 
      return n.kind=="apply" or 
        n.kind=="func" or 
        n.kind=="lua" or 
        n.kind=="index" end):process(
    function(inp)
      if inp.kind=="func" then
        return orion.resolveIdentifiers(inp, imfunc.xvar,imfunc.yvar,imfunc.zvar,env)
      elseif inp.kind=="lua" then
        return orion.quoteToOrion(inp.expr(inp:localEnvironment(rvalue,env)),inp)
      elseif inp.kind=="index" then
        if inp.expr.kind=="tapLUT" then
          local newnode = inp.expr:shallowcopy()
          newnode.kind="tapLUTLookup"
          newnode.index = inp.index
          assert(type(inp.expr.tapname)=="string")
          return orion.ast.new(newnode):copyMetadataFrom(inp)
        end
      else
        if inp.expr.kind=="macro" then
          local args = {}
          inp:map("arg",function(n) table.insert(args,n) end)
          assert(#args>0)
          
          local macroResult = inp.expr.func(inp, unpack(args))
          
          if orion.ast.isAST(macroResult)==false then
            orion.error("Macro returned something other than an AST")
          end
          
          return macroResult
        elseif inp.expr.kind=="tap" then
          assert(false)
        else
          if inp:keyCount("arg") < 2 then
            orion.error("At least two arguments must be specified when transforming.",
                        inp:linenumber(),inp:offset())
          end
          
          -- turn it into a transform
          local newnode = inp:shallowcopy()
          newnode.kind="transform"
          return orion.ast.new(newnode):copyMetadataFrom(inp)
        end
      end
    end)

  if orion.verbose then
    print("end process, node count:",rvalue:S("*"):count(),rvalue:name())
  end

  return rvalue
end

return {
  name = "orion";
  entrypoints = {"im"};
  keywords = {"map","reduce","let","in","switch","default","case"};
  statement = function(self,lex)
    local imfunc = Parser.Parse(lang,lex,"imageFunction")
    imfunc.rvalue:check()

    if orion.verbose then
      print("Parse Done -----------")
      imfunc.rvalue:printpretty()
    end
 
    return 
      function(envfn)
      return orion.compileTimeProcess(imfunc,envfn)
      end, {imfunc.identifier}
  end;
  localstatement = function(self,lex)
    local imfunc = Parser.Parse(lang,lex,"imageFunction")
    imfunc.rvalue:check()

    if orion.verbose then
      print("Parse Done -----------")
      imfunc.rvalue:printpretty()
    end
 
    return 
      function(envfn)
      return orion.compileTimeProcess(imfunc,envfn)
      end, {imfunc.identifier}
  end;
  expression = function(self,lex)
    local imfunc = Parser.Parse(lang,lex,"imageFunction")
    imfunc.rvalue:check()

    if orion.verbose then
      print("Parse Done -----------")
      imfunc.rvalue:printpretty()
    end


    return 
      function(envfn)
      return orion.compileTimeProcess(imfunc,envfn)
      end
  end
}
