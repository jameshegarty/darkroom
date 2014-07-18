darkroom.boolops = {["or"]=1,["and"]=1} -- bool -> bool -> bool
darkroom.cmpops = {["=="]=1,["~="]=1,["<"]=1,[">"]=1,["<="]=1,[">="]=1} -- number -> number -> bool
darkroom.binops = {["|"]=1,["^"]=1,["&"]=1,["<<"]=1,[">>"]=1,["+"]=1,["-"]=1,["%"]=1,["*"]=1,["/"]=1}
-- these binops only work on ints
darkroom.intbinops = {["<<"]=1,[">>"]=1,["&"]=1,["|"]=1,["^"]=1}
-- ! does a logical not in C, use 'not' instead
-- ~ does a bitwise not in C
darkroom.unops = {["not"]=1,["-"]=1}
appendSet(darkroom.binops,darkroom.boolops)
appendSet(darkroom.binops,darkroom.cmpops)
darkroom.keyremap = {r=0,g=1,b=2, x=0, y=1, z=2}
darkroom.dimToCoord={[1]="x",[2]="y",[3]="z"}

----------------------
-- these are macros, which will become ops in the orion langauge
-- orion macros are passed the AST of the macro itself as the first argument

function darkroom.crop( thisast, expr )
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="crop",shiftY=0,expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.pow( thisast, base, exp)
  assert(darkroom.ast.isAST(base))
  assert(darkroom.ast.isAST(exp))
  return darkroom.ast.new({kind="binop",op="pow",lhs=base,rhs=exp}):copyMetadataFrom(thisast)
end

function darkroom.min( thisast, ...)
  assert(darkroom.ast.isAST(thisast))
  local args = {...}
  local n = {kind="reduce",op="min"}
  for k,v in ipairs(args) do
    assert(darkroom.ast.isAST(v))
    n["expr"..k] = v
  end

  return darkroom.ast.new(n):copyMetadataFrom(thisast)
end

function darkroom.max( thisast, ...)
  assert(darkroom.ast.isAST(thisast))
  local args = {...}
  local n = {kind="reduce",op="max"}
  for k,v in ipairs(args) do
    assert(darkroom.ast.isAST(v))
    n["expr"..k] = v
  end

  return darkroom.ast.new(n):copyMetadataFrom(thisast)
end

function darkroom.sum( thisast, ... )
  assert(darkroom.ast.isAST(thisast))
  local args = {...}
  local n = {kind="reduce",op="sum"}
  for k,v in ipairs(args) do
    assert(darkroom.ast.isAST(v))
    n["expr"..k] = v
  end

  return darkroom.ast.new(n):copyMetadataFrom(thisast)
end

function darkroom.dot( thisast, v1,v2)
  assert(darkroom.ast.isAST(v1))
  assert(darkroom.ast.isAST(v2))
  return darkroom.ast.new({kind="binop",op="dot",lhs=v1,rhs=v2}):copyMetadataFrom(thisast)
end

function darkroom.floor( thisast, expr)
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="unary",op="floor",expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.ceil( thisast, expr)
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="unary",op="ceil",expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.abs( thisast, expr)
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="unary",op="abs",expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.cos( thisast, expr)
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="unary",op="cos",expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.sin( thisast, expr)
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="unary",op="sin",expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.exp( thisast, expr)
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="unary",op="exp",expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.arrayAnd( thisast, expr)
  assert(darkroom.ast.isAST(expr))
  return darkroom.ast.new({kind="unary",op="arrayAnd",expr=expr}):copyMetadataFrom(thisast)
end

function darkroom.vectorSelect( thisast, cond, a, b)
  assert(darkroom.ast.isAST(cond))
  assert(darkroom.ast.isAST(a))
  assert(darkroom.ast.isAST(b))
  return darkroom.ast.new({kind="vectorSelect",cond=cond,a=a,b=b}):copyMetadataFrom(thisast)
end


-- if cond if false, prints linenumber, printval
-- if cond is true, returns expr
function darkroom.assert( thisast, expr, printval, cond )
  assert(darkroom.ast.isAST(expr))
  assert(darkroom.ast.isAST(printval))
  assert(darkroom.ast.isAST(cond))

  return darkroom.ast.new({kind="assert",expr=expr,printval=printval,cond=cond}):copyMetadataFrom(thisast)
end

function darkroom.print( thisast, exp)
  return darkroom.ast.unary("print",exp)
end

-- input: the input image to gather from
-- x,y = offsets from current (x,y). ie should be small values, 0,1,2 etc
-- maxX, maxY maximum area to gather from. so x=[-maxX,maxX], y=[-maxY,maxY]
-- clamp: what to do when we go outside of maxX, maxY? clamp to that
--        window, or raise an assert?
function darkroom.gather( thisast, input,x,y,maxXV,maxYV,clamp)
  assert(darkroom.ast.isAST(input))
  assert(darkroom.ast.isAST(x))
  assert(darkroom.ast.isAST(y))

  assert(darkroom.ast.isAST(maxXV))
  assert(maxXV.kind=="value")
  local maxX = maxXV.value
  assert(type(maxX)=="number")

  assert(darkroom.ast.isAST(maxYV))
  assert(maxYV.kind=="value")
  local maxY = maxYV.value
  assert(type(maxY)=="number")

  assert(darkroom.ast.isAST(clamp))
  assert(clamp.kind=="value")
  clamp = clamp.value
  assert(type(clamp)=="boolean")

  return darkroom.ast.new({kind="gather",
                        input=input, 
                        x=x,
                        y=y,
                        maxX=maxX,
                        maxY=maxY,
                        clamp=clamp}):copyMetadataFrom(thisast)
end

---------------------------------

darkroom._currentBlock = ""
darkroom.error=function(msg,line,char,filename)
  if line==nil then
    print(msg)
  else
    if filename==nil then filename = "" end
    print(filename.." line "..line.." "..msg)
  end

  --print(darkroom._currentBlock)
  local code = explode("\n",darkroom._currentBlock)
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

function darkroom.warning(str, line, offset,filename)
  print("Warning: "..str,"line",line,"offset",offset,"filename",filename)
end

-- convert the luavalue to an orion type
-- implements behavior of dropping a unquoted lua value in Orion
-- ast is either a func node or a lua node that yielded this value
function darkroom.evalEscape(luavalue, ast)
  assert(darkroom.ast.isAST(ast))
  assert(ast.kind=="escape")

  if darkroom.ast.isAST(luavalue) then
    return luavalue
  elseif (type(luavalue)=="number") or (type(luavalue)=="boolean") then
    if ast:arraySize("arg")>0 then
      darkroom.warning("You are translating / scaling a constant - this is probably not what you wanted to do.")
    end

    return darkroom.ast.new({kind="value",value=luavalue}):copyMetadataFrom(ast)
  elseif type(luavalue)=="table" and terralib.types.istype(luavalue) then
    return darkroom.ast.new({kind="type",type=darkroom.type.fromTerraType(luavalue)}):copyMetadataFrom(ast)
  elseif type(luavalue)=="table" then
    for k,v in pairs(luavalue) do
      if type(k)~="number" then 
        if ast.identifier~=nil then print(table.concat(ast.identifier)) end
        darkroom.error("When converting a lua table to an orion type, all keys must be numeric",ast:linenumber(), ast:offset())
      end
    end

    local newnode = {kind="array"}

    for i=1,#luavalue do
      if type(luavalue[i])~="number" then
        darkroom.error("tables inserted into orion must contain all numbers", ast:linenumber(), ast:offset(),ast:filename())
      end

      newnode["expr"..i] = darkroom.ast.new({kind="value",value=luavalue[i]}):copyMetadataFrom(ast)
    end

    return darkroom.ast.new(newnode):copyMetadataFrom(ast)

  elseif(type(luavalue)=="function") then
    assert(ast.kind=="func")
    return darkroom.ast.new({kind="macro",func=luavalue}):copyMetadataFrom(ast)
  end
  
  darkroom.error("Type "..type(luavalue).." can't be converted to an orion type!", ast:linenumber(), ast:offset(), ast:filename())

  return nil
end

---------------------------------------------------------
darkroom.Parser = terralib.require("parsing")
darkroom.lang = {}

local function literal(p, v)
  p:next()
  return darkroom.ast.new({kind="value",value=v}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
end

local function unary(p)
  local op = p:next().type
  local expr = p:expr(5)
  return darkroom.ast.new({kind="unary",op=op,expr = expr}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
end

local function binary(p,lhs,fixity)
  local op = p:next().type
  local rhs = p:expr(op,fixity)
  return darkroom.ast.new({kind="binop", op=op, lhs=lhs, rhs=rhs}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
end
local leftbinary = binary
----------------------------------------------------------
darkroom.lang.expr = darkroom.Parser.Pratt()
:prefix("-", unary)
:prefix("not", unary)
:infix("or", 0, leftbinary)
:infix("and", 1, leftbinary)

:infix("<", 2, leftbinary)
:infix(">", 2, leftbinary)
:infix("<=", 2, leftbinary)
:infix(">=", 2, leftbinary)
:infix("==", 2, leftbinary)
:infix("~=", 2, leftbinary)

:infix("<<", 2, leftbinary)
:infix(">>", 2, leftbinary)

:infix("+", 3, leftbinary)
:infix("-", 3, leftbinary)
:infix("*", 4, leftbinary)
:infix('/', 4, leftbinary)
:infix('%', 4, leftbinary)
:infix('.', 7, function(p,lhs)
         p:next()
         return darkroom.ast.new({kind="fieldselect", expr = lhs, field = p:expect(p.name).value}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
               end)
:infix("[", 8, function(p,lhs)
         local begin = p:next().linenumber
         local idx = p:expr()
         p:expectmatch(']', '[', begin )
         return darkroom.ast.new({kind="index",index=idx,expr = lhs}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    end)
:infix("(", 8, function(p,lhs)
         local begin = p:next().linenumber

         local args = {}
         table.insert(args,p:expr())
      
         while p:nextif(",") do
           table.insert(args,p:expr())
         end

         p:expectmatch(')','(', begin)

         local newnode = {kind="apply",expr=lhs}
         for k,v in pairs(args) do newnode["arg"..k]=v end
         return darkroom.ast.new(newnode):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
    end)
:prefix(darkroom.Parser.name,function(p)
          local name = p:next().value
          p:ref(name) --make sure var appears in the envfn passed to constructor
          return darkroom.ast.new({kind="var",name=name}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
                   end)
:prefix(darkroom.Parser.number, function(P) return literal(P,P:cur().value) end)
:prefix('true', function(P) return literal(P,true) end)
:prefix('false', function(P) return literal(P,false) end)
:prefix("(",function(p)
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

    return darkroom.ast.new(ar):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)
:prefix("[",function(p)
    p:expect("[")
    local expr = p:luaexpr()
    p:expect("]")
    return darkroom.ast.new({kind="escape",expr = expr}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)
:prefix("if",function(p)
    p:expect("if")
    local condexpr = p:expr()
    p:expect("then")
    local a = p:letexpr()
    p:expect("else")
    local b = p:letexpr()
    p:expect("end")
    return darkroom.ast.new({kind="select",cond=condexpr,a=a,b=b}):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
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

    return darkroom.ast.new(switchexpr):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
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

    local expr = p:letexpr()
    
    p:expect("end")

    local newnode = {kind="mapreduce",expr=expr,reduceop=reduceop}
    for k,v in ipairs(vars) do newnode["varname"..k]=v[1];newnode["varlow"..k]=v[2];newnode["varhigh"..k]=v[3]; end
    return darkroom.ast.new(newnode):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  end)

darkroom.lang.letexpr = function(p)
  local letast = {kind="let"}
  local exprcnt = 0
  
  local seen = {}

  if p:lookahead().type=="=" then
    repeat
      local name = p:expect(p.name).value
      
      if seen[name]~=nil then
        p:error("name '"..name.."' is a duplicate, which isn't allowed")
      end
      seen[name] = 1
      
      p:expect("=")
      
      local expr = p:expr()
      
      p:nextif(";") -- accept a semicolon at the end if the user added one
      
      exprcnt = exprcnt+1    
      letast["exprname"..exprcnt]=name
      letast["expr"..exprcnt]=expr
    until p:nextif("in")
    letast.res = p:expr()
    return darkroom.ast.new(letast):setLinenumber(p:cur().linenumber):setOffset(p:cur().offset):setFilename(p:cur().filename)
  else
    return p:expr()  -- no lets, so just passthrough
  end
end

-----------------------------------------------------------
darkroom.lang.imageFunction = function(p)

    p:expect("im")

    local ident = {}

    -- functions can be anonymous
    if p:matches(p.name) then
      table.insert(ident,p:expect(p.name).value)
      p:ref(ident[1])
  
      -- parse a chain: lol.wtf.bbq    
      -- if people want to use the [] index operator, force them to
      -- use an explicit escape
      while p:nextif(".") do
        table.insert(ident,p:expect(p.name).value)
      end
    end

    local xvar = nil
    local yvar = nil
    local zvar = nil


    p:expect("(")
    xvar = p:expect(p.name).value
    p:expect(",")
    yvar = p:expect(p.name).value
    p:expect(")")

    local rvalue = p:letexpr()

    p:expect("end")

    local chosenName
    if #ident < 1 then
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

function darkroom.compileTimeProcess(imfunc, envfn)

  -- first, check that the users AST isn't totally messed up
  if darkroom.verbose then 
    print("start process, node count:",imfunc.rvalue:S("*"):count(),imfunc.rvalue:name()) 
  end


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
      
      local i,vars,low,high,id = 1,{},{},{},{}
      while inp["varname"..i] do 
        vars[inp["varname"..i]]=1;
        low[inp["varname"..i]]=inp["varlow"..i];
        high[inp["varname"..i]]=inp["varhigh"..i];
        newNode["varid"..i] = {}
        id[inp["varname"..i]] = newNode["varid"..i]

        i=i+1 
      end
      
      newNode.expr = inp.expr:S("var"):process(
        function(fin)
          if vars[fin.name]~=nil then
            return darkroom.ast.new({kind="mapreducevar", id = id[fin.name], variable=fin.name, low=low[fin.name], high=high[fin.name]}):copyMetadataFrom(fin)
          end
          return fin
        end)
      newNode = darkroom.ast.new(newNode):copyMetadataFrom(inp)
      
      return newNode
    end)
  
  -- desugar let statements

  local function removeLet( expr, namemap )
    return expr:S("var"):process(
      function(n)
        if namemap[n.name]~=nil then
          if n:arraySize("arg")>0 then
            darkroom.error("You can't index into a named expression in a let statement",n:linenumber(),n:offset())
          end
          return namemap[n.name]
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
        namemap[n["exprname"..cnt]] = darkroom.ast.new({kind="letvar",variable = n["exprname"..cnt]}):copyMetadataFrom(n)
        cnt = cnt + 1
      end

      newNode.res = removeLet( n.res, namemap )
      return darkroom.ast.new(newNode):copyMetadataFrom(n)
    end)

  if darkroom.verbose then
    print("Size preresolve:",rvalue:S("*"):count(),rvalue:name())
  end

  rvalue = rvalue:S(
    function(n)  return n.kind=="index" or n.kind=="apply" or  n.kind=="escape" or n.kind=="var" or n.kind=="fieldselect" end):process(
    function(inp)
      if inp.kind=="var" then
        if inp.name==imfunc.xvar then  return darkroom.ast.new({kind="position",coord="x"}):copyMetadataFrom(inp)
        elseif inp.name==imfunc.yvar then  return darkroom.ast.new({kind="position",coord="y"}):copyMetadataFrom(inp) 
        elseif darkroom.ast.isAST(env[inp.name]) then
          return env[inp.name]
        elseif env[inp.name]~=nil then
          return darkroom.ast.new({kind="value", value=env[inp.name]}):copyMetadataFrom(inp)
        else
          darkroom.error("Could not resolve identifier: "..inp.name, inp:linenumber(), inp:offset())
        end
      elseif inp.kind=="index" then
        if inp.expr.kind=="tap" and inp.expr.count~=nil then -- count~=nil indicates LUT
          local n = inp.expr:shallowcopy()
          n.kind="tapLUTLookup"
          n.index = inp.index
          n.type = darkroom.type.arrayOver(inp.expr.type)
          return darkroom.ast.new(n):copyMetadataFrom(inp)
        end
      elseif inp.kind=="fieldselect" then
        if darkroom.ast.isAST(inp.expr.value[inp.field]) then
          return inp.expr.value[inp.field]
        elseif inp.expr.value[inp.field]~=nil then
          return darkroom.ast.new({kind="value", value=inp.expr.value[inp.field]}):copyMetadataFrom(inp)
        else
          darkroom.error("Field is nil: "..inp.field, inp:linenumber(), inp:offset())
        end
      elseif inp.kind=="escape" then
        return darkroom.evalEscape(inp.expr(inp:localEnvironment(rvalue,env)),inp)
      elseif inp.kind=="apply" then
        if inp.expr.kind=="type" then -- a typecast
          if inp:arraySize("arg") ~= 1 then
            darkroom.error("Typecasts only take one argument",inp:linenumber(), inp:offset())
          end
          return darkroom.ast.new({kind="cast",expr=inp.arg1,type=inp.expr.type}):copyMetadataFrom(inp)
        elseif type(inp.expr.value)=="function" then
          local args = {}
          inp:map("arg",function(n) table.insert(args,n) end)
          assert(#args>0)
          
          local macroResult = inp.expr.value(inp, unpack(args))
          
          if darkroom.ast.isAST(macroResult)==false then
            darkroom.error("Macro returned something other than an AST")
          end
          
          return macroResult
        elseif inp.expr.kind=="tap" then
          assert(false)
        elseif darkroom.ast.isAST(inp.expr) then
          -- turn it into a transform
          local newnode = inp:shallowcopy()
          newnode.kind="transform"

          if inp:arraySize("arg") < 2 then
            darkroom.error("At least two arguments must be specified when transforming.",
                        inp:linenumber(),inp:offset())
          end

          return darkroom.ast.new(newnode):copyMetadataFrom(inp)
        else
          darkroom.error("Could not resolve identifier to a valid application",inp.expr:linenumber(), inp:offset())
        end
      end
    end)

  if darkroom.verbose then
    print("end process, node count:",rvalue:S("*"):count(),rvalue:name())
  end

  return rvalue
end
