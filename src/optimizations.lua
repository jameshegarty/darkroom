orion.optimize={}

-- I don't trust these optimizations, so make sure we print out exactly what we're doing
orion.optimize.verbose = true

-- lua doesn't iterate over keys in a consistant order
orion.optimize._keyOrderCache = {}
function orion.optimize.keyOrder(ast)
  assert(type(ast.kind)=="string")

  local mt = getmetatable(ast)
  if orion.optimize._keyOrderCache[mt]==nil then
    orion.optimize._keyOrderCache[mt] = setmetatable({}, {__mode="k"})
  end

  local aCnt = 0
  for k,v in pairs(ast) do
    aCnt = aCnt+1
  end

  if orion.optimize._keyOrderCache[mt][ast.kind]==nil then
    orion.optimize._keyOrderCache[mt][ast.kind]={}
  end

  if orion.optimize._keyOrderCache[mt][ast.kind][aCnt] == nil then
    orion.optimize._keyOrderCache[mt][ast.kind][aCnt] = {}

    for k,_ in pairs(ast) do
      table.insert(orion.optimize._keyOrderCache[mt][ast.kind][aCnt],k)
    end
  end

  return orion.optimize._keyOrderCache[mt][ast.kind][aCnt]
end

function orion.optimize.CSEHash(ast)

  local hash = ""
  
  local keyOrder = orion.optimize.keyOrder(ast)

  for _,k in ipairs(keyOrder) do

    if ast[k]==nil then
      print(ast.kind,k)
      for k,v in pairs(ast) do print(k,v) end
      print("ORDER")
      for k,v in ipairs(keyOrder) do print(k,v) end
      assert(false)
    end

    hash = hash..tostring(ast[k])
  end

  return hash
end

function orion.optimize.CSE(inast, hashRepo)
  assert(type(hashRepo)=="table")

  if orion.verbose or orion.printstage then 
    print("run CSE") 
    print(debug.traceback())
  end

  local outast = inast:S("*"):process(function(ast)
    local hash = orion.optimize.CSEHash(ast)
    --print("hash",hash)

    if hashRepo[hash]~=nil then
      -- potentially we have a match, make sure our hash isn't messed up
      assert(ast:equals(hashRepo[hash]))
      --print("Matchfound")
      return hashRepo[hash]
    end

    -- no CSE found. save this one.
    hashRepo[hash] = ast

    return ast
  end)

  return outast
end

function orion.optimize.optimizeMath(ast)

  -- remove identity ops. ie x+0, x*1
  ast = ast:S("binop"):process(function(ast)
    local lhs = orion.optimize.constantFoldCasts(ast.lhs)
    local rhs = orion.optimize.constantFoldCasts(ast.rhs)

    if ast.op=="+" and lhs.kind=="value" and orion.optimize.isZero(lhs.value) then
      return ast.rhs
    elseif ast.op=="+" and rhs.kind=="value" and orion.optimize.isZero(rhs.value) then
      return ast.lhs
    end

    return ast end) 

  -- turn mult/div by power of two into a shift
--  ast = ast:S("binop"):process(function(ast)
--    if ast.op=="/" and ast.rhs.kind=="value" and ast.rhs.value and false
--  end)

  -- take (a+b)/3 -> a/3+b/3
  -- this can be good for conv engine (pushes more stuff in map operator)

  ast = ast:S("binop"):process(function(ast)
    if ast.op=="/" and 
       ast.rhs.kind=="value" and 
       ast.lhs.kind=="binop" and 
       ast.lhs.op=="+" then
      print("DISTRIBUTE")
      local lhsdivop = orion.ast.binop("/",ast.lhs.lhs,ast.rhs)
      local rhsdivop = orion.ast.binop("/",ast.lhs.rhs,ast.rhs)
      local resast=orion.ast.binop("+",lhsdivop,rhsdivop)
      return orion._toTypedAST(resast)
    end
    return ast
  end)

  return ast	 
end

function orion.optimize.performOp(op, lhs, rhs)
  if op=="+" then
    return lhs+rhs
  elseif op=="/" then
    return lhs/rhs
  elseif op=="*" then
    return lhs*rhs
  elseif op=="==" then
    return lhs==rhs
  elseif op=="-" then
    return lhs - rhs
  elseif op=="<<" then
    return lhs * math.pow(2,rhs)
  elseif op==">>" then
    return lhs / math.pow(2,rhs)
  end

  print(op)

  assert(false)
end

function orion.optimize.constantFold(ast)

  ast = ast:S("binop"):process(function(ast)
    if ast.lhs.kind=="value" and ast.rhs.kind=="value" then
      local outval = orion.optimize.performOp(ast.op, ast.lhs.value, ast.rhs.value)
      if orion.optimize.verbose then
        print("Constant fold: "..ast.lhs.value.." "..ast.op.." "..ast.rhs.value,outval)
      end

--      return orion.ast.value(outval, orion.type.valueToType(outval))
      local res = {kind="value", value=outval, type=ast.type}
      return orion.internalIR.new(res):copyMetadataFrom(ast)
    end

    return ast
  end)

  return ast
end

function orion.optimize.constantFoldCasts(ast)
  -- do casts of constants
  ast = ast:S("cast"):process(function(ast)
    if ast.expr.kind=="value" then
      if orion.type.isArray(ast.type) and 
         orion.type.isNumber(ast.type.over) and
	 orion.type.isArray(ast.expr.type)==false and
	 orion.type.isNumber(ast.expr.type) then
	 
	 local newval = {}
	 for i=1,orion.type.arrayLength(ast.type) do newval[i] = ast.expr.value end

	 local oast = orion.ast.value(newval, ast.type)
	 oast.name = ast.name
	 return oast
      end
    end

    return ast
  end)

  return ast
end

function orion.optimize.isZero(val)
  if type(val)=="number" and val==0 then 
    return true 
  end

  if type(val)=="number" and val~=0 then 
    return false
  end

  if type(val)=="table" then
    print("IZ")
    print(to_string(val))
    for k,v in ipairs(val) do if v~=0 then return false end end
    print("VEC IS ZERO")
    return true
  end

  assert(false)
end

-- ast should be a typed ast
function orion.optimize.optimize(ast)
  assert(orion.internalIR.isInternalIR(ast))

  if orion.printstage then
    print("Optimize")
  end

  ast:check()

  -- remove noop casts
  ast = ast:S("cast"):process(function(ast) if ast.type==ast.expr.type then return ast.expr else return ast end end)

  -- we don't support non-int32 constants, but we can do the cast at compile time...
  ast = ast:S("cast"):process(
    function(ast) 
      if ast.expr.kind=="value" then 
        local n = ast.expr:shallowcopy()
        n.type = ast.type

        local vi, vf = math.modf(ast.expr.value)

        -- only do the optimization if the value isn't modified by it
        if (vf==0 and n.type==orion.type.uint(8) and ast.expr.value >=0 and ast.expr.value <256) or
          (vf==0 and n.type==orion.type.uint(16) and ast.expr.value >=0 and ast.expr.value < math.pow(2,16)) or
          (vf==0 and n.type==orion.type.uint(32) and ast.expr.value >=0 and ast.expr.value < math.pow(2,32)) then
          return orion.internalIR.new(n):copyMetadataFrom(ast.expr)
        end
      end
    end)

  if orion.fastmath then

    ast = orion.optimize.constantFold(ast)

    -- getting rid of unnecessary mults/ divides is important for conv engine
    -- prob doesn't help on cpu though
    ast = ast:S("binop"):process(
      function(ast) 
        if ast.op=="/" and ast.rhs.kind=="value" and ast.lhs.kind~="value" and
          (orion.type.isUint(ast.lhs.type) or orion.type.isInt(ast.lhs.type)) and
          (orion.type.isUint(ast.rhs.type) or orion.type.isInt(ast.rhs.type)) and
          ast.rhs.value > 0
        then
          
          -- this optimization isn't safe, it won't preserve values
          -- known issue: rounding on negative numbers
          -- (-109) >> 1 = -55
          -- (-109) / 2 = -54 

          local pow2 = math.floor(math.log(ast.rhs.value)/math.log(2))
          
          if ast.rhs.value==math.pow(2,pow2) then
            if orion.optimize.verbose then
              print("divok",ast.translate1_lhs, ast.translate2_lhs,ast.translate1_rhs, ast.translate2_rhs)
            end

            -- we can turn this into a right shift
            local nn = ast:shallowcopy()
            local nv = {kind="value",value=pow2,type=orion.type.uint(32)}
            nv = orion.internalIR.new(nv):copyMetadataFrom(ast.rhs)
            nn.rhs = nv
            nn.translate1_rhs = 0
            nn.translate2_rhs = 0
            nn.scale1_rhs = 1
            nn.scale2_rhs = 1


            nn.op=">>"
            local res = orion.internalIR.new(nn):copyMetadataFrom(ast)

            if false then -- for debugging
              local cond = {kind="binop",op="==",lhs=ast,rhs=res,type=orion.type.bool(),
                            translate1_lhs=0,translate2_lhs=0,scale1_lhs=1,scale2_lhs=1,
                            translate1_rhs=0,translate2_rhs=0,scale1_rhs=1,scale2_rhs=1}
              cond = orion.internalIR.new(cond):copyMetadataFrom(ast)
              local asrt = {kind="assert",expr=res,printval=ast.lhs,cond=cond,type=res.type,
                            translate1_expr=0,translate2_expr=0,scale1_expr=1,scale2_expr=1,
                            translate1_cond=0,translate2_cond=0,scale1_cond=1,scale2_cond=1,
                            translate1_printval=0,translate2_printval=0,scale1_printval=1,scale2_printval=1}
              
              orion.internalIR.new(asrt):copyMetadataFrom(ast)

              return asrt
            end

            return res
          else
            if orion.optimize.verbose then
              print("divfail",ast.rhs.value,ast.translate1_rhs,ast.translate2_rhs,ast:filename(),"line",ast:linenumber())
              print(ast.translate1_lhs, ast.translate2_lhs)
            end
          end
        elseif ast.op=="*" and ast.rhs.kind=="value" and ast.rhs.value==1 then
          return ast.lhs
        elseif ast.op=="*" and ast.lhs.kind=="value" and ast.lhs.value==1 then
          return ast.rhs
        elseif ast.op=="*" and ast.rhs.kind=="value" and ast.rhs.value==0 then
          return ast.rhs
        elseif ast.op=="*" and ast.lhs.kind=="value" and ast.lhs.value==0 then
          return ast.lhs
        elseif ast.op=="*" and 
          ((ast.rhs.kind=="value" and ast.lhs.kind~="value" and ast.rhs.value>1) or 
           (ast.lhs.kind=="value" and ast.rhs.kind~="value" and ast.lhs.value>1)) and
          (orion.type.isUint(ast.lhs.type) or orion.type.isInt(ast.lhs.type)) and
          (orion.type.isUint(ast.rhs.type) or orion.type.isInt(ast.rhs.type)) then

          --

          local valueOp = ast.rhs
          local otherOp = ast.lhs
          local valueT1 = ast.translate1_rhs
          local valueT2 = ast.translate2_rhs
          if ast.lhs.kind=="value" then
            valueOp = ast.lhs
            otherOp = ast.rhs
            valueT1 = ast.translate1_lhs
            valueT2 = ast.translate2_lhs
          end

          local pow2 = math.floor(math.log(valueOp.value)/math.log(2))

          assert(pow2>0)

          if valueOp.value==math.pow(2,pow2) then
            if orion.optimize.verbose then
              print("mulok",valueOp.value,pow2,math.pow(2,pow2),valueOp.value==math.pow(2,pow2))
            end

            -- we can turn this into a left shift
            local nn = ast:shallowcopy()
            local nv = {kind="value",value=pow2,type=orion.type.uint(32)}
            nv = orion.internalIR.new(nv):copyMetadataFrom(ast.rhs)
            nn.rhs = nv
            nn.lhs = otherOp
            nn.op="<<"

            -- swap the translates
            if ast.lhs.kind=="value" then
              nn.translate1_lhs = nn.translate1_rhs
              nn.translate2_lhs = nn.translate2_rhs
              nn.scale1_lhs = nn.scale1_rhs
              nn.scale2_lhs = nn.scale2_rhs
            end

            nn.translate1_rhs = 0
            nn.translate2_rhs = 0
            nn.scale1_rhs = 1
            nn.scale2_rhs = 1

            local res = orion.internalIR.new(nn):copyMetadataFrom(ast)

            if false then
              local cond = {kind="binop",op="==",lhs=ast,rhs=res,type=orion.type.bool(),
                            translate1_lhs=0,translate2_lhs=0,scale1_lhs=1,scale2_lhs=1,
                            translate1_rhs=0,translate2_rhs=0,scale1_rhs=1,scale2_rhs=1}
              cond = orion.internalIR.new(cond):copyMetadataFrom(ast)
              local asrt = {kind="assert",expr=res,printval=otherOp,cond=cond,type=res.type,
                            translate1_expr=0,translate2_expr=0,scale1_expr=1,scale2_expr=1,
                            translate1_cond=0,translate2_cond=0,scale1_cond=1,scale2_cond=1,
                            translate1_printval=0,translate2_printval=0,scale1_printval=1,scale2_printval=1}
              
              orion.internalIR.new(asrt):copyMetadataFrom(ast)
              return asrt
            end

            return res
          else
            if orion.optimize.verbose then
              print("mulfail",valueOp.value,valueT1,valueT2,ast:filename(),"line",ast:linenumber())
            end
          end
        elseif ast.op=="*" and  (ast.rhs.kind=="value" or ast.lhs.kind=="value") then
          if orion.optimize.verbose then
            print("fastmath FAIL")
          end
        end
      end)
  end
  
  ast:check()

  local cseRepo={}
  ast = orion.optimize.CSE(ast, cseRepo)

  --ast = orion.optimize.constantFold(ast)

  --ast = orion.optimize.optimizeMath(ast)


  ast:check()

  if orion.verbose then
    print("Optimizations Done --------------------------")
    ast:printpretty()
  end

  return ast
end

-- makes sure that children of index nodes aren't holding around
-- array nodes unnecessarily
--
-- theoretically, we could have done this before typechecking,
-- which would have maybe been easier. But I wasn't sure that our
-- type rules would behave 100% as expected, so I think this is safer...
function orion.optimize.cleanupVectors(ast)
  assert(orion.typedAST.isTypedAST(ast))
  ast:check()


  ast = ast:S("index"):process(
    function(n)
      local index = n.index
      local q = n.expr:S(function(n) return orion.type.isArray(n.type) end)

      return q:process(
        function(n)
          local out = n:shallowcopy()
          
          if n.kind=="binop" or
            n.kind=="unary" or
            n.kind=="select" or
            n.kind=="vectorSelect" or
            n.kind=="gather" or
            n.kind=="reduce" then
            -- these guys act elementwise so we can treat them all the same
            out.type = orion.type.arrayOver(out.type)
          elseif n.kind=="array" then
            -- by defn its inputs are scalar
return  n["expr"..(index+1)]
          elseif n.kind=="cast" or
            n.kind=="transformBaked" or
            n.kind=="cropBaked" then
            -- these guys just happen to share the same format
            out.type = orion.type.arrayOver(out.type)
          elseif n.kind=="input" then
            out.channel = index -- indicates to only read one channel
            out.type = orion.type.arrayOver(n.type)
          else
            print(n.kind)
            assert(false)
          end
          
          if orion.type.isArray(out.type) then
            print(n.kind)
            assert(false)
          end
          
          local res = orion.typedAST.new(out):copyMetadataFrom(n)
          return res
        end)
    end)

  return ast
end

function orion.optimize.toSOA(ast)
  -- we do this on the typed ast b/c the internal IR is confusing
  assert(orion.typedAST.isTypedAST(ast))
  ast:check()

  if orion.printstage then
    print("toSOA",ast:S("index"):count())
  end

  if orion.verbose then print("convert arrays to SOA") end

  -- find all potential 'sinks' for arrays, and convert them
  -- into an equivilant form that doesn't use arrays

  local function proc(ast)
    if orion.type.isArray(ast.type) then
      local len = orion.type.arrayLength(ast.type)

      local restable = {}
      for i=1,len do
        local expr = {kind="index", index=i-1, expr = ast, type = orion.type.arrayOver(ast.type)}
        table.insert(restable, orion.typedAST.new(expr):copyMetadataFrom(ast))
      end
      
      return restable
    end

    return ast
  end

  if ast.kind=="outputs" then
    -- need to special case this
    local newMultiout = ast:shallowcopy()

    for i=1,ast:arraySize("expr") do
      newMultiout["expr"..i] = proc(ast["expr"..i])
    end

    ast = orion.typedAST.new(newMultiout):copyMetadataFrom(ast)
  else
    ast = proc(ast)
  end

  -- These are binary operators that consume array(s) but produce a scalar value
  ast = ast:S("binop"):process(
    function(n)
      if n.op=="dot" then
        local newnode = {kind="multibinop", type = n.type, op=n.op}
        local len = orion.type.arrayLength(n.lhs.type)
        for i=1,len do
          local lhs = {kind="index", index=i-1, expr = n.lhs, type = orion.type.arrayOver(n.lhs.type)}
          newnode["lhs"..i] = orion.typedAST.new(lhs):copyMetadataFrom(n)

          local rhs = {kind="index", index=i-1, expr = n.rhs, type = orion.type.arrayOver(n.rhs.type)}
          newnode["rhs"..i] = orion.typedAST.new(rhs):copyMetadataFrom(n)
        end
        return orion.typedAST.new(newnode):copyMetadataFrom(n)
      end
    end)

  -- these are unary operators that consume an array put produce a scalar value
  ast = ast:S("unary"):process(
    function(n)
      if n.op=="arrayAnd" then
        local newnode = {kind="multiunary", type = n.type, op=n.op}
        local len = orion.type.arrayLength(n.expr.type)
        for i=1,len do
          local expr = {kind="index", index=i-1, expr = n.expr, type = orion.type.arrayOver(n.expr.type)}
          newnode["expr"..i] = orion.typedAST.new(expr):copyMetadataFrom(n)
        end
        return orion.typedAST.new(newnode):copyMetadataFrom(n)
        
      end
    end)

  ast:check()

  -- at this point, everything should be set up so that it's scalar
  -- run a pass to cleanup unnecessary array nodes

  if orion.printstage then print("cleanup") end

  ast = orion.optimize.cleanupVectors(ast)
  if orion.printstage then print("cleanup done, nodecount:",ast:S("*"):count()) end

  -- check that it worked
  ast:S("*"):process(function(n)
                       if n.kind~="toAOS" and n.kind~="outputs" then
                         if orion.type.isArray(n.type) then
                           n:printpretty()
                           assert(false)
                         end
                       end
                     end)

  if orion.verbose or orion.printstage then
    print("Conversion to SOA done -----------")
  end

  if orion.verbose then 
    ast:printpretty()
  end

  return ast
end