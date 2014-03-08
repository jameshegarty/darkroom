typedastMT={__index=astFunctions}

orion.typedAST={}
function orion.typedAST.isTypedAST(ast) return getmetatable(ast)==typedastMT end

function orion._toTypedAST(ast)
  assert(getmetatable(ast)==astMT)

  if ast.kind=="value" then
    if ast.type==nil then ast.type=orion.type.valueToType(ast.value) end
    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="unary" then
    -- it's possible this node was already typechecked (or macro provided type)
    if ast.type~=nil then
      return ast
    end

    ast.expr = orion._toTypedAST(ast.expr)

    if ast.op=="-" and orion.type.astIsUint(ast.expr) then
      orion.warning("You're negating a uint, this is probably not what you want to do!", ast.line, ast.char)
    end

    ast.type = ast.expr.type

    assert(ast.type~=nil)
    return ast    
  elseif ast.kind=="binop" then
    -- it's possible this node was already typechecked (or macro provided type)
    if ast.type~=nil then
      return ast
    end

    local lhs = orion._toTypedAST(ast.lhs)
    local rhs = orion._toTypedAST(ast.rhs)

    if lhs.type==nil then print("ST"..to_string(lhs)) end

    assert(lhs.type~=nil)
    assert(rhs.type~=nil)

    local thistype, lhscast, rhscast = orion.type.meet(lhs.type,rhs.type,ast.op)

    assert(thistype ~= nil)
    assert(lhscast ~= nil)
    assert(rhscast ~= nil)

    lhs = orion.ast.cast(lhs, lhscast)
    rhs = orion.ast.cast(rhs, rhscast)

    ast.type = thistype
    ast.lhs = lhs
    ast.rhs = rhs

    assert(ast.type~=nil)
    return ast

  elseif ast.kind=="assignment" then
    orion.error("Internal error, typechecking shouldn't be applied to assignment nodes",ast.line,ast.char)
  elseif ast.kind=="func" then
    orion.error("Internal error, typechecking shouldn't be applied to func nodes",ast.line,ast.char)
  elseif ast.kind=="special" then
    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="position" then
    -- if position is still in the tree at this point, it means it's being used in an expression somewhere
    -- choose a reasonable type...
    ast.type=orion.type.uint(16)
    return ast
  elseif ast.kind=="select" then
    local cond = orion._toTypedAST(ast.cond)
    local a = orion._toTypedAST(ast.a)
    local b = orion._toTypedAST(ast.b)

    if orion.type.astIsBool(cond)==false then
      orion.error("Error, condition of select must be boolean",ast.line,ast.char)
      return nil
    end

    local thistype, lhstype, rhstype =  orion.type.meet(a.type,b.type, "select")

    assert(thistype ~= nil)
    assert(lhstype ~= nil)
    assert(rhstype ~= nil)

    a = orion.ast.cast(a, lhstype)
    b = orion.ast.cast(b, rhstype)
    ast.type = thistype
    ast.a = a
    ast.b = b

    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="index" then
    local expr = orion._toTypedAST(ast.expr)

    if orion.type.astIsArray(expr)==false then
      orion.error("Error, you can only index into an array type!",ast.line,ast.char)
      os.exit()
    end

    ast.expr = expr
    ast.type = orion.type.astArrayOver(expr)
    
    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="translate" then
    ast.expr = orion._toTypedAST(ast.expr)

    -- this just gets the value of the thing we're translating
    ast.type = ast.expr.type

    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="array" then
    assert(#ast.vector>1)

    for i=1,#ast.vector do
      ast.vector[i] = orion._toTypedAST(ast.vector[i])
    end

    local mtype = ast.vector[1].type
    local atype, btype

    if orion.type.isArray(mtype) then
      orion.error("You can't have nested arrays (index 0 of vector)")
    end

    for i=2,#ast.vector do
      if orion.type.isArray(ast.vector[i].type) then
        orion.error("You can't have nested arrays (index "..(i-1).." of vector)")
      end

      mtype, atype, btype = orion.type.meet(mtype, ast.vector[i].type)

      if mtype==nil then
        orion.error("meet error")      
      end

      -- our type system should have guaranteed this...
      assert(mtype==atype)
      assert(mtype==btype)
    end

    -- now we've figured out what the type of the array should be

    -- may need to insert some casts
    for i=1,#ast.vector do
      -- meet should have failed if this isn't possible...
      assert(orion.type.checkImplicitCast(ast.vector[i].type,mtype))

      ast.vector[i] = orion.ast.cast(ast.vector[i], mtype)
    end

    ast.type = orion.type.array(mtype, #ast.vector)

    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="cast" then
    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="asserttype" then
    -- we can do this entirely at compile time
    ast.expr = orion._toTypedAST(ast.expr)

    assert(orion.type.isType(ast.expr.type))

    if ast.checktype=="float" then
      if orion.type.astIsFloat(ast.expr)==false then
        orion.error("Orion AssertType failure, expr is not a float", ast.line, ast.char)
      end
    else
      orion.error("Asserttype not implemented for checktype "..to_string(ast.checktype))
    end

    assert(ast.expr.type~=nil)
    return ast.expr
  elseif ast.kind=="assert" then
    ast.condexpr = orion._toTypedAST(ast.condexpr)
    ast.expr = orion._toTypedAST(ast.expr)
    ast.toprint = orion._toTypedAST(ast.toprint)

    if orion.type.astIsBool(ast.condexpr)==false then
      orion.error("Error, condition of assert must be boolean",ast.line,ast.char)
      return nil
    end

    ast.type = ast.expr.type

    assert(orion.type.isType(ast.type))
    assert(ast.type~=nil)
    return ast
  elseif ast.kind=="call" then
    for k,v in pairs(ast.args) do
      ast.args[k] = orion._toTypedAST(v)
    end

    -- should have set this manually when you made the call - we can't figure this out
    assert(orion.type.isType(ast.type))

    return ast
  end

  orion.error("Internal error, typechecking for "..ast.kind.." isn't implemented!",ast.line,ast.char)
  return nil

end
