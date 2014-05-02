typedASTFunctions={}
setmetatable(typedASTFunctions,{__index=IRFunctions})
typedASTMT={__index=typedASTFunctions,
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify typed AST node")
                  end}

orion.typedAST = {}



function orion.typedAST.checkOffsetExpr(expr, coord)
  assert(type(coord)=="string")

  if expr.kind=="binop" then
    if expr.op=="+" or expr.op=="-" or expr.op=="*" or expr.op=="/" then
      return orion.typedAST.checkOffsetExpr(expr.lhs,coord) and orion.typedAST.checkOffsetExpr(expr.rhs,coord)
    end

    orion.error("binop '"..expr.op.."' is not supported in an offset expr")
  elseif expr.kind=="value" then
    if type(expr.value)=="number" then
      return true
    else
      orion.error("type "..type(expr.value).." is not supported in offset expr")
    end
  elseif expr.kind=="position" then
    if expr.coord==coord then
      return true
    else
      orion.error("you can't use coord "..expr.coord.." in expression for coord "..coord)
    end
  elseif expr.kind=="cast" then
    return orion.typedAST.checkOffsetExpr(expr.expr,coord)
  else
    orion.error(expr.kind.." is not supported in offset expr")    
  end

  return false
end

function orion.typedAST.convertOffset(ast,coord)

  local translate = 0
  local scale = 1
 
  -- just ignore casts for now
  ast = ast:S("cast"):process(function(a) return a.expr end)

  -- try to coerce this expr into a translate/scale
  if ast.kind=="position" then

  elseif ast.kind=="binop" then
    if ast.lhs.kind=="position" then
      assert(ast.rhs.kind=="value")
	
      if ast.op=="+" then
        translate = ast.rhs.value
      elseif ast.op=="-" then
        translate = -ast.rhs.value
      elseif ast.op=="*" then
        scale = ast.rhs.value
      else
        assert(false)
      end
    elseif ast.rhs.kind=="position" then
        assert(ast.lhs.kind=="value")

        if ast.op=="+" then
          translate = ast.lhs.value
        elseif ast.op=="-" then
          translate = -ast.lhs.value
        else
          assert(false)
        end
    else
      print(ast.lhs.kind,ast.rhs.kind)
      assert(false)
    end
  else
    assert(false)
  end

  return translate,scale
end

-- take the ast input that's an offset expr for coord 'coord'
-- and convert it into a translate,scale. returns nil if there was an error.
function orion.typedAST.synthOffset(ast,coord)

  -- first check that there isn't anything in the expression that's definitely not allowed...
  local a = orion.typedAST.checkOffsetExpr(ast,coord)
  if a==false then return nil end

  -- now distribute the math ops. This should theoretically reduce the 
  -- expression to a standard form

  ast = orion.optimize.optimizeMath(ast)

  -- now try to match this to a standard form
  return orion.typedAST.convertOffset(ast,coord)
end


-- returns the stencil with (0,0,0) at the origin
-- if input isn't null, only calculate stencil for this input (a kernelGraph node)
function typedASTFunctions:stencil(input)

  if self.kind=="binop" then
    return self.lhs:stencil(input):unionWith(self.rhs:stencil(input))
  elseif self.kind=="multibinop" then
    local res = Stencil.new()

    for i=1,self:arraySize("lhs") do
      res = res:unionWith(self["lhs"..i]:stencil(input):translate(self["translate1_lhs"..i], self["translate2_lhs"..i],0))
    end

    for i=1,self:arraySize("rhs") do
      res = res:unionWith(self["rhs"..i]:stencil(input):translate(self["translate1_rhs"..i], self["translate2_rhs"..i],0))
    end

    return res
  elseif self.kind=="multiunary" then
    local res = Stencil.new()

    for i=1,self:arraySize("expr") do
      res = res:unionWith(self["expr"..i]:stencil(input):translate(self["translate1_expr"..i], self["translate2_expr"..i],0))
    end

    return res
  elseif self.kind=="unary" then
    return self.expr:stencil(input):translate(self.translate1_expr,self.translate2_expr,0)
  elseif self.kind=="assert" then
    return self.cond:stencil(input):translate(self.translate1_cond,self.translate2_cond,0)
    :unionWith(self.expr:stencil(input):translate(self.translate1_expr,self.translate2_expr,0))
  elseif self.kind=="cast" then
    return self.expr:stencil(input):translate(self.translate1_expr,self.translate2_expr,0)
  elseif self.kind=="select" or self.kind=="vectorSelect" then
    return self.cond:stencil(input)
    :translate(self.translate1_cond,self.translate2_cond,0)
    :unionWith(self.a:stencil(input)
               :translate(self.translate1_a,self.translate2_a,0) 
               :unionWith(self.b:stencil(input)
                          :translate(self.translate1_b,self.translate2_b,0)))
  elseif self.kind=="position" or self.kind=="tap" or self.kind=="value" then
    return Stencil.new()
  elseif self.kind=="tapLUTLookup" then
    return self.index:stencil(input):translate(self.translate1_index,self.translate2_index,0)
  elseif self.kind=="input" then
    --if input~=nil then assert(false) end
    return Stencil.new():add(0,0,0)
  elseif self.kind=="load" then
    local s = Stencil.new()
    if input==nil or input==self.from then s = s:add(0,0,0) end
    return s
  elseif self.kind=="gather" then
    --if input~=nil then assert(false) end
    assert(self.input.kind=="load")
    assert(orion.scheduledIR.isScheduledIR(self.input.from))

    if input~=nil and self.input.from~=input then
      return Stencil.new()
    else
      local g = self.input:stencil(input)
      g = g:unionWith(self.hackBL:stencil(input):translate(self.translate1_hackBL, self.translate2_hackBL,0))
      g = g:unionWith(self.hackTR:stencil(input):translate(self.translate1_hackTR, self.translate2_hackTR,0))
      return g:unionWith(self.x:stencil(input)):unionWith(self.y:stencil(input))
    end
  elseif self.kind=="array" then
    local exprsize = self:arraySize("expr")

    local s = Stencil.new()
    s = s:unionWith(Stencil.new())
    for i=1,exprsize do
      s = s:unionWith(self["expr"..i]:stencil(input):translate(
                        self["translate1_expr"..i],
                        self["translate2_expr"..i],
                        0))
    end

    return s
  elseif self.kind=="reduce" then
    local s = Stencil.new()
    local i=1
    while self["expr"..i] do
      s = s:unionWith(self["expr"..i]:stencil(input):translate(self["translate1_expr"..i],self["translate2_expr"..i],0))
      i=i+1
    end
    return s
  elseif self.kind=="index" then
    return self.expr:stencil(input)
  elseif self.kind=="crop" then
    return self.expr:stencil(input)
  elseif self.kind=="transformBaked" then
    return self.expr:stencil(input):translate(self.translate1,self.translate2,0)
  end

  print(self.kind)
  assert(false)
end

function typedASTFunctions:irType()
  return "typedAST"
end

-- get this node's value at compile time.
-- if can't be determined, return nil
function typedASTFunctions:eval()
  if self.kind=="value" then
    return self.value
  elseif self.kind=="unary" then
    if self.op=="-" then return -(self.expr:eval()) else
      assert(false)
    end
  else
    print(self.kind)
    assert(false)
  end

  return nil
end


function orion.typedAST._toTypedAST(inast)

  local res = inast:visitEach(
    function(origast,inputs)
      assert(orion.ast.isAST(origast))
      local ast = origast:shallowcopy()

      if ast.kind=="value" then
        if ast.type==nil then ast.type=orion.type.valueToType(ast.value) end
        assert(ast.type~=nil)
      elseif ast.kind=="unary" then
        ast.expr = inputs["expr"][1]
        
        if ast.op=="-" then
          if orion.type.astIsUint(ast.expr) then
            orion.warning("You're negating a uint, this is probably not what you want to do!", origast:linenumber(), origast:offset(), origast:filename())
          end
          
          ast.type = ast.expr.type
        elseif ast.op=="floor" then
          ast.type = orion.type.float(32)
        elseif ast.op=="abs" then
          if ast.expr.type==orion.type.float(32) then
            ast.type = orion.type.float(32)
          elseif orion.type.isInt(ast.expr.type) or orion.type.isUint(ast.expr.type) then
            -- obv can't make it any bigger
            ast.type = ast.expr.type
          else
            ast.expr.type:print()
            assert(false)
          end
        elseif ast.op=="not" then
          if orion.type.isBool(ast.expr.type) then
            ast.type = ast.expr.type
          else
            orion.error("not only works on bools",origast:linenumber(), origast:offset())
            assert(false)
          end
        elseif ast.op=="sin" or ast.op=="cos" or ast.op=="exp" then
          if ast.expr.type==orion.type.float(32) then
            ast.type = orion.type.float(32)
          else
            assert(false)
          end
        elseif ast.op=="arrayAnd" then
          if orion.type.isArray(ast.expr.type) and orion.type.isBool(orion.type.arrayOver(ast.expr.type)) then
            ast.type = orion.type.bool()
          else
            orion.error("vectorAnd only works on arrays of bools",origast:linenumber(), origast:offset())
          end
        else
          print(ast.op)
          assert(false)
        end
        
      elseif ast.kind=="binop" then
        
        local lhs = inputs["lhs"][1]
        local rhs = inputs["rhs"][1]
        
        if lhs.type==nil then print("ST"..to_string(lhs)) end
        
        assert(lhs.type~=nil)
        assert(rhs.type~=nil)
        
        local thistype, lhscast, rhscast = orion.type.meet( lhs.type, rhs.type, ast.op, origast )
        
        if thistype==nil then
          orion.error("Type error, inputs to "..ast.op,origast:linenumber(), origast:offset(), origast:filename())
        end
        
        if lhs.type~=lhscast then lhs = orion.typedAST.new({kind="cast",expr=lhs,type=lhscast}):copyMetadataFrom(origast) end
        if rhs.type~=rhscast then rhs = orion.typedAST.new({kind="cast",expr=rhs,type=rhscast}):copyMetadataFrom(origast) end
        
        ast.type = thistype
        ast.lhs = lhs
        ast.rhs = rhs
        
      elseif ast.kind=="input" then
      elseif ast.kind=="position" then
        -- if position is still in the tree at this point, it means it's being used in an expression somewhere
        -- choose a reasonable type...
        ast.type=orion.type.int(32)
      elseif ast.kind=="select" or ast.kind=="vectorSelect" then
        local cond = inputs["cond"][1]
        local a = inputs["a"][1]
        local b = inputs["b"][1]

        if ast.kind=="vectorSelect" then
          if orion.type.arrayOver(cond.type)~=orion.type.bool() then
            orion.error("Error, condition of vectorSelect must be array of booleans. ",origast:linenumber(),origast:offset())
            return nil
          end

          if orion.type.isArray(cond.type)==false or
            orion.type.isArray(a.type)==false or
            orion.type.isArray(b.type)==false or
            orion.type.arrayLength(a.type)~=orion.type.arrayLength(b.type) or
            orion.type.arrayLength(cond.type)~=orion.type.arrayLength(a.type) then
            orion.error("Error, all arguments to vectorSelect must be arrays of the same length",origast:linenumber(),origast:offset())
            return nil            
          end
        else
          if cond.type ~= orion.type.bool() then
            orion.error("Error, condition of select must be scalar boolean. Use vectorSelect",origast:linenumber(),origast:offset(),origast:filename())
            return nil
          end

          if orion.type.isArray(a.type)~=orion.type.isArray(b.type) then
            orion.error("Error, if any results of select are arrays, all results must be arrays",origast:linenumber(),origast:offset())
            return nil
          end
          
          if orion.type.isArray(a.type) and
            orion.type.arrayLength(a.type)~=orion.type.arrayLength(b.type) then
            orion.error("Error, array arguments to select must be the same length",origast:linenumber(),origast:offset())
            return nil
          end
        end

        local thistype, lhscast, rhscast =  orion.type.meet(a.type,b.type, ast.kind, origast)

        if a.type~=lhscast then a = orion.typedAST.new({kind="cast",expr=a,type=lhscast}):copyMetadataFrom(origast) end
        if b.type~=rhscast then b = orion.typedAST.new({kind="cast",expr=b,type=rhscast}):copyMetadataFrom(origast) end
        
        ast.type = thistype
        ast.cond = cond
        ast.a = a
        ast.b = b
        
      elseif ast.kind=="index" then
        local expr = inputs["expr"][1]
        
        if orion.type.isArray(expr.type)==false then
          orion.error("Error, you can only index into an array type!",origast:linenumber(),origast:offset())
          os.exit()
        end
        
        ast.expr = expr

        if inputs["index"][1].kind~="value" then
          orion.error("index must be a value",origast:linenumber(), origast:offset())
        end

        if orion.type.isInt(inputs["index"][1].type)==false and
          orion.type.isUint(inputs["index"][1].type)==false then
          orion.error("index must be a integer",origast:linenumber(), origast:offset())
        end

        ast.index = inputs["index"][1].value
        ast.type = orion.type.astArrayOver(expr)
        
      elseif ast.kind=="transform" then
        ast.expr = inputs["expr"][1]
        
        -- this just gets the value of the thing we're translating
        ast.type = ast.expr.type
        
        local i=1
        while ast["arg"..i] do
          ast["arg"..i] = inputs["arg"..i][1] 
          i=i+1
        end
        
        -- now make the new transformBaked node out of this
        local newtrans = {kind="transformBaked",expr=ast.expr,type=ast.expr.type}
        
        local noTransform = true

        local i=1
        while ast["arg"..i] do
          -- if we got here we can assume it's valid
          local translate,scale=orion.typedAST.synthOffset( ast["arg"..i], orion.dimToCoord[i])
          assert(translate~=nil)
          newtrans["translate"..i]=translate
          newtrans["scale"..i]=scale

          if translate~=0 or scale~=1 then noTransform = false end
          i=i+1
        end
        
        -- at least 2 arguments must be specified. 
        -- the parser was supposed to guarantee this.
        assert(i>2)

        ast=newtrans

      elseif ast.kind=="array" then
        
        local cnt = 1
        while ast["expr"..cnt] do
          ast["expr"..cnt] = inputs["expr"..cnt][1]
          cnt = cnt + 1
        end
        
        local mtype = ast.expr1.type
        local atype, btype
        
        if orion.type.isArray(mtype) then
          orion.error("You can't have nested arrays (index 0 of vector)")
        end
        
        local cnt = 2
        while ast["expr"..cnt] do
          if orion.type.isArray(ast["expr"..cnt].type) then
            orion.error("You can't have nested arrays (index "..(i-1).." of vector)")
          end
          
          mtype, atype, btype = orion.type.meet( mtype, ast["expr"..cnt].type, "array", origast)
          
          if mtype==nil then
            orion.error("meet error")      
          end
          
          -- our type system should have guaranteed this...
          assert(mtype==atype)
          assert(mtype==btype)
          
          cnt = cnt + 1
        end
        
        -- now we've figured out what the type of the array should be
        
        -- may need to insert some casts
        local cnt = 1
        while ast["expr"..cnt] do
          -- meet should have failed if this isn't possible...
          local from = ast["expr"..cnt].type

          if from~=mtype then
            if orion.type.checkImplicitCast(from, mtype,origast)==false then
              orion.error("Error, can't implicitly cast "..from:str().." to "..mtype:str(), origast:linenumber(), origast:offset())
            end
            
            ast["expr"..cnt] = orion.typedAST.new({kind="cast",expr=ast["expr"..cnt], type=mtype}):copyMetadataFrom(ast["expr"..cnt])
          end

          cnt = cnt + 1
        end
        
        local arraySize = cnt - 1
        ast.type = orion.type.array(mtype, arraySize)
        

      elseif ast.kind=="cast" then

        if inputs["type"][1].kind~="type" then
          orion.error("Error, this should be a type", origast:linenumber(), origast:offset())
        end

        -- note: we don't eliminate these cast nodes, because there's a difference
        -- between calculating a value at a certain precision and then downsampling,
        -- and just calculating the value at the lower precision.
        ast.expr = inputs["expr"][1]
        -- extract the type from the type node
        ast.type = inputs["type"][1].type
        
        if orion.type.checkExplicitCast(ast.expr.type,ast.type,origast)==false then
          orion.error("Casting from "..ast.expr.type:str().." to "..ast.type:str().." isn't allowed!",origast:linenumber(),origast:offset())
        end
        

      elseif ast.kind=="assert" then

        ast.cond = inputs["cond"][1]
        ast.expr = inputs["expr"][1]
        ast.printval = inputs["printval"][1]

        if orion.type.astIsBool(ast.cond)==false then
          orion.error("Error, condition of assert must be boolean",ast:linenumber(),ast:offset())
          return nil
        end

        ast.type = ast.expr.type

      elseif ast.kind=="mapreducevar" then
        local low = inputs["low"][1]
        local high = inputs["high"][1]

        -- we don't need this anymore. Only needed this to calculate the type.
        ast.low=nil
        ast.high=nil
        
        ast.type = orion.type.meet(low.type,high.type,"mapreducevar", origast)
      elseif ast.kind=="tap" then
        -- taps should be tagged with type already
      elseif ast.kind=="tapLUTLookup" then
        ast.index = inputs["index"][1]

        -- tapLUTs should be tagged with type already
        assert(orion.type.isType(ast.type))

        if orion.type.isUint(ast.index.type)==false and
          orion.type.isInt(ast.index.type)==false then

          orion.error("Error, index into tapLUT must be integer",ast:linenumber(),ast:offset())
          return nil
        end
      elseif ast.kind=="crop" then
        ast.expr = inputs["expr"][1]
        ast.type = ast.expr.type
      elseif ast.kind=="reduce" then
        local i=1
        local typeSet = {}

        while ast["expr"..i] do
          ast["expr"..i] = inputs["expr"..i][1]
          table.insert(typeSet,ast["expr"..i].type)
          
          i=i+1
        end

        ast.type = orion.type.reduce( ast.op, typeSet)
      elseif ast.kind=="outputs" then
        -- doesn't matter, this is always the root and we never need to get its type
        ast.type = inputs.expr1[1].type

        local i=1
        while ast["expr"..i] do
          ast["expr"..i] = inputs["expr"..i][1]
          i=i+1
        end

      elseif ast.kind=="type" then
        -- ast.type is already a type, so don't have to do anything
        -- shouldn't matter, but need to return something
      elseif ast.kind=="gather" then
        ast.type = inputs.input[1].type
        ast.input = inputs.input[1]
        ast.x = inputs.x[1]
        ast.y = inputs.y[1]
        ast.hackBL = inputs.hackBL[1]
        ast.hackTR = inputs.hackTR[1]

        if orion.type.isInt(ast.x.type)==false then
          orion.error("Error, x argument to gather must be int but is "..ast.x.type:str(), origast:linenumber(), origast:offset())
        end

        if orion.type.isInt(ast.y.type)==false then
          orion.error("Error, y argument to gather must be int but is "..ast.y.type:str(), origast:linenumber(), origast:offset())
        end

      else
        orion.error("Internal error, typechecking for "..ast.kind.." isn't implemented!",ast.line,ast.char)
        return nil
      end
      
      if orion.type.isType(ast.type)==false then print(ast.kind) end
      ast = orion.typedAST.new(ast):copyMetadataFrom(origast)
      assert(orion.type.isType(ast.type))

      return {ast}
    end)

  return res[1], res[2]
end

function orion.typedAST.astToTypedAST(ast, options)
  assert(orion.ast.isAST(ast))
  assert(type(options)=="table")

  -- first we run CSE to clean up the users code
  -- this will save us a lot of time/memory later on
  ast = orion.optimize.CSE(ast,{})

  if options.verbose or options.printstage then 
    print("toTyped") 
    print("nodecount",ast:S("*"):count())
    print("maxDepth",ast:maxDepth())
  end

  if orion.verbose or orion.printstage then 
    print("desugar")
  end

  -- desugar mapreduce expressions etc
  -- we can't do this in _toTypedAST, b/c mapreducevar's aren't
  -- allowed anywhere (so these expressions are technically invalid)
  -- so we do this first as a preprocessing step.
  --
  -- we also don't want to do these desugaring in compileTimeProcess,
  -- b/c that will potentially grow memory usage exponentially
  ast = ast:S(function(n) 
                return n.kind=="let" or 
                  n.kind=="switch" end):process(
    function(node)
      if node.kind=="let" then
        local cnt = 1
        local namemap = {}

        local function removeLet( expr, namemap )
          return expr:S("letvar"):process(
            function(n)
              if namemap[n.variable]~=nil then
                return namemap[n.variable]
              end
              return n
            end)
        end

        while node["expr"..cnt] do
          namemap[node["exprname"..cnt]] = removeLet(node["expr"..cnt],namemap)
          cnt = cnt + 1
        end

        return removeLet(node.res, namemap)
      elseif node.kind=="switch" then
        local cnt = node:arraySize("expr")
        
        local cond = orion.ast.new({kind="binop",op="==",lhs=node.controlExpr,rhs=node["val"..cnt]}):copyMetadataFrom(node)
        local select = orion.ast.new({kind="select",cond=cond,a=node["expr"..cnt],b=node.default}):copyMetadataFrom(node)
        
        cnt = cnt-1
        while cnt > 0 do
          cond = orion.ast.new({kind="binop",op="==",lhs=node.controlExpr,rhs=node["val"..cnt]}):copyMetadataFrom(node)
          select = orion.ast.new({kind="select",cond=cond,a=node["expr"..cnt],b=select}):copyMetadataFrom(node)
          cnt = cnt - 1
        end
        
        return select

      end
    end)

  if options.verbose then
    print("desugar done")
  end

  -- should have been eliminated
  if orion.debug then assert(ast:S(function(n) return n.kind=="mapreducevar" or n.kind=="letvar" or n.kind=="switch" end):count()==0) end

  if options.printstage then
    print("_toTypedAST",collectgarbage("count"))
  end

  local typedAST = orion.typedAST._toTypedAST(ast)

  if options.verbose or options.printstage then
    print("conversion to typed AST done ------------")
  end

  return typedAST
end


function orion.typedAST.isTypedAST(ast) return getmetatable(ast)==typedASTMT end

-- kind of a hack - so that the IR library can shallowcopy and then
-- modify an ast node without having to know its exact type
function typedASTFunctions:init()
  setmetatable(self,nil)
  orion.typedAST.new(self)
end

function orion.typedAST.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,typedASTMT)
end

