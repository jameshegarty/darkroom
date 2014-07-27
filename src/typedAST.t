typedASTFunctions={}
setmetatable(typedASTFunctions,{__index=IRFunctions})
typedASTMT={__index=typedASTFunctions,
  __newindex = function(table, key, value)
                    darkroom.error("Attempt to modify typed AST node")
                  end}

darkroom.typedAST = {}



function darkroom.typedAST.checkConstantExpr(expr, coord)

  -- no x+x allowed
  if coord~=nil and expr:S("position"):count() ~= 1 then
return false
  end

  expr:S("*"):traverse( 
    function(n)
      if expr.kind=="binop" then
        if expr.op~="+" and expr.op~="-" then
          darkroom.error("binop '"..expr.op.."' is not supported in an constant expr")
        end
      elseif expr.kind=="value" then
        if type(expr.value)~="number" then
          darkroom.error("type "..type(expr.value).." is not supported in constant expr")
        end
      elseif coord~=nil and expr.kind=="position" then
        if expr.coord~=coord then
          darkroom.error("you can't use coord "..expr.coord.." in expression for coord "..coord)
        end
      elseif expr.kind=="cast" then
      elseif expr.kind=="mapreducevar" then
      else
        darkroom.error(expr.kind.." is not supported in constant expr")    
      end
    end)

  return true
end

-- take the ast input that's an offset expr for coord 'coord'
-- and convert it into a translate,scale. returns nil if there was an error.
function darkroom.typedAST.synthOffset(ast,coord)
  -- note that we don't typecheck these expressions! we keep them as ASTs,
  -- because they aren't really part of the language
  assert(darkroom.ast.isAST(ast))

  -- first check that there isn't anything in the expression that's definitely not allowed...
  if darkroom.typedAST.checkConstantExpr(ast,coord)==false then
return nil
  end

  -- now distribute the multiplies until they can't be distributed any more. ((x+i)+j) * 2 => (x*2 + i*2)+j*2
  
  -- now, the path up the tree from the position to the root should only have 1 multiply and >=0 adds
  -- note that this thing must be a tree!
  local pos
  local mulCount = 0
  local addCount = 0
  ast:S("position"):traverse(function(n) pos = n end)
  while pos:parentCount(ast) > 0 do
    for k,_ in pos:parents(ast) do pos = k end
    if pos.kind=="binop" and (pos.op=="+" or pos.op=="-") then addCount = addCount+1
    elseif pos.kind=="binop" and (pos.op=="*" or pos.op=="/") then mulCount = mulCount+1
    else print(pos.kind,pos.op);assert(false) end
  end
  assert(mulCount==0)

  -- cheap hack, since the path from the position to the root is just a bunch of adds,
  -- we can get the translation by setting the position to 0
  local translate = ast:S("position"):process(
    function(n) 
      if n.kind=="position" then
        return darkroom.ast.new({kind="value",value=0}):copyMetadataFrom(n)
      end
    end)
  assert(translate:S("position"):count()==0)
  return translate:optimize(), 1
end

-- the translate operator can take a few different arguments as translations
-- it can contain binary + ops, and mapreduce vars. This evaluates all possible
-- index values to find the correct stencil for those situations
function darkroom.typedAST.transformArea(t1,t2)
  if type(t1)=="number" then t1=darkroom.ast.new({kind="value",value=t1}) end
  if type(t2)=="number" then t2=darkroom.ast.new({kind="value",value=t2}) end
  return t1:eval(1):sum(t2:eval(2))
end

-- returns the stencil with (0,0,0) at the origin
-- if input isn't null, only calculate stencil for this input (a kernelGraph node)
function typedASTFunctions:stencil(input)

  if self.kind=="binop" then
    return self.lhs:stencil(input):unionWith(self.rhs:stencil(input))
  elseif self.kind=="multibinop" then
    local res = Stencil.new()

    for i=1,self:arraySize("lhs") do
      res = res:unionWith(self["lhs"..i]:stencil(input))
    end

    for i=1,self:arraySize("rhs") do
      res = res:unionWith(self["rhs"..i]:stencil(input))
    end

    return res
  elseif self.kind=="multiunary" then
    local res = Stencil.new()

    for i=1,self:arraySize("expr") do
      res = res:unionWith(self["expr"..i]:stencil(input))
    end

    return res
  elseif self.kind=="unary" then
    return self.expr:stencil(input)
  elseif self.kind=="assert" then
    return self.cond:stencil(input):unionWith(self.expr:stencil(input))
  elseif self.kind=="cast" then
    return self.expr:stencil(input)
  elseif self.kind=="select" or self.kind=="vectorSelect" then
    return self.cond:stencil(input)
    :unionWith(self.a:stencil(input)
               :unionWith(self.b:stencil(input)
                          ))
  elseif self.kind=="position" or self.kind=="tap" or self.kind=="value" then
    return Stencil.new()
  elseif self.kind=="tapLUTLookup" then
    return self.index:stencil(input)
  elseif self.kind=="load" then
    local s = Stencil.new()
    if input==nil or input==self.from then s = darkroom.typedAST.transformArea(self.relX,self.relY) end
    return s
  elseif self.kind=="gather" then
    --if input~=nil then assert(false) end
    assert(self.input.kind=="load")

    if input~=nil and self.input.from~=input then
      return Stencil.new() -- not the input we're interested in
    else
      -- note the kind of nasty hack we're doing here: gathers read from loads, and loads can be shifted.
      -- so we need to shift this the same as the load
      return darkroom.typedAST.transformArea(self.input.relX, self.input.relY):sum( Stencil.new():add(-self.maxX,-self.maxY,0):add(self.maxX,self.maxY,0))
    end
  elseif self.kind=="array" then
    local exprsize = self:arraySize("expr")
    local s = Stencil.new()
    for i=1,exprsize do
      s = s:unionWith(self["expr"..i]:stencil(input))
    end

    return s
  elseif self.kind=="reduce" then
    local s = Stencil.new()
    local i=1
    while self["expr"..i] do
      s = s:unionWith(self["expr"..i]:stencil(input))
      i=i+1
    end
    return s
  elseif self.kind=="index" then
    return self.expr:stencil(input)
  elseif self.kind=="crop" then
    return self.expr:stencil(input)
  elseif self.kind=="transformBaked" then
    return self.expr:stencil(input):sum(darkroom.typedAST.transformArea(self.translate1,self.translate2))
  elseif self.kind=="mapreduce" then
    return self.expr:stencil(input)
  elseif self.kind=="mapreducevar" then
    return Stencil.new()
  end

  print(self.kind, debug.traceback())
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
    print("could not convert to a constant"..self.kind)
    assert(false)
  end

  return nil
end


function darkroom.typedAST._toTypedAST(inast)

  local res = inast:visitEach(
    function(origast,inputs)
      assert(darkroom.ast.isAST(origast))
      local ast = origast:shallowcopy()

      if ast.kind=="value" then
        if ast.type==nil then ast.type=darkroom.type.valueToType(ast.value) end
        if ast.type==nil then
          darkroom.error("Internal error, couldn't convert "..tostring(ast.value).." to orion type", origast:linenumber(), origast:offset(), origast:filename() )
        end
      elseif ast.kind=="unary" then
        ast.expr = inputs["expr"][1]
        
        if ast.op=="-" then
          if darkroom.type.astIsUint(ast.expr) then
            darkroom.warning("You're negating a uint, this is probably not what you want to do!", origast:linenumber(), origast:offset(), origast:filename())
          end
          
          ast.type = ast.expr.type
        elseif ast.op=="floor" or ast.op=="ceil" then
          ast.type = darkroom.type.float(32)
        elseif ast.op=="abs" then
          if ast.expr.type:baseType()==darkroom.type.float(32) then
            ast.type = ast.expr.type
          elseif ast.expr.type:baseType()==darkroom.type.float(64) then
            ast.type = ast.expr.type
          elseif ast.expr.type:baseType():isInt() or ast.expr.type:baseType():isUint() then
            -- obv can't make it any bigger
            ast.type = ast.expr.type
          else
            ast.expr.type:print()
            assert(false)
          end
        elseif ast.op=="not" then
          if ast.expr.type:baseType():isBool() then
            ast.type = ast.expr.type
          else
            darkroom.error("not only works on bools",origast:linenumber(), origast:offset())
            assert(false)
          end
        elseif ast.op=="sin" or ast.op=="cos" or ast.op=="exp" then
          if ast.expr.type==darkroom.type.float(32) then
            ast.type = darkroom.type.float(32)
          elseif ast.expr.type==darkroom.type.float(64) then
            ast.type = darkroom.type.float(64)
          else
            darkroom.error("sin, cos, and exp only work on floating point types",origast:linenumber(),origast:offset(),origast:filename())
          end
        elseif ast.op=="arrayAnd" then
          if darkroom.type.isArray(ast.expr.type) and darkroom.type.isBool(darkroom.type.arrayOver(ast.expr.type)) then
            ast.type = darkroom.type.bool()
          else
            darkroom.error("vectorAnd only works on arrays of bools",origast:linenumber(), origast:offset())
          end
        elseif ast.op=="print" then
          ast.type = ast.expr.type
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
        
        local thistype, lhscast, rhscast = darkroom.type.meet( lhs.type, rhs.type, ast.op, origast )
        
        if thistype==nil then
          darkroom.error("Type error, inputs to "..ast.op,origast:linenumber(), origast:offset(), origast:filename())
        end
        
        if lhs.type~=lhscast then lhs = darkroom.typedAST.new({kind="cast",expr=lhs,type=lhscast}):copyMetadataFrom(origast) end
        if rhs.type~=rhscast then rhs = darkroom.typedAST.new({kind="cast",expr=rhs,type=rhscast}):copyMetadataFrom(origast) end
        
        ast.type = thistype
        ast.lhs = lhs
        ast.rhs = rhs
        
      elseif ast.kind=="position" then
        -- if position is still in the tree at this point, it means it's being used in an expression somewhere
        -- choose a reasonable type...
        ast.type=darkroom.type.int(32)
      elseif ast.kind=="select" or ast.kind=="vectorSelect" then
        local cond = inputs["cond"][1]
        local a = inputs["a"][1]
        local b = inputs["b"][1]

        if ast.kind=="vectorSelect" then
          if darkroom.type.arrayOver(cond.type)~=darkroom.type.bool() then
            darkroom.error("Error, condition of vectorSelect must be array of booleans. ",origast:linenumber(),origast:offset())
            return nil
          end

          if darkroom.type.isArray(cond.type)==false or
            darkroom.type.isArray(a.type)==false or
            darkroom.type.isArray(b.type)==false or
            darkroom.type.arrayLength(a.type)~=darkroom.type.arrayLength(b.type) or
            darkroom.type.arrayLength(cond.type)~=darkroom.type.arrayLength(a.type) then
            darkroom.error("Error, all arguments to vectorSelect must be arrays of the same length",origast:linenumber(),origast:offset())
            return nil            
          end
        else
          if cond.type ~= darkroom.type.bool() then
            darkroom.error("Error, condition of select must be scalar boolean. Use vectorSelect",origast:linenumber(),origast:offset(),origast:filename())
            return nil
          end

          if darkroom.type.isArray(a.type)~=darkroom.type.isArray(b.type) then
            darkroom.error("Error, if any results of select are arrays, all results must be arrays",origast:linenumber(),origast:offset())
            return nil
          end
          
          if darkroom.type.isArray(a.type) and
            darkroom.type.arrayLength(a.type)~=darkroom.type.arrayLength(b.type) then
            darkroom.error("Error, array arguments to select must be the same length",origast:linenumber(),origast:offset())
            return nil
          end
        end

        local thistype, lhscast, rhscast =  darkroom.type.meet(a.type,b.type, ast.kind, origast)

        if a.type~=lhscast then a = darkroom.typedAST.new({kind="cast",expr=a,type=lhscast}):copyMetadataFrom(origast) end
        if b.type~=rhscast then b = darkroom.typedAST.new({kind="cast",expr=b,type=rhscast}):copyMetadataFrom(origast) end
        
        ast.type = thistype
        ast.cond = cond
        ast.a = a
        ast.b = b
        
      elseif ast.kind=="index" then
        local expr = inputs["expr"][1]
        
        if darkroom.type.isArray(expr.type)==false then
          expr.type:print()
          darkroom.error("Error, you can only index into an array type!",origast:linenumber(),origast:offset(), origast:filename())
          os.exit()
        end
        
        ast.expr = expr

        if darkroom.typedAST.checkConstantExpr(origast["index"])==false then
          darkroom.error("index must be a constant expression",origast:linenumber(), origast:offset(), origast:filename())
        end

        local range = origast["index"]:eval(1)

        if range:min(1)<0 or range:max(1) >= darkroom.type.arrayLength(expr.type) then
          darkroom.error("index value out of range. It is ["..range:min(1)..","..range:max(1).."] but should be within [0,"..(darkroom.type.arrayLength(expr.type)-1).."]",origast:linenumber())
        end

        ast.index = origast["index"]
        ast.type = darkroom.type.astArrayOver(expr)
        
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
          local translate,scale=darkroom.typedAST.synthOffset( origast["arg"..i], darkroom.dimToCoord[i])

          if translate==nil then
            darkroom.error("Error, non-stencil access pattern", origast:linenumber(), origast:offset(), origast:filename())
          end

          newtrans["translate"..i]=translate
          newtrans["scale"..i]=scale

          if translate~=0 or scale~=1 then noTransform = false end
          i=i+1
        end
        
        -- at least 2 arguments must be specified. 
        -- the parser was supposed to guarantee this.
        assert(i>2)

        if noTransform then -- eliminate unnecessary transforms early
          ast=ast.expr:shallowcopy()
        else
          ast=newtrans
        end

      elseif ast.kind=="array" then
        
        local cnt = 1
        while ast["expr"..cnt] do
          ast["expr"..cnt] = inputs["expr"..cnt][1]
          cnt = cnt + 1
        end
        
        local mtype = ast.expr1.type
        local atype, btype
        
        if darkroom.type.isArray(mtype) then
          darkroom.error("You can't have nested arrays (index 0 of vector)",origast:linenumber(),origast:offset(),origast:filename())
        end
        
        local cnt = 2
        while ast["expr"..cnt] do
          if darkroom.type.isArray(ast["expr"..cnt].type) then
            darkroom.error("You can't have nested arrays (index "..(i-1).." of vector)")
          end
          
          mtype, atype, btype = darkroom.type.meet( mtype, ast["expr"..cnt].type, "array", origast)
          
          if mtype==nil then
            darkroom.error("meet error")      
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
            if darkroom.type.checkImplicitCast(from, mtype,origast)==false then
              darkroom.error("Error, can't implicitly cast "..from:str().." to "..mtype:str(), origast:linenumber(), origast:offset())
            end
            
            ast["expr"..cnt] = darkroom.typedAST.new({kind="cast",expr=ast["expr"..cnt], type=mtype}):copyMetadataFrom(ast["expr"..cnt])
          end

          cnt = cnt + 1
        end
        
        local arraySize = cnt - 1
        ast.type = darkroom.type.array(mtype, arraySize)
        

      elseif ast.kind=="cast" then

        -- note: we don't eliminate these cast nodes, because there's a difference
        -- between calculating a value at a certain precision and then downsampling,
        -- and just calculating the value at the lower precision.
        ast.expr = inputs["expr"][1]
        
        if darkroom.type.checkExplicitCast(ast.expr.type,ast.type,origast)==false then
          darkroom.error("Casting from "..ast.expr.type:str().." to "..ast.type:str().." isn't allowed!",origast:linenumber(),origast:offset())
        end
      elseif ast.kind=="assert" then

        ast.cond = inputs["cond"][1]
        ast.expr = inputs["expr"][1]
        ast.printval = inputs["printval"][1]

        if darkroom.type.astIsBool(ast.cond)==false then
          darkroom.error("Error, condition of assert must be boolean",origast:linenumber(),origast:offset(), origast:filename())
          return nil
        end

        ast.type = ast.expr.type

      elseif ast.kind=="mapreducevar" then
        ast.low = ast.low:eval(1)
        if ast.low:area()~=1 then 
          darkroom.error("map reduce variable range must be a constant", origast:linenumber(), origast:offset(), origast:filename())
        end
        ast.low = ast.low:min(1)

        ast.high = ast.high:eval(1)
        if ast.high:area()~=1 then 
          darkroom.error("map reduce variable range must be a constant", origast:linenumber(), origast:offset(), origast:filename())
        end
        ast.high = ast.high:min(1)

        ast.type = darkroom.type.int(32)

--        ast.type = darkroom.type.meet(ast.low.type,ast.high.type,"mapreducevar", origast)
      elseif ast.kind=="tap" then
        -- taps should be tagged with type already
      elseif ast.kind=="tapLUTLookup" then
        ast.index = inputs["index"][1]
        
        -- tapLUTs should be tagged with type already
        assert(darkroom.type.isType(ast.type))
        
        if darkroom.type.isUint(ast.index.type)==false and
          darkroom.type.isInt(ast.index.type)==false then
          
          darkroom.error("Error, index into tapLUT must be integer",ast:linenumber(),ast:offset())
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

        ast.type = darkroom.type.reduce( ast.op, typeSet)
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

        if darkroom.type.isInt(ast.x.type)==false then
          darkroom.error("Error, x argument to gather must be int but is "..ast.x.type:str(), origast:linenumber(), origast:offset())
        end

        if darkroom.type.isInt(ast.y.type)==false then
          darkroom.error("Error, y argument to gather must be int but is "..ast.y.type:str(), origast:linenumber(), origast:offset())
        end
      elseif ast.kind=="load" then
        -- already has a type
      elseif ast.kind=="mapreduce" then
        if ast.reduceop=="sum" or ast.reduceop=="max" or ast.reduceop=="min" then
          ast.type = inputs.expr[1].type
        elseif ast.reduceop=="argmin" or ast.reduceop=="argmax" then
          if inputs.expr[1].type:isArray()==true then
            darkroom.error("argmin and argmax can only be applied to scalar quantities", origast:linenumber(), origast:offset(), origast:filename())
          end

          ast.type = darkroom.type.array(darkroom.type.int(32),origast:arraySize("varname"))
        else
          darkroom.error("Unknown reduce operator '"..ast.reduceop.."'")
        end

        ast.expr = inputs.expr[1]

        local i = 1
        while ast["varname"..i] do
          ast["varlow"..i] = ast["varlow"..i]:eval(1)
          if ast["varlow"..i]:area()~=1 then 
            darkroom.error("map reduce variable range must be a constant",ast:linenumber(),ast:offset(),ast:filename())
          end
          ast["varlow"..i] = ast["varlow"..i]:min(1)

          ast["varhigh"..i] = ast["varhigh"..i]:eval(1)
          if ast["varhigh"..i]:area()~=1 then 
            darkroom.error("map reduce variable range must be a constant",ast:linenumber(),ast:offset(),ast:filename())
          end
          ast["varhigh"..i] = ast["varhigh"..i]:min(1)

          i = i + 1
        end

      else
        darkroom.error("Internal error, typechecking for "..ast.kind.." isn't implemented!",ast.line,ast.char)
        return nil
      end
      
      if darkroom.type.isType(ast.type)==false then print(ast.kind) end
      ast = darkroom.typedAST.new(ast):copyMetadataFrom(origast)
      assert(darkroom.type.isType(ast.type))

      return {ast}
    end)

  return res[1], res[2]
end

function darkroom.typedAST.astToTypedAST(ast, options)
  assert(darkroom.ast.isAST(ast))
  assert(type(options)=="table")

  -- first we run CSE to clean up the users code
  -- this will save us a lot of time/memory later on
  ast = darkroom.optimize.CSE(ast,{})

  if options.verbose or options.printstage then 
    print("toTyped") 
    print("nodecount",ast:S("*"):count())
    print("maxDepth",ast:maxDepth())
  end

  if darkroom.verbose or darkroom.printstage then 
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
        
        local cond = darkroom.ast.new({kind="binop",op="==",lhs=node.controlExpr,rhs=node["val"..cnt]}):copyMetadataFrom(node)
        local select = darkroom.ast.new({kind="select",cond=cond,a=node["expr"..cnt],b=node.default}):copyMetadataFrom(node)
        
        cnt = cnt-1
        while cnt > 0 do
          cond = darkroom.ast.new({kind="binop",op="==",lhs=node.controlExpr,rhs=node["val"..cnt]}):copyMetadataFrom(node)
          select = darkroom.ast.new({kind="select",cond=cond,a=node["expr"..cnt],b=select}):copyMetadataFrom(node)
          cnt = cnt - 1
        end
        
        return select

      end
    end)

  if options.verbose then
    print("desugar done")
  end

  -- should have been eliminated
  if darkroom.debug then assert(ast:S(function(n) return n.kind=="letvar" or n.kind=="switch" end):count()==0) end

  if options.printstage then
    print("_toTypedAST",collectgarbage("count"))
  end

  local typedAST = darkroom.typedAST._toTypedAST(ast)

  if options.verbose or options.printstage then
    print("conversion to typed AST done ------------")
  end

  return typedAST
end


function darkroom.typedAST.isTypedAST(ast) return getmetatable(ast)==typedASTMT end

-- kind of a hack - so that the IR library can shallowcopy and then
-- modify an ast node without having to know its exact type
function typedASTFunctions:init()
  setmetatable(self,nil)
  darkroom.typedAST.new(self)
end

function darkroom.typedAST.new(tab)
  assert(type(tab)=="table")
  darkroom.IR.new(tab)
  return setmetatable(tab,typedASTMT)
end

