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


function orion.typedAST._toTypedAST(inast, inputWidth, inputHeight)
  assert(type(inputWidth)=="number")
  assert(type(inputHeight)=="number")

  local res = inast:visitEach(
    function(origast,inputs)
      assert(orion.ast.isAST(origast))
      local ast = origast:shallowcopy()

      -- crops and cropsTranslated are used to calculate the crop region on crop modes
      -- 'crops' is used for cropSame (this aren't translated
      -- 'cropsTranslated' is used for cropGrow and cropShrink
      -- The _key_ of these tables is the crop, so that we deduplicate crops (or else the list can
      -- grow huge for now reason). All the crop modes are unaffected by the deduplication...
      local crops = {} -- crops unaffected by translation
      local cropsTranslated = {} -- crops affected by translation

      if ast.kind=="value" then
        if ast.type==nil then ast.type=orion.type.valueToType(ast.value) end
        assert(ast.type~=nil)
        crops[orion.cropIR.infinite():copyMetadataFrom(origast)] = 1
        cropsTranslated[orion.cropIR.infinite():copyMetadataFrom(origast)] = 1
      elseif ast.kind=="unary" then
        ast.expr = inputs["expr"][1]
        crops = inputs["expr"][2]
        cropsTranslated = inputs["expr"][3]
        
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
        local lhscrops = inputs["lhs"][2]
        local lhscropsTranslated = inputs["lhs"][3]
        local rhs = inputs["rhs"][1]
        local rhscrops = inputs["rhs"][2]
        local rhscropsTranslated = inputs["rhs"][3]
        
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
        
        appendSet( crops, lhscrops )
        appendSet( crops, rhscrops )
        appendSet( cropsTranslated, lhscropsTranslated )
        appendSet( cropsTranslated, rhscropsTranslated )

      elseif ast.kind=="input" then
        local crp = orion.cropIR.explicit(0,0,inputWidth,inputHeight)
        crops[crp:copyMetadataFrom(origast)] = 1
        cropsTranslated[crp:copyMetadataFrom(origast)] = 1
      elseif ast.kind=="position" then
        -- if position is still in the tree at this point, it means it's being used in an expression somewhere
        -- choose a reasonable type...
        ast.type=orion.type.int(32)
        crops[orion.cropIR.infinite():copyMetadataFrom(origast)] = 1
        cropsTranslated[orion.cropIR.infinite():copyMetadataFrom(origast)] = 1
      elseif ast.kind=="select" or ast.kind=="vectorSelect" then
        local cond = inputs["cond"][1]
        local condcrops = inputs["cond"][2]
        local condcropsTranslated = inputs["cond"][3]
        local a = inputs["a"][1]
        local acrops = inputs["a"][2]
        local acropsTranslated = inputs["a"][3]
        local b = inputs["b"][1]
        local bcrops = inputs["b"][2]
        local bcropsTranslated = inputs["b"][3]

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
        
        appendSet(crops,condcrops)
        appendSet(crops,acrops)
        appendSet(crops,bcrops)

        appendSet( cropsTranslated, condcropsTranslated )
        appendSet( cropsTranslated, acropsTranslated )
        appendSet( cropsTranslated, bcropsTranslated )
        
      elseif ast.kind=="index" then
        local expr = inputs["expr"][1]
        crops = inputs["expr"][2]
        cropsTranslated = inputs["expr"][3]
        
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
        crops = inputs["expr"][2]
        local cropsTranslatedIn = inputs["expr"][3]
        
        -- this just gets the value of the thing we're translating
        ast.type = ast.expr.type
        
        local i=1
        while ast["arg"..i] do
          ast["arg"..i] = inputs["arg"..i][1] 
          
          for c,_ in pairs(inputs["arg"..i][2]) do
            assert(orion.cropIR.isCropIR(c))
            if c.kind~="infinite" then
              orion.error("Argument to image fn should have infinite crop!",origast:linenumber(),origast:offset())
            end
          end

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

        cropsTranslated = {}

        for c,_ in pairs(cropsTranslatedIn) do
          cropsTranslated[c:translate(newtrans.translate1, newtrans.translate2)]=1
        end

        ast=newtrans

      elseif ast.kind=="array" then
        
        local cnt = 1
        while ast["expr"..cnt] do
          ast["expr"..cnt] = inputs["expr"..cnt][1]
          local crop = inputs["expr"..cnt][2]
          appendSet(crops, crop)
          local cropTranslated = inputs["expr"..cnt][3]
          appendSet(cropsTranslated, cropTranslated)
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
        crops = inputs["expr"][2]
        cropsTranslated = inputs["expr"][3]
        
        if orion.type.checkExplicitCast(ast.expr.type,ast.type,origast)==false then
          orion.error("Casting from "..ast.expr.type:str().." to "..ast.type:str().." isn't allowed!",origast:linenumber(),origast:offset())
        end
        

      elseif ast.kind=="assert" then

        ast.cond = inputs["cond"][1]
        local condCrop = inputs["cond"][2]
        local condCropTranslated = inputs["cond"][3]
        ast.expr = inputs["expr"][1]
        local exprCrop = inputs["expr"][2]
        local exprCropTranslated = inputs["expr"][3]
        ast.printval = inputs["printval"][1]
        local printvalCrop = inputs["printval"][2]
        local printvalCropTranslated = inputs["printval"][3]
        
        if orion.type.astIsBool(ast.cond)==false then
          orion.error("Error, condition of assert must be boolean",ast:linenumber(),ast:offset())
          return nil
        end

        ast.type = ast.expr.type
        
        appendSet(crops, condCrop)
        appendSet(crops, exprCrop)
        appendSet(crops, printvalCrop)

        appendSet(cropsTranslated, condCropTranslated)
        appendSet(cropsTranslated, exprCropTranslated)
        appendSet(cropsTranslated, printvalCropTranslated)

      elseif ast.kind=="mapreducevar" then
        local low = inputs["low"][1]
        local lowCrop = inputs["low"][2]
        local lowCropTranslated = inputs["low"][3]
        local high = inputs["high"][1]
        local highCrop = inputs["high"][2]
        local highCropTranslated = inputs["high"][3]

        appendSet(crops,lowCrop)
        appendSet(crops,highCrop)

        appendSet(cropsTranslated, lowCropTranslated )
        appendSet(cropsTranslated, highCropTranslated )

        -- we don't need this anymore. Only needed this to calculate the type.
        ast.low=nil
        ast.high=nil
        
        ast.type = orion.type.meet(low.type,high.type,"mapreducevar", origast)
      elseif ast.kind=="tap" then
        -- taps should be tagged with type already
        crops[orion.cropIR.infinite():copyMetadataFrom(origast)]=1
        cropsTranslated[orion.cropIR.infinite():copyMetadataFrom(origast)]=1
      elseif ast.kind=="tapLUTLookup" then
        ast.index = inputs["index"][1]
        crops = inputs["index"][2]
        cropsTranslated = inputs["index"][3]

        -- tapLUTs should be tagged with type already
        assert(orion.type.isType(ast.type))

        crops[orion.cropIR.infinite():copyMetadataFrom(origast)]=1
        cropsTranslated[orion.cropIR.infinite():copyMetadataFrom(origast)]=1

        if orion.type.isUint(ast.index.type)==false and
          orion.type.isInt(ast.index.type)==false then

          orion.error("Error, index into tapLUT must be integer",ast:linenumber(),ast:offset())
          return nil
        end
      elseif ast.kind=="crop" then
        ast.expr = inputs["expr"][1]
        crops = inputs["expr"][2]
        cropsTranslated = inputs["expr"][3]
        
        ast.type = ast.expr.type
        
        assert(keycount(crops)>0)
        
        if ast.mode==orion.cropSame or
          ast.mode==orion.cropGrow or
          ast.mode==orion.cropShrink then

          local cropList = cropsTranslated
          if ast.mode==orion.cropSame then cropList = crops end

          for c,_ in pairs(cropList) do
            if ast.crop==nil then
              ast.crop = c
            else
              ast.crop = ast.crop:meet(c,ast.mode)
            end
          end

          ast.kind="cropBaked"
          ast.mode=nil
          ast.x=nil
          ast.y=nil
          ast.width=nil
          ast.height=nil

          crops = {}
          crops[ast.crop] = 1
          cropsTranslated = {}
          cropsTranslated[ast.crop] = 1
        elseif ast.mode==orion.cropExplicit then
          ast.kind="cropBaked"
          ast.crop = orion.cropIR.explicit( ast.x, ast.y, ast.x+ast.w, ast.y+ast.h ):copyMetadataFrom(origast)
          ast.mode=nil
          ast.x=nil
          ast.y=nil
          ast.w=nil
          ast.h=nil
          
          crops = {}
          crops[ast.crop] = 1
          cropsTranslated = {}
          cropsTranslated[ast.crop] = 1
        else
          print(ast.mode.name)
          assert(false)
        end
      elseif ast.kind=="reduce" then
        local i=1
        local typeSet = {}

        while ast["expr"..i] do
          ast["expr"..i] = inputs["expr"..i][1]
          local crop = inputs["expr"..i][2]
          local cropTranslated = inputs["expr"..i][3]
          appendSet(crops,crop)
          appendSet(cropsTranslated,cropTranslated)
          table.insert(typeSet,ast["expr"..i].type)
          
          i=i+1
        end

        ast.type = orion.type.reduce( ast.op, typeSet)
      elseif ast.kind=="outputs" then
        -- doesn't matter, this is always the root and we never need to get its type
        ast.type = inputs.expr1[1].type
        crops = inputs.expr1[2]
        cropsTranslated = inputs.expr1[3]

        local i=1
        while ast["expr"..i] do
          ast["expr"..i] = inputs["expr"..i][1]
          i=i+1
        end

      elseif ast.kind=="type" then
        -- ast.type is already a type, so don't have to do anything
        -- shouldn't matter, but need to return something
        crops[orion.cropIR.empty()] = 1
        cropsTranslated[orion.cropIR.empty()] = 1
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

        appendSet(crops, inputs.input[2])
        appendSet(crops, inputs.x[2])
        appendSet(crops, inputs.y[2])

        appendSet(cropsTranslated, inputs.input[3])
        appendSet(cropsTranslated, inputs.x[3])
        appendSet(cropsTranslated, inputs.y[3])
      else
        orion.error("Internal error, typechecking for "..ast.kind.." isn't implemented!",ast.line,ast.char)
        return nil
      end
      
      if orion.type.isType(ast.type)==false then print(ast.kind) end
      ast = orion.typedAST.new(ast):copyMetadataFrom(origast)
      assert(orion.type.isType(ast.type))
      assert(keycount(crops)>0)

      for c,k in pairs(crops) do
        assert(orion.cropIR.isCropIR(c))
        assert(type(k)=="number")
      end
      for c,k in pairs(cropsTranslated) do
        assert(orion.cropIR.isCropIR(c))
        assert(type(k)=="number")
      end

      return {ast, crops, cropsTranslated}
    end)

  return res[1], res[2]
end

function orion.typedAST.astToTypedAST(ast, inputWidth, inputHeight)
  assert(orion.ast.isAST(ast))
  assert(type(inputWidth)=="number")
  assert(type(inputHeight)=="number")

  -- first we run CSE to clean up the users code
  -- this will save us a lot of time/memory later on
  ast = orion.optimize.CSE(ast,{})

  if orion.verbose or orion.printstage then 
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
                return n.kind=="mapreduce" or 
                  n.kind=="let" or 
                  n.kind=="switch" end):process(
    function(node)
      if node.kind=="mapreduce" then
        local tmpAST = node:shallowcopy()
        
        -- figure out what the mapreduce variable ranges are
        -- this must happen at compile time
        local i=1
        while tmpAST["varname"..i] do
          local lowAST, lowCrop = orion.typedAST._toTypedAST(node["varlow"..i], inputWidth, inputHeight)
          local low = lowAST:eval()
          
          local highAST, highCrop = orion.typedAST._toTypedAST(node["varhigh"..i], inputWidth, inputHeight)
          local high = highAST:eval()
          
          if type(low)~="number" then
            orion.error("low range must be compile time const")
          end
          
          if type(high)~="number" then
            orion.error("low range must be compile time const")
          end
          
          if low>high then
            orion.error("low must be <= high")
          end
          
          tmpAST["varlow"..i] = low
          tmpAST["varhigh"..i] = high
          
          i=i+1
        end
        
        -- now desugar the mapreduce
        
        local newnode = {kind="reduce",op=tmpAST.reduceop, type=tmpAST.type}
        local exprCount = 1
        
        -- replace the vars
        -- remember: this is a N*M operation, where
        -- N = number of MR vars, M = # of values they can take on
        local function replacevar(index,inputast)
          --inputast:check()
          if tmpAST["varname"..index]==nil then -- base case
            newnode["expr"..exprCount]=inputast
            exprCount = exprCount+1
          else
            for varval = tmpAST["varlow"..index],tmpAST["varhigh"..index] do
              
              local newexpr = inputast:S("mapreducevar variable="..tmpAST["varname"..index]):process(
                function(fin)
                  assert(orion.ast.isAST(fin))
                  return orion.ast.new({kind="value",value=varval}):copyMetadataFrom(fin)
                end)
              
              replacevar(index+1,newexpr) -- recurse
            end
          end
        end
        
        replacevar(1,tmpAST.expr) -- this modifies newnode
        
        return orion.ast.new(newnode):copyMetadataFrom(node)
      elseif node.kind=="let" then
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

  if orion.verbose or orion.printstage then 
    print("desugar done")
  end

  -- should have been eliminated
  if orion.debug then assert(ast:S(function(n) return n.kind=="mapreducevar" or n.kind=="letvar" or n.kind=="switch" end):count()==0) end
  ast:check()

  if orion.printstage then
    print("_toTypedAST",collectgarbage("count"))
  end

  local typedAST = orion.typedAST._toTypedAST(ast, inputWidth, inputHeight)
  typedAST:check()

  if orion.verbose or orion.printstage then
    print("conversion to typed AST done ------------")
  end

  if orion.verbose then
    typedAST:printpretty()
  end

  return typedAST
end

function orion.typedAST.printprettys(node,root,parent,key,assignments)
  return typedASTFunctions.printprettys(node,root,parent,key,assignments)
end

-- parent,key is used by later IRs
-- assignments is used to store variables we've assigned to
-- assignments: varname -> string
function typedASTFunctions:printprettys(root,parent,key,assignments)
  assert(orion.IR.isIR(root))
  assert(type(assignments)=="table")

  local out = "["..orion.type.typeToString(self.type).."]"

  if self.kind=="func" then
    local i=1
    while self["identifier"..i] do
      out = out..self["identifier"..i]
      if self["identifier"..(i+1)] then out=out.."." end
      i=i+1
    end

  elseif self.kind=="binop" then
    out = out.."("..self.lhs:printprettys(root,self,"lhs",assignments)..")"
    out = out..self.op.."("..self.rhs:printprettys(root,self,"rhs",assignments)..")"
  elseif self.kind=="multibinop" then
    out = out.."("
    self:map("lhs",
             function(n,i) 
               out = out..n:printprettys(root,self,"lhs"..i,assignments)..","
             end)
    out = out..")"

    out = out..self.op.."("
    self:map("rhs",
             function(n,i) 
               out = out..n:printprettys(root,self,"rhs"..i,assignments)..","
             end)
    out = out..")"
  elseif self.kind=="multiunary" then
    out = out..self.op.."("
    self:map("expr",
             function(n,i) 
               out = out..n:printprettys(root,self,"expr"..i,assignments)..","
             end)
    out = out..")"

  elseif self.kind=="unary" then
    out=out..self.op.."("..self.expr:printprettys(root,self,"expr",assignments)..")"
  elseif self.kind=="value" then
    out=out..tostring(self.value)
  elseif self.kind=="special" then
    out=out.."_special_"..self.id
  elseif self.kind=="position" then
    out=out..self.coord
  elseif self.kind=="cast" then
    out = out..self.expr:printprettys(root,self,"expr",assignments)
  elseif self.kind=="tap" then
    out=out.."_tap_"..self.tapname
  elseif self.kind=="tapLUTLookup" then
    out=out.."_tapLUT_"..self.tapname.."["..self.index:printprettys(root,self,"index",assignments).."]"
  elseif self.kind=="transformBaked" then
    out = out..self.expr:printprettys(root,self,"expr",assignments).."("

    local i=1
    while self["translate"..i] do
      out = out..orion.dimToCoord[i].."*"..self["scale"..i].."+"..self["translate"..i]
      if self["translate"..(i+1)] then out = out.."," end
      i=i+1
    end
    out = out..")"

  elseif self.kind=="reduce" then
    out = out .. self.op .. "("

    local i=1
    while self["expr"..i] do
      out = out..self["expr"..i]:printprettys(root,self,"expr"..i,assignments)
      if self["expr"..(i+1)] then out = out..",\n" end
      i=i+1
    end
    
    out = out  .. ")"
  elseif self.kind=="select" then
    out=out.."if "..self.cond:printprettys(root,self,"cond",assignments).." then "..self.a:printprettys(root,self,"a",assignments).." else "..self.b:printprettys(root,self,"b",assignments).." end"
  elseif self.kind=="vectorSelect" then
    out=out.."vectorSelect("..self.cond:printprettys(root,self,"cond",assignments)..","..self.a:printprettys(root,self,"a",assignments)..","..self.b:printprettys(root,self,"b",assignments)..")"
  elseif self.kind=="cropBaked" then
    out = out.."cropBaked("..self.expr:printprettys(root,self,"expr",assignments)..","..self.crop:printprettys()..")"
  elseif self.kind=="array" or
    self.kind=="toAOS" then

    out = out..self.kind.."("

    self:map("expr", 
             function(n,index)
               
               out = out..n:printprettys(root,self,"expr"..index,assignments)..", "
             end)

    out = out..")"
  elseif self.kind=="assert" then
    out = out.."assert("..self.expr:printprettys(root,self,"expr",assignments)..","..self.printval:printprettys(root,self,"printval",assignments)
    out = out..","..self.cond:printprettys(root,self,"cond",assignments)..")"
  elseif self.kind=="multiout" then
    out = out.."multiout("

    self:map("expr", 
             function(n,index)
               out = out..n:printprettys(root,self,"expr"..index,assignments)..", "
             end)

    out = out..")"
  elseif self.kind=="toSOA" then
    out = out.."toSOA("..self.special..","..self.index..")"
  elseif self.kind=="index" then
    out = out.."("..self.expr:printprettys(root,self,"expr",assignments)..")["..self.index.."]"
  elseif self.kind=="gather" then
    out = "gather(\n"..self.input:printprettys(root,self,"input",assignments)..",\n"
    out = out..self.x:printprettys(root,self,"x",assignments)..",\n"
    out = out..self.y:printprettys(root,self,"y",assignments)..",\n"
    out = out..tostring(self.maxX)..", "..tostring(self.maxY)..", "..tostring(self.clamp)..")"
  else
    print(self.kind)  
    assert(false)
  end

  -- decide if we should display this stored in a variable
  local displayInVar = false
  if true then
    -- CSE mode, only store stuff that has multiple parents
    displayInVar = (self:parentCount(root)>1)
  else

  end

  if displayInVar then
    assignments[self:name()] = out
    return self:name()
  end

  return out
end

function typedASTFunctions:printpretty()
  local assignments = {}
  local out = self:printprettys(self,nil,nil,assignments)
  for k,v in pairs(assignments) do print(k,"=",v) end
  print(out)
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

