local C = terralib.includecstring [[
#include <sys/time.h>

  double CurrentTimeInSecondsTTT() {
  struct timeval tv;
  gettimeofday(&tv, 0);
  return tv.tv_sec + tv.tv_usec / 1000000.0;
                                       }

                                   ]]

-- this is the source for the terra compiler for orion


-- if pointer is true, generate a pointer instead of a value
function orion.terracompiler.symbol(_type,pointer,vectorN)
  assert(orion.type.isType(_type))
  assert(vectorN==nil or type(vectorN)=="number")
  
  return symbol(orion.type.toTerraType(_type,pointer,vectorN))
end

function orion.terracompiler.vectorizeBinaryPointwise(func,lhs,rhs,V)
  assert(terralib.isfunction(func))
  assert(terralib.isquote(lhs))
  assert(terralib.isquote(rhs))
  assert(type(V)=="number")

  -- if we didn't store these in a variable, they may not be a vector type
  lhs = `[vector(float,V)](lhs)
  rhs = `[vector(float,V)](rhs)

  local q = {}
  for i=1,V do
    table.insert(q,`func(lhs[i-1],rhs[i-1]) )
  end
  assert(#q==V)
  
  return `vector(q)
end

orion.terracompiler.numberBinops={
  ["+"]=function(lhs,rhs) return `lhs+rhs end,
  ["-"]=function(lhs,rhs) return `lhs-rhs end,
  ["/"]=function(lhs,rhs) return `lhs/rhs end,
  ["*"]=function(lhs,rhs) return `lhs*rhs end,
  ["<"]=function(lhs,rhs) return `lhs<rhs end,
  ["<="]=function(lhs,rhs) return `lhs<=rhs end,
  [">"]=function(lhs,rhs) return `lhs>rhs end,
  [">="]=function(lhs,rhs) return `lhs>=rhs end,
  ["=="]=function(lhs,rhs) return `lhs==rhs end,
  ["~="]=function(lhs,rhs) return `lhs~=rhs end,
  ["%"]=function(lhs,rhs) return `lhs%rhs end,
  ["&"]=function(lhs,rhs) return `lhs and rhs end,
  ["<<"]=function(lhs,rhs) return `lhs<<rhs end,
  [">>"]=function(lhs,rhs) return `lhs>>rhs end,
  ["min"]=function(lhs,rhs) return `terralib.select(lhs<rhs,lhs,rhs) end,
  ["max"]=function(lhs,rhs) return `terralib.select(lhs>rhs,lhs,rhs) end,
  ["pow"]=function(lhs,rhs,V)
    return orion.terracompiler.vectorizeBinaryPointwise(cmath.pow,lhs,rhs,V)
  end
}


-- func should take a single scalar value, and return a single scalar value
function orion.terracompiler.vectorizeUnaryPointwise(func,expr,V)
  assert(terralib.isfunction(func))
  assert(terralib.isquote(expr))
  assert(type(V)=="number")

  local q = {}
  for i=1,V do
    table.insert(q,`func(expr[i-1]))
  end
  assert(#q==V)

  return `vector(q)
end

orion.terracompiler.numberUnary={
  ["floor"] = function(expr,ast,V)
    return orion.terracompiler.vectorizeUnaryPointwise(cmath.floor,expr,V)
  end,
  ["-"] = function(expr,ast,V) return `-expr end,
  ["not"] = function(expr,ast,V) return `not expr end,
  ["abs"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(cmath.fabs,expr,V)
    elseif orion.type.isUint(ast.type) then
      -- a uint is always positive
      return expr
    elseif ast.type==orion.type.int(32) or 
      ast.type==orion.type.int(64) or 
      ast.type==orion.type.int(16) then
      return orion.terracompiler.vectorizeUnaryPointwise(cstdlib.abs,expr,V)
    else
      ast.type:print()
      assert(false)
    end
  end,
  ["sin"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(cmath.sin,expr,V)
    else
      assert(false)
    end
  end,
  ["cos"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(  cmath.cos, expr, V )
    else
      assert(false)
    end
  end,
  ["exp"] = function(expr,ast,V)
    if ast.type==orion.type.float(32) then
      return orion.terracompiler.vectorizeUnaryPointwise(  cmath.exp, expr, V )
    else
      assert(false)
    end
  end,
  ["print"]= function(expr,ast,V)
    if V>1 then
      assert(false)
    else
      if node.expr.type==orion.type.float(32) then
        table.insert(stat,quote cstdio.printf("orion.printf:%f\n",expr) end)
      elseif node.expr.type==orion.type.uint(8) then
        table.insert(stat,quote cstdio.printf("orion.printd:%d\n",expr) end)
      else
        print(orion.type.typeToString(node.expr.type))
        assert(false)
      end
   end
  end
}

orion.terracompiler.boolBinops={
  ["and"]=function(lhs,rhs) return `lhs and rhs end,
  ["or"]=function(lhs,rhs) return `lhs or rhs end
}

function orion.terracompiler.codegen(
  inkernel, V, xsymb, ysymb, loopid, stripCount, retime)

  assert(type(loopid)=="number")
  assert(type(stripCount)=="number")
  assert(orion.flatIR.isFlatIR(inkernel))
  assert(type(retime)=="number")

  local stat = {}

  inkernel = orion.optimize.CSE(inkernel,{})

  local expr =inkernel:visitEach(
    function(node,inputs)


      local out
      local resultSymbol = orion.terracompiler.symbol(node.type, false, V)

      for k,v in node:children() do
        assert(terralib.isquote(inputs[k]))
      end
      
      if node.kind=="loadConcrete" then
        out = node.from:get(loopid, node.x,node.y,V,retime);
      elseif node.kind=="binop" then
        local lhs = inputs["lhs"]
        local rhs = inputs["rhs"]
        
        if orion.type.astIsNumber(node.lhs) and orion.type.astIsNumber(node.rhs) then
          
          if orion.terracompiler.numberBinops[node.op]==nil then
            orion.error("Unknown scalar op "..node.op)
          end
          
          out = orion.terracompiler.numberBinops[node.op](lhs,rhs,V)
        elseif orion.type.astIsBool(node.lhs) and orion.type.astIsBool(node.rhs) then
          
          if orion.terracompiler.boolBinops[node.op]==nil then
            orion.error("Unknown scalar bool op "..node.op)
          end
          
          out = orion.terracompiler.boolBinops[node.op](lhs,rhs)
        else
          print("Unknown/bad type to binop")
          print(orion.type.typeToString(orion.getType(node.lhs)))
          print(orion.type.typeToString(orion.getType(node.rhs)))
          os.exit()
        end
      elseif node.kind=="multibinop" then

        if node.op=="dot" then
          local dotresult

          node:map("lhs",
                   function(n,i)
                     if dotresult==nil then
                       dotresult = `([inputs["lhs"..i]]*[inputs["rhs"..i]])
                     else
                       dotresult = `dotresult + ([inputs["lhs"..i]]*[inputs["rhs"..i]])
                     end
                   end)

          out = dotresult
       else
          assert(false)
       end
      elseif node.kind=="multiunary" then
 
        if node.op=="arrayAnd" then

          local cmpresult

          node:map("expr",
                   function(n,i)
                     if cmpresult==nil then
                       cmpresult = inputs["expr"..i]
                     else
                       cmpresult = `cmpresult and [inputs["expr"..i]]
                     end
                   end)

          out = cmpresult
        else
          assert(false)
        end
      elseif node.kind=="unary" then
        local expr = inputs["expr"]
        
        if orion.terracompiler.numberUnary[node.op]==nil then
          orion.error("Unknown unary op "..node.op)
        end

        out = orion.terracompiler.numberUnary[node.op](expr,node.expr,V)
      elseif node.kind=="value" then
        out = `[orion.type.toTerraType(node.type,false,V)](node.value)
      elseif node.kind=="tap" then
        out = `@[&orion.type.toTerraType(node.type)](orion.runtime.getTap(node.id))
      elseif node.kind=="tapLUTLookup" then
        local index = inputs["index"]

        local q = {}
        for i=1,V do
          local debugQ = quote end

          if orion.debug then
            debugQ = quote orionAssert(index[i-1]>=0,"LUT index <0");
              if index[i-1]>=[node.count] then
                cstdio.printf("i %d, c %d\n",index[i-1],[node.count]);
                orionAssert(false,"LUT index >= count");
                end end
          end

          table.insert(q,quote debugQ in [&orion.type.toTerraType(node.type)](orion.runtime.getTapLUT(node.id))[index[i-1]] end)
        end
        out = `vector(q)
      elseif node.kind=="cast" then

        local input = inputs["expr"]

        if node.type==orion.type.float(32) then
        elseif node.type==orion.type.float(64) then
        elseif node.type==orion.type.uint(8) then
        elseif node.type==orion.type.int(8) then
        elseif node.type==orion.type.int(32) then
        elseif node.type==orion.type.int(64) then
        elseif node.type==orion.type.uint(32) then
        elseif node.type==orion.type.int(16) then
        elseif node.type==orion.type.uint(16) then
        elseif node.type==orion.type.bool() then
        else
          orion.error("Cast to "..orion.type.typeToString(node.type).." not implemented!")
          assert(false)
        end

        local ttype = orion.type.toTerraType(node.type,false, V)

        local expr = inputs["expr"]
        out = `ttype(expr)

      elseif node.kind=="select" or node.kind=="vectorSelect" then
        local cond = inputs["cond"]
        local a = inputs["a"]
        local b = inputs["b"]

        out = `terralib.select(cond,a,b)
      elseif node.kind=="position" then

        if node.coord=="x" then
          -- xsymb is a scalar, we need to convert it to a vector
          --out = `xsymb
          local tmp = {}
          for i=0,V-1 do table.insert(tmp,`xsymb+i) end
          out = `vector(tmp)
        elseif node.coord=="y" then
          out = `ysymb
        elseif node.coord=="z" then
          assert(false)
        else
          assert(false)
        end

      elseif node.kind=="assert" then
        local expr = inputs["expr"]

        if orion.debug then
          local cond = inputs["cond"]
          local printval = inputs["printval"]
      
          if node.printval.type==orion.type.float(32) then
            for i = 1,V do
              table.insert(stat,quote if cond[i-1]==false then 
                               cstdio.printf("ASSERT FAILED, value %f line %d x:%d y:%d\n",printval[i-1],[node:linenumber()],xsymb,ysymb);
                             cstdlib.exit(1); 
                                      end end)
            end
          elseif node.printval.type==orion.type.int(32) then
            for i = 1,V do
              table.insert(stat,quote if cond[i-1]==false then 
                               cstdio.printf("ASSERT FAILED, value %d file %s line %d x:%d y:%d\n",printval[i-1],[node:filename()],[node:linenumber()],xsymb,ysymb);
                             cstdlib.exit(1); 
                                      end end)
            end
          else
            assert(false)
          end
        end

        out = expr
      elseif node.kind=="reduce" then
    
        local list = node:map("expr",function(v,i) return inputs["expr"..i] end)
        assert(#list == node:arraySize("expr"))

        -- theoretically, a tree reduction is faster than a linear reduction
        -- starti and endi are both inclusive
        local function foldt(list,starti,endi)
          assert(type(list)=="table")
          assert(type(starti)=="number")
          assert(type(endi)=="number")
          assert(starti<=endi)
          if starti==endi then
            return list[starti]
          else
            local half = math.floor((endi-starti)/2)
            local lhs = foldt(list,starti,starti+half)
            local rhs = foldt(list,starti+half+1,endi)

            if node.op=="sum" then
              return `(lhs+rhs)
            elseif node.op=="min" then
              return `terralib.select(lhs<rhs,lhs,rhs)
            elseif node.op=="max" then
              return `terralib.select(lhs>rhs,lhs,rhs)
            else
              assert(false)
            end
          end
        end

        out = foldt(list,1,#list)
      elseif node.kind=="gatherConcrete" then
        local inpX = inputs["x"]
        local inpY = inputs["y"]

        -- remember: the CSE algorithm transforms everything to be from hackBL to hackTR
        local blX = node.translate1_hackBL
        local blY = node.translate2_hackBL
        local trX = node.translate1_hackTR
        local trY = node.translate2_hackTR

        out = node.from:gather( 
          loopid,  node.clamp, 
          inpX, inpY, 
          blX, blY, trX, trY,
          V,
          stripCount,
          retime);

      else
--        node:printpretty()
        orion.error("Internal error, unknown ast kind "..node.kind)
      end

      --print(node.kind)
      assert(terralib.isquote(out))

      -- make absolutely sure that we end up with the type we expect
      out = `[orion.type.toTerraType(node.type,false,V)](out)

      -- only make a statement if necessary
      if node:parentCount(inkernel)==1 then
        return out
      else
        table.insert(stat,quote var [resultSymbol] = out end)
        return `[resultSymbol]
      end
    end)

  assert(terralib.isquote(expr))
  
  if orion.printstage then
    print("terracompiler.codegen astNodes:",inkernel:S("*"):count()," statements:",#stat,inkernel:name())
  end

  return expr,stat
end

function orion.terracompiler.codegenStrip(core, strip, L, stripCount,y,stripList, stripSymbolCache)
  assert(type(stripList)=="table")
  assert(type(stripSymbolCache)=="table")

  local x = symbol(int)
  local kCount = L:arraySize("kernel")
  local loopStartCode = {}
  local loopCode = {}

  for i=1,kCount do
    local kernel = L["kernel"..i]
    local outputNeededRegion = L["outputNeededRegion"..i]
    local validRegion = L["validRegion"..i]
    local outputImage = L["outputImage"..i]

    local needed = symbol(Crop)
    local valid = symbol(Crop)
    local validSS = symbol(Crop)

    L:initInputImages(i);
    outputImage:init(i);

    local rtY = symbol()

    -- we need to call these upfront so that the LBs can remember which IVs were actually used. But after init.
    local expr1,statements1=orion.terracompiler.codegen( kernel,  1, x, rtY, i, stripCount, L["retime"..i] )
    local exprV,statementsV=orion.terracompiler.codegen( kernel,  orion.tune.V, x, rtY, i, stripCount, L["retime"..i] )

    table.insert(loopStartCode,
      quote
        if orion.verbose then cstdio.printf("Run Kernel %d retime %d\n", i, [L["retime"..i]]) end

        var [needed] = [outputNeededRegion:stripRuntime(strip, stripCount, stripList, stripSymbolCache)];

        -- these intersections need to happen at runtime b/c we don't know the width that 
        -- will be passed in
        var [valid] = [validRegion:intersectRuntime(needed)];
        var [validSS] = valid:shrinkToNearestX(orion.tune.V); -- the vectorized sweet spot
        [validSS]:assertXMod(orion.tune.V)

        [L:declareInputImages(i)];
        [outputImage:declare(i)];

        [outputImage:setPosition( i, `needed.left,`needed.bottom, core, strip, stripCount, stripList, stripSymbolCache,L["retime"..i])];
        [L:setInputImagePositions( i, `needed.left,`needed.bottom, core, strip, stripCount, stripList, stripSymbolCache,L["retime"..i])];

        if orion.verbose then
          cstdio.printf("valid:\n")
          valid:print()
          cstdio.printf("validSS:\n")
          validSS:print()
          cstdio.printf("needed:\n")
          needed:print()
        end
      end)


    table.insert(loopCode,
      quote
        var [rtY] = y - [L["retime"..i]]
        var bottom = [L.outputNeededRegionUnion:getBottom()]
        var top = [L.outputNeededRegionUnion:getTop()]

        if valid.width>0 then orionAssert(bottom<=valid.bottom, "VB") end
        if needed.width>0 then orionAssert(bottom<=needed.bottom, "NB") end
        if valid.width>0 then orionAssert(top>=valid.top, "VT") end
        if needed.width>0 then orionAssert(top>=needed.top, "NT") end


        if needed.width==0 then
          -- nothing to do!
        elseif valid.width==0 then
          -- we requested a region totally in the boundary

          if rtY >= needed.bottom and rtY < needed.top then
            for [x] = needed.left, needed.right do
              [outputImage:set( i, L:boundary(i), 1 )];
              [outputImage:next( i, 1 )];
              [L:inputImagesNext( i, 1 )];
            end
            [outputImage:nextLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [L:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end

        else
          
          -- top row(s) (all boundary)
          -- theoretically we could do some of this vectorized, but it shouldn't really matter
          if rtY >= needed.bottom and rtY < valid.bottom then
            for [x] = needed.left, needed.right do
              [outputImage:set( i, L:boundary(i), 1 )];
              [outputImage:next( i, 1 )];
              [L:inputImagesNext( i, 1 )];
            end
            [outputImage:nextLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [L:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end
        
          -- interior row(s), mixed boundary and calculated region
          if rtY >= valid.bottom and rtY <valid.top then
            for [x] = needed.left, valid.left do
              [outputImage:set(i,L:boundary(i),1)];
              [outputImage:next(i,1)];
              [L:inputImagesNext(i,1)];
            end
            

            if validSS.width<=0 or validSS.height<=0 then

              -- sweet spot is empty
              for [x] = valid.left, valid.right do
                statements1;
                [outputImage:set(i,expr1,1)];
                [outputImage:next(i,1)];
                [L:inputImagesNext(i,1)];
              end

            else
              
              for [x] = valid.left, validSS.left do
                statements1;
                [outputImage:set(i,expr1,1)];
                [outputImage:next(i,1)];
                [L:inputImagesNext(i,1)];
              end

              for [x] = validSS.left, validSS.right, orion.tune.V do
                statementsV;
                [outputImage:set(i,exprV,orion.tune.V)];
                [outputImage:next( i, orion.tune.V )];
                [L:inputImagesNext( i, orion.tune.V )];
              end

              for [x] = validSS.right, valid.right do
                statements1;
                [outputImage:set(i,expr1,1)];
                [outputImage:next( i, 1 )];
                [L:inputImagesNext( i, 1 )];
              end
            end

            for [x] = valid.right, needed.right do
              [outputImage:set( i, L:boundary(i), 1 )];
              [outputImage:next( i, 1 )];
              [L:inputImagesNext( i, 1 )];
            end
          
            [outputImage:nextLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [L:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end
        
          -- last row(s), all boundary
          -- theoretically we could do some of this vectorized, but it shouldn't really matter
          if rtY >= valid.top and rtY < needed.top then
--        cstdio.printf("D DOLINE %d %s\n", rtY,[kernel:name()])
            for [x] = needed.left, needed.right do
              [outputImage:set( i, L:boundary(i), 1 )];
              [outputImage:next( i, 1 )];
              [L:inputImagesNext( i, 1 )];
            end
            [outputImage:nextLine( i,  `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
            [L:nextInputImagesLine( i, `needed.right-needed.left, stripCount, rtY,L["retime"..i])];
          end
        end   
        --end

      end)
  end

 return loopStartCode, loopCode
end

-- L : the loop IR node
-- region: 0=all, 1=left, 2=middle, 3=right
-- actually, we can't specialize on the regions. Consider for ex if the whole
-- first strip is border, and so the middle strip still has a border section
function orion.terracompiler.toTerraKernel(
  L, stripCount)

  assert(orion.loopIR.isLoopIR(L))
  assert(type(stripCount)=="number")

  local core = symbol(int)

  local y = symbol(int)

  -- must be run before all the strips
  local terra preRun()
    if orion.verbose then cstdio.printf("Kernel %s Prerun\n", [L:name()]) end
    [L:map("outputImage",function(n) return n:alloc(stripCount) end)]
  end

  local loopCode = {}
  assert(stripCount % orion.tune.cores == 0)
  local stripsPerCore = math.floor(stripCount/orion.tune.cores)

  ------
  local strip = symbol(int)
  local stripList = {} -- cropIR nodes we need for this image
  local stripSymbolCache = {[stripCount]={[strip]={}}}
  local thisLoopStartCode, thisLoopCode = orion.terracompiler.codegenStrip(core,strip,L,stripCount, y, stripList, stripSymbolCache)

  if orion.printstage then
    print("strip list count",#stripList)
  end

  local terra inner([strip], [core])
    stripList

    thisLoopStartCode
    
    var bottom = [L.outputNeededRegionUnion:getBottom()]
    var top = [L.outputNeededRegionUnion:getTop()]
    
    var start = C.CurrentTimeInSecondsTTT()
    for [y] = bottom, top do
      thisLoopCode
    end
    var endt = C.CurrentTimeInSecondsTTT()

--    cstdio.printf("core time %f strip:%d core:%d\n",endt-start,strip,core)
  end

--  inner:printpretty(false)

--  local start = C.CurrentTimeInSecondsTTT()
--  inner:compile()
--  local endt = C.CurrentTimeInSecondsTTT()
--  print("inner compile time",(endt-start))

--  inner:disas()

  for i=0,stripsPerCore-1 do
    table.insert(loopCode, quote inner(core*stripsPerCore+i,core) end)
  end
  
  local terra result( input : &opaque ) : &opaque
    var [core] = @([&int](input))
    if orion.verbose then cstdio.printf("Run Loop %s\n", [L:name()]) end

    var start = C.CurrentTimeInSecondsTTT()
    loopCode
    var endt = C.CurrentTimeInSecondsTTT()

--    cstdio.printf("core %d time %f\n",core,endt-start)
  end

  local terra postRun()
    if orion.verbose then cstdio.printf("Kernel %s Postrun\n", [L:name()]) end

    -- for debugging
    if orion.debug or orion.debugimages then
      [
        (function()
           return L:map("outputImage", function(outputImage,i)
                          -- we can't write out LBs
                          if L["consumedExternally"..i] then
                          return quote
                            if orion.verbose then cstdio.printf("debug write out\n"); end
                            var img : Image;
                            [outputImage:toImage(img)]
                            var fn : int8[100]
                            cstdio.snprintf(fn,95,"./debugimg/reg_%s.jjm",[L:name()])
                            img:save(fn);
                            cstdio.printf("write out done\n")
                          end end end) end)()
      ]
    end

    [L:releaseInputImages()];
  end

  return result, preRun, postRun
end