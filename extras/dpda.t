local dpda = {}

local yamlTab = "    "
local function writeYAML(input,tab, firstLineTab)
  if tab==nil then tab="" end
  if firstLineTab==nil then firstLineTab = tab end

  local output = {}

  -- check if this guy is a list
  if type(input)=="table" then
    if keycount(input)==0 then
      table.insert(output,"[]\n")
    else
      local isList = true
      for k,v in pairs(input) do 
        if type(k)~="number" then isList=false end
      end
    
      if isList then

        for k,v in ipairs(input) do
	  local t = tab
	  if k==1 then t=firstLineTab end
    table.insert(output,t.."- "..writeYAML(v,tab..yamlTab, "  "))
        end

      else
        for k,v in pairs(input) do
          assert(type(k)=="string")
          table.insert(output,tab..k..": ")
          if type(v)=="table" and keycount(v)>0 then table.insert(output,"\n") end
          table.insert(output,writeYAML(v,tab..yamlTab))
        end
      end
    end
  elseif type(input)=="string" then
    table.insert(output,input.."\n")
  elseif type(input)=="number" then
    table.insert(output,input.."\n")
  else
    print(type(input))
    assert(false)
  end

  return table.concat(output)
end

local convolution_varnames={}
local function generateVarName(v)
  assert(type(v)=="string")

  if darkroom.convolution_varnames[v]==nil then
    darkroom.convolution_varnames[v]=0
    return v
  else
    darkroom.convolution_varnames[v] = darkroom.convolution_varnames[v] + 1
    return v.."_"..darkroom.convolution_varnames[v]
  end
end


local function declareVar(name, ty, extradims)
  assert(type(name)=="string")
  assert(darkroom.type.isType(ty))

  local ed = ""

  if extradims~=nil then
    for k,v in ipairs(extradims) do
      assert(v>0)
      ed = ed.."["..(v-1)..":0]"
    end
  end

  if ty.type=="int" then
    return "fix_"..ty.precision.."_0 "..name..ed
  elseif ty.type=="uint" then
    return "ufix_"..ty.precision.."_0 "..name..ed
  elseif ty.type=="float" then
    if ty.precision==32 then
      return "float_23_8 "..name..ed
    else
      assert(false)
    end
  elseif ty.type=="bool" then
    return "bool "..name..ed
  elseif ty.type=="array" then
    if ty.over.type=="uint" then
      return "ufix_"..ty.over.precision.."_0 "..name..ed.."["..(darkroom.type.arrayLength(ty)-1)..":0]"
    elseif ty.over.type=="int" then
      return "fix_"..ty.over.precision.."_0 "..name..ed.."["..(darkroom.type.arrayLength(ty)-1)..":0]"
    elseif ty.over.type=="float" then
      return "float_"..ty.over.precision.."_0 "..name..ed.."["..(darkroom.type.arrayLength(ty)-1)..":0]"
    elseif ty.over.type=="bool" then
      return "bool "..name..ed.."["..(darkroom.type.arrayLength(ty)-1)..":0]"
    else
      print("Internal error, convDeclareVar unknown array type "..ty.over.type)
      assert(false)
    end
  end

  print("Internal error, convDeclareVar unknown type "..ty.type)
  assert(false)
end

-- returns codeString, resultName (codeString is the code that generates resultName)
-- codeString always ends in a newline

local function codegenReduce( kernelGraph, centroidX, centroidY, reduce_specialSeen, reduce_loadSeen )
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
  local ast = kernelGraph.kernel
  assert(darkroom.typedAST.isTypedAST(ast))

  local stat = {}
  
  -- don't declare vars multiple times
  local tapSeen = {}
  local tapLUTSeen = {}
  local usedCentroidPos = false

  local finalname = ast:visitEach(
    function(n, inputs)

      local out
      if n.kind=="binop" then
        local lhsName = inputs["lhs"]
        local rhsName = inputs["rhs"]
    
        local binopToJohn={["+"] = "add", ["%"]="mod", ["=="]="eq",["or"]="or",["and"]="and", ["-"] = "sub",["&"]="and",["max"]="max2",["min"]="min2",
                           ["/"]="div",["dot"]="inner",["pow"]="pow",["*"]="mult",[">>"]="rshift",["<<"]="lshift",[">"]="gt",["<"]="lt",["~="]="ne",[">="]="gte",["<="]="lte"}

        if binopToJohn[n.op]==nil then
          darkroom.error("Internal error, binopToJohn "..n.op)
        end

        local outname = n:name()
        local outStr = {}
        table.insert(stat, declareVar(outname,n.type))
        table.insert(stat, binopToJohn[n.op].." "..outname.." "..lhsName.." "..rhsName)
    
        out = outname
      elseif n.kind=="unary" then
        local exprName = inputs["expr"]
	
        local unaryToJohn={["arrayAnd"]="all",["-"] = "inv",["abs"]="abs",["floor"]="floor",["not"]="not"}

        if unaryToJohn[n.op]==nil then
          darkroom.error("Internal error, unaryToJohn "..n.op)
        end

        out = n:name()
        table.insert(stat, declareVar(out,n.type))
        table.insert(stat, unaryToJohn[n.op].." "..out.." "..exprName)

      elseif n.kind=="cast" then
        local exprName = inputs["expr"]
        local outname = n:name()
        local comment = " #cast "..darkroom.type.typeToString(n.expr.type).." to "..darkroom.type.typeToString(n.type)

        table.insert( stat, declareVar(outname,n.type) )

        if darkroom.type.isArray(n.type) and darkroom.type.isArray(n.expr.type)==false then
          table.insert(stat,"#broadcast")
          for i=0,darkroom.type.arrayLength(n.type)-1 do
            table.insert(stat,"mv "..outname.."["..i.."] "..exprName..comment)
          end
        elseif darkroom.type.isArray(n.type)==false and darkroom.type.isArray(n.expr.type) then
          assert(false)
        else
          table.insert(stat,"mv "..outname.." "..exprName..comment)
        end

        out = outname
      elseif n.kind=="value" then
        local outname = n:name()

        assert(darkroom.type.isArray(n.type)==false)

        local valstr = n.value


        table.insert(stat, declareVar(outname,n.type))
        
        if type(valstr)=="number" then
          table.insert(stat, "mv "..outname.." "..valstr.." #value")
        elseif type(valstr)=="boolean" then
          if valstr then
            table.insert(stat, "mv "..outname.." 0x1 #value")
          else
            table.insert(stat, "mv "..outname.." 0x0 #value")
          end
        else
          assert(false)
        end

        out = outname
      elseif n.kind=="select" or n.kind=="vectorSelect" then
        local condName = inputs["cond"]
        local aName = inputs["a"]
        local bName = inputs["b"]
    
        local outname = n:name()
    
        table.insert(stat, declareVar(outname, n.type))

        if n.kind=="select" and darkroom.type.isArray(n.a.type) then
          assert(darkroom.type.arrayLength(n.a.type)==darkroom.type.arrayLength(n.b.type))
          
          -- condition of select is scalar in orion, but vector in DPDA
          local len = darkroom.type.arrayLength(n.a.type)
          table.insert(stat,declareVar(condName.."_pack", n.cond.type, {darkroom.type.arrayLength(n.a.type)}))
          for i=1,len do
            table.insert(stat, "mv "..condName.."_pack["..(i-1).."] "..inputs["cond"].." # broadcast condition")
          end
          condName = condName.."_pack"
        end

        table.insert(stat, "mux "..outname.." "..condName.." "..aName.." "..bName)
    
        out = outname
      elseif n.kind=="position" then
        local popindex={x=0,y=1,z=2}
        usedCentroidPos = true
        out = "centroid_pos["..popindex[n.coord].."]"
      elseif n.kind=="reduce" then
        local outname = n:name()
        local exprCnt = n:arraySize("expr")

        if darkroom.type.isArray(n.type) then
          local arlen = darkroom.type.arrayLength(n.type)
          table.insert(stat,declareVar(outname.."_pack", darkroom.type.arrayOver(n.type),{arlen,exprCnt}))
        else
          table.insert(stat,declareVar(outname.."_pack",n.type,{exprCnt}))
        end

        table.insert(stat,declareVar(outname, n.type))

        n:map("expr", 
              function(node,i)

                if darkroom.type.isArray(n.type) then
                  for ch=0,darkroom.type.arrayLength(n.type)-1 do
                    table.insert(stat, "mv "..outname.."_pack["..ch.."]["..(i-1).."] "..inputs["expr"..i].."["..ch.."] # repack for reduce op")
                  end
                else
                  table.insert(stat, "mv "..outname.."_pack["..(i-1).."] "..inputs["expr"..i].." # repack for reduce op")
                end
              end)

        -- in orion, a reduce takes N inputs
        -- we need to pack those N into a vector before we sum them

        if n.op=="sum" then
          table.insert(stat,"sum "..outname.." "..outname.."_pack")
        elseif n.op=="min" then
          table.insert(stat,"min "..outname.." "..outname.."_pack")
        elseif n.op=="max" then
          table.insert(stat,"max "..outname.." "..outname.."_pack")
        else
          assert(false)
        end
        
        out = outname

      elseif n.kind=="array" then
        local outname = n:name()
        local exprCnt = n:arraySize("expr")
        table.insert(stat,declareVar(outname, n.type))

        n:map("expr", 
              function(node,i)
                table.insert(stat, "mv "..outname.."["..(i-1).."] "..inputs["expr"..i].." # repack for array")
              end)

        out = outname
      elseif n.kind=="load" then

        if type(n.from)=="number" then
          local stencilSize = kernelGraph.kernel:stencil():size()
	
          if reduce_specialSeen[n.id]==nil then
            table.insert(stat, declareVar("input"..n.from.."_pp", n.type, {stencilSize[1], stencilSize[2]}))
            reduce_specialSeen[n.from]=1
          end
          
          local xp = n.relX-kernelGraph.kernel:stencil():min(1)
          local yp = kernelGraph.kernel:stencil():max(2)-n.relY
          assert(xp>=0)
          assert(yp>=0)
          
          out = "input"..n.from.."_pp["..xp.."]["..yp.."]"
          
        else
          local stencilSize = kernelGraph.kernel:stencil():size()
          
          local conv = n.from
          
          if reduce_loadSeen[conv]==nil then
            table.insert(stat, declareVar(conv.kernel:name().."_pp", n.type, {stencilSize[1], stencilSize[2]}))
            reduce_loadSeen[conv] = 1
          end

          local rx = n.relX

          if type(rx)=="table" then
          rx = rx:eval(1)
          assert(rx:area()==1)
          rx = rx:min(1)
          end

          local ry = n.relY
          if type(ry)=="table" then
          ry = ry:eval(1)
          assert(ry:area()==1)
          ry = ry:min(1)
          end

          local xp = rx-kernelGraph.kernel:stencil():min(1)
          local yp = kernelGraph.kernel:stencil():max(2)-ry
          assert(xp>=0)
          assert(yp>=0)

          out = conv.kernel:name().."_pp["..xp.."]["..yp.."]"
        end
      elseif n.kind=="index" then
        local outname = n:name()

        local range = n.index:eval(1)
        print("RA",range:area())
        assert(range:area()==1)

        table.insert(stat, declareVar(outname, n.type))
        table.insert(stat, "mv "..outname.." "..inputs.expr.."["..range:min(1).."] #index")

        out = outname
      elseif n.kind=="tap" then
        if tapSeen[n]==nil then
          table.insert(stat, declareVar("tap_"..n.id, n.type))
          tapSeen[n] = 1
        end

        out = "tap_"..n.id
      elseif n.kind=="tapLUTLookup" then
        if tapLUTSeen[n]==nil then
          table.insert(stat,"KLM "..declareVar("tapLUT_"..n.id, n.type,{n.count}))
          tapLUTSeen[n] = 1
        end

        local outname = n:name()
        table.insert(stat, declareVar(outname, n.type))
        table.insert(stat,"lookup "..outname.." tapLUT_"..n.id.." "..inputs["index"])
        out = outname
      elseif n.kind=="gather" then
        out = n:name()

        assert(n.input.kind=="load")
        local conv = n.input.from
        local gatherFrom = conv.kernel:name().."_pp"

        table.insert(stat, declareVar(out.."_index", n.x.type, {2}))
        -- john specifies gather relative to the stencil coords instead of the centroid
        table.insert(stat, "add "..out.."_index[0] "..inputs.x.." "..centroidX)
        table.insert(stat, "add "..out.."_index[1] "..inputs.y.." "..centroidY)
        table.insert(stat, declareVar(out, n.type))
        table.insert(stat, "gmux "..out.." "..out.."_index "..gatherFrom)
      elseif n.kind=="assert" then
        out = inputs["expr"]
      elseif n.kind=="crop" then
        out = inputs["expr"]
      else
        darkroom.error("Don't know how to conv codegen kind "..n.kind)
      end

      assert(type(out)=="string")
      return out
    end)

  if usedCentroidPos then
    table.insert(stat,1,"fix_13_0 centroid_pos[1:0]")
  end

  return stat, finalname, usedCentroidPos
end

local function codegenMap( ast, map_specialSeen, map_loadSeen )
  assert(darkroom.typedAST.isTypedAST(ast))

  local res = {}

  ast:S("specialConv"):traverse(
    function(n)
      if map_specialSeen[n.id]==nil then
        table.insert(res,declareVar("special"..n.id.."_pp",n.type))
        table.insert(res,declareVar("special"..n.id,n.type))
        table.insert(res,"mv special"..n.id.."_pp special"..n.id)
        map_specialSeen[n.id]=1
      end
    end)

  ast:S("loadConv"):traverse(
    function(n)
      local conv = n.from["conv"..n.index]

      if map_loadSeen[conv]==nil then
        darkroom.convIR.isConvIR(conv)
        darkroom.flatIR.isFlatIR(conv.kernel)
        table.insert(res,declareVar(conv.kernel:name(),n.type))
        table.insert(res,declareVar(conv.kernel:name().."_pp",n.type))
        table.insert(res,"mv "..conv.kernel:name().."_pp "..conv.kernel:name())
        map_loadSeen[conv] = 1
      end
    end)


  return res
end


-- treats the ast as a single fused conv engine kernel
-- (other code should decide where to break up the kernels)
local function synthSingle(
  kernelGraph, 
  config, 
  isRoot)

  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))

  print("synth single")
  print(collectgarbage("count"))

  local cfgname = kernelGraph:name()
  config[cfgname.."_cfg"] = {}

  local mapop = {}
  local reduceop = {}
  local outputNames = {}
  local outputDeclarations = {}
  local usedCentroidPos = false

  local tapsSeen= {}
  local tapLUTsSeen= {}
  local tapList = {}

  local map_specialSeen = {}
  local map_loadSeen = {}

  local reduce_specialSeen = {}
  local reduce_loadSeen = {}

  local inputNames = {}
  local inputs = {}
  local PixelPart = {}
  local RPixelPart = {}

  local stencilSize
  local centroidX
  local centroidY

  print("Stencil:")
  kernelGraph.kernel:stencil():print()
      
  local lstencilSize
  if kernelGraph.kernel:stencil():area()==0 then
    lstencilSize = {1,1}
  else
    lstencilSize = kernelGraph.kernel:stencil():size()
  end

  local lcentroidX, lcentroidY
  if kernelGraph.kernel:stencil():area()==0 then
    lcentroidX = 0
    lcentroidY = 0
  else
    lcentroidX = -kernelGraph.kernel:stencil():min(1)
    lcentroidY = kernelGraph.kernel:stencil():max(2)
    assert(lcentroidX>=0)
    assert(lcentroidY>=0)
  end
  
  if stencilSize and 
    (lstencilSize[1]~=stencilSize[1] or 
     lstencilSize[2]~=stencilSize[2]) then
      assert(false)
  end
  
  if centroidX and
    (centroidX~=lcentroidX or centroidY~=lcentroidY) then
    assert(false)
  end
  
  stencilSize = lstencilSize
  centroidX = lcentroidX
  centroidY = lcentroidY
  
  -- now produce the map/reduce operators
  local lmapop = codegenMap( kernelGraph.kernel, map_specialSeen, map_loadSeen)
  local lreduceop, finaloutname, lusedCentroidPos = codegenReduce( kernelGraph, centroidX, centroidY, reduce_specialSeen, reduce_loadSeen )

  -- hack: for stuff like values, special nodes, make sure 
  -- they get assigned to the output variable
  if finaloutname ~= kernelGraph.kernel:name() then
    table.insert(lreduceop, declareVar(kernelGraph.kernel:name(), kernelGraph.kernel.type))
    table.insert(lreduceop,"mv "..kernelGraph.kernel:name().." "..finaloutname.." #rename output")
    finaloutname = kernelGraph.kernel:name()
  end

  appendTable(mapop, lmapop)
  appendTable(reduceop, lreduceop)
  table.insert(outputNames, finaloutname)
  table.insert(outputDeclarations, declareVar( finaloutname, kernelGraph.kernel.type ))
  usedCentroidPos = usedCentroidPos or lusedCentroidPos

  kernelGraph.kernel:S("tap"):traverse(
    function(n)
      if tapsSeen[n.tapname]==nil then
        table.insert(tapList, declareVar("tap_"..n.id, n.type)) 
        tapsSeen[n.id] = 1
      end
    end)
  
  kernelGraph.kernel:S("specialConv"):traverse(
    function(n) 
      table.insert(inputNames, "special"..n.id) 
      table.insert(inputs, declareVar("special"..n.id,n.type)) 
      table.insert(PixelPart, declareVar("special"..n.id.."_pp", n.type)) 
      table.insert(RPixelPart, declareVar("special"..n.id.."_pp", n.type,{stencilSize[1],stencilSize[2]})) 
    end)



  if usedCentroidPos then
    table.insert(RPixelPart, "fix_13_0 centroid_pos[1:0]")
  end

  kernelGraph:map("child",
    function(n) 
      -- get the name of the output node
      table.insert(inputNames, n.kernel:name() )
      table.insert(inputs, declareVar(n.kernel:name(), n.kernel.type)) 
      table.insert(PixelPart, declareVar(n.kernel:name().."_pp", n.kernel.type)) 
      table.insert(RPixelPart, declareVar(n.kernel:name().."_pp", n.kernel.type, {stencilSize[1], stencilSize[2]} )) 
    end)

  -- write cfg file
  config.top[cfgname] = {}
  config.top[cfgname].PixelIn=inputNames
  config.top[cfgname].PixelOut=outputNames
  config.top[cfgname].RunTimeResize = 0
  config.top[cfgname].IfcCtl = 0
  config.top[cfgname].MaximumDomainSize = {256,256}
  config.top[cfgname].MaximumTileSize = {256,256}
  config.top[cfgname].Config = cfgname.."_cfg"

  config[cfgname.."_cfg"].Name = cfgname.."_cfg"
  config[cfgname.."_cfg"].StencilIn = {stencilSize[1],stencilSize[2]}
  config[cfgname.."_cfg"].Centroid = {centroidX, centroidY}
  config[cfgname.."_cfg"].StencilOut = {1,1}
  config[cfgname.."_cfg"].Stride = {1,1}

  config[cfgname.."_cfg"].InputBoundary = "zero-pad"
  
  config[cfgname.."_cfg"].PixelIn = inputs

  -- this is the pixels produced by the map operator
  -- for now, this is all the inputs (either from 
  -- other kernels, or from special inputs)
  config[cfgname.."_cfg"].PixelPart = PixelPart

  -- same as PixelPart, but lifted to their vector form
  config[cfgname.."_cfg"].RPixelPart = RPixelPart
  
  -- Output pixel. always 1 in our system for now
  config[cfgname.."_cfg"].PixelOut = outputDeclarations

  -- taps used in the map
  config[cfgname.."_cfg"].Tap = {}

  -- taps used in the reduce
  config[cfgname.."_cfg"].RTap = tapList
  config[cfgname.."_cfg"].Map = cfgname.."_map"
  config[cfgname.."_cfg"].Reduce = cfgname.."_reduce"

  config[cfgname.."_map"] = {mapop}
  config[cfgname.."_reduce"] = {reduceop}

  if darkroom.printstage then
    print("synthSingleDone")
  end
end

local function synth( inputs, kernelGraph )
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))

  local config = {}
  config.top = {}
  config.top.input_image = {}

  for k,v in pairs(inputs) do
    table.insert(config.top.input_image,"special"..(k.id))
  end

  config.top.MaximumDomainSize = {256,256}
  config.top.MaximumTileSize = {256,256}

  -- traverse the kernel graph and synth each kernel
  -- also, calculate the total buffering and print it out
  local totalBuffering = 0
  local totalBufferingFused = 0
  kernelGraph:S("*"):traverse(
    function(node)
        local isRoot = false
        if node:parentCount(kernelGraph)==0 then isRoot=true end
        synthSingle(node, config, isRoot)

        if node:inputCount() >1 then
          darkroom.error("Only straight pipes are currently supported",node.kernel:linenumber(),node.kernel:offset(),node.kernel:filename())
        end

        local outputBytes = 0
        for k,v in node:inputs() do
          v:map("conv",function(convIR)
                  assert(darkroom.convIR.isConvIR(convIR))
                  local IB = convIR.kernel.type:sizeof()
                  print("IB",IB)
                  outputBytes = outputBytes + IB
                       end)
        end

        local cfgname = node:name()
        print("kernel",cfgname,"inputBytes",outputBytes)
        local stencilSize = config[cfgname.."_cfg"].StencilIn[2]
        print("stencil",stencilSize)

        totalBuffering = totalBuffering + stencilSize*outputBytes

        if stencilSize>0 then
          totalBufferingFused = totalBufferingFused + (stencilSize-1)*outputBytes
        end

  end)

  print("totalBuffering",totalBuffering)
  print("totalBufferingFused",totalBufferingFused)

  config.top.output_image = {kernelGraph.kernel:name()}

  local runconfig = {}

  runconfig.test_top={}
  runconfig.test_top.inputImages = {}

  -- these are reference images
  -- for now, just use the input. it will throw errors
  runconfig.test_top.bindings = {}

  runconfig.test_top.bindings[kernelGraph.kernel:name()] = runconfig.test_top.inputImages.special0

  runconfig.test_top.taps = {}
  kernelGraph:S("*"):traverse(
    function(node)
            kernelGraph.kernel:S("tap"):traverse(
              function(n)
                local name = "tap_"..n.id.."_"..node:name()
                local value = 0
                runconfig.test_top.taps[name] = value
              end)
    end)
  
  if darkroom.printstage then
    print("save taps done")
  end

  return writeYAML(config), writeYAML(runconfig)
end

function dpda.compile( inputs, outputs, taps )

  if #outputs > 1 then
    darkroom.error("multiple outputs not supported by dpda")
  end

  local output = outputs[1]

  local kernelGraph = darkroom.frontEnd( output, {} )

  local oldToNewRemap = {}
  local newToOldRemap = {}
  kernelGraph = kernelGraph:S("*"):process(
    function(kernelGraphNode, orig)
      local newKernelGraphNode = kernelGraphNode:shallowcopy()

        -- eliminate transforms
        -- this is wildly inefficient in general. But its only slow in corner cases b/c it is uncommon to have transforms within kernel graph nodes
        newKernelGraphNode.kernel = kernelGraphNode.kernel:S("transformBaked"):process(
          function(n)
            return n.expr:S(function(n) return n.kind=="load" or n.kind=="position" end):process(
              function(nn)
                if nn.kind=="load" then
                  local r = nn:shallowcopy()                  

                  r.relX = synthRel(r.relX, n.translate1)
                  r.relY = synthRel(r.relY, n.translate2)

                  if type(nn.from)=="table" then r.from = oldToNewRemap[nn.from]; assert(r.from~=nil) end
                  return darkroom.typedAST.new(r):copyMetadataFrom(nn)
                elseif nn.kind=="position" then

                  local res = {kind="binop", lhs=nn, type = nn.type, op="+"}

                  if nn.coord=="x" then
                    res.rhs = darkroom.typedAST._toTypedAST(n.translate1)
                  elseif nn.coord=="y" then
                    res.rhs = darkroom.typedAST._toTypedAST(n.translate2)
                  else
                    assert(false)
                  end

                  return darkroom.typedAST.new(res):copyMetadataFrom(nn)
                else
                  assert(false)
                end
              end)
          end)

        local res = darkroom.kernelGraph.new(newKernelGraphNode):copyMetadataFrom(kernelGraphNode)
        oldToNewRemap[orig] = res
        oldToNewRemap[res] = res -- remember, we might touch nodes multiple times
        newToOldRemap[res] = orig
        newToOldRemap[orig] = orig
        return res
    end)

  return synth( inputs, kernelGraph )
end

return dpda