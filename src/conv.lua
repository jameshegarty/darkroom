-- used to print out code for john's conv engine


orion.convolution = {}

orion.convolution.yamlTab = "    "
function orion.convolution.writeYAML(input,tab, firstLineTab)
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
    table.insert(output,t.."- "..orion.convolution.writeYAML(v,tab..orion.convolution.yamlTab, "  "))
        end

      else
        for k,v in pairs(input) do
          assert(type(k)=="string")
          table.insert(output,tab..k..": ")
          if type(v)=="table" and keycount(v)>0 then table.insert(output,"\n") end
          table.insert(output,orion.convolution.writeYAML(v,tab..orion.convolution.yamlTab))
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

orion.convolution_varnames={}
function orion.convolution.generateVarName(v)
  assert(type(v)=="string")

  if orion.convolution_varnames[v]==nil then
    orion.convolution_varnames[v]=0
    return v
  else
    orion.convolution_varnames[v] = orion.convolution_varnames[v] + 1
    return v.."_"..orion.convolution_varnames[v]
  end
end


function orion.convolution.declareVar(name, ty, extradims)
  assert(type(name)=="string")
  assert(orion.type.isType(ty))

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
      return "ufix_"..ty.over.precision.."_0 "..name..ed.."["..(orion.type.arrayLength(ty)-1)..":0]"
    elseif ty.over.type=="int" then
      return "fix_"..ty.over.precision.."_0 "..name..ed.."["..(orion.type.arrayLength(ty)-1)..":0]"
    elseif ty.over.type=="float" then
      return "float_"..ty.over.precision.."_0 "..name..ed.."["..(orion.type.arrayLength(ty)-1)..":0]"
    elseif ty.over.type=="bool" then
      return "bool "..name..ed.."["..(orion.type.arrayLength(ty)-1)..":0]"
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

function orion.convolution.codegenReduce(convIR,centroidX,centroidY, reduce_specialSeen, reduce_loadSeen)
  assert(orion.convIR.isConvIR(convIR))
  local ast = convIR.kernel
  assert(orion.flatIR.isFlatIR(ast))

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
          orion.error("Internal error, binopToJohn "..n.op)
        end

        local outname = n:name()
        local outStr = {}
        table.insert(stat, orion.convolution.declareVar(outname,n.type))
        table.insert(stat, binopToJohn[n.op].." "..outname.." "..lhsName.." "..rhsName)
    
        out = outname
      elseif n.kind=="unary" then
        local exprName = inputs["expr"]
	
        local unaryToJohn={["arrayAnd"]="all",["-"] = "inv",["abs"]="abs",["floor"]="floor",["not"]="not"}

        if unaryToJohn[n.op]==nil then
          orion.error("Internal error, unaryToJohn "..n.op)
        end

        out = n:name()
        table.insert(stat, orion.convolution.declareVar(out,n.type))
        table.insert(stat, unaryToJohn[n.op].." "..out.." "..exprName)

      elseif n.kind=="cast" then
        local exprName = inputs["expr"]
        local outname = n:name()
        local comment = " #cast "..orion.type.typeToString(n.expr.type).." to "..orion.type.typeToString(n.type)

        table.insert(stat,orion.convolution.declareVar(outname,n.type))

        if orion.type.isArray(n.type) and orion.type.isArray(n.expr.type)==false then
          table.insert(stat,"#broadcast")
          for i=0,orion.type.arrayLength(n.type)-1 do
            table.insert(stat,"mv "..outname.."["..i.."] "..exprName..comment)
          end
        elseif orion.type.isArray(n.type)==false and orion.type.isArray(n.expr.type) then
          assert(false)
        else
          table.insert(stat,"mv "..outname.." "..exprName..comment)
        end

        out = outname
      elseif n.kind=="value" then
        local outname = n:name()

        assert(orion.type.isArray(n.type)==false)

        local valstr = n.value


        table.insert(stat, orion.convolution.declareVar(outname,n.type))
        
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
    
        table.insert(stat, orion.convolution.declareVar(outname, n.type))

        if n.kind=="select" and orion.type.isArray(n.a.type) then
          assert(orion.type.arrayLength(n.a.type)==orion.type.arrayLength(n.b.type))
          
          -- condition of select is scalar in orion, but vector in DPDA
          local len = orion.type.arrayLength(n.a.type)
          table.insert(stat,orion.convolution.declareVar(condName.."_pack", n.cond.type, {orion.type.arrayLength(n.a.type)}))
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
      elseif n.kind=="specialConv" then
        local stencilSize = convIR.stencil:size()
	
        if reduce_specialSeen[n.id]==nil then
          table.insert(stat, orion.convolution.declareVar("special"..n.id.."_pp", n.type, {stencilSize[1], stencilSize[2]}))
          reduce_specialSeen[n.id]=1
        end

        local xp = n.x-convIR.stencil:min(1)
        local yp = convIR.stencil:max(2)-n.y
        assert(xp>=0)
        assert(yp>=0)

        out = "special"..n.id.."_pp["..xp.."]["..yp.."]"
      elseif n.kind=="reduce" then
        local outname = n:name()
        local exprCnt = n:arraySize("expr")

        if orion.type.isArray(n.type) then
          local arlen = orion.type.arrayLength(n.type)
          table.insert(stat,orion.convolution.declareVar(outname.."_pack", orion.type.arrayOver(n.type),{arlen,exprCnt}))
        else
          table.insert(stat,orion.convolution.declareVar(outname.."_pack",n.type,{exprCnt}))
        end

        table.insert(stat,orion.convolution.declareVar(outname, n.type))

        n:map("expr", 
              function(node,i)

                if orion.type.isArray(n.type) then
                  for ch=0,orion.type.arrayLength(n.type)-1 do
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
        table.insert(stat,orion.convolution.declareVar(outname, n.type))

        n:map("expr", 
              function(node,i)
                table.insert(stat, "mv "..outname.."["..(i-1).."] "..inputs["expr"..i].." # repack for array")
              end)

        out = outname
      elseif n.kind=="loadConv" then
        assert(n.from.kind=="conv")
        local stencilSize = convIR.stencil:size()

        local conv = n.from["conv"..n.index]
	
        if reduce_loadSeen[conv]==nil then
          table.insert(stat, orion.convolution.declareVar(conv.kernel:name().."_pp", n.type, {stencilSize[1], stencilSize[2]}))
          reduce_loadSeen[conv] = 1
        end
        
        local xp = n.x-convIR.stencil:min(1)
        local yp = convIR.stencil:max(2)-n.y
        assert(xp>=0)
        assert(yp>=0)

        out = conv.kernel:name().."_pp["..xp.."]["..yp.."]"
      elseif n.kind=="index" then
        local outname = n:name()
        table.insert(stat, orion.convolution.declareVar(outname, n.type))
        table.insert(stat, "mv "..outname.." "..inputs.expr.."["..n.index.."] #index")

        out = outname
      elseif n.kind=="tap" then
        if tapSeen[n]==nil then
          table.insert(stat, orion.convolution.declareVar("tap_"..n.tapname, n.type))
          tapSeen[n] = 1
        end

        out = "tap_"..n.tapname
      elseif n.kind=="tapLUTLookup" then
        if tapLUTSeen[n]==nil then
          table.insert(stat,"KLM "..orion.convolution.declareVar("tapLUT_"..n.tapname, n.type,{n.count}))
          tapLUTSeen[n] = 1
        end

        local outname = n:name()
        table.insert(stat, orion.convolution.declareVar(outname, n.type))
        table.insert(stat,"lookup "..outname.." tapLUT_"..n.tapname.." "..inputs["index"])
        out = outname
      elseif n.kind=="gather" then
        out = n:name()

        assert(n.input.kind=="loadConv")
        local conv = n.input.from["conv"..n.input.index]
        local gatherFrom = conv.kernel:name().."_pp"

        table.insert(stat, orion.convolution.declareVar(out.."_index", n.x.type, {2}))
        -- john specifies gather relative to the stencil coords instead of the centroid
        table.insert(stat, "add "..out.."_index[0] "..inputs.x.." "..centroidX)
        table.insert(stat, "add "..out.."_index[1] "..inputs.y.." "..centroidY)
        table.insert(stat, orion.convolution.declareVar(out, n.type))
        table.insert(stat, "gmux "..out.." "..out.."_index "..gatherFrom)
      elseif n.kind=="assert" then
        out = inputs["expr"]
      else
        orion.error("Don't know how to conv codegen kind "..n.kind)
      end

      assert(type(out)=="string")
      return out
    end)

  if usedCentroidPos then
    table.insert(stat,1,"fix_13_0 centroid_pos[1:0]")
  end

  return stat, finalname, usedCentroidPos
end

function orion.convolution.codegenMap(ast, map_specialSeen, map_loadSeen)
  assert(orion.flatIR.isFlatIR(ast))

  local res = {}

  ast:S("specialConv"):traverse(
    function(n)
      if map_specialSeen[n.id]==nil then
        table.insert(res,orion.convolution.declareVar("special"..n.id.."_pp",n.type))
        table.insert(res,orion.convolution.declareVar("special"..n.id,n.type))
        table.insert(res,"mv special"..n.id.."_pp special"..n.id)
        map_specialSeen[n.id]=1
      end
    end)

  ast:S("loadConv"):traverse(
    function(n)
      local conv = n.from["conv"..n.index]

      if map_loadSeen[conv]==nil then
        orion.convIR.isConvIR(conv)
        orion.flatIR.isFlatIR(conv.kernel)
        table.insert(res,orion.convolution.declareVar(conv.kernel:name(),n.type))
        table.insert(res,orion.convolution.declareVar(conv.kernel:name().."_pp",n.type))
        table.insert(res,"mv "..conv.kernel:name().."_pp "..conv.kernel:name())
        map_loadSeen[conv] = 1
      end
    end)


  return res
end


-- treats the ast as a single fused conv engine kernel
-- (other code should decide where to break up the kernels)
function orion.convolution.synthSingle(
  scheduledIR, 
  config, 
  isRoot, 
  base1, 
  base2)

  assert(orion.scheduledIR.isScheduledIR(scheduledIR))
  assert(scheduledIR.kind=="conv")

  print("synth single")
  print(collectgarbage("count"))

  local cfgname = scheduledIR:name()
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

  assert(scheduledIR:arraySize("conv")>0)

  scheduledIR:map("conv",
    function(convIR)
      assert(orion.convIR.isConvIR(convIR))

      if orion.verbose then
        convIR.kernel:printpretty()
      end
      
      print("Stencil:")
      convIR.stencil:print()
      
      local lstencilSize
      if convIR.stencil:area()==0 then
        lstencilSize = {1,1}
      else
        lstencilSize = convIR.stencil:size()
      end

      local lcentroidX, lcentroidY
      if convIR.stencil:area()==0 then
        lcentroidX = 0
        lcentroidY = 0
      else
        lcentroidX = -convIR.stencil:min(1)
        lcentroidY = convIR.stencil:max(2)
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
      local lmapop = orion.convolution.codegenMap(convIR.kernel, map_specialSeen, map_loadSeen)
      local lreduceop, finaloutname, lusedCentroidPos = orion.convolution.codegenReduce(convIR,centroidX,centroidY, reduce_specialSeen, reduce_loadSeen)

      -- hack: for stuff like values, special nodes, make sure 
      -- they get assigned to the output variable
      if finaloutname ~= convIR.kernel:name() then
        table.insert(lreduceop,orion.convolution.declareVar(convIR.kernel:name(), convIR.kernel.type))
        table.insert(lreduceop,"mv "..convIR.kernel:name().." "..finaloutname.." #rename output")
        finaloutname = convIR.kernel:name()
      end

      appendTable(mapop, lmapop)
      appendTable(reduceop, lreduceop)
      table.insert(outputNames, finaloutname)
      table.insert(outputDeclarations, orion.convolution.declareVar(finaloutname,convIR.kernel.type))
      usedCentroidPos = usedCentroidPos or lusedCentroidPos

      convIR.kernel:S("tap"):traverse(
        function(n)
          if tapsSeen[n.tapname]==nil then
            table.insert(tapList, orion.convolution.declareVar("tap_"..n.tapname, n.type)) 
            tapsSeen[n.tapname] = 1
          end
        end)

      convIR.kernel:S("specialConv"):traverse(
        function(n) 
          table.insert(inputNames, "special"..n.id) 
          table.insert(inputs, orion.convolution.declareVar("special"..n.id,n.type)) 
          table.insert(PixelPart, orion.convolution.declareVar("special"..n.id.."_pp", n.type)) 
          table.insert(RPixelPart, orion.convolution.declareVar("special"..n.id.."_pp", n.type,{stencilSize[1],stencilSize[2]})) 
        end)


    end)

  if usedCentroidPos then
    table.insert(RPixelPart, "fix_13_0 centroid_pos[1:0]")
  end

  scheduledIR:map("child",
    function(n) 
      assert(n.kind=="conv")
      -- get the name of the output node
      n:map("conv",
        function(convIR)
          table.insert(inputNames, convIR.kernel:name() )
          table.insert(inputs, orion.convolution.declareVar(convIR.kernel:name(), convIR.kernel.type)) 
          table.insert(PixelPart, orion.convolution.declareVar(convIR.kernel:name().."_pp", convIR.kernel.type)) 
          table.insert(RPixelPart, orion.convolution.declareVar(convIR.kernel:name().."_pp", convIR.kernel.type, {stencilSize[1], stencilSize[2]} )) 
        end)
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

  if orion.printstage then
    print("synthSingleDone")
  end
end

-- convert an acyclic pipeline to a straight pipe
function orion.convolution.straighten(convIR)
  assert(orion.scheduledIR.isScheduledIR(convIR))

  local specialNode
  convIR:S("*"):traverse(
    function(node)
      if node.kind=="conv" then
        node.conv1.kernel:S("specialConv"):traverse(
          function(n)
            if specialNode~=nil then 
              orion.error("only one input is supported1")
            end
            specialNode = node
          end)
      else
        assert(false)
      end
    end)

  assert(specialNode~=nil) -- need to have an input!

  -- greedily label the nodes based on distance from the input
  local pipe = {} -- distance -> {map of scheduledIR nodes at that distance}
  local distance = {} -- scheduledIR node -> distance
  local liveness = {} -- distance -> {map of scheduledIR nodes that need to be carried through}
  distance[specialNode] = 0
  pipe[0] = {}
  pipe[0][specialNode] = 1
  liveness[0] = {}

  convIR:S("*"):traverse(
    function(node)
      if node.kind=="conv" and node~=specialNode then
        local maxDist = 0
        for k,v in node:children() do
          if distance[v]+1 > maxDist then maxDist = distance[v]+1 end
        end
        distance[node] = maxDist
        if pipe[maxDist]==nil then pipe[maxDist] = {} end
        pipe[maxDist][node] = 1

        if liveness[maxDist]==nil then liveness[maxDist] = {} end

        -- store liveness
        for k,v in node:children() do
          if pipe[maxDist-1][v]==nil then
            liveness[maxDist-1][v] = 1
          end
        end
      end
    end)

  -- propagate liveness: figure out how long we need to keep something live
  local k = #pipe-1
  while k >= 0 do
    if liveness[k+1]~=nil then
      for n,_ in pairs(liveness[k+1]) do 
        if pipe[k][n]==nil then liveness[k][n] = 1 end
      end
    end
    k = k-1
  end

  for k=0,#pipe do
    print("stage",k)
    for kk,vv in pairs(pipe[k]) do print(" ",kk:name()) end
    print(" ","liveness")
    for kk,vv in pairs(liveness[k]) do print(" ",kk:name()) end
  end

  -- now rewrite the graph
  -- the scheduledIR has an ordered list "child" or inputs, and an ordered list "conv" of outputs
  local newScheduledIR = {kind="conv",conv1=specialNode.conv1}
  newScheduledIR = orion.scheduledIR.new(newScheduledIR):copyMetadataFrom(specialNode)
  newScheduledIR:setName("stage0")

  local indexMap = {} -- stageNumber -> orig scheuledIR node -> indexNumber
  indexMap[0] = {}
  indexMap[0][specialNode] = 1

  for stage=1,#pipe do
    print("generate stage",stage)
    local n = {kind="conv",child1=newScheduledIR}
    local d = 1
    for k,_ in pairs(pipe[stage]) do
      local conv = k.conv1:shallowcopy()
      conv.kernel = conv.kernel:S("loadConv"):process(
        function(n)
          local nn = n:shallowcopy()
          nn.index = indexMap[stage-1][n.from]
          nn.from = newScheduledIR
          assert(nn.index)
          assert(newScheduledIR["conv"..nn.index])
          return orion.flatIR.new(nn):copyMetadataFrom(n)
        end)

      n["conv"..d] = orion.convIR.new(conv):copyMetadataFrom(k.conv1)
      if indexMap[stage]==nil then indexMap[stage]={} end
      indexMap[stage][k] = d
      d = d+1
    end
    for k,_ in pairs(liveness[stage]) do
      -- make a new convIR that just passes through the value
      local flatIR = {kind="loadConv",x=0,y=0, from=newScheduledIR, index = indexMap[stage-1][k],type=k.conv1.kernel.type}
      assert(flatIR.index)
      assert(newScheduledIR["conv"..flatIR.index])
      flatIR = orion.flatIR.new(flatIR):setLinenumber(0):setOffset(0):setFilename("null_passthrough")
      flatIR:setName(k.conv1.kernel:name().."_passthrough")
      local conv = {kernel=flatIR, stencil=Stencil.new():add(0,0,0), validRegion=orion.cropIR.empty()}
      conv = orion.convIR.new(conv):setLinenumber(0):setOffset(0):setFilename("null_passthrough")

      n["conv"..d] = conv
      if indexMap[stage]==nil then indexMap[stage]={} end
      indexMap[stage][k] = d
      d = d+1
    end
    assert(d>1)
    newScheduledIR = orion.scheduledIR.new(n):copyMetadataFrom(newScheduledIR)

    -- adjust the stencil/centroid so that it's consistant for every output
    -- like, if the centroid was [0,0] but is now [4,4], offset the loads by [-4,-4]
    local function fixCentroid(node)
      local centroid = {0,0}
      local largestStencil = Stencil.new()
      node:map("conv",
        function(convIR)
          largestStencil = largestStencil:unionWith(convIR.stencil)
        end)
      -- now centroid holds the centroid we will use for this group of conv engines
      
      -- adjust all the engines
      local newnode = node:shallowcopy()
      node:map("conv",
        function(convIR,idx)
          local newConv = convIR:shallowcopy()
          newConv.stencil = largestStencil

          newConv = orion.convIR.new(newConv):copyMetadataFrom(convIR)
          newnode["conv"..idx] = newConv
        end)
      return orion.scheduledIR.new(newnode):copyMetadataFrom(node)
    end

    newScheduledIR = fixCentroid(newScheduledIR)
    newScheduledIR:setName("stage"..stage)
  end

  newScheduledIR:check()

  return newScheduledIR
end

function orion.convolution.synth(scheduledIR, base1, base2, options)
  assert(orion.scheduledIR.isScheduledIR(scheduledIR))
  assert(type(base1)=="number")
  assert(type(base2)=="number")
  assert(type(options)=="table")

  local specialSeen = {}
  scheduledIR:visitEach(
    function(n)
      if n.kind=="conv" then
        n.conv1.kernel:S("specialConv"):traverse(
          function(s)
            print("SEEN",s.id)
            specialSeen[s.id] = 1
          end)
      end
    end)

  if options.straighten then
    scheduledIR = orion.convolution.straighten(scheduledIR)
  end

  local config = {}
  config.top = {}
  config.top.input_image = {}

  for k,v in ipairs(orion._boundImages) do
    if specialSeen[k-1] then
      table.insert(config.top.input_image,"special"..(k-1))
    end
  end

  config.top.MaximumDomainSize = {256,256}
  config.top.MaximumTileSize = {256,256}

  -- traverse the kernel graph and synth each kernel
  -- also, calculate the total buffering and print it out
  local totalBuffering = 0
  local totalBufferingFused = 0
  scheduledIR:S("*"):traverse(
    function(node)
      if node.kind=="conv" then
        local isRoot = false
        if node:parentCount(scheduledIR)==0 then isRoot=true end
        orion.convolution.synthSingle(node, config, isRoot, base1, base2)

        assert(node:childrenCount()<=1)  -- currently we only support straight pipes
        local outputBytes = 0
        for k,v in node:children() do
          v:map("conv",function(convIR)
                  assert(orion.convIR.isConvIR(convIR))
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

      elseif node.kind=="multiout" then
        orion.error("multiout not supported in conv engine")
      else
        assert(false)
      end
  end)

  print("totalBuffering",totalBuffering)
  print("totalBufferingFused",totalBufferingFused)

  if scheduledIR.kind=="conv" then
    -- should only have one output! (just to make things easier)
    assert(scheduledIR.conv2==nil)

    config.top.output_image = {scheduledIR.conv1.kernel:name()}
  elseif scheduledIR.kind=="multiout" then
  else
    assert(false)
  end

  local runconfig = {}

  runconfig.test_top={}
  runconfig.test_top.inputImages = {}

  for k,v in ipairs(orion._boundImages) do
    if type(v.filename)=="string" and specialSeen[k-1] then
      runconfig.test_top.inputImages["special"..(k-1)] = "/horowitz/users/jhegarty/Stanford-SEEC-Convolution-Engine/orion/tests/"..v.filename
    else
      -- this was constructed in memory by a function. dno what to do here.
    end
  end

  -- these are reference images
  -- for now, just use the input. it will throw errors
  runconfig.test_top.bindings = {}

  if scheduledIR.kind=="conv" then
    -- should only have one output! (just to make things easier)
    assert(scheduledIR.conv2==nil)

    runconfig.test_top.bindings[scheduledIR.conv1.kernel:name()] = runconfig.test_top.inputImages.special0
  elseif scheduledIR.kind=="multiout" then
  else
    assert(false)
  end

  if orion.printstage then
    print("save taps")
  end

  runconfig.test_top.taps = {}
  scheduledIR:S("*"):traverse(
    function(node)
      if node.kind=="conv" then
        node:map("conv",
          function(convIR)
            convIR.kernel:S("tap"):traverse(
              function(n)
                local name = "tap_"..n.tapname.."_"..node:name()
                local value = orion.getTap(n)
                runconfig.test_top.taps[name] = value
              end)
          end)
      end
    end)
  
  if orion.printstage then
    print("save taps done")
  end

  return orion.convolution.writeYAML(config), orion.convolution.writeYAML(runconfig)
end