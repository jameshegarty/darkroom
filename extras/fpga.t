local fpga = {}
fpga.util = require("fpgautil")
fpga.modules = require("fpgamodules")

BRAM_SIZE_BYTES = 2048

function fpga.codegenKernel( kernel, inputLinebufferFifos, imageWidth, imageHeight )
  assert(darkroom.kernelGraph.isKernelGraph(kernel))
  assert(type(inputLinebufferFifos)=="table")
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")

  local x,y = systolic.input("x",int16), systolic.input("y",int16)

  local moduleInputs = {x,y}
  local datasourceToInput = {}
  map( inputLinebufferFifos, function(v,k) table.insert(moduleInputs,systolic.input(v.key,v.type)); datasourceToInput[k] = moduleInputs[#moduleInputs]:read() end )


  local systolicFn = kernel.kernel:visitEach(
    function( node, inputs)
      if node.kind=="load" then
        return systolic.index( datasourceToInput[node.from], {inputs.relX, inputs.relY})
      elseif node.kind=="crop" then
        local cond = systolic.__or(systolic.ge(x:read()-systolic.cast(node.shiftX,int16),systolic.cast(imageWidth,int16)),systolic.ge(y:read()-systolic.cast(node.shiftY,int16),systolic.cast(imageHeight,int16)))
        assert(systolicAST.isSystolicAST(cond))
        assert(systolicAST.isSystolicAST(inputs.expr))
        return systolic.select(cond , systolic.cast(0,inputs.expr.type), inputs.expr )
      else
        local n = node:shallowcopy()
        for k,v in node:inputs() do
          n[k] = inputs[k]
        end
        return systolicAST.new(n):copyMetadataFrom(node)
      end
    end)

  local out = systolic.output( "out", kernel.kernel.type )
  local kernelModule = systolic.module( kernel:name() )
  local kernelFn = kernelModule:addFunction( "process", moduleInputs, out )
  kernelFn:addAssign( out, systolicFn )

  return kernelModule
end

function fpga.allocateLinebuffers( kernelGraph, options, pipeline, pipelineMain, moduleInputs, validIn )
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(options)=="table")
  assert(statemachine.isStateMachine(pipeline))
  assert(statemachine.isBlock(pipelineMain))
  assert(type(moduleInputs)=="table")
  assert(systolicInstance.isSystolicInstance(validIn))
  
  -- these tables contain instances of linebuffer modules
  -- or, modules with an equivilant interface in the case of pipeline inputs.
  -- The keys are kernelname_regular or kernelname_gather, which match
  -- the names of the inputs to the kernel functions, to make 
  -- connecting this stuff up easy.
  local regularInputLinebuffers = {}
  local outputLinebuffers = {}

  local inputFifos = {}
  local function allocateALinebuffer( src, kind )
    assert(type(kind)=="string")

    local key
    if type(src)=="number" then
      key = "input"..src
      if inputFifos[src]==nil then
        local input = moduleInputs[src]
        local fifo = fpga.modules.fifo( input.type ):instantiate("fifo_input"..src)
        inputFifos[src] = fifo
        pipeline:add(fifo)
        pipelineMain:addIfLaunch( validIn:read(), fifo:pushBack({indata=input:read()}) )
      end
      return inputFifos[src], key
    elseif darkroom.kernelGraph.isKernelGraph(src) then
      key = src:name().."_regular"
      if outputLinebuffers[src][key]==nil then
        assert(src:maxUse(1)<=0)
        assert(src:maxUse(2)<=0)
        outputLinebuffers[src][key] = fpga.modules.linebuffer( -src:minUse(1), -src:minUse(2), src.kernel.type, options.stripWidth):instantiate("linebuffer_"..key)
        pipeline:add(outputLinebuffers[src][key])
      end
      return outputLinebuffers[src][key], key
    else
        assert(false)
    end
  end

  local function addFifo( src, dst, key, LB )
    assert(type(src)=="number" or darkroom.kernelGraph.isKernelGraph(src))
    assert(darkroom.kernelGraph.isKernelGraph(dst))
    assert( type(key) == "string" )
    assert( systolicInstance.isSystolicInstance( LB ) )

    if regularInputLinebuffers[dst][src]==nil then
      if type(src)=="number" then
        -- this is already a fifo
        regularInputLinebuffers[dst][src] = {inst=LB,key=key,type=moduleInputs[src].type}
      else
        local lowX, lowY = 0,0
        if dst.kernel~=nil then lowX, lowY = src:minUse(1, dst), src:minUse(2, dst) end
        local ty = darkroom.type.array( src.kernel.type, {-lowX+1,-lowY+1} )
        local fifo = fpga.modules.fifo( ty ):instantiate("fifo_"..key.."_to_"..dst:name())
        pipeline:add(fifo)
        pipelineMain:addIfLaunch( LB:ready() , fifo:pushBack( {indata=LB:load()} ) )
        regularInputLinebuffers[dst][src] = {inst=fifo,key=key,type=ty}
      end
    end
  end

  kernelGraph:visitEach(
    function(node, inputArgs)
      regularInputLinebuffers[node] = {}
      outputLinebuffers[node] = {}

      -- we do this sort of backwards (lazily): for this node, we figure out
      -- what its inputs are, and then we allocate the LBs for these
      -- inputs on the producing nodes if they don't already exist.
      -- This way, we only allocate the linebuffers that are actually used.

      if node.kernel~=nil then
        node.kernel:S("load"):process(
          function(v)
            local LB, key = allocateALinebuffer( v.from, "regular" )
            addFifo( v.from, node, key, LB )
          end)
      else
        for k,v in node:inputs() do
          local LB, key = allocateALinebuffer( v, "regular" )
          addFifo( v, node, key, LB )
        end
      end

    end)

  return regularInputLinebuffers,{},outputLinebuffers
end

function fpga.codegenPipeline( inputs, kernelGraph, shifts, options, largestEffectiveCycles, imageWidth, imageHeight )
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(largestEffectiveCycles)=="number")
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")

  local definitions = {}

  local validIn = systolic.input("validIn",darkroom.type.bool())
  local validOut = systolic.output("validOut",darkroom.type.bool())
  local moduleInputs = {validIn}
  local inputIdsToSystolic = {}
  for k,v in pairs(inputs) do inputIdsToSystolic[k-1]=systolic.input("input"..k, darkroom.type.array(v[1].expr.type,{1,1})); table.insert(moduleInputs,inputIdsToSystolic[k-1]) end
  local finalOut = systolic.output("out", kernelGraph.child1.kernel.type )
  local moduleOutputs  = {validOut,finalOut}
  local pipelineMain = statemachine.block( "main", validIn:read() )
  local pipeline = statemachine.module("Pipeline", moduleInputs, moduleOutputs, pipelineMain)
  local inputLinebufferFifos, gatherInputLinebuffers, outputLinebuffers = fpga.allocateLinebuffers( kernelGraph, options, pipeline, pipelineMain, inputIdsToSystolic, validIn )

  kernelGraph:visitEach(
    function(node, inputArgs)
      if node.kernel~=nil then
        local kernelModule = fpga.codegenKernel( node, inputLinebufferFifos[node], imageWidth, imageHeight )
        local kernelModuleInst = kernelModule:instantiate(node:name())
        pipeline:add( kernelModuleInst )
        local xygen = fpga.modules.xygen(imageWidth, imageHeight ):instantiate("xygen_"..node:name())
        pipeline:add(xygen)
        local kernelArgs = {x=xygen:x(), y=xygen:y()}
        map( inputLinebufferFifos[node], function(v) kernelArgs[v.key] = v.inst:popFront() end )
        local kernelCall = kernelModuleInst:process( kernelArgs )
        local fifoReady = mapToArray(map(inputLinebufferFifos[node], function(n) return n.inst:ready() end))
        local allFifosReady = foldt( fifoReady, function(a,b) if b==nil then return a else return systolic.__and(a,b) end end)
        assert(systolicAST.isSystolicAST(allFifosReady))
        pipelineMain:addIfLaunch( allFifosReady, systolic.array(mapToArray(map(outputLinebuffers[node], function(v) return v:store({indata=kernelCall}) end))) )
      end
    end)

  for k,v in pairs(inputLinebufferFifos[kernelGraph]) do
    pipelineMain:addAssign( validOut, v.inst:ready())
    pipelineMain:addAssign( finalOut, systolic.cast(v.inst:popFront({},v.inst:ready()), kernelGraph.child1.kernel.type) )
  end

  return pipeline
end

local function calcMaxStencil( kernelGraph )
  local maxStencil = Stencil.new()
  kernelGraph:visitEach(
    function(node)
      if node.kernel~=nil then maxStencil = maxStencil:unionWith(neededStencil(true,kernelGraph,node,1,nil)) end
    end)
  return maxStencil
end

function delayToXY(delay, width)
  local lines = math.floor(delay/width)
  local xpixels = delay - lines*width
  return xpixels, lines
end


function fpga.codegenHarness( inputs, outputs, kernelGraph, shifts, options, largestEffectiveCycles, padMinX, padMinY, padMaxX, padMaxY, imageWidth, imageHeight)
  assert(type(padMaxY)=="number")

  local maxStencil = calcMaxStencil(kernelGraph)

  local shiftX, shiftY = delayToXY(shifts[kernelGraph.child1], options.stripWidth)
  maxStencil = maxStencil:translate(shiftX,shiftY,0)

  local totalInputBytes = 0
  for k,v in ipairs(inputs) do totalInputBytes = totalInputBytes + inputs[k][1].expr.type:sizeof() end

  local outputChannels = kernelGraph.child1.kernel.type:channels()
  local outputBytes = kernelGraph.child1.kernel.type:sizeof()

  local metadata = {minX = maxStencil:min(1), maxX=maxStencil:max(1), minY=maxStencil:min(2), maxY = maxStencil:max(2), outputShift = shifts[kernelGraph.child1], outputChannels = outputChannels, outputBytes = outputBytes, stripWidth = options.stripWidth, stripHeight=options.stripHeight, uartClock=options.uartClock, downsampleX=looprate(kernelGraph.child1.kernel.scaleN1,kernelGraph.child1.kernel.scaleD1,1), downsampleY=looprate(kernelGraph.child1.kernel.scaleN2,kernelGraph.child1.kernel.scaleD2,1), padMinX=padMinX, padMaxX=padMaxX, padMinY=padMinY, padMaxY=padMaxY, cycles = largestEffectiveCycles}

  for k,v in ipairs(inputs) do
    metadata["inputFile"..k] = v[3]
    metadata["inputBytes"..k] = inputs[k][1].expr.type:sizeof()
  end

  if outputs[1][2]=="vga" then
    return fpga.modules.stageVGA(), metadata
  elseif outputs[1][2]=="uart" then
    return fpga.modules.stageUART(options, totalInputBytes, outputBytes, options.stripWidth, options.stripHeight), metadata
  elseif outputs[1][2]=="sim" then
    for k,v in ipairs(inputs) do
      if v[3]:find(".raw")==nil then darkroom.error("sim only supports raw files") end
    end

    -- sim framework assumes this is the case
    print(imageWidth,imageHeight,options.stripWidth, options.stripHeight)
    assert(imageWidth+metadata.padMaxX-metadata.padMinX==options.stripWidth)
    assert(imageHeight+metadata.padMaxY-metadata.padMinY==options.stripHeight)
    return fpga.modules.sim(totalInputBytes, outputBytes, imageWidth, imageHeight, shifts[kernelGraph.child1], metadata), metadata
  elseif outputs[1][2]=="axi" then
    -- sim framework assumes this is the case
    print(imageWidth,imageHeight,options.stripWidth, options.stripHeight)
    assert(imageWidth+metadata.padMaxX-metadata.padMinX==options.stripWidth)
    assert(imageHeight+metadata.padMaxY-metadata.padMinY==options.stripHeight)
    return fpga.modules.axi(totalInputBytes, outputBytes, imageWidth, shifts[kernelGraph.child1], metadata), metadata
  else
    print("unknown data source "..outputs[1][2])
    assert(false)
  end

end

local function choosePadding( kernelGraph, imageWidth, imageHeight, smallestScaleX, smallestScaleY)
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")
  assert(type(smallestScaleX)=="number")
  assert(type(smallestScaleY)=="number")

  local maxStencil=calcMaxStencil(kernelGraph)

  local padMinX = downToNearest(smallestScaleX,maxStencil:min(1))
  local padMaxX = upToNearest(smallestScaleX,maxStencil:max(1))
  local padMinY = downToNearest(smallestScaleY,maxStencil:min(2))
  local padMaxY = upToNearest(smallestScaleY,maxStencil:max(2))
  return imageWidth+padMaxX-padMinX, imageHeight+padMaxY-padMinY, padMinX, padMaxX, padMinY, padMaxY
end

function fpga.compile(inputs, outputs, imageWidth, imageHeight, options)
  assert(#outputs==1)
  assert(type(options)=="table" or options==nil)

  if options.clockMhz==nil then options.clockMhz=32 end
  if options.uartClock==nil then options.uartClock=57600 end

  -- do the compile
  local newnode = {kind="outputs"}
  for k,v in ipairs(outputs) do
    newnode["expr"..k] = v[1]
  end
  local ast = darkroom.ast.new(newnode):setLinenumber(0):setOffset(0):setFilename("null_outputs")

  for k,v in ipairs(outputs) do
    if v[1]:parentCount(ast)~=1 then
      darkroom.error("Using image functions as both outputs and intermediates is not currently supported. Output #"..k)
    end
  end

  local kernelGraph, _, smallestScaleX, smallestScaleY, largestEffectiveCycles = darkroom.frontEnd( ast, {} )

  local padMinX, padMaxX, padMinY, padMaxY
  options.stripWidth, options.stripHeight, padMinX, padMaxX, padMinY, padMaxY = choosePadding( kernelGraph, imageWidth, imageHeight, smallestScaleX, smallestScaleY)
  options.padMinX = padMinX -- used for valid bit calculation

  local shifts = schedule(kernelGraph, 1, options.stripWidth)
  kernelGraph, shifts = shift(kernelGraph, shifts, 1, options.stripWidth)

  ------------------------------
  local result = {}
  table.insert(result, "`timescale 1ns / 10 ps\n")

  local pipeline = fpga.codegenPipeline( inputs, kernelGraph, shifts, options, largestEffectiveCycles, imageWidth, imageHeight )
  result = concat(result, pipeline:toVerilog())

  local harness, metadata = fpga.codegenHarness( inputs, outputs, kernelGraph, shifts, options, largestEffectiveCycles, padMinX, padMinY, padMaxX, padMaxY, imageWidth, imageHeight )
  result = concat(result, harness)

  return table.concat(result,""), metadata
end

return fpga