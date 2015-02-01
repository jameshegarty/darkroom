local fpga = {}
fpga.util = terralib.require("fpgautil")
fpga.modules = terralib.require("fpgamodules")

BRAM_SIZE_BYTES = 2048

function fpga.codegenKernel( kernel, moduleInputs, x, y, imageWidth, imageHeight )
  assert(darkroom.kernelGraph.isKernelGraph(kernel))
  assert(type(moduleInputs)=="table")
  assert(systolicAST.isSystolicAST(x))
  assert(systolicAST.isSystolicAST(y))
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")

  local systolicFn = kernel.kernel:visitEach(
    function( node, inputs)
      if node.kind=="load" then
        local r =  systolic.index(moduleInputs[node.from+1], {inputs.relX, inputs.relY})
        return r
      elseif node.kind=="crop" then
        local cond = systolic.__or(systolic.ge(x-node.shiftX,imageWidth),systolic.ge(y-node.shiftY,imageWidth))
        assert(systolicAST.isSystolicAST(cond))
        assert(systolicAST.isSystolicAST(inputs.expr))
        return systolic.select(cond , 0, inputs.expr )
      else
        local n = node:shallowcopy()
        for k,v in node:inputs() do
          n[k] = inputs[k]
        end
        return systolicAST.new(n)
      end
    end)

  local out = systolic.output( kernel.kernel:name(), kernel.kernel.type )
  local kernelFn = systolic.pureFunction( kernel:name(), {}, {out} )
  kernelFn:addAssign( out, systolicFn )

  return kernelFn
end

function fpga.collectLinebuffers( kernelGraph )
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))

  local regularInputLinebuffers = {}

  kernelGraph:visitEach(
    function(node, inputArgs)
      regularInputLinebuffers[node] = {}
    end)

  return regularInputLinebuffers,{},{}
end

function fpga.codegenPipeline( inputs, kernelGraph, shifts, options, largestEffectiveCycles, imageWidth, imageHeight )
  assert(darkroom.kernelGraph.isKernelGraph(kernelGraph))
  assert(type(largestEffectiveCycles)=="number")
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")

  local definitions = {}
  local pipeline = statemachine.module("Pipeline")
  local moduleInputs = map( inputs, function(v,k) return systolic.input("input"..k, darkroom.type.array(v[1].expr.type,{1,1})) end )
  local moduleOutputs  = {systolic.output("output", kernelGraph.child1.kernel.type )}
  local processImFunction = pipeline:addFunction("processIm", moduleInputs, moduleOutputs)
  local x,y = systolic.input("x",int16), systolic.input("y",int16)
  local regularInputLinebuffers, gatherInputLinebuffers, outputLinebuffers = fpga.collectLinebuffers( kernelGraph )

  kernelGraph:visitEach(
    function(node, inputArgs)
      if node.kernel~=nil then
        local kernelFunction = fpga.codegenKernel( node, moduleInputs, x, y, imageWidth, imageHeight )

        local fifos = map( regularInputLinebuffers[node], 
                           function(v) local fifo =  module.fifo(); processImFunction:addIf( v:ready(), fifo:pushBack(v:get())); return fifo end )

        local kernelCall = kernelFunction( map( fifos, function(v) v:get() end ) )
        processImFunction:addIf( foldl( regularInputLinebuffers[node], function(a,b) return a and b:isReady() end), map(outputLinebuffers[node], function(v) return v:store(kernelCall) end) )
      end
    end)


  return pipeline
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

local function calcMaxStencil( kernelGraph )
  local maxStencil = Stencil.new()
  kernelGraph:visitEach(
    function(node)
      if node.kernel~=nil then maxStencil = maxStencil:unionWith(neededStencil(true,kernelGraph,node,1,nil)) end
    end)
  return maxStencil
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