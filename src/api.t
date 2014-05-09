orion._inputCount = 0

function orion.input( imtype )
  assert(orion.type.isType(imtype))

  -- channel == -1 indicates to use all the channels
  -- channel == 1,2, etc indicates to only use that one channel
  local res  = orion.ast.new({kind="input", channel = -1, type = imtype, baseType = imtype, id=orion._inputCount} ):setLinenumber(0):setOffset(0):setFilename("null_special")
  res:setName("cropSpecial"..(orion._inputCount).."Node")
  orion._inputCount = orion._inputCount + 1

  -- this is kind of a trick: crop from special node will propagate
  res = orion.ast.new({kind="crop", expr=res, mode=orion.cropSame}):setLinenumber(0):setOffset(0):setFilename("null_special")

  return res
end

orion._tapcount = 0
function orion.tap(ty)
  assert(orion.type.isType(ty))
  assert(orion.type.isArray(ty)==false)
  assert(type(name)=="string")

  if orion._usedTapNames[name]~=nil then
    print("Error, tap name ",name," was used twice, which isn't allowed!")
    assert(false)
  end
  orion._usedTapNames[name] = 1

  orion._tapcount = orion._tapcount+1
  return orion.ast.new({kind="tap",type=ty,tapname=name,id=orion._tapcount-1}):setLinenumber(0):setOffset(0):setFilename("null_tap")
end

orion._tapLUTcount = 0
function orion.tapLUT(ty, count, name)
  assert(orion.type.isType(ty))
  assert(orion.type.isArray(ty)==false)
  assert(type(count)=="number")
  assert(type(name)=="string")

  orion._tapLUTcount = orion._tapLUTcount+1
  return orion.ast.new({kind="tapLUT",type=ty,count=count,tapname=name,id=orion._tapLUTcount-1}):setLinenumber(0):setOffset(0):setFilename("null_tapLUT")
end

function orion.frontEnd(ast, options)
  assert(type(options)=="table")

  if options.callbackAST~=nil then options.callbackAST(ast) end

  local typedAST = orion.typedAST.astToTypedAST( ast, options )
  if options.callbackTypedAST~=nil then options.callbackTypedAST(typedAST) end

  -- optimize
  local optimizedTypedAST = orion.optimize.optimize(typedAST, options)

  -- determine the set of nodes to make scheduling decisions on
  local kernelGraph = orion.kernelGraph.typedASTToKernelGraph(optimizedTypedAST, options)
  if options.callbackKernelGraph~=nil then options.callbackKernelGraph(kernelGraph) end

  return kernelGraph
end

function orion.backEnd( kernelGraph, inputImages, options)

  assert(type(options)=="table")
  assert(type(inputImages)=="table")

  if options.platform=="cpu" or options.platform == nil then
    
    if options.verbose then print("start compile") end
    
    local res = orion.terracompiler.compile( 
      kernelGraph, 
      inputImages, 
      options)
    
    if options.printstage or options.verbose then 
      print("Start terra compile") 
      print("mem",collectgarbage("count"))
    end
    
    local start = orion.currentTimeInSeconds()
    res:compile() -- we may want to do this later (out of this scope), so that more stuff can be GCed?
    local endt = orion.currentTimeInSeconds()
    
    if options.printstage or options.verbose then
      print("Terra compile done, compile time",(endt-start))
    end

return res

  elseif options.platform=="convolution" then
    local convIR = orion.convIR.convert(scheduledIR)
return orion.convolution.synth(convIR, base1, base2, options)
  else
    assert(false)
  end

end

function orion.compile(inputImageFunctions, outputImageFunctions, tapInputs, inputWidth, inputHeight, options)

  local function checkinput(tab,arg,argtype,minsize)
    if type(tab)~="table" or #tab<minsize then
      orion.error("Error, "..arg.." orion.compile must be an array of "..argtype)
    end

    for k,v in ipairs(tab) do
      if tab[k]==nil or orion.ast.isAST(tab[k])==false then
        orion.error("Error, "..arg.." orion.compile must be an array of "..argtype)
      end  
    end
  end

  checkinput(inputImageFunctions,"input to","image functions",1)
  checkinput(outputImageFunctions,"output from","image functions",1)
  checkinput(tapInputs,"tap inputs to","tap image functions",0)
  assert(type(inputWidth)=="number")
  assert(type(inputHeight)=="number")
  assert(type(options)=="table" or options==nil)

  -- make it so that we can pipe stdout to a file and 
  -- see results before the compile is totally complete
  io.stdout:setvbuf("no") 

  local function checkoptions(options)
    if options==nil then options={} end
    if options.platform==nil then options.platform="cpu" end
    assert(options.platform=="cpu" or options.platform=="convolution")
    if options.verbose ~= nil then assert(type(options.verbose)=="boolean"); orion.verbose = options.verbose; end
    if options.printruntime ~= nil then assert(type(options.printruntime)=="boolean") else options.printruntime = false; end
    if options.looptimes ~= nil then assert(type(options.looptimes)=="number") else options.looptimes = 1; end
    if options.printasm ~=nil then assert(type(options.printasm)=="boolean"); orion.printasm = options.printasm; end
    if options.V == nil then options.V=4 end
    if options.cores == nil then options.cores=1 end
    if options.stripcount == nil then options.stripcount=options.cores end
    if options.fastmath ~= nil then assert(type(options.fastmath)=="boolean") else options.fastmath = false end
    if options.terradebug ~= nil then assert(type(options.terradebug)=="boolean") else options.terradebug = false end
    if options.pagesize ~= nil then assert(type(options.pagesize)=="number") else options.pagesize = 4*1024 end
    
    options.width = inputWidth
    options.height = inputHeight

    return options
  end

  options = checkoptions(options)

  if options.printstage then print("start compile") end

  -- do the compile
  local newnode = {kind="outputs"}
  for k,v in ipairs(outputImageFunctions) do
    newnode["expr"..k] = v
  end
  local ast = orion.ast.new(newnode):setLinenumber(0):setOffset(0):setFilename("null_outputs")

  local kernelGraph = orion.frontEnd( ast, options )
  return orion.backEnd( kernelGraph, inputImageFunctions, options)
end