darkroom._inputCount = 0

function darkroom.input( imtype )
  imtype = darkroom.type.fromTerraType(imtype)

  assert(darkroom.type.isType(imtype))

  local res  = darkroom.ast.new({kind="load", type = imtype, relX=0, relY=0, from=darkroom._inputCount} ):setLinenumber(0):setOffset(0):setFilename("null_special")
  res:setName("cropSpecial"..(darkroom._inputCount).."Node")
  darkroom._inputCount = darkroom._inputCount + 1

  res = darkroom.ast.new({kind="crop", shiftY = 0, shiftX = 0,expr=res}):setLinenumber(0):setOffset(0):setFilename("null_special")

  return res
end

darkroom._tapcount = 0
function darkroom.tap( ty )
  ty = darkroom.type.fromTerraType(ty)

  assert(darkroom.type.isType(ty))
  assert(darkroom.type.isArray(ty)==false)

  darkroom._tapcount = darkroom._tapcount+1
  return darkroom.ast.new({kind="tap", type=ty, id=darkroom._tapcount-1}):setLinenumber(0):setOffset(0):setFilename("null_tap")
end

function darkroom.tapLUT(ty)
  ty = darkroom.type.fromTerraType(ty)

  assert(darkroom.type.isArray(ty)==false)
  assert(ty:isUint())

  -- there must be a entry for every possible value of input
  local count = math.pow(2,ty.precision)

  darkroom._tapcount = darkroom._tapcount+1
  return darkroom.ast.new({kind="tap", type=darkroom.type.array(ty,count), count=count, id=darkroom._tapcount-1}):setLinenumber(0):setOffset(0):setFilename("null_tapLUT")
end

function darkroom.frontEnd(ast, options)
  assert(type(options)=="table")

  if options.callbackAST~=nil then options.callbackAST(ast) end

  local typedAST = darkroom.typedAST.astToTypedAST( ast, options )
  if options.callbackTypedAST~=nil then options.callbackTypedAST(typedAST) end

  -- optimize
  local optimizedTypedAST = darkroom.optimize.optimize(typedAST, options)

  -- determine the set of nodes to make scheduling decisions on
  local kernelGraph = darkroom.kernelGraph.typedASTToKernelGraph(optimizedTypedAST, options)
  if options.callbackKernelGraph~=nil then options.callbackKernelGraph(kernelGraph) end

  return kernelGraph
end

function darkroom.backEnd( kernelGraph, inputImages, taps, options )

  assert(type(options)=="table")
  assert(type(inputImages)=="table")

  local shifts = schedule(kernelGraph)
  kernelGraph, shifts = shift(kernelGraph, shifts)
  
  if options.callbackScheduledKernelGraph~=nil then options.callbackScheduledKernelGraph(kernelGraph) end
  
  if options.verbose then print("start compile") end
  
  local res = darkroom.terracompiler.compile( 
    kernelGraph, 
    inputImages, 
    taps,
    shifts,
    options)
  
  if options.printstage or options.verbose then 
    print("Start terra compile") 
    print("mem",collectgarbage("count"))
  end
  
  local start = darkroom.currentTimeInSeconds()
  res:compile() -- we may want to do this later (out of this scope), so that more stuff can be GCed?
  local endt = darkroom.currentTimeInSeconds()
  
  if options.printstage or options.verbose then
    print("Terra compile done, compile time",(endt-start))
  end
  
  return res
end

function darkroom.compile(inputImageFunctions, outputImageFunctions, tapInputs, inputWidth, inputHeight, options)

  local function checkinput(tab,arg,argtype,minsize)
    if type(tab)~="table" or #tab<minsize then
      darkroom.error("Error, "..arg.." darkroom.compile must be an array of "..argtype)
    end

    for k,v in ipairs(tab) do
      if tab[k]==nil or darkroom.ast.isAST(tab[k])==false then
        darkroom.error("Error, "..arg.." darkroom.compile must be an array of "..argtype)
      end  
    end
  end

  checkinput(inputImageFunctions,"input to","image functions",0)
  checkinput(outputImageFunctions,"output from","image functions",1)
  checkinput(tapInputs,"tap inputs to","tap image functions",0)

  if type(inputWidth)~="number" then darkroom.error("forth argument to darkroom.compile must be image width") end
  if type(inputHeight)~="number" then darkroom.error("fifth argument to darkroom.compile must be image width") end

  if type(options)~="table" and options~=nil then darkroom.error("sixth argument to darkroom.compile must be options table or nil") end

  -- make it so that we can pipe stdout to a file and 
  -- see results before the compile is totally complete
  io.stdout:setvbuf("no") 

  local function checkoptions(options)
    if options==nil then options={} end
    if options.verbose ~= nil then assert(type(options.verbose)=="boolean") else options.verbose = false; end
    if options.debug ~= nil then assert(type(options.debug)=="boolean") else options.debug = false; end
    if options.printruntime ~= nil then assert(type(options.printruntime)=="boolean") else options.printruntime = false; end
    if options.looptimes ~= nil then assert(type(options.looptimes)=="number") else options.looptimes = 1; end
    if options.printasm ~=nil then assert(type(options.printasm)=="boolean"); darkroom.printasm = options.printasm; end
    if options.V == nil then options.V=4 end
    if options.cores == nil then options.cores=1 else assert(type(options.cores)=="number") end
    if options.stripcount == nil then options.stripcount=options.cores end
    if options.fastmath ~= nil then assert(type(options.fastmath)=="boolean") else options.fastmath = false end
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
  local ast = darkroom.ast.new(newnode):setLinenumber(0):setOffset(0):setFilename("null_outputs")

  for k,v in ipairs(outputImageFunctions) do
    if v:parentCount(ast)~=1 then
      darkroom.error("Using image functions as both outputs and intermediates is not currently supported. Output #"..k)
    end
  end

  local kernelGraph = darkroom.frontEnd( ast, options )
  return darkroom.backEnd( kernelGraph, inputImageFunctions, tapInputs, options)
end