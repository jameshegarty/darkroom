--require "darkroomDebug"

-- luts are the number of 6:2 luts (4 per CLB)
-- brams is number of 18Kb brams

local fpgaEstimate = {}

function linebuffer(lines, bytesPerPixel, imageWidth, consumers)

  local brams = math.ceil((lines*bytesPerPixel*imageWidth) / 2250)
  print("BUFFER:", lines,"lines", bytesPerPixel, "bytes", brams, "brams")

  local availableBW = brams*4
  local usedBW = 0
  local luts = 0

  for _,v in ipairs(consumers) do
    -- have to read one column each clock
    usedBW = usedBW + bytesPerPixel*v:size()[2]
    -- one shift register entry per stencil pixel
    luts = luts + v:area()*bytesPerPixel*(8/2)
    print(v:area(),"stencil",luts,"luts")
  end

  assert(usedBW <= availableBW)

  return {brams = brams, luts = luts}
end

function sum(a,b)
  if a==nil then return b end
  if b==nil then return a end
  assert(type(a)=="table")
  assert(type(b)=="table")
  local t = {}

  for k,v in pairs(a) do
    if b[k]==nil then t[k]=v else t[k] = v+b[k] end
  end
  for k,v in pairs(b) do
    if t[k]==nil then t[k]=v end
  end
  return t
end

function mult(a,b)
  if a==nil then return nil end
  assert(type(a)=="table")
  assert(type(b)=="number")
  local t = {}
  for k,v in pairs(a) do
    t[k] = v*b
  end
  return t
end

function reduce(op, ty, width)
  if op=="sum" or op=="max" or op=="min" then
    return mult(binopToCost(op,ty),width*2-1)
  else
    print("reduce",op)
    assert(false)
  end
end

function binopToCost(op,type, lhsType, rhsType)
  local t={}
  t[op] = 1
  if op==">>" or op=="<<" then
  elseif op=="+" or op=="sum" or op=="-" or op==">" or op=="<" or op=="<=" or op==">=" then
    t.luts = type:sizeof()*8
  elseif op=="max" or op=="min" then
    t.luts = type:sizeof()*(8+(8/2))
  elseif op=="*" then
    t.dsps = 1
  elseif op=="and" or op=="==" or op=="or"  then
    t.luts = type:sizeof()*(8/2)
  elseif op=="dot" then
    assert(lhsType:isArray())
    t = sum(t, mult(binopToCost("*",lhsType:arrayOver()),lhsType:arrayLength()))
    t = sum(t, reduce("+",type,lhsType:arrayLength()))
  else
    print("binop",op)
    assert(false)
  end

  return t
end


function estimate(kernelGraph, imageWidth)

  local function parentIsOutput(node)
    for v,k in node:parents(kernelGraph) do if v==kernelGraph then return true end end
    return false
  end

  local cnt = {}
  kernelGraph:S("*"):traverse(
    function(node)
      local consumers = {}
      for v,_ in node:parents(kernelGraph) do
        if v.kernel~=nil then
          table.insert(consumers, v.kernel:stencil(node))
        end
      end

      if parentIsOutput(node)==false and node~=kernelGraph then
        local l = linebuffer(node:bufferSize(kernelGraph), node.kernel.type:sizeof(), imageWidth, consumers)
        cnt = sum(cnt, l)
      end

      if node.kernel~=nil then
        local r = node.kernel:visitEach(
          function(k, args)
            if k.type:isFloat() then
              print("float not supported "..k:linenumber())
            end

            if k.kind=="binop" then
              return sum(args.lhs, sum(args.rhs, binopToCost(k.op, k.type, k.lhs.type, k.rhs.type)))
            elseif k.kind=="mapreduce" then
              local r = {}
              local area = 1
              local i=1
              while k["varid"..i] do
                area = area * (k["varhigh"..i]-k["varlow"..i]+1)
                i = i + 1
              end

              if k.reduceop=="sum" then
                r = sum(r,reduce("sum",k.type,area))
              end
              return sum(r,mult(args["expr"],area))
            elseif k.kind=="load" or k.kind=="crop" or k.kind=="cast" or k.kind=="index" then
              return args["expr"]
            elseif k.kind=="reduce" then
              local r = {}
              local i=1
              while args["expr"..i] do
                r = sum(r,args["expr"..i])
                i=i+1
              end
              return sum(r,reduce(k.op,k.type,i-1))
            elseif k.kind=="array" then
              local r = {}
              local i=1
              while args["expr"..i] do
                r = sum(r,args["expr"..i])
                i=i+1
              end
              return r
            elseif k.kind=="value" or k.kind=="position" then
            elseif k.kind=="tap" then
              return {luts=k.type:sizeof()*(8/4)} -- hold values in DFF?
            elseif k.kind=="select" or k.kind=="vectorSelect" then
              local r = {luts = k.type:sizeof()*(8/4)}
              r[k.kind]=1
              return sum(args.a,sum(args.b,sum(args.cond,r)))
            elseif k.kind=="unary" then
              if k.op=="arrayAnd" then -- ands N bools
                local i=1
                while args["expr"..i] do i = i + 1 end
                assert(i-1 <= 5)
                assert(k.type:isBool())
                return {luts = 1,arrayAnd=1}
              elseif k.op=="-" or k.op=="abs" then
              else
                print(k.op)
                assert(false)
              end
            else
              print(k.kind)
              assert(false)
            end
          end)
        cnt = sum(r,cnt)
      end
    end)

  local s = ""
  for k,v in pairs(cnt) do s = s..k.." = "..v.."\n" end
  return s
end

function fpgaEstimate.compile(outputs, imageWidth)
  print("DOESTIMATE")

  assert(type(imageWidth)=="number")
  -- do the compile
  local newnode = {kind="outputs"}
  for k,v in ipairs(outputs) do
    newnode["expr"..k] = v
  end
  local ast = darkroom.ast.new(newnode):setLinenumber(0):setOffset(0):setFilename("null_outputs")

  for k,v in ipairs(outputs) do
    if v:parentCount(ast)~=1 then
      darkroom.error("Using image functions as both outputs and intermediates is not currently supported. Output #"..k)
    end
  end

  local kernelGraph = darkroom.frontEnd( ast, {} )
  local shifts = schedule(kernelGraph)
  kernelGraph, shifts = shift(kernelGraph, shifts)

  --kernelGraphPrintPretty(kernelGraph)
  return estimate(kernelGraph, imageWidth)

end

return fpgaEstimate