--require "darkroomDebug"

-- luts are the number of 6:2 luts (4 per CLB)
-- brams is number of 18Kb brams

local fpgaEstimate = {}
local perline = {}

local procstats = {}
procstats["xc7z020"] = {dsps = 220, brams=280, luts=53200}
procstats["xc7z100"] = {dsps = 2020, brams=1510, luts=277400}
procstats["xc6slx9"] = {dsps = 16, brams=32, luts=5720}

function displayProcstats(cnt)
  local s = ""
  for k,v in pairs(procstats) do
    s = s..k.."\n"
    for kk,vv in pairs(v) do
      local cntkk = cnt[kk] or 0
      s = s..kk.." : "..cntkk.."/"..v[kk].."  "..math.ceil((cntkk/v[kk])*100).."%\n"
    end
  end
  return s
end

function displayPerline()

  local s = ""
  for k,v in pairs(perline) do
    s = s..k.."\n"
    s = s.."LUT DSP BRAM\n"
    if io.open(k) then
      io.input(k)
      local ln = 1
      while true do
        local line = io.read("*line")
        if line==nil then break end

        if perline[k][ln] then
          local luts = perline[k][ln].luts or 0
          local dsps = perline[k][ln].dsps or 0
          local brams = perline[k][ln].brams or 0
          s = s..string.format("%4d",luts)..string.format("%4d",dsps)..string.format("%4d",brams)..line.."\n"
        else
          s = s.."            "..line.."\n"
        end

        ln = ln + 1
      end
    else
      for kk,v in pairs(perline[k]) do
        local luts = perline[k][kk].luts or 0
        local dsps = perline[k][kk].dsps or 0
        local brams = perline[k][kk].brams or 0
        s = s..string.format("%4d",luts)..string.format("%4d",dsps)..string.format("%4d",brams).."\n"
      end
    end
  end

  return s
end

function linebuffer(lines, bytesPerPixel, imageWidth, consumers)

  local brams = math.ceil((lines*bytesPerPixel*imageWidth) / 2250)
  print("BUFFER:", lines,"lines", bytesPerPixel, "bytes", brams, "brams")

  local availableBW = brams*4 -- in bytes
  local usedReadBW = 0 -- in bytes
  local luts = 0

  for _,v in ipairs(consumers) do
    -- have to read one column each clock
    usedReadBW = usedReadBW + bytesPerPixel*v:size()[2]
    -- one shift register entry per stencil pixel
    luts = luts + v:area()*bytesPerPixel*(8/2)
    print(v:area(),"stencil",luts,"luts")
  end

--  assert(usedReadBW <= availableBW)

  local usedWriteBW = bytesPerPixel -- in bytes
  assert(usedWriteBW <= 4) -- we can only write to one bram

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
  assert(width>0)
  if op=="sum" or op=="max" or op=="min" or op=="+"  then
    local r = mult(binopToCost(op,ty),(width/2)*2-1)
    print("reduce",width,"luts:",r.luts)
--    r["reduce"..op..width]=1
    return r
  elseif op=="argmin" or op=="argmax" then
    local r = mult(binopToCost("min",ty),(width/2)*2-1)
    print("reduce",width,"luts:",r.luts)
    return r
  else
    print("reduce",op)
    assert(false)
  end
end

function binopToCost(op,type, lhsType, rhsType)
  local t={}
--  t["binop"..op] = 1
  if op==">>" or op=="<<" then
  elseif op=="+" or op=="sum" or op=="-" or op==">" or op=="<" or op=="<=" or op==">=" then
    t.luts = type:sizeof()*8
  elseif op=="max" or op=="min" then
    t.luts = type:sizeof()*(8+(8/2))
  elseif op=="*" then
    t.dsps = 1
  elseif op=="pow" then
    t.luts = (type:sizeof()*8)*(type:sizeof()*8)
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

  print("binop",op,"typesize:",type:sizeof(),"luts:",t.luts)
  return t
end

-- some inputs might be duplicated - we don't want to double count their cost
-- eg broadcasting 1 channel into an array of 3
function uniq(key, kernelGraphNode, args)
  local t = {}
  local i=1
  while kernelGraphNode[key..i] do
    t[kernelGraphNode[key..i]] = i
    i=i+1
  end

  local cnt = 0
  local res = {}
  for k,v in pairs(t) do
    table.insert(res, args[key..v])
  end

  print("uniq",i-1,cnt)
  return res
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
        if perline[node:filename()]==nil then perline[node:filename()]={} end
        perline[node:filename()][node:linenumber()] = sum(perline[node:filename()][node:linenumber()],l)
      end

      if node.kernel~=nil then
        local r = node.kernel:visitEach(
          function(k, args)

            if k.type:isFloat() then
              print("float not supported "..k:linenumber())
            end

            local resInputs = {}
            local resThis = {}

            if k.kind=="binop" then
              resThis = binopToCost(k.op, k.type, k.lhs.type, k.rhs.type)
              resThis["binop"..k.op] = 1
              
              resInputs = args.lhs
              resInputs = sum(resInputs, args.rhs)
            elseif k.kind=="mapreduce" then
              local area = 1
              local i=1
              while k["varid"..i] do
                area = area * (k["varhigh"..i]-k["varlow"..i]+1)
                i = i + 1
              end

              resThis = reduce(k.reduceop, k.type, area)
              resInputs = mult(args["expr"],area)
            elseif k.kind=="crop" or k.kind=="cast" or k.kind=="index" then
              resInputs = args["expr"]
            elseif k.kind=="tapLUTLookup" then
              resInputs = args["index"]
              resThis.brams = 1
            elseif k.kind=="reduce" then

              local i=1
              while k["expr"..i] do
                resInputs = sum(resInputs,args["expr"..i])
                i=i+1
              end

              resThis = reduce(k.op,k.type,i-1)
              resThis["reduce"..k.op..(i-1)] = 1
            elseif k.kind=="array" then
              resThis.array = 1
              local t = uniq("expr", k, args)
              for _,v in pairs(t) do
                resInputs = sum(resInputs,v)
              end

            elseif k.kind=="load" or k.kind=="value" or k.kind=="position" or k.kind=="mapreducevar" then

            elseif k.kind=="gather" then
              -- this thing is fed by a linebuffer
              resThis["gather"..k.maxX.."x"..k.maxY] = 1

              local w = k.maxX*2+1
              local h = k.maxY*2+1
              
              --assert(w<math.pow(2,5))
              --assert(h<math.pow(2,5))

              -- for each bit of output, use a lut to mux the correct bit
              local muxluts = math.ceil(math.ceil(math.log(w+h)/math.log(2))/5)
              print("gather",w,h,"muxlutPerBit:",muxluts)
              resThis.luts = muxluts*w*h*k.type:sizeof()*8

              -- reduce the bits down
              local i=w*h
              local reduceLuts = 0
              while i>1 do
                reduceLuts = math.ceil(reduceLuts/5)
                i = i / 5
              end
              resThis.luts = resThis.luts + reduceLuts*k.type:sizeof()*8

              print("gather total luts:",resThis.luts)
            elseif k.kind=="tap" then
              resThis = {tap=1,luts=k.type:sizeof()*(8/4)} -- hold values in DFF?
            elseif k.kind=="select" or k.kind=="vectorSelect" then
              resThis = {luts = k.type:sizeof()*(8/4)}
              resThis[k.kind]=1
              
              resInputs = sum(args.a,sum(args.b,args.cond))
            elseif k.kind=="unary" then
              if k.op=="arrayAnd" then -- ands N bools
                local i=1
                while args["expr"..i] do i = i + 1 end
                assert(i-1 <= 5)
                assert(k.type:isBool())
                resThis = {luts = 1,arrayAnd=1}
              elseif k.op=="-" or k.op=="abs" then
              else
                print(k.op)
                assert(false)
              end
            else
              print(k.kind)
              assert(false)
            end

            if perline[k:filename()]==nil then perline[k:filename()]={} end
            perline[k:filename()][k:linenumber()] = sum( perline[k:filename()][k:linenumber()], resThis)
            return sum(resThis, resInputs)
          end)
        cnt = sum(r,cnt)
      end
    end)

  local s = "return {"
  for k,v in pairs(cnt) do s = s.."['"..k.."'] = "..v..",\n" end

  return s.."rofl=0}", displayPerline()..displayProcstats(cnt)
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
  local shifts = schedule(kernelGraph,1)
  kernelGraph, shifts = shift(kernelGraph, shifts,1)

  --kernelGraphPrintPretty(kernelGraph)
  return estimate(kernelGraph, imageWidth)

end

return fpgaEstimate