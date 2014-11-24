local modules = {}

function modules.reduce(compilerState, op, cnt, datatype, argminVars)
  assert(type(op)=="string")
  assert(darkroom.type.isType(datatype))
  print("REDUCE",datatype:str())
  local name = "Reduce_"..op.."_"..cnt

  if compilerState.declaredReductionModules[name] then
    return name, {} -- already declared somewhere
  end
  compilerState.declaredReductionModules[name] = 1

  local module = {"module "..name.."(input CLK, output["..(datatype:sizeof()*8-1)..":0] out"}
  local argmindecl = {}
  if op=="argmin" then
    assert(type(argminVars)=="table")
    local partials=0
    local funroll = {function(vals, mrvValues) 
                       table.insert(module,", input["..(datatype:sizeof()*8-1)..":0] partial"..vals) 
                       table.insert(argmindecl,"wire["..(datatype:sizeof()*8-1)..":0] partial_"..partials.." = partial"..vals..";\n")
                       local r = 1
                       while argminVars["varname"..r] do
                         table.insert(argmindecl,"wire[31:0] partial_"..partials.."_"..argminVars["varname"..r].." = "..valueToVerilogLL(mrvValues[argminVars["varname"..r]],true,32)..";\n")
                         r = r + 1
                       end
                       partials = partials+1
                     end}
    local i = 1
    while argminVars["varname"..i] do
      table.insert(module, ", output [31:0] out_"..argminVars["varname"..i])
      local ii = i
      table.insert(funroll, function(vals, mrvValues) for j=argminVars["varlow"..ii],argminVars["varhigh"..ii] do mrvValues[argminVars["varname"..ii]]=j;funroll[ii]("_"..argminVars["varname"..ii]..numToVarname(j)..vals, mrvValues ) end end)
      i = i + 1
    end
    funroll[#funroll]("",{})
  else
    for i=0,cnt-1 do table.insert(module,", input["..(datatype:sizeof()*8-1)..":0] partial_"..i.."") end
    if op=="valid" then
      for i=0,cnt-1 do table.insert(module,", input  partial_valid_"..i.."") end
    end
  end

  table.insert(module,");\n")
  module = concat(module,argmindecl)

  local clockedLogic = {}

  local remain = cnt
  local level = 0
  while remain>1 do
    local r = math.floor(remain/2)
    print("remain",remain,r)

    local l = ""
    if level>0 then l="_l"..level end

    for i=0,r-1 do
      local n = "partial_l"..(level+1).."_"..i
      table.insert(module, declareReg(datatype,n))
      
      if op=="sum" then
        table.insert(clockedLogic, n.." <= partial"..l.."_"..(i*2).." + partial"..l.."_"..(i*2+1)..";\n")
      elseif op=="max" then
        local a = "partial"..l.."_"..(i*2)
        local b = "partial"..l.."_"..(i*2+1)
        table.insert(clockedLogic, n.." <= ("..a..">="..b..")?("..a.."):("..b..");\n")
      elseif op=="argmin" then
        local a = "partial"..l.."_"..(i*2)
        local b = "partial"..l.."_"..(i*2+1)
        -- we have to do <= here so that at least one signal gets a value (eg if all values in the input are the same).
        -- similarly, this will me we will choose the lowest map reduce index values by default if all input values are
        -- the same, which matches the behavior on the CPU
        table.insert(clockedLogic, n.." <= ("..a.."<="..b..")?("..a.."):("..b..");\n")
        local i = 1
        while argminVars["varname"..i] do
          table.insert(module, declareReg(datatype,n.."_"..argminVars["varname"..i]))
          table.insert(clockedLogic, n.."_"..argminVars["varname"..i].." <= ("..a.."<="..b..")?("..a.."_"..argminVars["varname"..i].."):("..b.."_"..argminVars["varname"..i]..");\n")
          i = i + 1
        end
      elseif op=="valid" then
        local nv = "partial_valid_l"..(level+1).."_"..i
        table.insert(module, declareReg(darkroom.type.bool(),nv))
        table.insert(clockedLogic, n.." <= (partial_valid"..l.."_"..(i*2)..")?(partial"..l.."_"..(i*2).."):(partial"..l.."_"..(i*2+1)..");\n")
        table.insert(clockedLogic, nv.." <= (partial_valid"..l.."_"..(i*2).." || partial_valid"..l.."_"..(i*2+1)..");\n")
      else
        assert(false)
      end
    end

    -- codegen the dangle
    assert(remain-r*2 == 0 or remain-r*2==1)
    if remain-r*2==1 then
      local n = "partial_l"..(level+1).."_"..r
      table.insert(module, declareReg(datatype,n))
      if level==0 then
        table.insert(clockedLogic, n.." <= partial_"..(remain-1)..";\n")	
        if op=="argmin" then
          local i = 1
          while argminVars["varname"..i] do table.insert(clockedLogic, n.."_"..argminVars["varname"..i].." <= partial_"..(remain-1).."_"..argminVars["varname"..i]..";\n"); table.insert(module, declareReg(datatype,n.."_"..argminVars["varname"..i])); i=i+1 end
        elseif op=="valid" then
          table.insert(module, declareReg(darkroom.type.bool(),"partial_valid_l"..(level+1).."_"..r))
          table.insert(clockedLogic, "partial_valid_l"..(level+1).."_"..r.." <= partial_valid_"..(remain-1)..";\n");
        end
      else
        table.insert(clockedLogic, n.." <= partial_l"..level.."_"..(remain-1)..";\n")	
        if op=="argmin" then
          local i = 1
          while argminVars["varname"..i] do table.insert(clockedLogic, n.."_"..argminVars["varname"..i].." <= partial_l"..level.."_"..(remain-1).."_"..argminVars["varname"..i]..";\n"); table.insert(module, declareReg(datatype,n.."_"..argminVars["varname"..i])); i=i+1 end
        elseif op=="valid" then
          table.insert(module, declareReg(darkroom.type.bool(),"partial_valid_l"..(level+1).."_"..r))
          table.insert(clockedLogic, "partial_valid_l"..(level+1).."_"..r.." <= partial_valid_l"..level.."_"..(remain-1)..";\n");
        end

      end
    end

    remain = remain-r
    level=level+1
  end

  table.insert(module, "assign out = partial_l"..level.."_0;\n")

  if op=="argmin" then
    local i = 1
    while argminVars["varname"..i] do 
      table.insert(module, "assign out_"..argminVars["varname"..i].." = partial_l"..level.."_0_"..argminVars["varname"..i]..";\n")
      i = i + 1
    end
  end

  table.insert(module, "always @ (posedge CLK) begin\n")
  module = concat(module, clockedLogic)
  table.insert(module,"end\nendmodule\n")


  return name, module
end

lbCnt = 0
function modules.linebuffer(maxdelay, datatype, stripWidth, consumers, downsampledInput, upsampledYConsumer)
  assert(type(maxdelay)=="number")
  assert(type(downsampledInput)=="boolean")
  assert(type(upsampledYConsumer)=="boolean")

  assert(darkroom.type.isType(datatype))
  local bytesPerPixel = datatype:sizeof()
  local name = "Linebuffer_"..numToVarname(maxdelay).."delay_"..bytesPerPixel.."bpp_"..stripWidth.."w_"..lbCnt
  lbCnt = lbCnt + 1

  local outputs = ""
  for k,v in ipairs(consumers) do
    for x=v:min(1),v:max(1) do
      for y=v:min(2), v:max(2) do
        outputs = outputs .. "output ["..(bytesPerPixel*8-1)..":0] out"..k.."_x"..numToVarname(x).."_y"..numToVarname(y)..",\n"
      end
    end
  end

  local t = {"module "..name.."(input CLK,\n"..outputs.."input ["..(bytesPerPixel*8-1)..":0] in, input validInNextCycleX, input validInNextCycleY);\n"}

  table.insert(t,"wire validInNextCycle;\n")
  table.insert(t,"assign validInNextCycle = validInNextCycleX & validInNextCycleY;\n")

  if downsampledInput then
    table.insert(t,"reg validInThisCycle = 1'b0;\n")
    table.insert(t,"reg validInThisCycleX = 1'b0;\n")
  else
    table.insert(t,"reg validInThisCycle = 1'b1;\n")
    table.insert(t,"reg validInThisCycleX = 1'b1;\n")
  end

  table.insert(t,"always @ (posedge CLK) begin validInThisCycle <= validInNextCycle; end\n")
  table.insert(t,"always @ (posedge CLK) begin validInThisCycleX <= validInNextCycleX; end\n")

  local xpixels, lines = delayToXY(maxdelay, stripWidth)
  if upsampledYConsumer then lines = lines + 1 end -- if we're usampling in Y, we have to store at least 1 line, b/c we need to be able to repeat that line
  print("linebuffer lines",lines,"xpixels",xpixels)

  if lines==0 and xpixels==0 then
    for k,v in ipairs(consumers) do      
      table.insert(t, "assign out"..k.."_x0_y0 = (validInThisCycle)?(in):(inLastCycle);\n")
    end

    table.insert(t,"reg ["..(bytesPerPixel*8-1)..":0] inLastCycle;\n")
    table.insert(t,"always @ (posedge CLK) begin if(validInThisCycle) begin inLastCycle <= in; end end\n")
--    table.insert(t,[=[initial begin $monitor("LB this %d next %d\n",validInThisCycle, validInNextCycle); end]=].."\n")
  elseif lines==0 then

    -- we're only delaying a few pixels, don't use a bram
    local clockedLogic = {}
    local prev = "(validInThisCycle)?(in):(lb_0)"
    table.insert(t,declareReg(datatype,"lb_0"))
    local i=-1
    while i>=-xpixels do
      local n = "lb_"..numToVarname(i)
      table.insert(t,declareReg(datatype,n))

      for k,v in ipairs(consumers) do
        assert(v:min(2)==0 and v:max(2)==0)
        for x=v:min(1),v:max(1) do
          if x==i then
            table.insert(t, "assign out"..k.."_x"..numToVarname(x).."_y0 = "..n..";\n")
          end
        end
      end
      
      table.insert(clockedLogic, n.." <= "..prev..";\n")
      prev = n
      i = i - 1
    end

    for k,v in ipairs(consumers) do
        assert(v:min(2)==0 and v:max(2)==0)
        for x=v:min(1),v:max(1) do
          if x==0 then
            table.insert(t, "assign out"..k.."_x0_y0 = (validInThisCycle)?(in):(lb_0);\n")
          end
        end
      end

    table.insert(t,"always @ (posedge CLK) begin\n")
    table.insert(t,"if (validInThisCycle) begin lb_0 <= in; end\n")
    table.insert(t,"if (validInNextCycle) begin\n")
    t = concat(t,clockedLogic)
    table.insert(t,"end\n")
    table.insert(t,"end\n")
  else
    local clockedLogic = {}

    table.insert(t,"reg validInLastCycle = 1'b0;\n")
    table.insert(t,"always @ (posedge CLK) begin validInLastCycle <= validInThisCycle; end\n")

    -- we make a bram for each full line. 
    assert(stripWidth*bytesPerPixel < BRAM_SIZE_BYTES)
    assert(bytesPerPixel==1 or bytesPerPixel==2 or bytesPerPixel==4)

    local extraBits = math.log(bytesPerPixel)/math.log(2)

    local smallestX = 0
    for k,v in ipairs(consumers) do
      -- HACK: we restrict the entire stencil X to always be <=0 to simplify the linebuffer design
      -- this wouldn't have to be the case if our linebuffer supported more general access patterns
      assert(v:max(1)<=0)
      if v:min(1) < smallestX then smallestX = v:min(1) end
    end

    -- we start these one address ahead of where we want to read this cycle
    -- b/c it takes one cycle to get data out of the ram
    table.insert(t,"reg ["..(10-extraBits)..":0] lbReadAddr = "..(10-extraBits+1).."'d2;\n")
    -- After a valid cycle, we advance the address. We do this because it takes 1 cycle for the new address
    -- to load from ram. We basically preload the value that we are going to need,
    -- but we don't latch it into the SSR until we need it in the SSR (validInNextCycle)
    table.insert(clockedLogic, "if (validInThisCycleX) begin if (lbReadAddr == "..(stripWidth-1)..") begin lbReadAddr <= 0; end else begin lbReadAddr <= lbReadAddr + 1; end end\n")

    table.insert(t,declareReg(datatype,"lastIn"))

    local i=0
    while i>-lines do
      local startAddr = 1
      if downsampledInput==false then
        -- if we're running full tilt, we need to skew where we write into older line buffers -
        -- because it takes 1 clock cycle to read from the address, the older LB address needs to
        -- be skewed by one, and the next one one more, etc
        startAddr = i
      end

      if startAddr <0 then startAddr = startAddr + stripWidth end
      table.insert(t,"reg ["..(10-extraBits)..":0] lbWriteAddr"..numToVarname(i).." = "..valueToVerilogLL(startAddr,false,(10-extraBits))..";\n")
      table.insert(clockedLogic, "if (validInThisCycle) begin if (lbWriteAddr"..numToVarname(i).." == "..(stripWidth-1)..") begin lbWriteAddr"..numToVarname(i).." <= 0; end else begin lbWriteAddr"..numToVarname(i).." <= lbWriteAddr"..numToVarname(i).." + 1; end end\n")

      table.insert(t,declareWire(datatype,"evicted_"..numToVarname(i)))
      table.insert(t,declareWire(datatype,"readout_"..numToVarname(i)))

      local indata = "in"
      local configParams = [=[.WRITE_MODE_A("READ_FIRST"),.WRITE_MODE_B("READ_FIRST")]=]

      local leadingVar = "lb_x1_y"..numToVarname(i)
      if i==0 then leadingVar = "lb_x0_y"..numToVarname(i) end


      if i==0 then
        table.insert(t,declareWire(datatype,leadingVar))
        table.insert(t,"assign "..leadingVar.." = (validInThisCycle)?(in):(lastIn);\n")
      else
        table.insert(t,declareReg(datatype,leadingVar))
        table.insert(clockedLogic,"if (validInLastCycle) begin "..leadingVar.." <= readout_"..numToVarname(i+1).."; end\n")
        indata = "evicted_"..numToVarname(i+1)
      end

      local DIPA = "1'b0"
      if bytesPerPixel==4 then DIPA = "4'b0" end -- needs to be correct for the simulator
      table.insert(t, [=[RAMB16_S]=]..(bytesPerPixel*9)..[=[_S]=]..(bytesPerPixel*9)..[=[ #(]=]..configParams..[=[) ram_line]=]..numToVarname(i)..[=[(
.DIPA(]=]..DIPA..[=[), // needed for the spartan 6 chips for some reason
.ADDRA(lbWriteAddr]=]..numToVarname(i)..[=[),
// we write new data into the oldest entry in the buffer, and simultaneously read the old value out
.DIA(]=]..indata..[=[),
.DOA(evicted_]=]..numToVarname(i)..[=[),
.WEA(validInThisCycle),
.ENA(1'b1),

.WEB(1'b0),
.ENB(1'b1),
.ADDRB(lbReadAddr),
.DOB(readout_]=]..numToVarname(i)..[=[),

.CLKA(CLK),
.CLKB(CLK),
.SSRA(1'b0),
.SSRB(1'b0));
]=])
      i = i - 1
    end

    local leadingVar = "lb_x1_y"..numToVarname(-lines)
    table.insert(t,declareReg(datatype,leadingVar))
    table.insert(clockedLogic,"if (validInLastCycle) begin "..leadingVar.." <= readout_"..numToVarname(-lines+1).."; end\n")

    -- stencil shift register
    -- note that this also codegens for the dangles in the last (oldest) row
    for y=-lines,0 do

      local x=0
      local prev
      if y==0 then 
        x=-1 
        prev = "lb_x"..(x+1).."_y"..numToVarname(y)
      else
        prev = "(validInLastCycle)?(readout_"..numToVarname(y+1).."):(lb_x"..(x+1).."_y"..numToVarname(y)..")"
      end


      while x>=-xpixels do
        local n = "lb_x"..numToVarname(x).."_y"..numToVarname(y)
        table.insert(t,declareReg(datatype,n))
        table.insert(clockedLogic, "if (validInNextCycle) begin "..n.." <= "..prev.."; end // SSR\n")
        prev = n
        x = x - 1
      end
    end

    for k,v in ipairs(consumers) do
      for y=v:min(2),v:max(2) do
        for x=v:min(1),v:max(1) do
          table.insert(t, "assign out"..k.."_x"..numToVarname(x).."_y"..numToVarname(y).." = lb_x"..numToVarname(x).."_y"..numToVarname(y)..";\n")
        end
      end
    end

    table.insert(t,"always @ (posedge CLK) begin\n")
    t = concat(t,clockedLogic)
    table.insert(t,"if (validInThisCycle) begin lastIn <= in; end else if (validInNextCycleX) begin lastIn <= readout_0; end\n")
    table.insert(t,"end\n")

  end

  table.insert(t,"endmodule\n\n")

  return name, t
end

local function fixedBram(conf)
  local A = "A"
  local B = "B"
  if conf.A.chunk>conf.B.chunk then A,B=B,A end
  local res = {}
  table.insert(res,"RAMB16_S"..(conf[A].chunk*9).."_S"..(conf[B].chunk*9).." "..conf.name.."(\n")
    table.insert(res,".DIPA(1'b0),\n")
    table.insert(res,".DIPB(1'b0),\n")
    if conf[A].DI~=nil then table.insert(res,".DIA("..conf[A].DI.."),\n") end
    if conf[B].DI~=nil then table.insert(res,".DIB("..conf[B].DI.."),\n") end
    if conf[A].DO~=nil then table.insert(res,".DOA("..conf[A].DO.."),\n") end
    if conf[B].DO~=nil then table.insert(res,".DOB("..conf[B].DO.."),\n") end
    table.insert(res,".ADDRA("..conf[A].ADDR.."),\n")
    table.insert(res,".ADDRB("..conf[B].ADDR.."),\n")
    table.insert(res,".WEA("..conf[A].WE.."),\n")
    table.insert(res,".WEB("..conf[B].WE.."),\n")
    table.insert(res,".ENA(1'b1),\n")
    table.insert(res,".ENB(1'b1),\n")
    table.insert(res,".CLKA("..conf[A].CLK.."),\n")
    table.insert(res,".CLKB("..conf[B].CLK.."),\n")
    table.insert(res,".SSRA(1'b0),\n")
    table.insert(res,".SSRB(1'b0)\n")
    table.insert(res,");\n\n")
  return res
end

function modules.buffer(moduleName, sizeBytes, inputBytes, outputBytes)
  assert(type(inputBytes)=="number")
  assert(type(outputBytes)=="number")

  local bramCnt = math.ceil(sizeBytes / 2048)
  local extraBits = math.ceil(math.log(bramCnt)/math.log(2))

  local chunkBits = {}
  local outputChunkSize = nearestPowerOf2(outputBytes)
  chunkBits.outaddr = math.log(outputChunkSize)/math.log(2)
  local inputChunkSize = nearestPowerOf2(inputBytes)
  chunkBits.inaddr = math.log(inputChunkSize)/math.log(2)

  -- if inputBytes==1
  local chunkSize, contiguous, strideAddr, nonstrideAddr, strideClk

  if inputBytes==1 then
    chunkSize = nearestPowerOf2(outputBytes)
    contiguous = outputBytes
    strideAddr = "inaddr"
    nonstrideAddr = "outaddr"
    strideClk = "CLK_INPUT"
  elseif outputBytes==1 then
    chunkSize = nearestPowerOf2(inputBytes)
    contiguous = inputBytes
    strideAddr = "outaddr"
    nonstrideAddr = "inaddr"
    strideClk = "CLK_OUTPUT"
  elseif inputBytes==outputBytes then
    chunkSize = nearestPowerOf2(inputBytes)
    contiguous = chunkSize
  else
    assert(false)
  end

  assert(chunkSize<=4)

  local res = {"module "..moduleName.."(\ninput CLK_INPUT, \ninput CLK_OUTPUT,\ninput ["..(10+extraBits-chunkBits.inaddr)..":0] inaddr,\ninput WE,\ninput ["..(inputBytes*8-1)..":0] indata,\ninput ["..(10+extraBits-chunkBits.outaddr)..":0] outaddr,\noutput ["..(outputBytes*8-1)..":0] outdata\n);\n\n"}

  if contiguous~=chunkSize and (outputBytes~=inputBytes) then
    table.insert(res,"reg ["..(10+extraBits-chunkBits[strideAddr])..":0] lastaddr = 0;\n")
    table.insert(res,"reg [4:0] cycleCNT = 0;\n")
    table.insert(res,"reg ["..(10+extraBits-chunkBits[strideAddr])..":0] "..strideAddr.."Internal = 0;\n")
    table.insert(res,"wire ["..(10+extraBits-chunkBits[nonstrideAddr])..":0] "..nonstrideAddr.."Internal = "..nonstrideAddr..";\n")
  else
    table.insert(res,"wire ["..(10+extraBits-chunkBits.inaddr)..":0] inaddrInternal = inaddr;\n")
    table.insert(res,"wire ["..(10+extraBits-chunkBits.outaddr)..":0] outaddrInternal = outaddr;\n")
  end

  local assn = "outdata0"
  for i=0,bramCnt-1 do
    table.insert(res,"wire ["..(outputChunkSize*8-1)..":0] outdata"..i..";\n")

    local bramconf = {name="ram"..i,A={DI="indata",WE="WE", ADDR="inaddrInternal", CLK="CLK_INPUT", chunk=inputChunkSize},
                     B={DO="outdata"..i, WE="1'b0", ADDR="outaddrInternal", CLK="CLK_OUTPUT", chunk=outputChunkSize}}
    if bramCnt > 1 then
      bramconf.A.WE = "(WE && (inaddrInternal["..(10+extraBits-chunkBits.inaddr)..":"..(11-chunkBits.inaddr).."]=="..extraBits.."'d"..i.."))"
    end
    res = concat(res, fixedBram(bramconf))

    if i>0 then assn = "(outaddr["..(10+extraBits-chunkBits.outaddr)..":"..(11-chunkBits.outaddr).."]=="..extraBits.."'d"..i..")? outdata"..i.." : ("..assn..")" end
  end

  table.insert(res, "wire ["..(outputChunkSize*8-1)..":0] outdata_tmp;\n")
  table.insert(res, "assign outdata_tmp = "..assn..";\n")
  table.insert(res, "assign outdata = outdata_tmp["..(outputBytes*8-1)..":0];\n")

  if contiguous~=chunkSize and (outputBytes~=inputBytes) then
    table.insert(res,[=[always @(posedge ]=]..strideClk..[=[) begin
  if(]=]..strideAddr..[=[ != lastaddr) begin
    if(]=]..strideAddr..[=[==0) begin
      cycleCNT <= 0;
      ]=]..strideAddr..[=[Internal <= 0;
    end else if(cycleCNT == ]=]..(contiguous-1)..[=[) begin
      cycleCNT <= 0;
      ]=]..strideAddr..[=[Internal <= ]=]..strideAddr..[=[Internal+1+]=]..(chunkSize-contiguous)..[=[;
    end else begin
      cycleCNT <= cycleCNT+1;
      ]=]..strideAddr..[=[Internal <= ]=]..strideAddr..[=[Internal+1;
    end

  end
  lastaddr <= ]=]..strideAddr..[=[;
end
]=])

  end

  table.insert(res,"endmodule\n\n")
  return res
end

function modules.sim(inputBytes, outputBytes, stripWidth, imageHeight, outputShift, metadata)
  assert(type(inputBytes)=="number")
  assert(type(outputBytes)=="number")
  assert(type(stripWidth)=="number")

  return [=[`define EOF 32'hFFFF_FFFF
module sim;
  integer file, c, r,fileout;
  reg     CLK;
  reg []=]..(inputBytes*8-1)..[=[:0] pipelineInput;
  wire []=]..(outputBytes*8-1)..[=[:0] pipelineOutput;
  reg [12:0] posX = 0;
  reg [12:0] posY = 0;
  reg inValid = 0;
  wire outValid;
  integer realX = ]=]..(stripWidth+metadata.padMaxX-1)..[=[;
  integer realY = ]=]..(metadata.padMinY-1)..[=[;
  integer addr = -PIPE_DELAY+1-]=]..outputShift..[=[;
  reg [10000:0] inputFilename;
  reg [10000:0] outputFilename; 
  reg [7:0] i = 0;

  Pipeline pipeline(.CLK(CLK),.inX(posX),.inY(posY),.packedinput(pipelineInput),.out(pipelineOutput),.inValid(inValid),.outValid(outValid));

  initial begin
   $display("HELLO");

   $value$plusargs("inputFilename=%s",inputFilename);
   $value$plusargs("outputFilename=%s",outputFilename);

   file = $fopen(inputFilename,"r");
   fileout = $fopen(outputFilename,"w");

   // prime the pipe
   posX = realX;
   posY = realY;
   inValid = 0;
   CLK = 0;
   #10
   CLK = 1;
   #10

   realY = ]=]..(metadata.padMinY)..[=[;
   while (realY < ]=]..(imageHeight+metadata.padMaxY)..[=[) begin
     realX = ]=]..(metadata.padMinX)..[=[;
     while (realX < ]=]..(stripWidth+metadata.padMaxX)..[=[) begin
       if ( realX>=0 && realX<]=]..stripWidth..[=[ && realY>=0 && realY <]=]..imageHeight..[=[ ) begin
         pipelineInput = $fgetc(file);
       end else begin
         pipelineInput = 0;
       end
       posX = realX;
       posY = realY;
       inValid = 1;
       CLK = 0;
       #10
       CLK = 1;
       #10
//     $display(modOutput);

       if(addr>=0 && outValid) begin 
         i = 0;
         while( i<]=]..outputBytes..[=[) begin
           $fwrite(fileout, "%c", pipelineOutput[i*8+:8]); 
           i = i + 1;
         end
       end

       addr = addr + 1;
       realX = realX + 1;
     end
     realY = realY + 1;
   end // while (c != `EOF)

   // drain pipe
   addr = -PIPE_DELAY-]=]..outputShift..[=[;

   realX = ]=]..(metadata.padMinX)..[=[;
   while (addr<0) begin
     pipelineInput = 0;
     posX = realX;
     posY = realY;
     inValid = 1;
     CLK = 0;
     #10
     CLK = 1;
     #10
     if (outValid) begin 
       i=0;
       while( i<]=]..outputBytes..[=[) begin
         $fwrite(fileout, "%c", pipelineOutput[i*8+:8]);
         i = i + 1;
       end
     end
     addr = addr + 1;
     realX = realX + 1;
   end	   

   $display("DONE");
   $fclose(fileout);
  end // initial begin

endmodule // sim        ]=]
end

function modules.tx(clockMhz, uartClock)
assert(type(clockMhz)=="number")
assert(type(uartClock)=="number")
  return {[=[module TXMOD(
input CLK,
output TX,
input [7:0] inbits,
input enable,
output ready // when true, we're ready to transmit a new bit
    );

  reg TXd = 1;
  assign TX = TXd;

  reg [28:0] d;
  wire [28:0] dInc = d[28] ? (]=]..uartClock..[=[) : (]=]..uartClock..[=[ - ]=]..clockMhz..[=[000000);
  wire [28:0] dNxt = d + dInc;
  always @(posedge CLK)
  begin
    d = dNxt;
  end
  wire SLOWCLK = ~d[28]; // this is the 115200 Hz clock


  reg [3:0] counter = 0;

  always @ (posedge SLOWCLK)
  begin
    if(enable && counter==0) begin
      TXd <= 0; // signal start
      counter <= 1;
    end else if(enable && counter==9) begin
      TXd <= 1; // signal end
      counter <= 0;
    end else if(enable) begin
		  TXd <= inbits[counter-1];
      counter <= counter + 1;
    end else begin
	   TXd <= 1;
     counter <= 0;
	 end
  end

  reg readyBitSent = 0;
  reg readyBit = 0;
  assign ready = readyBit;
  
  always @ (posedge CLK)
  begin
    if(enable && counter==0 && readyBitSent==0) begin      
      readyBit <= 1;
      readyBitSent <= 1;
    end else if(enable && counter==0 ) begin
      readyBit <= 0;
    end else begin
      readyBit <= 0;
		readyBitSent <= 0;
    end
  end

endmodule

]=]}
end

function modules.rx(clockMhz, uartClock)
assert(type(clockMhz)=="number")
assert(type(uartClock)=="number")
return {[=[module RXMOD(
input RX, 
input CLK,
output [7:0] outbits,
output outvalid
    );

reg [8:0] data;
assign outbits = data[8:1];

reg [3:0] readClock = 0; // which subclock?
reg [3:0] readBitClock = 0; // which bit?
reg reading = 0;

reg outvalidReg = 0;
assign outvalid = outvalidReg;


// we'd better see some 1s on the line before we start reading data
//reg [7:0] started = 0;

reg [28:0] d;
  wire [28:0] dInc = d[28] ? (]=]..(uartClock*16)..[=[) : (]=]..(uartClock*16)..[=[ - ]=]..clockMhz..[=[000000);
  wire [28:0] dNxt = d + dInc;
  always @(posedge CLK)
  begin
    d = dNxt;
  end
  wire SMPCLK = ~d[28]; // this is the 115200 Hz clock


always @ (posedge SMPCLK)
begin
  if(RX==0 && reading==0) begin
    reading <= 1;
    readClock <= 0;
    readBitClock <= 0;
  end else if(reading==1 && readClock==7 && readBitClock==9) begin
    // we're done
    reading <= 0;
    readClock <= readClock + 1;
  end else if(reading==1 && readClock==7) begin
    // read a byte
    data[readBitClock] <= RX;
    readClock <= readClock + 1;
	 readBitClock <= readBitClock + 1;
  end else begin
    readClock <= readClock + 1;
  end
end

reg wrote = 0;

always @(posedge CLK)
begin
  if(RX==0 && reading==0) begin
  	 wrote <= 0;
	 outvalidReg <= 0;
  end else if(reading==1 && readClock==7 && readBitClock==9 && wrote==0) begin
    outvalidReg <= 1;
	 wrote <= 1;
  end else begin
    outvalidReg <= 0;
  end
end

endmodule

]=]}
end

function modules.stageUART(options, inputBytes, outputBytes, stripWidth, stripHeight)

  local result = {}
  result = concat(result, fpga.modules.tx(options.clockMhz, options.uartClock))
  result = concat(result, fpga.modules.rx(options.clockMhz, options.uartClock))
  result = concat(result, fpga.modules.buffer("InputBuffer",stripWidth*stripHeight,1,inputBytes))
  result = concat(result, fpga.modules.buffer("OutputBuffer",stripWidth*stripHeight,outputBytes,1))

  local pxcnt = stripWidth*stripHeight
  local metadataBytes = 4
  local rxStartAddr = math.pow(2,13)-metadataBytes

  local shiftInMetadata = "metadata[31:24] <= rxbits;\n"
  for i=0,metadataBytes-2 do
    shiftInMetadata = shiftInMetadata .. "metadata["..(i*8+7)..":"..(i*8).."] <= metadata["..(i*8+15)..":"..(i*8+8).."];\n"
  end

table.insert(result, [=[module stage(
input CLK, 
input RX, 
output TX,
output [7:0] LED);

reg [12:0] addr = ]=]..rxStartAddr..[=[;
reg [12:0] sendAddr = -1;

reg []=]..(metadataBytes*8-1)..[=[:0] metadata = 0;
reg [12:0] posX = 0;
reg [12:0] posY = 0;

reg receiving = 1;
reg processing = 0;
reg sending = 0;

wire [7:0] rxbits;
wire []=]..(inputBytes*8-1)..[=[:0] pipelineInput;
reg [12:0] pipelineReadAddr = 0; 
InputBuffer inputBuffer(.CLK_INPUT(CLK), .CLK_OUTPUT(CLK), .inaddr(addr), .WE(receiving), .indata(rxbits), .outaddr(pipelineReadAddr), .outdata(pipelineInput));

wire []=]..(outputBytes*8-1)..[=[:0] pipelineOutput;
wire [7:0] outbuf;
reg [12:0] pipelineWriteAddr = -PIPE_DELAY; // pipe delay
OutputBuffer outputBuffer(.CLK_INPUT(CLK), .CLK_OUTPUT(CLK), .inaddr(pipelineWriteAddr), .WE(processing), .indata(pipelineOutput), .outaddr(sendAddr), .outdata(outbuf));

Pipeline pipeline(.CLK(CLK), .inX(posX+metadata[12:0]), .inY(posY+metadata[28:16]), .packedinput(pipelineInput), .out(pipelineOutput));

reg [7:0] rxCRC = 0;
reg [7:0] sendCRC = 0;
   
wire rxvalid;
wire txready;
RXMOD rxmod(.RX(RX),.CLK(CLK),.outbits(rxbits),.outvalid(rxvalid));
TXMOD txmod(.TX(TX),.CLK(CLK),.inbits( (sendAddr>]=]..(pxcnt*outputBytes-1)..[=[)?((sendAddr==]=]..(pxcnt*outputBytes)..[=[)?sendCRC:rxCRC):outbuf),.enable(sending),.ready(txready));

always @(posedge CLK) begin
  if(receiving) begin
  if(addr == ]=]..(pxcnt*inputBytes)..[=[) begin
      addr <= ]=]..rxStartAddr..[=[;
      receiving <= 0;
		  sending <= 0;
		  processing <= 1;
      pipelineReadAddr <= 1; // it will have addr 0 valid on the output on next clock, then 1 on the output on following clock
    end else if(rxvalid) begin
      if(addr>=]=]..rxStartAddr..[=[) begin
        ]=]..shiftInMetadata..[=[
      end
      addr <= addr + 1;
      rxCRC <= rxCRC + rxbits;
    end
  end
  
  if(processing) begin
    if(rxvalid) begin // restart if new data comes in
      sending <= 0;
      receiving <= 1;
      rxCRC <= 0;
      processing <= 0;
      sendAddr <= -1;
      pipelineWriteAddr <= -PIPE_DELAY;
      pipelineReadAddr <= 0;
      posX <= 0;
      posY <= 0;
    end else if(pipelineWriteAddr == ]=]..pxcnt..[=[) begin
      pipelineWriteAddr <= -PIPE_DELAY;
		  pipelineReadAddr <= 0;
      posX <= 0;
      posY <= 0;
      receiving <= 0;
		  sending <= 1;
		  processing <= 0;
    end else begin
	    pipelineReadAddr <= pipelineReadAddr + 1;
      pipelineWriteAddr <= pipelineWriteAddr + 1;
      if (posX == ]=]..(stripWidth-1)..[=[) begin
        posX <= 0;
        posY <= posY+1; // inc y
      end else begin
        posX <= posX + 1; // inc x
      end
	 end
  end
  
  if(sending) begin
    if(rxvalid) begin // restart if new data comes in
      sending <= 0;
      receiving <= 1;
      rxCRC <= 0;
      processing <= 0;
      sendAddr <= -1;
      end else if(sendAddr==]=]..(pxcnt*outputBytes+2)..[=[) begin
      // we're done
      sending <= 0;
      receiving <= 1;
      rxCRC <= 0;
      processing <= 0;
	    sendAddr <= -1;
      sendCRC <= 0;
    end else if(txready) begin
      sendAddr <= sendAddr + 1;
      if (sendAddr >= 0 && sendAddr < ]=]..(pxcnt*outputBytes)..[=[) begin sendCRC <= sendCRC + outbuf; end
    end
  end

end

assign LED = {addr[6:1],receiving,processing,sending};
endmodule

]=])

  return table.concat(result,"")
end

function modules.sccbctrl()
return [=[module SCCBCtrl (clk_i, rst_i, sccb_clk_i, data_pulse_i, addr_i, data_i, data_o, rw_i, start_i, ack_error_o, 
                  done_o, sioc_o, siod_io);

   input       clk_i;               // Main clock.
   input       rst_i;               // Reset.
   input       sccb_clk_i;          // SCCB clock. Typical - 100KHz as per SCCB spec.
   input       data_pulse_i;        // Negative mid sccb_clk_i cycle pulse.
   input       [7:0] addr_i;        // Device ID. Bit 0 is ignored since read/write operation is specified by rw_i.
   input       [15:0] data_i;       // Register address in [15:8] and data to write in [7:0] if rw_i = 1 (write).
                                    // Register address in [15:8] if rw_i = 0 (read).
   output reg  [7:0] data_o;        // Data received if rw_i = 0 (read).
   input       rw_i;                // 0 - read command. 1 - write command. 
   input       start_i;             // Start transaction.
   output      ack_error_o;         // Error occurred during the transaction.
   output reg  done_o;              // 0 - transaction is in progress. 1 - transaction has completed.
   output      sioc_o;              // SIOC line.
   inout       siod_io;             // SIOD line. External pull-up resistor required.
   

   reg         sccb_stm_clk = 1;
   reg         [6:0] stm = 0;
   reg         bit_out = 1;
   reg         ack_err1 = 1;
   reg         ack_err2 = 1;
   reg         ack_err3 = 1;
   
   assign   sioc_o = (start_i == 1 && 
                     (stm >= 5 && stm <= 12 || stm == 14 ||   
                     stm >= 16 && stm <= 23 || stm == 25 ||
                     stm >= 27 && stm <= 34 || stm == 36 ||
                     stm >= 44 && stm <= 51 || stm == 53 ||
                     stm >= 55 && stm <= 62 || stm == 64)) ? sccb_clk_i : sccb_stm_clk;
                     
   // Output acks and read data only.
   assign   siod_io = (stm == 13 || stm == 14 || stm == 24 || stm == 25 || stm == 35 || stm == 36 ||
                        stm == 52 || stm == 53 || stm >= 54 && stm <= 62) ? 1'bz : bit_out;
                       
   assign   ack_error_o = ack_err1 | ack_err2 | ack_err3;
   //assign   ack_error_o = ack_err1 || ack_err2;
   //assign   ack_error_o = ack_err1;

   always @(posedge clk_i or negedge rst_i) begin
      if(rst_i == 0) begin 
         stm <= 0;
         sccb_stm_clk <= 1;
         bit_out <= 1; 
         data_o <= 0;  
         done_o <= 0;
         ack_err1 <= 1; 
         ack_err2 <= 1; 
         ack_err3 <= 1;          
      end else if (data_pulse_i) begin
         if (start_i == 0 || done_o == 1) begin
            stm <= 0;
         end else if (rw_i == 0 && stm == 25) begin
            stm <= 37;
         end else if (rw_i == 1 && stm == 36) begin
            stm <= 65;
         end else if (stm < 68) begin
            stm <= stm + 1;
         end

         if (start_i == 1) begin
                (* parallel_case *) case(stm)
                  // Initialize
                  7'd0 : bit_out <= 1;
                  7'd1 : bit_out <= 1;

                  // Start write transaction.
                  7'd2 : bit_out <= 0;
                  7'd3 : sccb_stm_clk <= 0;
                  
                  // Write device`s ID address.
                  7'd4 : bit_out <= addr_i[7];
                  7'd5 : bit_out <= addr_i[6];
                  7'd6 : bit_out <= addr_i[5];
                  7'd7 : bit_out <= addr_i[4];
                  7'd8 : bit_out <= addr_i[3];
                  7'd9 : bit_out <= addr_i[2];
                  7'd10: bit_out <= addr_i[1];
                  7'd11: bit_out <= 0;
                  7'd12: bit_out <= 0;
                  7'd13: ack_err1 <= siod_io;
                  7'd14: bit_out <= 0;
                  
                  // Write register address.
                  7'd15: bit_out <= data_i[15];
                  7'd16: bit_out <= data_i[14];
                  7'd17: bit_out <= data_i[13];
                  7'd18: bit_out <= data_i[12];
                  7'd19: bit_out <= data_i[11];
                  7'd20: bit_out <= data_i[10];
                  7'd21: bit_out <= data_i[9];
                  7'd22: bit_out <= data_i[8];
                  7'd23: bit_out <= 0;
                  7'd24: ack_err2 <= siod_io;
                  7'd25: bit_out <= 0;
                  
                  // Write data. This concludes 3-phase write transaction.
                  7'd26: bit_out <= data_i[7];
                  7'd27: bit_out <= data_i[6];
                  7'd28: bit_out <= data_i[5];
                  7'd29: bit_out <= data_i[4];
                  7'd30: bit_out <= data_i[3];
                  7'd31: bit_out <= data_i[2];
                  7'd32: bit_out <= data_i[1];
                  7'd33: bit_out <= data_i[0];
                  7'd34: bit_out <= 0;
                  7'd35: ack_err3 <= siod_io;
                  7'd36: bit_out <= 0;

                  // Stop transaction.
                  7'd37: sccb_stm_clk <= 0; 
                  7'd38: sccb_stm_clk <= 1;   
                  7'd39: bit_out <= 1;

                  // Start read tranasction. At this point register address has been set in prev write transaction.  
                  7'd40: sccb_stm_clk <= 1;
                  7'd41: bit_out <= 0;
                  7'd42: sccb_stm_clk <= 0;
                  
                  // Write device`s ID address.
                  7'd43: bit_out <= addr_i[7];
                  7'd44: bit_out <= addr_i[6];
                  7'd45: bit_out <= addr_i[5];
                  7'd46: bit_out <= addr_i[4];
                  7'd47: bit_out <= addr_i[3];
                  7'd48: bit_out <= addr_i[2];
                  7'd49: bit_out <= addr_i[1];
                  7'd50: bit_out <= 1;
                  7'd51: bit_out <= 0;
                  7'd52: ack_err3 <= siod_io;
                  7'd53: bit_out <= 0;
                  
                  // Read register value. This concludes 2-phase read transaction.
                  7'd54: bit_out <= 0; 
                  7'd55: data_o[7] <= siod_io;
                  7'd56: data_o[6] <= siod_io; 
                  7'd57: data_o[5] <= siod_io; 
                  7'd58: data_o[4] <= siod_io;
                  7'd59: data_o[3] <= siod_io;
                  7'd60: data_o[2] <= siod_io; 
                  7'd61: data_o[1] <= siod_io;
                  7'd62: data_o[0] <= siod_io;
                  7'd63: bit_out <= 1;
                  7'd64: bit_out <= 0;

                  // Stop transaction.
                  7'd65: sccb_stm_clk <= 0;
                  7'd66: sccb_stm_clk <= 1;
                  7'd67: begin 
                     bit_out <= 1;
                     done_o <= 1;
                  end
                  default: sccb_stm_clk <= 1;
               endcase
            
         end else begin
            sccb_stm_clk <= 1;
            bit_out <= 1; 
            data_o <= data_o;
            done_o <= 0;
            ack_err1 <= 1; 
            ack_err2 <= 1; 
            ack_err3 <= 1;
         end
      end
   end
   
endmodule

]=]
end

function modules.vga()
return [=[module VGA(input CLK,
input RST,
output [9:0] X,
output [9:0] Y,
output VGA_CLK,
input [7:0] R,
input [7:0] G,
input [7:0] B,
                          output VGA_VSYNC, 
                          output VGA_HSYNC,
                          output [2:0] VGA_RED,
                          output [2:0] VGA_GREEN,
                          output [1:0] VGA_BLUE);

parameter DATA_WIDTH = 640;
parameter DATA_HEIGHT = 480;

parameter H_FRONT_PORCH = 16;
parameter H_BACK_PORCH = 48;
parameter H_PULSE = 96;
parameter H_TOTAL = 800;

parameter V_FRONT_PORCH = 10;
parameter V_BACK_PORCH = 33;
parameter V_PULSE = 2;
parameter V_TOTAL = 525; 

//wire rst = 0;//~JOY_UP;

wire CLK50; 
assign VGA_CLK = CLK50;
  DCM_SP #(
.CLKDV_DIVIDE(2.0), // Divide by: 1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5
//   7.0,7.5,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0 or 16.0
.CLKFX_DIVIDE(14),   // Can be any integer from 1 to 32
.CLKFX_MULTIPLY(11), // Can be any integer from 2 to 32
.CLKIN_DIVIDE_BY_2("FALSE"), // TRUE/FALSE to enable CLKIN divide by two feature
.CLKIN_PERIOD(31.25),  // Specify period of input clock
.CLKOUT_PHASE_SHIFT("NONE"), // Specify phase shift of NONE, FIXED or VARIABLE
.CLK_FEEDBACK("1X"),  // Specify clock feedback of NONE, 1X or 2X
.DESKEW_ADJUST("SYSTEM_SYNCHRONOUS"), // SOURCE_SYNCHRONOUS, SYSTEM_SYNCHRONOUS or
//   an integer from 0 to 15
.DLL_FREQUENCY_MODE("LOW"),  // HIGH or LOW frequency mode for DLL
.DUTY_CYCLE_CORRECTION("TRUE"), // Duty cycle correction, TRUE or FALSE
.PHASE_SHIFT(0),     // Amount of fixed phase shift from -255 to 255
.STARTUP_WAIT("FALSE")   // Delay configuration DONE until DCM LOCK, TRUE/FALSE
) DCM_SP_inst (
//.CLK0(CLK0),     // 0 degree DCM CLK output
//.CLK180(CLK180), // 180 degree DCM CLK output
//.CLK270(CLK270), // 270 degree DCM CLK output
//.CLK2X(CLK2X),   // 2X DCM CLK output
//.CLK2X180(CLK2X180), // 2X, 180 degree DCM CLK out
//.CLK90(CLK90),   // 90 degree DCM CLK output
//.CLKDV(CLKDV),   // Divided DCM CLK out (CLKDV_DIVIDE)
.CLKFX(CLK50),   // DCM CLK synthesis out (M/D)
//.CLKFX180(CLKFX180), // 180 degree CLK synthesis out
//.LOCKED(LOCKED), // DCM LOCK status output
//.PSDONE(PSDONE), // Dynamic phase adjust done output
//.STATUS(STATUS), // 8-bit DCM status bits output
//.CLKFB(CLKFB),   // DCM clock feedback
.CLKIN(CLK),   // Clock input (from IBUFG, BUFG or DCM)
//.PSCLK(PSCLK),   // Dynamic phase adjust clock input
//.PSEN(PSEN),     // Dynamic phase adjust enable input
//.PSINCDEC(PSINCDEC), // Dynamic phase adjust increment/decrement
.RST(0)        // DCM asynchronous reset input
);


reg [9:0] posx;
reg [9:0] posy;

reg [4:0] JX;
reg [4:0] JY;
 
assign xreset = RST || (posx == (H_TOTAL-1));
assign yreset = RST || (posy ==  (V_TOTAL-1));

wire frontporch;
assign frontporch = (posx == DATA_WIDTH-20);

//wire [9:0] Y;
//wire [9:0] X;
assign Y = posy;
assign X = posx;
assign valid = X < DATA_WIDTH && Y < DATA_HEIGHT;

assign injoy = valid && (X < 16*JX + 16 && X >= 16*JX &&
                         Y < 16*JY + 16 && Y >= 16*JY); 

assign c = valid && (Y[4:4] ^ X[4:4]);
assign VGA_RED = {R[2:2] && valid, R[1:1] && valid, R[0:0] && valid};
//assign VGA_GREEN = {c ,c , c};
assign VGA_GREEN = {G[2:2] && valid, G[1:1] && valid, G[0:0] && valid};
//assign VGA_BLUE = {c,c};
assign VGA_BLUE = {B[1:1] && valid, B[0:0] && valid};

always @(posedge CLK50)
    if(xreset)
        posx <= 0;
    else
        posx <= posx + 1;
        
always @(posedge CLK50)
    if(frontporch)
        if(yreset)
            posy <= 0;
        else
            posy <= posy + 1;

reg hsync;
always @(posedge CLK50)
    if(posx == DATA_WIDTH+H_FRONT_PORCH)
        hsync <= 0;
    else if(RST || posx == DATA_WIDTH+H_FRONT_PORCH+H_PULSE)
        hsync <= 1;
		  
reg vsync;
always @(posedge CLK50)
    if(posy == DATA_HEIGHT+V_FRONT_PORCH && frontporch)
        vsync <= 0;
    else if(RST || (posy == DATA_HEIGHT+V_FRONT_PORCH+V_PULSE && frontporch))
        vsync <= 1;

assign VGA_VSYNC = vsync;
assign VGA_HSYNC = hsync;

endmodule

]=]

end

function modules.stageVGA()
  local res = {}

  table.insert(res, modules.sccbctrl())
  table.insert(res, modules.vga())
  table.insert(res, table.concat(modules.buffer("OutputBuffer",2048*4,3,3),""))

table.insert(res,[=[module stage(input RAW_CLK,
input [7:0] CAM_DOUT,
input CAM_VSYNC,
input CAM_HREF,
output CAM_SCL,
inout CAM_SDATA,
output CAM_PWDN,
input CAM_PCLK,
output CAM_XCLK,
output VGA_VSYNC, 
output VGA_HSYNC,
output [2:0] VGA_RED,
output [2:0] VGA_GREEN,
output [1:0] VGA_BLUE,
output LED1,
input REAL_JOYLEFT,
input REAL_JOYRIGHT
    ); 

wire CLK;
IBUFG clockbuffer ( .I(RAW_CLK), .O(CLK));

reg JOY = 1;
reg JOYDOWN = 1;
reg JOYUP = 1;
reg JOYLEFT = 1;
reg JOYRIGHT = 1;

reg jlset = 0;

reg [25:0] switchCnt = 0;
always @(posedge CLK) begin
	switchCnt <= switchCnt + 1;
	if(switchCnt[25:23]==3'b001 && jlset==0) begin
	   JOYRIGHT <= 0;
		JOYLEFT <= 1;
		JOYDOWN <= 1;
	end else if(switchCnt[25:23]==3'b011 && jlset==0) begin
		JOYLEFT <= 0;
		JOYRIGHT <= 1;
		JOYDOWN <= 1;
	end else if(switchCnt[25:23]==3'b101 && jlset==0) begin
		JOYDOWN <= 0;
		JOYLEFT <= 1;
		JOYRIGHT <= 1;
   end else if(switchCnt[25:23]==3'b111) begin
	  jlset <= 1;
	end else begin
	  JOYLEFT <= 1;
	  JOYRIGHT <= 1;
	  JOYDOWN <= 1;
	end
end

wire PCLK;
IBUFG clockbuffer2 ( .I(CAM_PCLK), .O(PCLK));

parameter   IN_FREQ = 32_000_000;   // clk_i frequency in Hz.
localparam  SCCB_FREQ = 100_000;    // SCCB frequency in Hz.
localparam  SCCB_PERIOD = IN_FREQ/SCCB_FREQ/2;
reg         [8:0] sccb_clk_cnt = 0;
reg         sccb_clk = 0;
wire        data_pulse = (sccb_clk_cnt == SCCB_PERIOD/2 && sccb_clk == 0); 
parameter   CAM_ID = 8'h42;         // OV7670 ID. Bit 0 is Don't Care since we specify r/w op in register lookup table.

// Generate clock for the SCCB.
   always @(posedge CLK) begin
      if (0) begin
         sccb_clk_cnt <= 0;
         sccb_clk <= 0;
      end else begin
         if (sccb_clk_cnt < SCCB_PERIOD) begin
            sccb_clk_cnt <= sccb_clk_cnt + 1;
         end else begin
            sccb_clk <= ~sccb_clk;
            sccb_clk_cnt <= 0;
         end
      end
   end

reg transDone = 0;

wire ack_error;
assign LED1 = transDone;

//reg [15:0] data = 16'h1540; //hsync
//reg [15:0] data = 16'h1204; //rgb mode
//reg [15:0] data = 16'h1280; 
//reg [15:0] data = 16'h0A80; // product ID
//reg [15:0] data = 16'h0B80; // com4
//40D0 - 565
//1280 - reset
//0010 - gain
// 1215 - QVGA raw 
// 0e81 - high frame rate mode?
//1709 - this sort of changed it to the left a little
//reg [15:0] data = (JOYLEFT==0)? 16'h1204 : 16'h1540; //rgb mode
//reg [15:0] data = (JOYDOWN==0)?16'h0E80:((JOYLEFT==0)? 16'h1210 : 16'h1542); //rgb mode
//reg [15:0] data = (JOYDOWN==0)?16'h1700:((JOYLEFT==0)? 16'h1205 : ((JOYRIGHT==0)? 16'h1542 : ((JOYUP==0) ? 16'h1841 : 16'h1280) )); //rgb mode
//reg [15:0] data = (JOYDOWN==0)?16'h1700:((JOYLEFT==0)? 16'h1205 : ((JOYRIGHT==0)? 16'h1542 : ((JOYUP==0) ? 16'h32A0 : 16'h1280) )); //rgb mode
//reg [15:0] data = (JOYDOWN==0)?16'h0E80:((JOYLEFT==0)? 16'h1215 : ((JOYRIGHT==0)? 16'h0e81 : ((JOYUP==0) ? 16'h32A0 : 16'h1280) )); //rgb mode

reg [15:0] data = (JOYDOWN==0)?16'h1542:((JOYLEFT==0)? 16'h1205 : ((JOYRIGHT==0)? 16'h0e81 : ((JOYUP==0) ? 16'h0e81 : 16'h1280) )); //rgb mode
    
wire [7:0] dataOut;
wire done;

SCCBCtrl sccb(.clk_i(CLK),
.rst_i(1'b1),
.sccb_clk_i(sccb_clk),
.data_pulse_i(data_pulse), 
.addr_i(CAM_ID),
.data_i(data), 
.data_o(dataOut), 
//.rw_i(JOYDOWN==0 || JOYLEFT==0), 
.rw_i(1'b1), 
.start_i( (JOY==0 || JOYLEFT==0 || JOYDOWN==0 || JOYUP==0 || JOYRIGHT==0) && transDone==0), 
.ack_error_o(ack_error), 
.done_o(done), 
.sioc_o(CAM_SCL), 
.siod_io(CAM_SDATA));
			
//assign CAM_SCL = 	sccb_clk;
//assign CAM_SDATA = 1'bz;
 
wire RCLK48;
   

DCM_CLKGEN #(
   .CLKFXDV_DIVIDE(2),
   .CLKFX_DIVIDE(125),
   .CLKFX_MD_MAX(0.0),
   .CLKFX_MULTIPLY(187),
   .CLKIN_PERIOD(31.25),
   .STARTUP_WAIT("FALSE")
)
DCM_CLKGEN_inst (
   .CLKFX(RCLK48),
//   .CLKFX180(CLKFX180),
//   .CLKFXDV(CLKFXDV),
//   .LOCKED(LOCKED), 
//   .PROGDONE(PROGDONE),
//   .STATUS(STATUS),
   .CLKIN(CLK),
.FREEZEDCM(1'b0),
.PROGCLK(1'b0),
.PROGDATA(1'b0),
.PROGEN(1'b0),
   .RST(1'b0)
);


 wire CLK24;
 ODDR2 #(
    .DDR_ALIGNMENT("NONE"), // Sets output alignment to "NONE", "C0" or "C1" 
    .SRTYPE    ("SYNC") // Specifies "SYNC" or "ASYNC" set/reset
) ODDR2_inst (
    .Q     (CLK24),   // 1-bit DDR output data
    .C0    (RCLK48),   // 1-bit clock input
    .C1    (~RCLK48),  // 1-bit clock input
    .CE    (1'b1),       // 1-bit clock enable input
    .D0    (1'b1),       // 1-bit data input (associated with C0)
    .D1    (1'b0),       // 1-bit data input (associated with C1)
    .R     (1'b0),       // 1-bit reset input
    .S     (1'b0) );     // 1-bit set input

reg [7:0] pipelineInput;
wire [23:0] pipelineOutput;

reg [12:0] posX = 0;
reg [12:0] posY = 0;
reg [10:0] writeAddr = 0;
reg [10:0] readAddr = 0;
reg VGA_RST = 0;
reg vgaSyncOccured = 0; // we only need to reset VGA once
reg CAM_HREF_LAST;
reg CAM_VSYNC_R;
reg CAM_HREF_R;

reg [24:0] phase = 0;
always @(posedge PCLK) begin
  pipelineInput <= CAM_DOUT;
  if(CAM_VSYNC_R==0) begin
    posY <= 0;
    if(vgaSyncOccured==0) begin
      VGA_RST <= 1;
    end
  end else begin
    VGA_RST <= 0;
    vgaSyncOccured <= vgaSyncOccured || (VGA_RST==1);

    if(CAM_HREF_R==1 && CAM_HREF_LAST==0) begin
     posY <= posY + 1;
     posX <= 0;
    end else begin
     posX <= posX + 1;
    end
  end

  CAM_HREF_R <= CAM_HREF;
  CAM_HREF_LAST <= CAM_HREF_R;
  CAM_VSYNC_R <= CAM_VSYNC;
  writeAddr <= posX+phase[24:15];
  if(REAL_JOYLEFT==0) begin
    phase <= phase - 1;
  end

  if(REAL_JOYRIGHT==0) begin
    phase <= phase + 1;
  end
end

wire VGA_CLK;
wire [23:0] outdata;
wire [7:0] VGA_IN_R;
assign VGA_IN_R = outdata[7:0];
wire [7:0] VGA_IN_G;
assign VGA_IN_G = outdata[15:8];
wire [7:0] VGA_IN_B;
assign VGA_IN_B = outdata[23:16];
wire [9:0] VGA_X;
wire [9:0] VGA_Y;

//assign VGA_GREEN={posX==0,CAM_VSYNC_R,CAM_HREF_R}; // for debug
VGA vga(.CLK(CLK), .RST(VGA_RST), .VGA_CLK(VGA_CLK), .VGA_VSYNC(VGA_VSYNC), .VGA_HSYNC(VGA_HSYNC), .VGA_RED(VGA_RED), .VGA_GREEN(VGA_GREEN), .VGA_BLUE(VGA_BLUE), .R(VGA_IN_R), .G(VGA_IN_G), .B(VGA_IN_B),.X(VGA_X), .Y(VGA_Y));
//pipelineOutput
OutputBuffer outputBuffer(.CLK_INPUT(PCLK), .CLK_OUTPUT(VGA_CLK), .WE(1'b1), .inaddr(writeAddr), .indata(pipelineOutput), .outaddr(readAddr), .outdata(outdata));
Pipeline pipeline(.CLK(PCLK), .inX(posX), .inY(posY), .packedinput(pipelineInput), .out(pipelineOutput));

always @(posedge VGA_CLK) begin
  readAddr <= VGA_X;
//  VGA_IN_R <= VGA_X[5:5];
end

assign CAM_XCLK = CLK24;
assign CAM_PWDN = 1'b0;

always @(posedge CLK) begin
  transDone = (transDone | done) && JOY && JOYLEFT && JOYDOWN && JOYRIGHT && JOYUP;
end

endmodule
]=])

  return table.concat(res,"")
end

function modules.axi(inputBytes, outputBytes, stripWidth, metadata)
  return [=[module PipelineInterface(input CLK,input validIn, output validOut, input []=]..(inputBytes*8-1)..[=[:0] pipelineInput, output []=]..(outputBytes*8-1)..[=[:0] pipelineOutput);
reg [12:0] posX = ]=]..valueToVerilogLL(metadata.padMinX,true,13)..[=[;
reg [12:0] posY = ]=]..valueToVerilogLL(metadata.padMinY,true,13)..[=[;
Pipeline pipeline(.CLK(CLK),.inX(posX),.inY(posY),.packedinput(pipelineInput),.out(pipelineOutput),.inValid(validIn),.outValid(validOut));
always @ (posedge CLK) begin
  if (validIn) begin
    if (posX == ]=]..(stripWidth+metadata.padMaxX-1)..[=[) begin
      posX <= 0;
      posY <= posY + 1;
    end else begin
      posX <= posX + 1;
    end
  end else begin
    posX <= ]=]..valueToVerilogLL(metadata.padMinX,true,13)..[=[;
    posY <= ]=]..valueToVerilogLL(metadata.padMinY,true,13)..[=[;
  end
end
endmodule]=]
end

return modules