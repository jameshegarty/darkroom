local fpga = {}
fpga.util = terralib.require("fpgautil")

--UART_CLOCK = 115200
UART_CLOCK = 57600
--UART_CLOCK = 19200

BRAM_SIZE_BYTES = 2048

function concat(t1,t2)
    for i=1,#t1 do
      assert(type(t1[i])=="string")
    end

    for i=1,#t2 do
      assert(type(t2[i])=="string")
      t1[#t1+1] = t2[i]
    end
    return t1
end

local function declareReg(type, name, initial)
  if initial==nil then 
    initial=""
  else
    initial = " = "..initial
  end

  if type:isBool() then
    return "reg "..name..initial..";\n"
  else
    return "reg ["..(type:sizeof()*8-1)..":0] "..name..initial..";\n"
 end
end

local function declareWire(ty, name, str, comment)
  assert(type(str)=="string" or str==nil)

  if comment==nil then comment="" end

  if str == nil or str=="" then
    str = ""
  else
    str = " = "..str
  end

  if ty:isBool() then
    return "wire "..name..str..";\n"
  else
    return "wire ["..(ty:sizeof()*8-1)..":0] "..name..str..";"..comment.."\n"
  end
end

function numToVarname(x)
  if x>0 then return x end
  if x==0 then return "0" end
  return "m"..math.abs(x)
end

function pointerToVarname(x)
  return tostring(x):sub(10)
end

function getStencilCoord(rel)
  if type(rel)=="number" then return rel end
  local s = rel:eval(1)
  if s:area()==1 then
    return numToVarname(s:min(1))
  else
    -- this involves something like a mapreducevar
    return pointerToVarname(rel)
  end
end

function astFunctions:evalFunrolled(dim, mrvValues)
  assert(type(dim)=="number")
  assert(type(mrvValues)=="table")

  if self.kind=="value" then
    assert(type(self.value)=="number")
    return Stencil.new():addDim(dim, self.value)
  elseif self.kind=="unary" and self.op=="-" then
    return self.expr:evalFunrolled(dim, mrvValues):flipDim(dim)
  elseif self.kind=="mapreducevar" then
    assert(type(mrvValues[self.variable])=="number")
    return Stencil.new():addDim(dim, mrvValues[self.variable])
  elseif self.kind=="binop" and self.op=="+" then
    return self.lhs:evalFunrolled(dim, mrvValues):sum(self.rhs:evalFunrolled(dim, mrvValues))
  elseif self.kind=="binop" and self.op=="-" then
    return self.lhs:evalFunrolled(dim, mrvValues):sum(self.rhs:evalFunrolled(dim, mrvValues):flipDim(dim))
  elseif self.kind=="binop" and self.op=="*" then
    return self.lhs:evalFunrolled(dim, mrvValues):product(self.rhs:evalFunrolled(dim, mrvValues))
  else
    print("internal error, couldn't statically evaluate ", self.kind)
    assert(false)
  end
end

function getStencilCoordFunrolled(rel, mrvValues)
  if type(rel)=="number" then return rel end
  local s = rel:evalFunrolled(1, mrvValues)
  s:print()
  assert(s:area()==1)
  return numToVarname(s:min(1))
end

function delayToXY(delay, width)
  local lines = math.floor(delay/width)
  local xpixels = delay - lines*width
  return xpixels, lines
end

local declaredReductionModules = {}
function fpga.reduce(op, cnt, datatype)
  assert(type(op)=="string")
  assert(darkroom.type.isType(datatype))

  local name = "Reduce_"..op.."_"..cnt

  if declaredReductionModules[name] then
    return name, {} -- already declared somewhere
  end
  declaredReductionModules[name] = 1

  local module = {"module "..name.."(input CLK, output["..(datatype:sizeof()*8-1)..":0] out"}
  for i=0,cnt-1 do table.insert(module,", input["..(datatype:sizeof()*8-1)..":0] partial_"..i.."") end
  table.insert(module,");\n")

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
        table.insert(clockedLogic, n.." <= ("..a..">"..b..")?("..a.."):("..b..");\n")
      else
        assert(false)
      end
    end

    -- codegen the dangle
    assert(remain-r*2 == 0 or remain-r*2==1)
    if remain-r*2==1 then
      assert(level==0) -- should only be a remainder on the first iteration
      local n = "partial_l"..(level+1).."_"..r
      table.insert(module, declareReg(datatype,n))
      table.insert(clockedLogic, n.." <= partial_"..(remain-1)..";\n")	
    end

    remain = remain-r
    level=level+1
  end

  table.insert(module, "assign out = partial_l"..level.."_0;\n")
  table.insert(module, "always @ (posedge CLK) begin\n")
  module = concat(module, clockedLogic)
  table.insert(module,"end\nendmodule\n")


  return name, module
end

lbCnt = 0
function fpga.linebuffer(maxdelay, datatype, stripWidth, consumers)
  assert(type(maxdelay)=="number")
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

  local t = {"module "..name.."(input CLK, input[12:0] inX, input[12:0] inY, output[12:0] outX, output[12:0] outY,\n"..outputs.."input ["..(bytesPerPixel*8-1)..":0] in);\n"}

  local xpixels, lines = delayToXY(maxdelay, stripWidth)
  print("linebuffer lines",lines,"xpixels",xpixels)

  table.insert(t,"assign outX = inX;\n")
  table.insert(t,"assign outY = inY;\n")

  if lines==0 and xpixels==0 then
    for k,v in ipairs(consumers) do      
      table.insert(t, "assign out"..k.."_x0_y0 = in;\n")
    end
  elseif lines==0 then

    -- we're only delaying a few pixels, don't use a bram
    local clockedLogic = {}
    local prev = "in"
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
            table.insert(t, "assign out"..k.."_x0_y0 = in;\n")
          end
        end
      end

    table.insert(t,"always @ (posedge CLK) begin\n")
    t = concat(t,clockedLogic)
    table.insert(t,"end\n")
  else
    local clockedLogic = {}

    -- we make a bram for each full line. 
    assert(stripWidth*bytesPerPixel < BRAM_SIZE_BYTES)
    assert(bytesPerPixel==1)

    local smallestX = 0
    for k,v in ipairs(consumers) do
      assert(v:max(1)==0)
      if v:min(1) < smallestX then smallestX = v:min(1) end
    end

    table.insert(t,"reg [10:0] lbWriteAddr = 0;\n")
    table.insert(t,"reg [10:0] lbReadAddr = 1;\n")
    table.insert(clockedLogic, "if (lbWriteAddr == "..(stripWidth-1)..") begin lbWriteAddr <= 0; end else begin lbWriteAddr <= lbWriteAddr + 1; end\n")
    table.insert(clockedLogic, "if (lbReadAddr == "..(stripWidth-1)..") begin lbReadAddr <= 0; end else begin lbReadAddr <= lbReadAddr + 1; end\n")

    local i=0
    while i>-lines do
      table.insert(t,declareWire(datatype,"evicted_"..numToVarname(i)))

      local indata = "in"
      local configParams = [=[.WRITE_MODE_A("READ_FIRST")]=]

      local leadingVar = "lb_x0_y"..numToVarname(i)
      table.insert(t,declareWire(datatype,leadingVar))

      if i==0 then
        table.insert(t,"assign "..leadingVar.." = in;\n")
      else
        table.insert(t,"assign "..leadingVar.." = evicted_"..numToVarname(i+1)..";\n")
        indata = "evicted_"..numToVarname(i+1)
      end

      table.insert(t, [=[RAMB16_S9_S9 #(]=]..configParams..[=[) ram_line]=]..numToVarname(i)..[=[(
.DIPA(1'b0),
.DIA(]=]..indata..[=[),
//.DOA(),
.DOB(evicted_]=]..numToVarname(i)..[=[),
.ADDRA(lbWriteAddr),
.WEA(1'b1),
.WEB(1'b0),
.ENA(1'b1),
.ENB(1'b1),
.ADDRB(lbReadAddr),
.CLKA(CLK),
.CLKB(CLK),
.SSRA(1'b0),
.SSRB(1'b0));
]=])
      i = i - 1
    end

    local leadingVar = "lb_x0_y"..numToVarname(-lines)
    table.insert(t,declareWire(datatype,leadingVar))
    table.insert(t,"assign "..leadingVar.." = evicted_"..numToVarname(-lines+1)..";\n")

    -- stencil shift register
    -- note that this also codegens for the dangles in the last (oldest) row
    for y=-lines,0 do
      local prev = "lb_x0_y"..numToVarname(y)

      local x=-1
      while x>=-xpixels do
        local n = "lb_x"..numToVarname(x).."_y"..numToVarname(y)
        table.insert(t,declareReg(datatype,n))
        table.insert(clockedLogic, n.." <= "..prev.."; // SSR\n")
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
    table.insert(t,"end\n")

  end

  table.insert(t,"endmodule\n\n")

  return name, t
end

function fpga.buffer(moduleName, sizeBytes, inputBytes, outputBytes)
  assert(type(inputBytes)=="number")
  assert(type(outputBytes)=="number")

  local bramCnt = math.ceil(sizeBytes / 2048)
  local extraBits = math.ceil(math.log(bramCnt)/math.log(2))

  local chunkSize
  local outputChunkSize = nearestPowerOf2(outputBytes)
  local inputChunkSize = nearestPowerOf2(inputBytes)
  local contiguous
  local writePort = "A"
  local readPort = "B"

  local addrA = "inaddr"
  local addrB = "outaddr"

  if inputBytes==1 then
    chunkSize = nearestPowerOf2(outputBytes)
    contiguous = outputBytes
  elseif outputBytes==1 then
    chunkSize = nearestPowerOf2(inputBytes)
    contiguous = inputBytes
    readPort = "A"
    writePort = "B"
    addrA = "outaddr"
    addrB = "inaddr"
  else
    assert(false)
  end

  assert(chunkSize<=4)

  local res = {"module "..moduleName.."(\ninput CLK,\ninput ["..(10+extraBits)..":0] inaddr,\ninput WE,\ninput ["..(inputBytes*8-1)..":0] indata,\ninput ["..(10+extraBits)..":0] outaddr,\noutput ["..(outputBytes*8-1)..":0] outdata\n);\n\n"}

  if contiguous~=chunkSize then
    table.insert(res,"reg [10:0] lastaddr = 0;\n")
    table.insert(res,"reg [4:0] cycleCNT = 0;\n")
    table.insert(res,"reg [10:0] addrA = 0;\n")
  else
    table.insert(res,"wire [10:0] addrA;\n")
    table.insert(res,"assign addrA=inaddr;\n")
  end

  local assn = "outdata0"
  for i=0,bramCnt-1 do
    table.insert(res,"wire ["..(outputChunkSize*8-1)..":0] outdata"..i..";\n")
    table.insert(res,"RAMB16_S9_S"..(chunkSize*9).." #(.INIT_00(256'h0123456789ABCDEF00000000000000000000000000000000000000000000BBBB)\n")
    table.insert(res,") ram"..i.."(\n")
--    table.insert(res,".DIPA(1'b0),\n")
    table.insert(res,".DI"..writePort.."(indata),\n")
    table.insert(res,".DO"..readPort.."(outdata"..i.."),\n")
    table.insert(res,".ADDRA(addrA),\n")
if bramCnt > 1 then
    table.insert(res,".WE"..writePort.."(WE && (inaddr["..(10+extraBits)..":11]=="..i..")),\n")
else
    table.insert(res,".WE"..writePort.."(WE),\n")
end

    table.insert(res,".WE"..readPort.."(1'b0),\n")
    table.insert(res,".ENA(1'b1),\n")
    table.insert(res,".ENB(1'b1),\n")
    table.insert(res,".ADDRB("..addrB.."[10:0]),\n")
    table.insert(res,".CLKA(CLK),\n")
    table.insert(res,".CLKB(CLK),\n")
    table.insert(res,".SSRA(1'b0),\n")
    table.insert(res,".SSRB(1'b0)\n")
    table.insert(res,");\n\n")

    if i>0 then assn = "(outaddr["..(10+extraBits)..":11]=="..i..")? outdata"..i.." : ("..assn..")" end
  end

  table.insert(res, "assign outdata = "..assn.."["..(outputBytes*8-1)..":0];\n")

  if contiguous~=chunkSize then

    table.insert(res,[=[always @(posedge CLK) begin
  if(]=]..addrA..[=[ != lastaddr) begin
    if(]=]..addrA..[=[==0) begin
      cycleCNT <= 0;
      addrA <= 0;
    end else if(cycleCNT == ]=]..(contiguous-1)..[=[) begin
      cycleCNT <= 0;
      addrA <= addrA+1+]=]..(chunkSize-contiguous)..[=[;
    end else begin
      cycleCNT <= cycleCNT+1;
      addrA <= addrA+1;
    end

  end
  lastaddr <= ]=]..addrA..[=[;
end
]=])

  end

  table.insert(res,"endmodule\n\n")
  return res
end

function fpga.sim()
  return [=[`define EOF 32'hFFFF_FFFF
module sim;
 integer file, c, r,fileout;
 reg     CLK;
 reg [7:0] modInput;
 wire [7:0] modOutput;
 integer addr = -PIPE_DELAY+2;
 reg [10000:0] inputFilename;
 reg [10000:0] outputFilename;

 Pipeline pipeline(.CLK(CLK),.in(modInput),.out(modOutput));

 initial begin
   $display("HELLO");

   $value$plusargs("inputFilename=%s",inputFilename);
   $value$plusargs("outputFilename=%s",outputFilename);

   file = $fopen(inputFilename,"r");
   fileout = $fopen(outputFilename,"w");

   c = $fgetc(file);
   while (c != `EOF) begin
//     $display(c);
     modInput = c;
     CLK = 0;
     #10
     CLK = 1;
     #10
//     $display(modOutput);

     if(addr>=0) begin $fwrite(fileout, "%c", modOutput); end

     c = $fgetc(file);
     addr = addr + 1;
   end // while (c != `EOF)

   // drain pipe
   addr = -PIPE_DELAY+2;

   while (addr<0) begin
     CLK = 0;
     #10
     CLK = 1;
     #10
     addr = addr + 1;     
     $fwrite(fileout, "%c", modOutput);
   end	   

   $display("DONE");
   $fclose(fileout);
  end // initial begin

endmodule // sim        ]=]
end

function fpga.tx(clockMhz)
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
  wire [28:0] dInc = d[28] ? (]=]..UART_CLOCK..[=[) : (]=]..UART_CLOCK..[=[ - ]=]..clockMhz..[=[000000);
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

function fpga.rx(clockMhz)
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
  wire [28:0] dInc = d[28] ? (]=]..(UART_CLOCK*16)..[=[) : (]=]..(UART_CLOCK*16)..[=[ - ]=]..clockMhz..[=[000000);
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


STUPIDGLOBALinternalDelays = {}

function kernelGraphFunctions:internalDelay()
  return STUPIDGLOBALinternalDelays[self]
end

function typedASTFunctions:cname(c)
  return self:name().."_c"..c
end

function typedASTFunctions:internalDelay()
  if self.kind=="binop" or self.kind=="unary" or self.kind=="select" or self.kind=="crop" or self.kind=="vectorSelect" then
    return 1
  elseif self.kind=="load" or self.kind=="value" or self.kind=="cast" or self.kind=="position" or self.kind=="mapreducevar" or self.kind=="array" or self.kind=="index" then
    return 0
  elseif self.kind=="mapreduce" then
    local area = 1
    local i=1
    while self["varid"..i] do
      area = area * (self["varhigh"..i]-self["varlow"..i]+1)
      i = i + 1
    end

    return math.ceil(math.log(area)/math.log(2)) -- for the reduce
  elseif self.kind=="reduce" then
    return math.ceil(math.log(self:arraySize("expr"))/math.log(2)) -- for the reduce
  else
    print(self.kind)
    assert(false)
  end
end

-- The retiming number is the delay at the output of the node.
-- This dumb retiming function puts all the input (leaf) nodes at delay 0.
-- The one output node's delay is the delay for the entire pipeline.
-- Nodes can have 'internal' delays. These are the delays inside the node.
-- eg a big reduce takes 5 cycles etc. The number of pipelining registers
-- we need to create is the difference in delays minus the internal delays.
function fpga.trivialRetime(typedAST)
  local retiming = {}

  typedAST:visitEach(
    function(n, inputs)
      local maxDelay = 0
      for k,v in n:inputs() do
        if inputs[k]>maxDelay then maxDelay = inputs[k] end
      end
      retiming[n] = maxDelay + n:internalDelay()
      return retiming[n]
    end)

  return retiming
end

local binopToVerilog={["+"]="+",["*"]="*",["<<"]="<<",[">>"]=">>",["pow"]="**",["=="]="==",["and"]="&&",["-"]="-",["<"]="<",[">"]=">",["<="]="<=",[">="]=">="}

function fpga.codegenKernel(kernelGraphNode, retiming, imageWidth, imageHeight)
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")

  local kernel = kernelGraphNode.kernel

  local inputs = ""

  for k,v in kernelGraphNode:inputs() do
    local s = kernel:stencil(v)
    for x=s:min(1),s:max(1) do
      for y=s:min(2), s:max(2) do
        inputs = inputs.."input ["..(v.kernel.type:sizeof()*8-1)..":0] in_"..v:name().."_x"..numToVarname(x).."_y"..numToVarname(y)..",\n"
      end
    end
  end

  if kernelGraphNode:inputCount()==0 then
    inputs = "input [7:0] in_x0_y0,\n"
  end

  local result = {"module Kernel_"..kernelGraphNode:name().."(input CLK, input[12:0] inX, input[12:0] inY, output[12:0] outX, output[12:0] outY, \n"..inputs.."output ["..(kernel.type:sizeof()*8-1)..":0] out);\n"}
  local clockedLogic = {}

  table.insert(result,"wire [12:0] inX_0;\n")
  table.insert(result,"assign inX_0 = inX;\n")
  table.insert(result,"wire [12:0] inY_0;\n")
  table.insert(result,"assign inY_0 = inY;\n")

  for i=1,retiming[kernel] do
    table.insert(result,"reg [12:0] inX_"..i..";\n")
    table.insert(result,"reg [12:0] inY_"..i..";\n")
    table.insert(clockedLogic, "inX_"..i.." <= inX_"..(i-1)..";\n")
    table.insert(clockedLogic, "inY_"..i.." <= inY_"..(i-1)..";\n")
  end

  table.insert(result,"assign outX = inX_"..retiming[kernel]..";\n")
  table.insert(result,"assign outY = inY_"..retiming[kernel]..";\n")

  local finalOut = kernel:visitEach(
    function(n, args)

      local inputs = {}
      local declarationsSeen = {}
      local declarations = {}
      local clockedLogicSeen = {}
      local clockedLogic = {}

      local function merge(src,dest,seen)
        for kk, vv in pairs(src) do
          assert(type(vv)=="string")
          if seen[vv]==nil then table.insert(dest, vv); seen[vv]=1 end
        end
      end
      for k,v in pairs(args) do
        inputs[k] = {}
        for c=1,n[k].type:channels() do inputs[k][c] = args[k][1][c] end
        merge(args[k][2], clockedLogic, clockedLogicSeen)
        merge(args[k][3], declarations, declarationsSeen)
      end

      if not (n.type:baseType():isInt() or n.type:baseType():isUint() or n.type:baseType():isBool()) then
        darkroom.error("Only integer types are allowed "..n.type:str(), n:linenumber(), n:offset(), n:filename())
      end

      -- insert pipeline delays
      for k,v in n:inputs() do
        local delays = retiming[n] - retiming[v] - n:internalDelay()
        assert(delays>=0)

        for c=1,v.type:channels() do
          local prev = inputs[k][c]
          for i=1, delays do
            local sn = inputs[k][c].."_"..n:cname(c).."_retime"..i
            table.insert(declarations, declareReg( n.type:baseType(), sn ))
            table.insert(clockedLogic, sn.." <= "..prev..";\n")
            prev = sn
          end
          if delays>0 then inputs[k][c] = inputs[k][c].."_"..n:cname(c).."_retime"..delays end
        end
      end

      local finalOut = {}

      -- mapreduce mixes channels is weird ways, so codegen this separately
      if n.kind=="mapreduce" then

        local moduledef = {"module Map_"..n:name().."(input CLK, input[12:0] inX, input[12:0] inY, \n"}

        local i = 1
        while n["varname"..i] do
          table.insert(moduledef,"input[31:0] mrvar_"..n["varname"..i]..",\n")
          i=i+1
        end

        local loadSeen = {}
        local loadList = {}
        n:S("load"):traverse(
        function(node)
          print("LOAD",getStencilCoord(node.relX), getStencilCoord(node.relY))
           local inp
          if type(node.from)=="number" then
            inp = "in_x"..getStencilCoord(node.relX).."_y"..getStencilCoord(node.relY)
          else
            inp = "in_"..node.from:name().."_x"..getStencilCoord(node.relX).."_y"..getStencilCoord(node.relY)
          end
        if loadSeen[inp]==nil then
        loadSeen[inp]=1
        table.insert(moduledef,"input["..(node.type:sizeof()*8-1)..":0] "..inp..",\n")
        table.insert(loadList,node)
	      end
        end)

        local function loadInputList(mrvValues)
          local res = ""
          for _,node in pairs(loadList) do
            if type(node.from)=="number" then
              res = res..",.in_x"..getStencilCoord(node.relX).."_y"..getStencilCoord(node.relY).."(in_x"..getStencilCoordFunrolled(node.relX,mrvValues).."_y"..getStencilCoordFunrolled(node.relY,mrvValues)..")"
            else
              res = res..",.in_"..node.from:name().."_x"..getStencilCoord(node.relX).."_y"..getStencilCoord(node.relY).."(in_"..node.from:name().."_x"..getStencilCoordFunrolled(node.relX,mrvValues).."_y"..getStencilCoordFunrolled(node.relY,mrvValues)..")"
            end
          end
          return res
        end

        table.insert(moduledef,"output ["..(n.expr.type:sizeof()*8-1)..":0] out);\n\n")

        table.insert(moduledef,table.concat(declarations,""))
        table.insert(moduledef,"always @ (posedge CLK) begin\n"..table.concat(clockedLogic,"").."end\n")

        table.insert(moduledef,"assign out = {"..inputs.expr[#inputs.expr])
        local c = #inputs.expr-1
        while c>=1 do table.insert(moduledef,","..outputName[c]); c=c-1 end
        table.insert(moduledef, "};\n")
        table.insert(moduledef,"endmodule\n\n")

        result = concat(moduledef,result)

        clockedLogic = {}
        declarations = {}

        -- funroll
        local partials = -1
        local funroll = {function(inputList, mrvValues) partials = partials+1; return {declareWire(n.type,n:name().."_partial"..partials).."Map_"..n:name().." map_"..n:name().."_"..partials.."(.CLK(CLK),.out("..n:name().."_partial"..partials.."),.inX(inX),.inY(inY)"..inputList..loadInputList(mrvValues)..");\n"} end}

        local i = 1
        while n["varname"..i] do
          print(n["varlow"..i],n["varhigh"..i],type(n["varlow"..i]),type(n["varhigh"..i]))
          local ii = i
          table.insert(funroll, function(inputList, mrvValues) local res = {}; for j=n["varlow"..ii],n["varhigh"..ii] do mrvValues[n["varname"..ii]]=j; res = concat(res, funroll[ii](",.mrvar_"..n["varname"..ii].."("..j..")"..inputList, mrvValues)) end; return res end)
          i=i+1
        end
        
        declarations = concat(declarations, funroll[#funroll]("",{}))

        local rname, rmod = fpga.reduce(n.reduceop, partials+1, n.expr.type)
        result = concat(rmod, result)

        local finalOut = {}

        for c=1,n.type:channels() do
          table.insert(declarations, declareWire(n.type, n:cname(c)))
          table.insert(declarations,rname.." reduce_"..n:cname(c).."(.CLK(CLK),.out("..n:cname(c)..")")
          for i=0,partials do table.insert(declarations,",.partial_"..i.."("..n:name().."_partial"..i..")") end
          table.insert(declarations,");\n")
          table.insert(finalOut, n:cname(c))
        end

        return {finalOut, clockedLogic, declarations}
      end

      for c=1,n.type:channels() do
        local res
        if n.kind=="binop" then
          table.insert(declarations, declareReg( n.type:baseType(), n:cname(c) ))
          local op = binopToVerilog[n.op]
          if type(op)~="string" then print("OP",n.op); assert(false) end
          table.insert(clockedLogic, n:name().."_c"..c.." <= "..inputs.lhs[c]..op..inputs.rhs[c]..";\n")
          res = n:name().."_c"..c
        elseif n.kind=="unary" then
          if n.op=="abs" then
            if n.type:baseType():isInt() then
              table.insert(declarations, declareReg( n.type:baseType(), n:cname(c) ))
              table.insert(clockedLogic, n:cname(c).." <= ("..inputs.expr[c].."["..(n.type:baseType():sizeof()*8-1).."])?(-"..inputs.expr[c].."):("..inputs.expr[c].."); //abs\n")
              res = n:cname(c)
            else
              return inputs.expr[c] -- must be unsigned
            end
          elseif n.op=="-" then
            assert(n.type:baseType():isInt())
            table.insert(declarations, declareReg(n.type:baseType(),n:cname(c)))
            table.insert(clockedLogic, n:cname(c).." <= -"..inputs.expr[c].."; // unary sub\n")
            res = n:cname(c)
          else
            print(n.op)
            assert(false)
          end
        elseif n.kind=="select" or n.kind=="vectorSelect" then
          table.insert(declarations,declareReg( n.type:baseType(), n:cname(c) ))
          local condC = 1
          if n.kind=="vectorSelect" then condC=c end

          table.insert(clockedLogic, n:cname(c).." <= ("..inputs.cond[condC]..")?("..inputs.a[c].."):("..inputs.b[c]..");\n")
          res = n:cname(c)
        elseif n.kind=="load" then
          if type(n.from)=="number" then
            res = "in_x"..getStencilCoord(n.relX).."_y"..getStencilCoord(n.relY)
          else
            res = "in_"..n.from:name().."_x"..getStencilCoord(n.relX).."_y"..getStencilCoord(n.relY)
          end
        elseif n.kind=="position" then
          local str = "inX"
          if n.coord=="y" then str="inY" end
          table.insert(declarations, declareWire(n.type, n:name(), str))
          res = n:name()
        elseif n.kind=="crop" then
          local delay = retiming[n] - n:internalDelay()
          table.insert(declarations, declareReg( n.type:baseType(), n:cname(c) ))
          -- hilariously, this also checks for values <0, b/c values <= in 2s complement are large, larger than image width...
          table.insert(clockedLogic, n:cname(c).." <= ((inX_"..delay.."-"..n.shiftX..")>="..imageWidth.." || (inY_"..delay.."-"..n.shiftY..")>="..imageHeight..")?(0):("..inputs.expr[c].."); // crop\n")
          res = n:cname(c)
        elseif n.kind=="cast" then
          local expr
          if n.type:isArray() and n.expr.type:isArray()==false then
            expr = inputs["expr"][1] -- broadcast
          else
            expr = inputs["expr"][c]
          end

          table.insert(declarations, declareWire(n.type:baseType(), n:cname(c), expr," //cast"))
          res = n:cname(c)
        elseif n.kind=="value" then
          table.insert(declarations,declareWire(n.type:baseType(), n:cname(c), tostring(n.value), " //value" ))
          res = n:cname(c)
        elseif n.kind=="mapreducevar" then
          res = "mrvar_"..n.variable
        elseif n.kind=="array" then
          res = inputs["expr"..c][1]
        elseif n.kind=="index" then
          if n.index:eval(1):area()==1 then
            res = inputs["expr"][n.index:eval(1):min(1)+1]
          else
            assert(false)
          end
        elseif n.kind=="reduce" then
          local rname, rmod = fpga.reduce(n.op, n:arraySize("expr"), n.type)
          result = concat(rmod, result)
          table.insert(declarations, declareWire(n.type, n:cname(c),"", "// reduce result"))
          local str = rname.." reduce_"..n:cname(c).."(.CLK(CLK),.out("..n:cname(c)..")"
          n:map("expr",function(_,i) str = str..",.partial_"..(i-1).."("..inputs["expr"..i][c]..")" end)
          table.insert(declarations,str..");\n")
          res = n:cname(c)
        else
          print(n.kind)
          assert(false)
        end

        assert(type(res)=="string")
        finalOut[c] = res
      end

      return {finalOut, clockedLogic, declarations}
    end)

  local outputName = finalOut[1]

  result = concat(result, finalOut[3])
  clockedLogic = concat(clockedLogic, finalOut[2])

  table.insert(result,"always @ (posedge CLK) begin\n"..table.concat(clockedLogic,"").."end\n")
  table.insert(result,"assign out = {"..outputName[#outputName])
  local c = #outputName-1
  while c>=1 do table.insert(result,","..outputName[c]); c=c-1 end
  table.insert(result, "};\n")
  table.insert(result,"endmodule\n\n")
  return result
end

function fpga.compile(inputs, outputs, imageWidth, imageHeight, stripWidth, stripHeight, options)
  assert(#inputs==1)
  assert(#outputs==1)
  assert(type(stripHeight)=="number")
  assert(type(options)=="table" or options==nil)


  if options.clockMhz==nil then options.clockMhz=32 end

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

  local kernelGraph = darkroom.frontEnd( ast, {} )

  local shifts = schedule(kernelGraph, stripWidth)
  kernelGraph, shifts = shift(kernelGraph, shifts, stripWidth)

  local inputBytes = inputs[1][1].expr.type:sizeof()
  local outputBytes = kernelGraph.child1.kernel.type:sizeof()
  local outputChannels = kernelGraph.child1.kernel.type:channels()
  print("INPUTBYTeS",inputBytes,"OUTPUTBYTES",outputBytes)

  local maxStencil = Stencil.new()
  kernelGraph:visitEach(
    function(node)
      print("SS",node:name(),shifts[node])
      for k,v in node:inputs() do
        if node.kernel~=nil then print("ST",node.kernel:stencil(v):min(1),node.kernel:stencil(v):max(1),"Y",node.kernel:stencil(v):min(2),node.kernel:stencil(v):max(2)) end
      end
      maxStencil = maxStencil:unionWith(neededStencil(true,kernelGraph,node,nil))
    end)

  print("S",shifts[kernelGraph.child1])
  local shiftX, shiftY = delayToXY(shifts[kernelGraph.child1], stripWidth)
  maxStencil = maxStencil:translate(shiftX,shiftY,0)
  print("Max Stencil x="..maxStencil:min(1)..","..maxStencil:max(1).." y="..maxStencil:min(2)..","..maxStencil:max(2))


  ------------------------------
  local result = {}
  table.insert(result, "`timescale 1ns / 10 ps\n")
  result = concat(result, fpga.tx(options.clockMhz))
  result = concat(result, fpga.rx(options.clockMhz))
  result = concat(result, fpga.buffer("InputBuffer",stripWidth*stripHeight,1,inputBytes))
  result = concat(result, fpga.buffer("OutputBuffer",stripWidth*stripHeight,outputBytes,1))

  local pipeline = {[=[module Pipeline(
input CLK, input[12:0] inX, input[12:0] inY,
input []=]..(inputBytes*8-1)..[=[:0] in,
output []=]..(outputBytes*8-1)..[=[:0] out);
]=]}

  local function parentIsOutput(node)
    for v,k in node:parents(kernelGraph) do if v==kernelGraph then return true end end
    return false
  end

  local kernelRetiming = {}
  kernelGraph:visitEach(
    function(node, inputArgs)
      if node.kernel~=nil then
        kernelRetiming[node] = fpga.trivialRetime(node.kernel)
        STUPIDGLOBALinternalDelays[node] = kernelRetiming[node][node.kernel]
        assert(type(STUPIDGLOBALinternalDelays[node])=="number")
        if parentIsOutput(node)==false and node:bufferSize(kernelGraph,stripWidth)>0 then 
          STUPIDGLOBALinternalDelays[node] = STUPIDGLOBALinternalDelays[node]
        end
      else
        STUPIDGLOBALinternalDelays[node] = 0
      end
    end)

  -- now we retime the whole pipeline, to account for the delays of each kernel
  local pipelineRetiming = fpga.trivialRetime(kernelGraph)

  local totalDelay = kernelGraph:visitEach(
    function(node, inputArgs)
      if node.kernel~=nil then
        local verilogKernel = fpga.codegenKernel(node, kernelRetiming[node], imageWidth, imageHeight)
        result = concat(result, verilogKernel)
        
        local inputs = ""
        local inputXY = ""
        if node:inputCount()==0 then
          inputs = ".in_x0_y0(in),"
          inputXY = ".inX(inX),.inY(inY)"
        else
          for _,v in node:inputs() do
            if inputXY=="" then
              inputXY = ".inX(kernelOutX_"..v:name().."),.inY(kernelOutY_"..v:name()..")"
            end
            local s = node.kernel:stencil(v)
            for x=s:min(1),s:max(1) do
              for y=s:min(2), s:max(2) do
                inputs = inputs..".in_"..v:name().."_x"..numToVarname(x).."_y"..numToVarname(y).."("..v:name().."_to_"..node:name().."_x"..numToVarname(x).."_y"..numToVarname(y).."),"
              end
            end
          end
        end
        
        table.insert(pipeline,"wire ["..(node.kernel.type:sizeof()*8-1)..":0] kernelOut_"..node:name()..";\n")
        table.insert(pipeline,"wire [12:0] kernelOutX_"..node:name()..";\n")
        table.insert(pipeline,"wire [12:0] kernelOutY_"..node:name()..";\n")
        table.insert(pipeline,"Kernel_"..node:name().." kernel_"..node:name().."(.CLK(CLK),"..inputXY..",.outX(kernelOutX_"..node:name().."),.outY(kernelOutY_"..node:name().."),"..inputs..".out(kernelOut_"..node:name().."));\n")
        
        local lboutputs = ""

        local consumers = {}
	local linebufferSize = 0 -- note: this code duplicates kernelGraph:bufferSize()
        for v,_ in node:parents(kernelGraph) do
          if v.kernel~=nil then
	    -- bake the extra retiming pipeline delay into the stencil.
	    -- Don't get confused here! We need to add extra delay to match the delays of each kernel.
	    -- we could do that by adding extra FIFOs, but instead we bake it into the linebuffer 
	    -- (shifting the stencil we read by 1 is equivilant to 1 extra cycle of delay)
	    --
	    -- we should probably refactor this so that the extra delay and stencil are separated out so
	    -- that it's less confusing.
	    local extraPipeDelay = pipelineRetiming[v]-pipelineRetiming[node]-v:internalDelay()
	    local stencil = v.kernel:stencil(node):translate(-extraPipeDelay,0,0)
            table.insert(consumers, stencil)
	    
	    -- note: this code duplicates kernelGraph:bufferSize()
	    local b = -stencil:min(1)-stencil:min(2)*stripWidth
	    linebufferSize = math.max(linebufferSize,b)

            for x=stencil:min(1),stencil:max(1) do
              for y=stencil:min(2), stencil:max(2) do
                local wirename = node:name().."_to_"..v:name().."_x"..numToVarname(x+extraPipeDelay).."_y"..numToVarname(y)
                table.insert(pipeline,"wire ["..(node.kernel.type:sizeof()*8-1)..":0] "..wirename..";\n")
                lboutputs = lboutputs..".out"..(#consumers).."_x"..numToVarname(x).."_y"..numToVarname(y).."("..wirename.."),"
              end
            end
          end
        end
        
        if parentIsOutput(node)==false then -- output nodes don't write to linebuffer
          local lbname, lbmod = fpga.linebuffer(linebufferSize, node.kernel.type, stripWidth, consumers)
          result = concat(result, lbmod)
          table.insert(pipeline,lbname.." kernelBuffer_"..node:name().."(.CLK(CLK),"..lboutputs..".in(kernelOut_"..node:name().."));\n")
        end

        return 0
      else
        local totalDelay = 0
        for k,v in pairs(inputArgs) do
          if v > totalDelay then totalDelay=v end
        end
        return totalDelay
      end

    end)

  -- account for the linebuffering delay
  -- it turns out that the linebuffer sizes / linebuffer doesn't actually impact the pipeline delay (pipe delay meaning: if we put
  -- a pixel in the pipe, how long until its output value comes out?). The reason is that, we always write to time=0 slot
  -- in the linebuffer, and then pipe stages that consume the same value just read it (results in delay of exactly 1 due to being passed through the ram).
  -- However, due to the fact that we are retiming each module, we have to add extra buffering to each input we read to account for
  -- the differences in pipe stages of the different modules (ie if we're reading from pipe delay 10 (for A) and 20 (for B), we need to add 
  -- an extra 10 buffers on the end of A to get the correct result. The observation is that we can implement this by simply shifting where
  -- we read in the linebuffer - we don't have to actually instantiate extra buffering.
  
  assert(kernelGraph.kernel==nil)
  assert(kernelGraph:inputCount()==1)

--  totalDelay = pipelineRetiming[kernelGraph.child1] + shifts[kernelGraph.child1]
  totalDelay = pipelineRetiming[kernelGraph.child1]

  local metadata = {maxStencil = maxStencil, outputShift = shifts[kernelGraph.child1], outputChannels = outputChannels, outputBytes = outputBytes, stripWidth = stripWidth, stripHeight=stripHeight}

  table.insert(pipeline, "assign out = kernelOut_"..kernelGraph.child1:name()..";\n")
  table.insert(pipeline,"endmodule\n\n")
  table.insert(pipeline,"parameter PIPE_DELAY = "..(totalDelay)..";\n")
  result = concat(result, pipeline)

  local pxcnt = stripWidth*stripHeight
  local metadataBytes = 4
  local rxStartAddr = math.pow(2,13)-metadataBytes

  local shiftInMetadata = "metadata[31:24] <= rxbits;\n"
  for i=0,metadataBytes-2 do
    shiftInMetadata = shiftInMetadata .. "metadata["..(i*8+7)..":"..(i*8).."] <= metadata["..(i*8+15)..":"..(i*8+8).."];\n"
  end

  table.insert(result,[=[module stage(
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
wire [7:0] pipelineInput;
reg [12:0] pipelineReadAddr = 0; 
InputBuffer inputBuffer(.CLK(CLK), .inaddr(addr), .WE(receiving), .indata(rxbits), .outaddr(pipelineReadAddr), .outdata(pipelineInput));

wire []=]..(outputBytes*8-1)..[=[:0] pipelineOutput;
wire [7:0] outbuf;
reg [12:0] pipelineWriteAddr = -PIPE_DELAY; // pipe delay
OutputBuffer outputBuffer(.CLK(CLK), .inaddr(pipelineWriteAddr), .WE(processing), .indata(pipelineOutput), .outaddr(sendAddr), .outdata(outbuf));

Pipeline pipeline(.CLK(CLK), .inX(posX+metadata[12:0]), .inY(posY+metadata[28:16]), .in(pipelineInput), .out(pipelineOutput));

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

  table.insert(result, fpga.sim())
  return table.concat(result,""), metadata
end

return fpga