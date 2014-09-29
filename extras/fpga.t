local fpga = {}

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

  return "reg ["..(type:sizeof()*8-1)..":0] "..name..initial..";\n"
end

local function declareWire(type, name, str)
  if str == nil then
    str = ""
  else
    str = " = "..str
  end

  return "wire ["..(type:sizeof()*8-1)..":0] "..name..str..";\n"
end

function numToVarname(x)
  if x>0 then return x end
  if x==0 then return "0" end
  return "m"..math.abs(x)
end

function getStencilCoord(rel)
  if type(rel)=="number" then return rel end
  local s = rel:eval(1)
  assert(s:area()==1)
  return s:min(1)
end

function delayToXY(delay, imageWidth)
  local lines = math.floor(delay/imageWidth)
  local xpixels = delay - lines*imageWidth
  return xpixels, lines
end

lbCnt = 0
function fpga.linebuffer(maxdelay, datatype, imageWidth, consumers)
  assert(type(maxdelay)=="number")
  assert(darkroom.type.isType(datatype))
  local bytesPerPixel = datatype:sizeof()
  local name = "Linebuffer_"..numToVarname(maxdelay).."delay_"..bytesPerPixel.."bpp_"..imageWidth.."w_"..lbCnt
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

  local xpixels, lines = delayToXY(maxdelay, imageWidth)
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
    assert(imageWidth*bytesPerPixel < BRAM_SIZE_BYTES)
    assert(bytesPerPixel==1)

    local smallestX = 0
    for k,v in ipairs(consumers) do
      assert(v:max(1)==0)
      if v:min(1) < smallestX then smallestX = v:min(1) end
    end

    table.insert(t,"reg [10:0] lbWriteAddr = 0;\n")
    table.insert(t,"reg [10:0] lbReadAddr = 1;\n")
    table.insert(clockedLogic, "if (lbWriteAddr == "..(imageWidth-1)..") begin lbWriteAddr <= 0; end else begin lbWriteAddr <= lbWriteAddr + 1; end\n")
    table.insert(clockedLogic, "if (lbReadAddr == "..(imageWidth-1)..") begin lbReadAddr <= 0; end else begin lbReadAddr <= lbReadAddr + 1; end\n")

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

function fpga.buffer(sizeBytes)
  local bramCnt = math.ceil(sizeBytes / 2048)
  local extraBits = math.ceil(math.log(bramCnt)/math.log(2))

  local res = {"module Buffer(\ninput CLK,\ninput ["..(10+extraBits)..":0] inaddr,\ninput WE,\ninput [7:0] indata,\ninput ["..(10+extraBits)..":0] outaddr,\noutput [7:0] outdata\n);\n\n"}

  local assn = "outdata0"
  for i=0,bramCnt-1 do
    table.insert(res,"wire [7:0] outdata"..i..";\n")
    table.insert(res,"RAMB16_S9_S9 #(.INIT_00(256'h0123456789ABCDEF00000000000000000000000000000000000000000000BBBB)\n")
    table.insert(res,") ram"..i.."(\n")
    table.insert(res,".DIPA(1'b0),\n")
    table.insert(res,".DIA(indata),\n")
    table.insert(res,".DOB(outdata"..i.."),\n")
    table.insert(res,".ADDRA(inaddr[10:0]),\n")
if bramCnt > 1 then
    table.insert(res,".WEA(WE && (inaddr["..(10+extraBits)..":11]=="..i..")),\n")
else
    table.insert(res,".WEA(WE),\n")
end

    table.insert(res,".WEB(1'b0),\n")
    table.insert(res,".ENA(1'b1),\n")
    table.insert(res,".ENB(1'b1),\n")
    table.insert(res,".ADDRB(outaddr[10:0]),\n")
    table.insert(res,".CLKA(CLK),\n")
    table.insert(res,".CLKB(CLK),\n")
    table.insert(res,".SSRA(1'b0),\n")
    table.insert(res,".SSRB(1'b0)\n")
    table.insert(res,");\n\n")

    if i>0 then assn = "(outaddr["..(10+extraBits)..":11]=="..i..")? outdata"..i.." : ("..assn..")" end
  end

  table.insert(res, "assign outdata = "..assn..";\n")
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

function typedASTFunctions:internalDelay()
  if self.kind=="binop" or self.kind=="unary" or self.kind=="select" or self.kind=="crop" then
    return 1
  elseif self.kind=="load" or self.kind=="value" or self.kind=="cast" or self.kind=="position" then
    return 0
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


function fpga.codegenKernel(kernelGraphNode, retiming, imageWidth, imageHeight)
  assert(type(imageWidth)=="number")
  assert(type(imageHeight)=="number")

  local kernel = kernelGraphNode.kernel

  local inputs = ""

  for k,v in kernelGraphNode:inputs() do
    for sk,_ in pairs(kernel:stencil(v)) do
      inputs = inputs.."input ["..(v.kernel.type:sizeof()*8-1)..":0] in_"..v:name().."_x"..numToVarname(sk[1]).."_y"..numToVarname(sk[2])..",\n"
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
    function(n, inputs)
      if n.type:isInt()==false and n.type:isUint()==false and n.type:isBool()==false then
        darkroom.error("Only integer types are allowed "..n.type:str(), n:linenumber(), n:offset(), n:filename())
      end

      -- insert pipeline delays
      for k,v in n:inputs() do
        local delays = retiming[n] - retiming[v] - n:internalDelay()
        assert(delays>=0)
        local prev = inputs[k]
        for i=1, delays do
          local sn = inputs[k].."_"..n:name().."_retime"..i
          table.insert(result,declareReg(n.type,sn))
          table.insert(clockedLogic, sn.." <= "..prev..";\n")
          prev = sn
        end
        if delays>0 then inputs[k] = inputs[k].."_"..n:name().."_retime"..delays end
      end

      local res
      if n.kind=="binop" then
        table.insert(result,declareReg(n.type,n:name()))
        local op = n.op
        if op=="pow" then op="**" end
        table.insert(clockedLogic, n:name().." <= "..inputs.lhs..op..inputs.rhs..";\n")
        res = n:name()
      elseif n.kind=="unary" then
        if n.op=="abs" then
          if n.type:isInt() then
            table.insert(result,declareReg(n.type,n:name()))
            table.insert(clockedLogic, n:name().." <= ("..inputs.expr.."["..(n.type:sizeof()*8-1).."])?(-"..inputs.expr.."):("..inputs.expr.."); //abs\n")
            res = n:name()          
          else
            return inputs.expr
          end
        elseif n.op=="-" then
          assert(n.type:isInt())
          table.insert(result,declareReg(n.type,n:name()))
          table.insert(clockedLogic, n:name().." <= -"..inputs.expr..";\n")
          res = n:name()          
        else
          print(n.op)
          assert(false)
        end
      elseif n.kind=="select" then
        table.insert(result,declareReg(n.type,n:name()))
        table.insert(clockedLogic, n:name().." <= ("..inputs.cond..")?("..inputs.a.."):("..inputs.b..");\n")
        res = n:name()
      elseif n.kind=="load" then
        if type(n.from)=="number" then
          res = "in_x"..numToVarname(getStencilCoord(n.relX)).."_y"..numToVarname(getStencilCoord(n.relY))
        else
          res = "in_"..n.from:name().."_x"..numToVarname(getStencilCoord(n.relX)).."_y"..numToVarname(getStencilCoord(n.relY))
        end
      elseif n.kind=="position" then
        local str = "inX"
        if n.coord=="y" then str="inY" end
        table.insert(result, declareWire(n.type, n:name(), str))
        res = n:name()
      elseif n.kind=="crop" then
        local delay = retiming[n] - n:internalDelay()
        table.insert(result,declareReg(n.type,n:name()))
        -- hilariously, this also checks for values <0, b/c values <= in 2s complement are large, larger than image width...
        table.insert(clockedLogic, n:name().." <= ((inX_"..delay.."-"..n.shiftX..")>="..imageWidth.." || (inY_"..delay.."-"..n.shiftY..")>="..imageHeight..")?(0):("..inputs.expr.."); // crop\n")
        res = n:name()
      elseif n.kind=="cast" then
        table.insert(result, declareWire(n.type, n:name(), inputs.expr))
        res = n:name()
      elseif n.kind=="value" then
        table.insert(result,declareWire(n.type,n:name(),n.value))
        res = n:name()
      else
        print(n.kind)
        assert(false)
      end

      assert(type(res)=="string")
      return res
    end)

  table.insert(result,"always @ (posedge CLK) begin\n"..table.concat(clockedLogic,"").."end\n")
  table.insert(result,"assign out = "..finalOut..";\n")
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
  result = concat(result, fpga.buffer(stripWidth*stripHeight))

  local pipeline = {[=[module Pipeline(
input CLK, input[12:0] inX, input[12:0] inY,
input [7:0] in,
output [7:0] out);
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
            for k,_ in pairs(s) do
              inputs = inputs..".in_"..v:name().."_x"..numToVarname(k[1]).."_y"..numToVarname(k[2]).."("..v:name().."_to_"..node:name().."_x"..numToVarname(k[1]).."_y"..numToVarname(k[2]).."),"
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

            for k,_ in pairs(stencil) do
              local wirename = node:name().."_to_"..v:name().."_x"..numToVarname(k[1]+extraPipeDelay).."_y"..numToVarname(k[2])
              table.insert(pipeline,"wire ["..(node.kernel.type:sizeof()*8-1)..":0] "..wirename..";\n")
              lboutputs = lboutputs..".out"..(#consumers).."_x"..numToVarname(k[1]).."_y"..numToVarname(k[2]).."("..wirename.."),"
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

  local metadata = {maxStencil = maxStencil, outputShift = shifts[kernelGraph.child1]}

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
Buffer inputBuffer(.CLK(CLK), .inaddr(addr), .WE(receiving), .indata(rxbits), .outaddr(pipelineReadAddr), .outdata(pipelineInput));

wire [7:0] pipelineOutput;
wire [7:0] outbuf;
reg [12:0] pipelineWriteAddr = -PIPE_DELAY; // pipe delay
Buffer outputBuffer(.CLK(CLK), .inaddr(pipelineWriteAddr), .WE(processing), .indata(pipelineOutput), .outaddr(sendAddr), .outdata(outbuf));

Pipeline pipeline(.CLK(CLK), .inX(posX+metadata[12:0]), .inY(posY+metadata[28:16]), .in(pipelineInput), .out(pipelineOutput));

reg [7:0] rxCRC = 0;
reg [7:0] sendCRC = 0;
   
wire rxvalid;
wire txready;
RXMOD rxmod(.RX(RX),.CLK(CLK),.outbits(rxbits),.outvalid(rxvalid));
TXMOD txmod(.TX(TX),.CLK(CLK),.inbits( (sendAddr>]=]..(pxcnt-1)..[=[)?((sendAddr==]=]..pxcnt..[=[)?sendCRC:rxCRC):outbuf),.enable(sending),.ready(txready));

always @(posedge CLK) begin
  if(receiving) begin
  if(addr == ]=]..pxcnt..[=[) begin
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
      end else if(sendAddr==]=]..(pxcnt+2)..[=[) begin
      // we're done
      sending <= 0;
      receiving <= 1;
      rxCRC <= 0;
      processing <= 0;
	    sendAddr <= -1;
      sendCRC <= 0;
    end else if(txready) begin
      sendAddr <= sendAddr + 1;
      if (sendAddr >= 0 && sendAddr < ]=]..pxcnt..[=[) begin sendCRC <= sendCRC + outbuf; end
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