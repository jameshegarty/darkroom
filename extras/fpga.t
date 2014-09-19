local fpga = {}

--UART_CLOCK = 115200
UART_CLOCK = 57600
--UART_CLOCK = 19200

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

function fpga.linebuffer(lines, bytesPerPixel, imageWidth, consumers)

  local name = "Linebuffer_"..lines.."lines_"..bytesPerPixel.."bpp_"..imageWidth.."w"

  local outputs = ""
  for k,v in ipairs(consumers) do
    for x=v:min(1),v:max(1) do
      for y=v:min(2), v:max(2) do
        outputs = outputs .. "output ["..(bytesPerPixel*8-1)..":0] out"..k.."_x"..x.."_y"..y..",\n"
      end
    end
  end

  local t = {"module "..name.."(input CLK,\n"..outputs.."input ["..(bytesPerPixel*8-1)..":0] in);\n"}

  if lines==1 then
    -- we're only delaying a few pixels, don't use a bram
    for k,v in ipairs(consumers) do      
      table.insert(t, "assign out"..k.."_x0_y0 = in;\n")
    end

  else
    assert(false)
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

function fpga.tx(size)
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
  wire [28:0] dInc = d[28] ? (]=]..UART_CLOCK..[=[) : (]=]..UART_CLOCK..[=[ - 32000000);
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

function fpga.rx()
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
  wire [28:0] dInc = d[28] ? (]=]..(UART_CLOCK*16)..[=[) : (]=]..(UART_CLOCK*16)..[=[ - 32000000);
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


function typedASTFunctions:internalDelay()
  if self.kind=="binop" or self.kind=="unary" or self.kind=="select" then
    return 1
  elseif self.kind=="load" or self.kind=="crop" or self.kind=="value" or self.kind=="cast" then
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

local function declareReg(type, name)
  return "reg ["..(type:sizeof()*8-1)..":0] "..name..";\n"
end

local function declareWire(type, name, str)
  return "wire ["..(type:sizeof()*8-1)..":0] "..name.." = "..str..";\n"
end

function fpga.codegenKernel(kernelGraphNode, retiming)
  local kernel = kernelGraphNode.kernel

  local inputs = ""

  for k,v in kernelGraphNode:inputs() do
    inputs = inputs.."input ["..(v.kernel.type:sizeof()*8-1)..":0] in_"..v:name().."_x0_y0,\n"
  end

  if kernelGraphNode:inputCount()==0 then
    inputs = "input [7:0] in_x0_y0,\n"
  end

  local result = {"module Kernel_"..kernelGraphNode:name().."(input CLK,\n"..inputs.."output ["..(kernel.type:sizeof()*8-1)..":0] out);\n"}
  local clockedLogic = {}

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
            table.insert(clockedLogic, n:name().." <= ("..inputs.expr.."["..(n.type:sizeof()*8-1)..":0] == 1'b1)?(-"..inputs.expr.."):("..inputs.expr..");\n")
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
--        res = "input_x"..n.relX.."_y"..n.relY

        if type(n.from)=="number" then
          res = "in_x0_y0"
        else
          res = "in_"..n.from:name().."_x0_y0"
        end
      elseif n.kind=="crop" then
        res = inputs.expr
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

function fpga.compile(inputs, outputs, width, height, options)
  assert(#inputs==1)
  assert(#outputs==1)

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
  local shifts = schedule(kernelGraph)
  kernelGraph, shifts = shift(kernelGraph, shifts)

  ------------------------------
  local result = {}
  result = concat(result, fpga.tx())
  result = concat(result, fpga.rx())
  result = concat(result, fpga.buffer(width*height))

  local pipeline = {[=[module Pipeline(
input CLK,
input [7:0] in,
output [7:0] out);
]=]}

  local function parentIsOutput(node)
    for v,k in node:parents(kernelGraph) do if v==kernelGraph then return true end end
    return false
  end

  local totalDelay = 0
  kernelGraph:visitEach(
    function(node)
      if node.kernel~=nil then
        local retiming = fpga.trivialRetime(node.kernel)
        totalDelay = totalDelay + retiming[node.kernel]
        local verilogKernel = fpga.codegenKernel(node, retiming)
        result = concat(result, verilogKernel)
        
        local inputs = ""
        if node:inputCount()==0 then
          inputs = ".in_x0_y0(in),"
        else
          for k,v in node:inputs() do
            inputs = inputs..".in_"..v:name().."_x0_y0("..v:name().."_to_"..node:name().."),"
          end
        end
        
        table.insert(pipeline,"wire ["..(node.kernel.type:sizeof()*8-1)..":0] kernelOut_"..node:name()..";\n")
        table.insert(pipeline,"Kernel_"..node:name().." kernel_"..node:name().."(.CLK(CLK),"..inputs..".out(kernelOut_"..node:name().."));\n")
        
        local lboutputs = ""

        local consumers = {}
        for v,_ in node:parents(kernelGraph) do
          if v.kernel~=nil then
            table.insert(consumers, v.kernel:stencil(node))
            table.insert(pipeline,"wire ["..(node.kernel.type:sizeof()*8-1)..":0] "..node:name().."_to_"..v:name()..";\n")
            lboutputs = ".out"..(#consumers).."_x0_y0("..node:name().."_to_"..v:name().."),"
          end
        end
        
        if parentIsOutput(node)==false then -- output nodes don't write to linebuffer
          local lbname, lbmod = fpga.linebuffer(node:bufferSize(kernelGraph), node.kernel.type:sizeof(), width, consumers)
          result = concat(result, lbmod)
          table.insert(pipeline,lbname.." kernelBuffer_"..node:name().."(.CLK(CLK),"..lboutputs..".in(kernelOut_"..node:name().."));\n")
        end
      end
    end)

  table.insert(pipeline, "assign out = kernelOut_"..kernelGraph.child1:name()..";\n")
  table.insert(pipeline,"endmodule\n\n")
  table.insert(pipeline,"parameter PIPE_DELAY = "..(totalDelay+1)..";\n") -- one cycle delay for the bram or something?
  result = concat(result, pipeline)

local pxcnt = width*height
  table.insert(result,[=[module stage(
input CLK, 
input RX, 
output TX,
output [7:0] LED);

reg [12:0] addr = 0;
reg [12:0] sendAddr = -1;

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

Pipeline pipeline(.CLK(CLK), .in(pipelineInput), .out(pipelineOutput));

reg [7:0] rxCRC = 0;
reg [7:0] sendCRC = 0;
   
wire rxvalid;
wire txready;
RXMOD rxmod(.RX(RX),.CLK(CLK),.outbits(rxbits),.outvalid(rxvalid));
TXMOD txmod(.TX(TX),.CLK(CLK),.inbits( (sendAddr>]=]..(pxcnt-1)..[=[)?((sendAddr==]=]..pxcnt..[=[)?sendCRC:rxCRC):outbuf),.enable(sending),.ready(txready));

always @(posedge CLK) begin
  if(receiving) begin
  if(addr == ]=]..pxcnt..[=[) begin
      addr <= 0;
      receiving <= 0;
		  sending <= 0;
		  processing <= 1;
    end else if(rxvalid) begin
      addr <= addr + 1;
      rxCRC <= rxCRC + rxbits;
    end
  end
  
  if(processing) begin
    if(rxvalid) begin
      sending <= 0;
      receiving <= 1;
      rxCRC <= 0;
      processing <= 0;
      sendAddr <= -1;
      pipelineWriteAddr <= -PIPE_DELAY;
      pipelineReadAddr <= 0;
    end else if(pipelineWriteAddr == ]=]..pxcnt..[=[) begin
      pipelineWriteAddr <= -PIPE_DELAY;
		  pipelineReadAddr <= 0;
      receiving <= 0;
		  sending <= 1;
		  processing <= 0;
    end else begin
	    pipelineReadAddr <= pipelineReadAddr + 1;
      pipelineWriteAddr <= pipelineWriteAddr + 1;
	 end
  end
  
  if(sending) begin
    if(rxvalid) begin
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
endmodule]=])

  return table.concat(result,"")
end

return fpga