local fpga = {}

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
  for _,v in ipairs(consumers) do
    for x=v:min(1),v:max(1) do
      for y=v:min(2), v:max(2) do
        outputs = outputs .. "output ["..(bytesPerPixel*8-1)..":0] out_x"..x.."_y"..y..",\n"
      end
    end
  end

  local t = {"module "..name.."(input CLK,\ninput ["..(bytesPerPixel*8-1)..":0] in,\n"..outputs..");\n"}

  if lines==1 then
    -- we're only delaying a few pixels, don't use a bram
    for _,v in ipairs(consumers) do      
      table.insert(t, "assign out_x0_y0 = in;\n")
    end

  else
    assert(false)
  end

  table.insert(t,"endmodule\n")

  return name, t
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
  wire [28:0] dInc = d[28] ? (230400) : (230400 - 32000000);
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
  wire [28:0] dInc = d[28] ? (3686400) : (3686400 - 32000000);
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
  if self.kind=="binop" then
    return 1
  elseif self.kind=="load" or self.kind=="crop" or self.kind=="value" then
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

local function declare(type, name, str)
  return "reg ["..(type:sizeof()*8-1)..":0] "..name.." = "..str..";\n"
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

  local result = {"module Kernel_"..kernelGraphNode:name().."(input CLK,\n"..inputs.."output ["..(kernel.type:sizeof()*8-1)..":0] out);\n"}

  local finalOut = kernel:visitEach(
    function(n, inputs)
      local res
      if n.kind=="binop" then
        table.insert(result,declare(n.type,n:name(),inputs.lhs..n.op..inputs.rhs))
        res = n:name()
      elseif n.kind=="load" then
--        res = "input_x"..n.relX.."_y"..n.relY

        if type(n.from)=="number" then
          res = "in_tx_x0_y0"
        else
          res = "in_"..n.from:name().."_x0_y0"
        end
      elseif n.kind=="crop" then
        res = inputs.expr
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

  table.insert(result,"assign output = "..finalOut..";\n")
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

  local pipeline = {[=[module Pipeline(
input CLK,
input [7:0] in,
output [7:0] out);
]=]}

  local function parentIsOutput(node)
    for v,k in node:parents(kernelGraph) do if v==kernelGraph then return true end end
    return false
  end

  kernelGraph:visitEach(
   function(node)
     if node.kernel~=nil then
       local retiming = fpga.trivialRetime(node.kernel)
       local verilogKernel = fpga.codegenKernel(node, retiming)
       result = concat(result, verilogKernel)

       local inputs = ""
       if node:inputCount()==0 then
         inputs = ".in_x0_y0(in),"
       else

       end

       table.insert(pipeline,"wire [:0] kernelOut_"..node:name()..";\n")
       table.insert(pipeline,"Kernel_"..node:name().." kernel_"..node:name().."(.CLK(CLK),"..inputs..",.out(kernelOut_"..node:name().."));\n")
       
        local consumers = {}
        for v,_ in node:parents(kernelGraph) do
          if v.kernel~=nil then
            table.insert(consumers, v.kernel:stencil(node))
          end
        end

        if parentIsOutput(node)==false then -- output nodes don't write to linebuffer
          local lbname, lbmod = fpga.linebuffer(node:bufferSize(kernelGraph), node.kernel.type:sizeof(), width, consumers)
          result = concat(result, lbmod)
          table.insert(pipeline,lbname.." kernelBuffer_"..node:name().."(.CLK(CLK),in(kernelOut_"..node:name().."));\n")
        end
     end
   end)
--  table.insert(pipeline, "assign out = "..kernelGraph.expr1:name()..";\n")
  table.insert(pipeline,"endmodule\n\n")
  result = concat(result, pipeline)
  return table.concat(result,"")
end

return fpga