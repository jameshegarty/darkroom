local fpga = {}
fpga.util = terralib.require("fpgautil")


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

function declareReg(type, name, initial, comment)
  if comment==nil then comment="" end

  if initial==nil or initial=="" then 
    initial=""
  else
    initial = " = "..initial
  end

  if type:isBool() then
    return "reg "..name..initial..";"..comment.."\n"
  else
    return "reg ["..(type:sizeof()*8-1)..":0] "..name..initial..";"..comment.."\n"
 end
end

function declareWire(ty, name, str, comment)
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

function kernelToVarname(x)
  if type(x)=="number" then return "_"..x
  else return "_"..x:name() end
end

fpga.modules = terralib.require("fpgamodules")

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

function valueToVerilog(value,ty)

  if ty:isInt() then
    assert(type(value)=="number")
    if value<0 then
      return "-"..(ty:sizeof()*8).."'d"..math.abs(value)
    else
      return (ty:sizeof()*8).."'d"..value
    end
  elseif ty:isUint() then
    assert(type(value)=="number")
    assert(value>=0)
    return (ty:sizeof()*8).."'d"..value
  else
    assert(false)
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
    print(type(mrvValues[self.variable]))
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
  elseif self.kind=="load" or self.kind=="value" or self.kind=="cast" or self.kind=="position" or self.kind=="mapreducevar" or self.kind=="array" or self.kind=="index" or self.kind=="lifted" then
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
--
-- Remember, this is not at all affected by the darkroom schedule shifts.
-- the darkroom schedules takes a pipeline, and produces a pipeline.
-- There is no side-band information needed to implement the pipeline the
-- scheduler produces. So we can totally ignore the darkroom schedule here,
-- and just rate match due to the extra retiming registers we introduced.
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

local binopToVerilog={["+"]="+",["*"]="*",["<<"]="<<",[">>"]=">>",["pow"]="**",["=="]="==",["and"]="&",["-"]="-",["<"]="<",[">"]=">",["<="]="<=",[">="]=">="}
local binopToVerilogBoolean={["=="]="==",["and"]="&&"}

function fpga.codegenKernel(compilerState, kernelGraphNode, retiming, imageWidth, imageHeight)
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
    assert(kernel:S("load"):count()<=1)
    kernel:S("load"):traverse(
      function(n)
        inputs = "input ["..(n.type:sizeof()*8-1)..":0] in_"..n.from.."_x0_y0,\n"
      end)
  end

  local result = {"module Kernel_"..kernelGraphNode:name().."(input CLK, input[12:0] inX, input[12:0] inY, output[12:0] outX, output[12:0] outY, \n"..inputs.."output ["..(kernel.type:sizeof()*8-1)..":0] out);\n"}
  local clockedLogic = {}

  table.insert(result,"wire [12:0] inX_0;\n")
  table.insert(result,"assign inX_0 = inX;\n")
  table.insert(result,"wire [12:0] inY_0;\n")
  table.insert(result,"assign inY_0 = inY;\n")

  -- these variable only serve to delay the input x,y to the output x,y
  -- They are not used internally by this kernel - the x,y used
  -- internally by the kernel are delayed using the normal retiming infrastructure.
  -- Potentially, we could accomplish the same thing with adding a delay
  -- to x,y using an add operator, which may be better in some cases?
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
             -- type is determined by producer, b/c consumer op can change type
            table.insert(declarations, declareReg( v.type:baseType(), sn, "", " // retiming" ))
            table.insert(clockedLogic, sn.." <= "..prev.."; // retiming\n")
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

        for i=1,n["countLifted"] do
          table.insert(moduledef,"input ["..(n["typeLifted"..i]:sizeof()*8-1)..":0] lifted"..i..",\n")
        end
        
        table.insert(moduledef,"output ["..(n.expr.type:sizeof()*8-1)..":0] out);\n\n")

        table.insert(moduledef,table.concat(args.expr[3],""))
        table.insert(moduledef,"always @ (posedge CLK) begin\n"..table.concat(args.expr[2],"").."end\n")

        table.insert(moduledef,"assign out = {"..inputs.expr[#inputs.expr])
        local c = #inputs.expr-1
        while c>=1 do table.insert(moduledef,","..outputName[c]); c=c-1 end
        table.insert(moduledef, "};\n")
        table.insert(moduledef,"endmodule\n\n")

        result = concat(moduledef,result)

        clockedLogic = {}
        declarations = {}

        local declSeen={}
        for k,v in pairs(n) do
          if k:sub(0,6)=="lifted" then
            assert(retiming[v]==0) -- we don't support pipelining lifted stuff
            merge(args[k][3], declarations, declSeen)
          end
        end
        -- funroll
        local partials = -1
        local argminPartials = ""
        local funroll = {function(inputList, mrvValues) 
                           partials = partials+1; 
                           if n.reduceop=="argmin" then argminPartials = argminPartials..",.partial"
                             local i=1
                             while n["varname"..i] do argminPartials = argminPartials.."_"..n["varname"..i]..numToVarname(mrvValues[n["varname"..i]]); i=i+1; end
                             argminPartials = argminPartials.."("..n:name().."_partial"..partials..")"
                           end
                           local liftedInputs = ""
                           local term = ""
                           local i=1
                           while n["varname"..i] do
                             term = term.."_"..n["varname"..i]..numToVarname(mrvValues[n["varname"..i]])
                             i = i + 1
                           end
                           for i=1,n["countLifted"] do
                             local v = "lifted"..i..term
                             liftedInputs = liftedInputs..",.lifted"..i.."("..args[v][1][1]..")"
                           end

                           return {declareWire(n.type,n:name().."_partial"..partials).."Map_"..n:name().." map_"..n:name().."_"..partials.."(.CLK(CLK),.out("..n:name().."_partial"..partials.."),.inX(inX),.inY(inY)"..inputList..liftedInputs..");\n"} end}

        local i = 1
        while n["varname"..i] do
          local ii = i
          table.insert(funroll, function(inputList, mrvValues) local res = {}; for j=n["varlow"..ii],n["varhigh"..ii] do mrvValues[n["varname"..ii]]=j; res = concat(res, funroll[ii](",.mrvar_"..n["varname"..ii].."("..j..")"..inputList, mrvValues)) end; return res end)
          i=i+1
        end
        
        declarations = concat(declarations, funroll[#funroll]("",{}))

        local rtype = n.expr.type
        if n.reduceop=="argmin" then rtype=n.type:baseType() end
        local rname, rmod = fpga.modules.reduce(compilerState, n.reduceop, partials+1, rtype, n)

        result = concat(rmod, result)

        local finalOut = {}

        if n.reduceop=="argmin" then
          for c=1,n.type:channels() do table.insert(declarations, declareWire(rtype, n:cname(c))) end
          table.insert(declarations,rname.." reduce_"..n:cname(c).."(.CLK(CLK),.out("..n:cname(n.type:channels())..")")
          for c=1,n.type:channels()-1 do table.insert(declarations, ",.out_"..n["varname"..c].."("..n:cname(c)..")") end
          table.insert(declarations,argminPartials..");\n")
          for c=1,n.type:channels() do table.insert(finalOut, n:cname(c)) end
        else
          for c=1,n.type:channels() do
            table.insert(declarations, declareWire(n.type, n:cname(c)))
            table.insert(declarations,rname.." reduce_"..n:cname(c).."(.CLK(CLK),.out("..n:cname(c)..")")
            for i=0,partials do table.insert(declarations,",.partial_"..i.."("..n:name().."_partial"..i..")") end
            table.insert(declarations,");\n")
            table.insert(finalOut, n:cname(c))
          end
        end

        return {finalOut, clockedLogic, declarations}
      end

      for c=1,n.type:channels() do
        local res
        if n.kind=="binop" then
          table.insert(declarations, declareReg( n.type:baseType(), n:cname(c) ))

          if n.op=="<" or n.op==">" or n.op=="<=" or n.op==">=" then
            if n.type:baseType():isBool() and n.lhs.type:baseType():isInt() and n.rhs.type:baseType():isInt() then
              table.insert(clockedLogic, n:name().."_c"..c.." <= ($signed("..inputs.lhs[c]..")"..n.op.."$signed("..inputs.rhs[c].."));\n")
            elseif n.type:baseType():isBool() and n.lhs.type:baseType():isUint() and n.rhs.type:baseType():isUint() then
              table.insert(clockedLogic, n:name().."_c"..c.." <= (("..inputs.lhs[c]..")"..n.op.."("..inputs.rhs[c].."));\n")
            else
              print( n.type:baseType():isBool() , n.lhs.type:baseType():isInt() , n.rhs.type:baseType():isInt(),n.type:baseType():isBool() , n.lhs.type:baseType():isUint() , n.rhs.type:baseType():isUint())
              assert(false)
            end

          elseif n.type:isBool() then
            local op = binopToVerilogBoolean[n.op]
            if type(op)~="string" then print("OP_BOOLEAN",n.op); assert(false) end
            table.insert(clockedLogic, n:name().."_c"..c.." <= "..inputs.lhs[c]..op..inputs.rhs[c]..";\n")
          else
            local op = binopToVerilog[n.op]
            if type(op)~="string" then print("OP",n.op); assert(false) end
            table.insert(clockedLogic, n:name().."_c"..c.." <= "..inputs.lhs[c]..op..inputs.rhs[c]..";\n")
          end

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
          table.insert(declarations,declareReg( n.type:baseType(), n:cname(c), "", " // "..n.kind.." result" ))
          local condC = 1
          if n.kind=="vectorSelect" then condC=c end

          table.insert(clockedLogic, n:cname(c).." <= ("..inputs.cond[condC]..")?("..inputs.a[c].."):("..inputs.b[c].."); // "..n.kind.."\n")
          res = n:cname(c)
        elseif n.kind=="load" then
          local v = "in"..kernelToVarname(n.from).."_x"..getStencilCoord(n.relX).."_y"..getStencilCoord(n.relY)
          local tys = n.type:baseType():sizeof()*8
          table.insert(declarations,declareWire( n.type:baseType(), n:cname(c), v.."["..(c*tys-1)..":"..((c-1)*tys).."]" ))
          res = n:cname(c)
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
          local cmt = " // cast "..n.expr.type:str().." to "..n.type:str()
          if n.type:isArray() and n.expr.type:isArray()==false then
            expr = inputs["expr"][1] -- broadcast
            cmt = " // broadcast "..n.expr.type:str().." to "..n.type:str()
          else
            expr = inputs["expr"][c]
          end

          if n.type:isInt() and n.expr.type:isInt() and n.type:sizeof()>n.expr.type:sizeof() then
            -- must sign extend
            expr = "{ {"..(8*(n.type:sizeof()-n.expr.type:sizeof())).."{"..expr.."["..(n.expr.type:sizeof()*8-1).."]}},"..expr.."["..(n.expr.type:sizeof()*8-1)..":0]}"
          end
          
          table.insert(declarations, declareWire(n.type:baseType(), n:cname(c), "",cmt))
          table.insert(declarations, "assign "..n:cname(c).." = "..expr..";"..cmt.."\n")
          res = n:cname(c)
        elseif n.kind=="value" then
          local v
          if type(n.value)=="table" then 
            v = valueToVerilog(n.value[c], n.type:baseType()) 
          else
            v = valueToVerilog(n.value, n.type:baseType())
          end
          table.insert(declarations,declareWire(n.type:baseType(), n:cname(c), v, " //value" ))
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
          local rname, rmod = fpga.modules.reduce(compilerState, n.op, n:arraySize("expr"), n.type)
          result = concat(rmod, result)
          table.insert(declarations, declareWire(n.type, n:cname(c),"", "// reduce result"))
          local str = rname.." reduce_"..n:cname(c).."(.CLK(CLK),.out("..n:cname(c)..")"
          n:map("expr",function(_,i) str = str..",.partial_"..(i-1).."("..inputs["expr"..i][c]..")" end)
          table.insert(declarations,str..");\n")
          res = n:cname(c)
        elseif n.kind=="lifted" then
          res = "lifted"..n.id
        else
          print(n.kind)
          assert(false)
        end

        assert(type(res)=="string")
        assert(res:match("[%w%[%]]")) -- should only be alphanumeric
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

local function calcMaxStencil(kernelGraph)
  local maxStencil = Stencil.new()
  kernelGraph:visitEach(
    function(node)
      for k,v in node:inputs() do
        if node.kernel~=nil then print("ST",node.kernel:stencil(v):min(1),node.kernel:stencil(v):max(1),"Y",node.kernel:stencil(v):min(2),node.kernel:stencil(v):max(2)) end
      end
      if node.kernel~=nil then maxStencil = maxStencil:unionWith(neededStencil(true,kernelGraph,node,1,nil)) end
    end)
  return maxStencil
end

local function chooseStrip(options, inputs, kernelGraph)
  if options.stripWidth~=nil or options.stripHeight~=nil then 
    assert(type(options.stripWidth)=="number")
    assert(type(options.stripHeight)=="number")
    return options.stripWidth, options.stripHeight
  end

  local BLOCKX = 74
  local BLOCKY = 6

  for k,v in ipairs(inputs) do
    assert(v[1].kind=="crop" and v[1].expr.kind=="load")
    BLOCKX = math.floor(BLOCKX/v[1].expr.type:sizeof())
  end

  local maxStencil=calcMaxStencil(kernelGraph)

--  assert( (maxStencil:max(1)-maxStencil:min(1)+1) < BLOCKX )
--  assert( (maxStencil:max(2)-maxStencil:min(2)+1) < BLOCKY )
  
  if (maxStencil:max(1)-maxStencil:min(1)+1) >= BLOCKX then
    print("Expanding strip width to contain stencil.")
    print("oldwidth", BLOCKX)
    BLOCKX = maxStencil:max(1)-maxStencil:min(1)+2
    print("newwidth", BLOCKX)
  end

  if (maxStencil:max(2)-maxStencil:min(2)+1) >= BLOCKY then
    print("Expanding strip height to contain stencil.")
    print("oldheight", BLOCKY)
    BLOCKY = maxStencil:max(2)-maxStencil:min(2)+2
    print("newheight", BLOCKY)
  end

  return BLOCKX, BLOCKY
end

local function liftMapreduce(kernelGraph, shifts)
  local newShifts={}
  local oldToNewRemap = {}
  local res =  kernelGraph:S("*"):process(
    function(node, orig)
      if node.kernel~=nil then
        local newNode = node:shallowcopy()
        newNode.kernel = node.kernel:S("mapreduce"):process(
          function(mapreduceNode)
            local newMR = mapreduceNode:shallowcopy()
            local liftedList = {}
            local liftedCnt = 0
            newMR.expr = mapreduceNode.expr:S(
              function(n)
                -- heuristic for what we lift
                return n.kind=="index" or n.kind=="load"
              end):process(
              function(n)
                liftedCnt = liftedCnt + 1
                liftedList["lifted"..liftedCnt] = n
                newMR["typeLifted"..liftedCnt] = n.type
                return darkroom.typedAST.new({kind="lifted",id=liftedCnt,type=n.type}):copyMetadataFrom(n)
              end)
            newMR["countLifted"] = liftedCnt

            -- funroll the lifted vars
            local i = 1
            while mapreduceNode["varname"..i]~=nil do
              local liftedListNew = {}
              for f=mapreduceNode["varlow"..i], mapreduceNode["varhigh"..i] do
                for k,v in pairs(liftedList) do
                  liftedListNew[k.."_"..mapreduceNode["varname"..i]..numToVarname(f)] = v:S("*"):process(
                    function(anode)
                      local newAnode = anode:shallowcopy()
                      local found = false
                      for k,v in pairs(anode) do
                        if darkroom.ast.isAST(v) then
                          newAnode[k] = v:S("mapreducevar"):process(
                            function(n)
                              if n.variable==mapreduceNode["varname"..i] then found=true;return darkroom.ast.new({kind="value",value=f,type=n.type}):copyMetadataFrom(n) end
                            end)
                        end
                      end
                      if found then return darkroom.typedAST.new(newAnode):copyMetadataFrom(anode) end
                    end)
                end
              end
              liftedList = liftedListNew
              i = i + 1
            end
            for k,v in pairs(liftedList) do
              newMR[k] = v
            end

            return darkroom.typedAST.new(newMR):copyMetadataFrom(mapreduceNode)
          end)

        newNode.kernel = newNode.kernel:S("load"):process(
          function(n)
            local nn = n:shallowcopy()
            if type(nn.from)=="table" then nn.from = oldToNewRemap[nn.from] end
            return darkroom.typedAST.new(nn):copyMetadataFrom(n)
          end)
        newShifts[newNode] = shifts[orig]
        oldToNewRemap[orig] = newNode
        return darkroom.kernelGraph.new(newNode):copyMetadataFrom(node.kernel)
      else
        oldToNewRemap[orig] = node
        newShifts[node] = shifts[orig]
      end
    end)

  return res, newShifts
end

function fpga.compile(inputs, outputs, imageWidth, imageHeight, options)
  assert(#outputs==1)
  assert(type(options)=="table" or options==nil)

  local compilerState = {declaredReductionModules = {}}

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

  local kernelGraph = darkroom.frontEnd( ast, {} )

  options.stripWidth, options.stripHeight = chooseStrip(options,inputs,kernelGraph)

  local shifts = schedule(kernelGraph, 1, options.stripWidth)
  kernelGraph, shifts = shift(kernelGraph, shifts, 1, options.stripWidth)

  kernelGraph, shifts = liftMapreduce(kernelGraph, shifts)

  local totalInputBytes = 0
  for k,v in ipairs(inputs) do totalInputBytes = totalInputBytes + inputs[k][1].expr.type:sizeof() end
  local outputBytes = kernelGraph.child1.kernel.type:sizeof()
  local outputChannels = kernelGraph.child1.kernel.type:channels()
  print("INPUTBYTeS",totalInputBytes,"OUTPUTBYTES",outputBytes)

  local maxStencil = calcMaxStencil(kernelGraph)

  local shiftX, shiftY = delayToXY(shifts[kernelGraph.child1], options.stripWidth)
  maxStencil = maxStencil:translate(shiftX,shiftY,0)
  print("Max Stencil x="..maxStencil:min(1)..","..maxStencil:max(1).." y="..maxStencil:min(2)..","..maxStencil:max(2))


  ------------------------------
  local result = {}
  table.insert(result, "`timescale 1ns / 10 ps\n")

  local pipeline = {[=[module Pipeline(
input CLK, input[12:0] inX, input[12:0] inY,
input []=]..(totalInputBytes*8-1)..[=[:0] packedinput,
output []=]..(outputBytes*8-1)..[=[:0] out);
]=]}

  -- map the packed input bytes into a variable for each image
  local packedInputPos = 0
  for k,v in ipairs(inputs) do 
    assert(inputs[k][1].expr.kind=="load")
    local ty = inputs[k][1].expr.type
    table.insert(pipeline, declareWire(ty, "in_"..inputs[k][1].expr.from,"packedinput["..(packedInputPos+ty:sizeof()*8-1)..":"..packedInputPos.."]"," // unpack input"))
    packedInputPos = packedInputPos + ty:sizeof()*8
  end

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
        if parentIsOutput(node)==false and node:bufferSize(kernelGraph,options.stripWidth)>0 then 
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
        local verilogKernel = fpga.codegenKernel(compilerState, node, kernelRetiming[node], imageWidth, imageHeight)
        result = concat(result, verilogKernel)
        
        local inputs = ""
        local inputXY = ""
        if node:inputCount()==0 then
          assert(node.kernel:S("load"):count()<=1)
          node.kernel:S("load"):traverse(
            function(n)
              inputs = ".in_"..n.from.."_x0_y0(in_"..n.from.."),"
            end)
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
	    local b = -stencil:min(1)-stencil:min(2)*options.stripWidth
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
          local lbname, lbmod = fpga.modules.linebuffer(linebufferSize, node.kernel.type, options.stripWidth, consumers)
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

  local metadata = {minX = maxStencil:min(1), maxX=maxStencil:max(1), minY=maxStencil:min(2), maxY = maxStencil:max(2), outputShift = shifts[kernelGraph.child1], outputChannels = outputChannels, outputBytes = outputBytes, stripWidth = options.stripWidth, stripHeight=options.stripHeight, uartClock=options.uartClock}

  for k,v in ipairs(inputs) do
    metadata["inputFile"..k] = v[3]
  end

  table.insert(pipeline, "assign out = kernelOut_"..kernelGraph.child1:name()..";\n")
  table.insert(pipeline,"endmodule\n\n")
  table.insert(pipeline,"parameter PIPE_DELAY = "..(totalDelay)..";\n")
  result = concat(result, pipeline)

  if outputs[1][2]=="vga" then
    table.insert(result, fpga.modules.stageVGA())
  elseif outputs[1][2]=="uart" then
    table.insert(result, fpga.modules.stageUART(options, totalInputBytes, outputBytes, options.stripWidth, options.stripHeight))
  elseif outputs[1][2]=="sim" then
    -- sim framework assumes this is the case
    assert(imageWidth==options.stripWidth)
    assert(imageHeight==options.stripHeight)
    table.insert(result, fpga.modules.sim(totalInputBytes, outputBytes, imageWidth, shifts[kernelGraph.child1]))
  else
    print("unknown data source "..outputs[1][2])
    assert(false)
  end

  return table.concat(result,""), metadata
end

return fpga