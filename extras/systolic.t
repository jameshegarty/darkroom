local systolic={}

systolicModuleFunctions = {}
systolicModuleMT={__index=systolicModuleFunctions}

local binopToVerilog={["+"]="+",["*"]="*",["<<"]="<<<",[">>"]=">>>",["pow"]="**",["=="]="==",["and"]="&",["-"]="-",["<"]="<",[">"]=">",["<="]="<=",[">="]=">="}

function systolic.module(name)
  assert(type(name)=="string")
  local t = {name=name,regs={},callsites={},functions={}}
  return setmetatable(t,systolicModuleMT)
end

function systolicModuleFunctions:add(inst)
  if inst.kind=="reg" then
    table.insert(self.regs,inst)
  else
    assert(false)
  end
end

function systolicModuleFunctions:getInstance(instname,callsites)
  local t = {}
  table.insert(t,self.name.." "..instname.."(.CLK(CLK)")

  for k,v in pairs(callsites) do
    local fn = self.functions[v[1]]
    assert(fn~=nil)

    assert(type(v[2]["valid"])=="string")
    table.insert(t,", .C"..k.."_"..v[1].."_valid("..v[2]["valid"]..")")

    for _,inout in pairs(joinTables(fn.inputs,fn.outputs)) do
      assert(inout.kind=="input" or inout.kind=="output")
      local actual = v[2][inout.varname]
      if actual==nil then actual = v[3][inout.varname] else assert(v[3][inout.varname]==nil) end
      if type(actual)~="string" and type(actual)~="number" then
        print("Error, missing input/output ",inout.varname," on function ",v[1])
        for k,v in pairs(v[2]) do
          print("INP",k,v)
        end
      end

      table.insert(t,", .C"..k.."_"..inout.varname.."("..actual..")")
    end
  end

  table.insert(t,");\n")
  return t
end

function systolicModuleFunctions:getDefinition(callsites)
  assert(type(callsites)=="table")
  local t = {}

  table.insert(t,"module "..self.name.."(input CLK")
  
  local fnCallsites = {}
  for k,v in pairs(callsites) do
    fnCallsites[v[1]] = v

    local fn = self.functions[v[1]]
    assert(fn~=nil)

    table.insert(t,", input C"..k.."_"..v[1].."_valid")

    for _,input in pairs(fn.inputs) do
      table.insert(t,", input ["..(input.type:sizeof()*8-1)..":0] C"..k.."_"..input.varname)
    end

    for _,output in pairs(fn.outputs) do
      table.insert(t,", output ["..(output.type:sizeof()*8-1)..":0] C"..k.."_"..output.varname)
    end
  end
  table.insert(t,")\n")

  for k,v in pairs(self.functions) do
    t=concat(t,v:getDefinition(callsites))
  end

  table.insert(t,"endmodule\n\n")

  return t
end

systolicFunctionFunctions = {}
systolicFunctionMT={__index=systolicFunctionFunctions}

function systolicModuleFunctions:addFunction(name, inputs, outputs)

  assert(type(inputs)=="table")
  for k,v in pairs(inputs) do assert(v.kind=="input") end

  assert(type(outputs)=="table")
  for k,v in pairs(outputs) do assert(v.kind=="output") end

  local t = {name=name, inputs=inputs, outputs=outputs, assignments={}, asserts={}}
  assert(self.functions[name]==nil)
  self.functions[name]=t
  return setmetatable(t,systolicFunctionMT)
end

function systolicFunctionFunctions:getDelay()
  local maxd = 0
  for _,v in pairs(self.assignments) do
    if v.retime[v.expr]>maxd then maxd = v.retime[v.expr] end
  end
  return maxd
end

local function retime(expr)
  local retiming = {}

  expr:visitEach(
    function(n, inputs)
      local maxDelay = 0
      for k,v in n:inputs() do
        -- only retime nodes that are actually used
        if inputs[k]>maxDelay then maxDelay = inputs[k] end
      end
      retiming[n] = maxDelay + n:internalDelay(expr)
      return retiming[n]
    end)

  return retiming
end

function systolicFunctionFunctions:addAssign(dst,expr)
  assert(dst.kind=="output" or dst.kind=="reg")
  
  table.insert(self.assignments,{dst=dst,expr=expr,retiming=retime(expr)})
end

local function codegen(ast)
  local resDeclarations = {}
  local resClockedLogic = {}

  local finalOut = ast:visitEach(
    function(n, inputs)
      local res
      if n.kind=="binop" then
        table.insert(resDeclarations, declareReg( n.type:baseType(), n:name() ))
        
        local op = binopToVerilog[n.op]
        if type(op)~="string" then print("OP",n.op); assert(false) end
        local lhs = inputs.lhs
        if n.lhs.type:baseType():isInt() then lhs = "$signed("..lhs..")" end
        local rhs = inputs.rhs
        if n.rhs.type:baseType():isInt() then rhs = "$signed("..rhs..")" end
        table.insert(resClockedLogic, n:name().." <= "..lhs..op..rhs..";\n")

        res = n:name()
      elseif n.kind=="input" or n.kind=="reg" then
        res = n.varname
      elseif n.kind=="value" then 
        local v = valueToVerilog(n.value, n.type:baseType())
        table.insert(resDeclarations,declareWire(n.type:baseType(), n:name(), v, " //value" ))
        res = n:name()
      else
        print(n.kind)
        assert(false)
      end
      return res
    end)

  return resDeclarations, resClockedLogic, finalOut
end

function systolicFunctionFunctions:addAssert(expr)
  table.insert(self.asserts,expr)
end

function systolicFunctionFunctions:getDefinition(callsites)
  local t = {}
  table.insert(t,"  // function: "..self.name.."\n")

  local clocked = {}
  for k,v in pairs(self.assignments) do
    local decl, clk, res = codegen(v.expr)
    t = concat(t,decl)
    clocked = concat(clocked,clk)
    table.insert(t, "assign "..v.dst.varname.." = "..res..";\n")
  end

  table.insert(t,"always @ (posedge CLK) begin\n")
  t = concat(t, clocked)
  table.insert(t,"end\n")

  return t
end

local function convert(ast)
  if getmetatable(ast)==systolicASTMT then
    return ast
  elseif type(ast)=="number" then
    return setmetatable({kind="value", value=ast, type=darkroom.type.uint(8)},systolicASTMT)
  else
    assert(false)
  end
end

local function binop(lhs, rhs, op)
  lhs = convert(lhs)
  rhs = convert(rhs)
  return setmetatable({kind="binop",op=op,lhs=lhs,rhs=rhs,type=lhs.type},systolicASTMT)
end

systolicASTFunctions = {}
setmetatable(systolicASTFunctions,{__index=IRFunctions})
systolicASTMT={__index=systolicASTFunctions,__add=function(l,r) return binop(l,r,"+") end}

function systolicASTFunctions:internalDelay()
  if self.kind=="binop" then
    return 1
  elseif self.kind=="input" or self.kind=="reg" or self.kind=="value" then
    return 0
  else
    print(self.kind)
    assert(false)
  end
end

function systolic.reg( name, ty, initial )
  assert(type(name)=="string")
  ty = darkroom.type.fromTerraType(ty, 0, 0, "")
  return setmetatable({kind="reg",varname=name,initial=initial,type=ty},systolicASTMT)
end

function systolic.output(name, ty)
  assert(type(name)=="string")
  ty = darkroom.type.fromTerraType(ty, 0, 0, "")
  return setmetatable({kind="output",varname=name,type=ty},systolicASTMT)
end

function systolic.input(name, ty)
  assert(type(name)=="string")
  ty = darkroom.type.fromTerraType(ty, 0, 0, "")
  return setmetatable({kind="input",varname=name,type=ty},systolicASTMT)
end

return systolic