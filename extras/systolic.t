local systolic={}

systolicModuleFunctions = {}
systolicModuleMT={__index=systolicModuleFunctions}

systolicInstanceFunctions = {}

local definitionCache = {}
function systolic.addDefinition(module)
  if definitionCache[module]==nil then
    if systolicFunction.isSystolicFunction(module) then
      assert(module.pure)
      definitionCache[module] = module:getDefinition()
    elseif systolicInstance.isSystolicInstance(module) then
      definitionCache[module] = module:getDefinition()
    else
      assert(false)
    end
  end
end

function systolic.getDefinitions()
  local t = ""
  for k,v in pairs(definitionCache) do
    t = t..table.concat(v)
  end

  return t
end

local binopToVerilog={["+"]="+",["*"]="*",["<<"]="<<<",[">>"]=">>>",["pow"]="**",["=="]="==",["and"]="&",["-"]="-",["<"]="<",[">"]=">",["<="]="<=",[">="]=">="}

local binopToVerilogBoolean={["=="]="==",["and"]="&&",["~="]="!=",["or"]="||"}

function declareReg(ty, name, initial, comment)
  assert(type(name)=="string")

  if comment==nil then comment="" end

  if initial==nil or initial=="" then 
    initial=""
  else
    initial = " = "..initial
  end

  if ty:isBool() then
    return "reg "..name..initial..";"..comment.."\n"
  else
    return "reg ["..(ty:sizeof()*8-1)..":0] "..name..initial..";"..comment.."\n"
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
    return "wire "..name..str..";"..comment.."\n"
  else
    return "wire ["..(ty:sizeof()*8-1)..":0] "..name..str..";"..comment.."\n"
  end
end

function declarePort(ty,name,isInput)
  local t = "input "
  if isInput==false then t = "output " end

  if ty:isBool()==false then
    t = t .."["..(ty:sizeof()*8-1)..":0] "
  end
  t = t..name
  
  return t
end

function numToVarname(x)
  if x>0 then return x end
  if x==0 then return "0" end
  return "m"..math.abs(x)
end

function pointerToVarname(x)
  assert(type(x)=="table")
  return tostring(x):sub(10)
end

function systolic.module(name)
  assert(type(name)=="string")
  local t = {name=name,regs={},functions={},ram128s={}}
  return setmetatable(t,systolicModuleMT)
end

function systolicModuleFunctions:add(inst)
  assert(systolicInstance.isSystolicInstance(inst))
  assert( (inst.kind=="instance" and inst.pure==nil) or inst.kind=="reg" or inst.kind=="ram128")

  if inst.kind=="reg" then
    table.insert(self.regs,inst)
    return inst
  elseif inst.kind=="ram128" then
    table.insert(self.ram128s,inst)
    return inst
  else
    assert(false)
  end
end

function valueToVerilogLL(value,signed,bits)
  assert(type(value)=="number")

  if signed then
    if value==0 then
      return bits.."'d0"
    elseif value<0 then
      return "-"..bits.."'d"..math.abs(value)
    else
      return bits.."'d"..value
    end
  else
    assert(value>=0)
    return bits.."'d"..math.abs(value)
  end
end

function valueToVerilog(value,ty)

  if ty:isInt() then
    assert(type(value)=="number")
    if value==0 then
      return (ty:sizeof()*8).."'d0"
    elseif value<0 then
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

function systolicModuleFunctions:instantiate(name)
  assert(type(name)=="string")
  return systolicInstance.new({kind="module",module=self,name=name,callsites={}})
end

function systolicModuleFunctions:getInstance(instname,callsites)
  local t = {}
  table.insert(t,self.name.." "..instname.."(.CLK(CLK)")

  for fnname,v in pairs(callsites) do
    assert(type(v)=="table")
    local fn = self.functions[fnname]
    assert(fn~=nil)

    for id,vv in pairs(v) do
      assert(type(vv[1].valid)=="string")
      table.insert(t,", .C"..id.."_"..fnname.."_valid("..vv[1].valid..")")

      for _,inout in pairs(joinTables(fn.inputs,fn.outputs)) do
        assert(inout.kind=="input" or inout.kind=="output")
        local actual = vv[1][inout.varname]
        if actual==nil then actual = vv[2][inout.varname] else assert(vv[2][inout.varname]==nil) end
        if type(actual)~="string" and type(actual)~="number" then
          print("Error, missing input/output ",inout.varname," on function ",fnname)
        end
        
        table.insert(t,", .C"..id.."_"..inout.varname.."("..actual..")")
      end
    end
  end

  table.insert(t,");\n")
  return t
end


local function checkArgList( fn, args, calltable )
  assert(systolicFunction.isSystolicFunction(fn))

  calltable.type = darkroom.type.null()
  if fn.output~=nil then calltable.type=fn.output.type end

  if fn.inputs==nil or keycount(fn.inputs)==0 then
    assert(args==nil or keycount(args)==0)
    return true
  end

  assert(type(args)=="table")
  assert(systolicAST.isSystolicAST(args)==false)
  assert(type(calltable)=="table")
  local correct = keycount(args)==keycount(fn.inputs)

  map(fn.inputs, function(v,k) 
        assert(systolicAST.isSystolicAST(v))
        correct = correct and (args[k].type==v.type)
        calltable["input_"..k] = args[k]
                 end)
  
  if correct==false then
    print("Error, pure function call incorrect argument type")
    print("Expected:")
    map(fn.inputs,function(v,k) print(k..":"..tostring(v.type)) end)
    print("Actual:")
    map(args,function(v,k) print(k..":"..tostring(v.type)) end)
    assert(false)
  end
end

systolicFunctionFunctions = {}
systolicFunctionMT={__index=systolicFunctionFunctions, 
                    __call = function(self,args) -- args is a table of key,value pairs for the named arguments
                      assert(self.pure)
                      local t = {kind="call",func=self,pure=true,type=self.outputtype}
                      checkArgList( self, args, t)
                      return systolicAST.new(t):setLinenumber(0):setOffset(0):setFilename("")
                    end
}

systolicFunction = {}
function systolicFunction.isSystolicFunction(t)
  return getmetatable(t)==systolicFunctionMT
end

function systolic.pureFunction(name, inputs, output)
  assert(systolicAST.isSystolicAST(output))

  local t = {name=name, inputs=inputs, output=output, assignments={}, ram128assignments={}, asserts={}, pure=true}

  assert(type(inputs)=="table")
  map( inputs, function(v,k) assert(type(k)=="string"); assert(v.kind=="input") end )
  
  return setmetatable(t,systolicFunctionMT)
end

function systolicModuleFunctions:addFunction(name, inputs, output)
  assert(type(inputs)=="table")
  for k,v in pairs(inputs) do assert(type(k)=="string" or type(k)=="number");assert(systolicAST.isSystolicAST(v));assert(v.kind=="input") end

  assert(systolicAST.isSystolicAST(output) or output==nil)

  local t = {name=name, inputs=inputs, output=output, assignments={}, asserts={}}
  assert(self.functions[name]==nil)
  self.functions[name]=t
  return setmetatable(t,systolicFunctionMT)
end

function systolicFunctionFunctions:getDelay()
  local maxd = 0
  for _,v in pairs(self.assignments) do
    print("GETDELAY",v.dst.varname,v.retiming[v.expr])
    if v.dst.kind=="output" and v.retiming[v.expr]>maxd then 

      maxd = v.retiming[v.expr] 
    end
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

function systolicFunctionFunctions:addAssign( dst, expr )
  assert(systolicInstance.isSystolicInstance(dst) or (dst.kind=="output" or dst.kind=="reg"))
  assert(systolicAST.isSystolicAST(expr))
  
  table.insert(self.assignments,{dst=dst,expr=expr,retiming=retime(expr)})
end

function systolicFunctionFunctions:writeRam128( ram128, addr, expr )
  assert(systolicAST.isSystolicAST(ram128))
  assert(ram128.kind=="ram128")
  assert(systolicAST.isSystolicAST(addr))
  assert(systolicAST.isSystolicAST(expr))

  table.insert(self.ram128assignments,{dst=ram128,expr=expr,addr=addr,retiming=retime(expr)})
end

local function codegen(ast, callsiteId)
  assert(systolicAST.isSystolicAST(ast))
  assert(type(callsiteId)=="number" or callsiteId==nil)

  local resDeclarations = {}
  local resClockedLogic = {}

  local finalOut = ast:visitEach(
    function(n, inputs)
      local finalOut = {}
      print("NK",n.kind)

      for c=1,n.type:channels() do
        local res

        if n.kind=="binop" then
          table.insert(resDeclarations, declareReg( n.type:baseType(), n:cname(c) ))

          if n.op=="<" or n.op==">" or n.op=="<=" or n.op==">=" then
            if n.type:baseType():isBool() and n.lhs.type:baseType():isInt() and n.rhs.type:baseType():isInt() then
              table.insert(resClockedLogic, n:name().."_c"..c.." <= ($signed("..inputs.lhs[c]..")"..n.op.."$signed("..inputs.rhs[c].."));\n")
            elseif n.type:baseType():isBool() and n.lhs.type:baseType():isUint() and n.rhs.type:baseType():isUint() then
              table.insert(resClockedLogic, n:name().."_c"..c.." <= (("..inputs.lhs[c]..")"..n.op.."("..inputs.rhs[c].."));\n")
            else
              print( n.type:baseType():isBool() , n.lhs.type:baseType():isInt() , n.rhs.type:baseType():isInt(),n.type:baseType():isBool() , n.lhs.type:baseType():isUint() , n.rhs.type:baseType():isUint())
              assert(false)
            end
          elseif n.type:isBool() then
            local op = binopToVerilogBoolean[n.op]
            if type(op)~="string" then print("OP_BOOLEAN",n.op); assert(false) end
            table.insert(resClockedLogic, n:name().."_c"..c.." <= "..inputs.lhs[c]..op..inputs.rhs[c]..";\n")
          else
            local op = binopToVerilog[n.op]
            if type(op)~="string" then print("OP",n.op); assert(false) end
            local lhs = inputs.lhs[c]
            if n.lhs.type:baseType():isInt() then lhs = "$signed("..lhs..")" end
            local rhs = inputs.rhs[c]
            if n.rhs.type:baseType():isInt() then rhs = "$signed("..rhs..")" end
            table.insert(resClockedLogic, n:name().."_c"..c.." <= "..lhs..op..rhs..";\n")
          end

          res = n:name().."_c"..c
        elseif n.kind=="unary" then
          if n.op=="abs" then
            if n.type:baseType():isInt() then
              table.insert(resDeclarations, declareReg( n.type:baseType(), n:cname(c) ))
              table.insert(resClockedLogic, n:cname(c).." <= ("..inputs.expr[c].."["..(n.type:baseType():sizeof()*8-1).."])?(-"..inputs.expr[c].."):("..inputs.expr[c].."); //abs\n")
              res = n:cname(c)
            else
              return inputs.expr[c] -- must be unsigned
            end
          elseif n.op=="-" then
            assert(n.type:baseType():isInt())
            table.insert(resDeclarations, declareReg(n.type:baseType(),n:cname(c)))
            table.insert(resClockedLogic, n:cname(c).." <= -"..inputs.expr[c].."; // unary sub\n")
            res = n:cname(c)
          else
            print(n.op)
            assert(false)
          end
        elseif n.kind=="select" or n.kind=="vectorSelect" then
          table.insert(resDeclarations,declareReg( n.type:baseType(), n:cname(c), "", " // "..n.kind.." result" ))
          local condC = 1
          if n.kind=="vectorSelect" then condC=c end

          table.insert(resClockedLogic, n:cname(c).." <= ("..inputs.cond[condC]..")?("..inputs.a[c].."):("..inputs.b[c].."); // "..n.kind.."\n")
          res = n:cname(c)
        elseif n.kind=="cast" then
          local expr
          local cmt = " // cast "..tostring(n.expr.type).." to "..tostring(n.type)
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
          
          table.insert(resDeclarations, declareWire(n.type:baseType(), n:cname(c), "",cmt))
          table.insert(resDeclarations, "assign "..n:cname(c).." = "..expr..";"..cmt.."\n")
          res = n:cname(c)
        elseif n.kind=="value" then
          local v
          if type(n.value)=="table" then 
            v = valueToVerilog(n.value[c], n.type:baseType()) 
          else
            v = valueToVerilog(n.value, n.type:baseType())
          end
          table.insert(resDeclarations,declareWire(n.type:baseType(), n:cname(c), v, " //value" ))
          res = n:cname(c)
        elseif n.kind=="array" then
          print("SST",inputs["expr"..c],c)
          for k,v in pairs(inputs["expr"..c]) do print("ST",k,v) end
          res = inputs["expr"..c][1]
        elseif n.kind=="index" then
          local flatIdx = 0

          local dim = 1
          local scalefactor = 1
          while n["index"..dim] do
            assert(n["index"..dim].constLow == n["index"..dim].constHigh)
            flatIdx = flatIdx + (n["index"..dim].constLow)*scalefactor
            dim = dim + 1
          end

          res = inputs["expr"][flatIdx+1]

--[=[          else
            for k,v in pairs(n) do print(k,v) end
            local range = n.index:eval(1,kernel)
            -- synth a reduction tree to select the element we want
            local rname, rmod = fpga.modules.reduce(compilerState, "valid", range:bbArea(), n.type)
            result = concat(rmod, result)
            table.insert(resDeclarations, declareWire(n.type, n:cname(c),"", "// index result"))
            local str = rname.." indexreduce_"..n:cname(c).."(.CLK(CLK),.out("..n:cname(c)..")"
            for i=range:min(1), range:max(1) do
              local idx = i-range:min(1)
              str = str..",.partial_"..idx.."("..inputs["expr"][i+1]..")"
              str = str..",.partial_valid_"..idx.."("..n.index:codegenHW(kernel).." == "..valueToVerilogLL(i,true,32)..")"
            end
            table.insert(resDeclarations,str..");\n")
            res = n:cname(c)
          end]=]
        elseif n.kind=="input" then
          if callsiteId~=nil then
            res = "C"..callsiteId.."_"..n.varname
          else
            res = n.varname
          end
        elseif n.kind=="readreg" then
          if callsiteId~=nil then
            res = "C"..callsiteId.."_"..n.reg.name
          else
            res = n.reg.name
          end
        elseif n.kind=="call" then
          for k,v in pairs(n) do print("INS",k,v) end
--          print(debug.traceback())
          if n.pure then
            systolic.addDefinition( n.func )
            table.insert(resDeclarations,n.func.name.." "..n:name().."(.CLK(CLK));\n")
            res = n:name().."_"..c
          else
            systolic.addDefinition( n.inst )
            res = n:name().."_"..c
          end
        else
          print(n.kind)
          assert(false)
        end

        print("KND",n.kind)
        assert(type(res)=="string")
        assert(res:match("[%w%[%]]")) -- should only be alphanumeric
        finalOut[c] = res
      end

      assert(keycount(finalOut)>0)
      return finalOut
    end)

  if ast.type:isArray() then
    finalOut = "{"..table.concat(finalOut,",").."}"
  else
    finalOut = finalOut[1]
  end

  return resDeclarations, resClockedLogic, finalOut
end

function systolicFunctionFunctions:addAssert(expr)
  table.insert(self.asserts,expr)
end

function systolicFunctionFunctions:getDefinition(callsites)
  local t = {}

  if self.pure then
    table.insert(t,"module "..self.name.."(input CLK")
    map(self.inputs, function(v) table.insert(t, ", "..declarePort(v.type, v.varname, true)) end)
    if type(self.outputs)=="table" then map(self.outputs, function(v) table.insert(t, ", "..declarePort(v.type, v.varname, false)) end) end
    table.insert(t,");\n")
    callsites={"LOL"}
  end

  for id,_ in pairs(callsites) do
    local callsite = "C"..id.."_"
    if self.pure then callsite="" end
    table.insert(t,"  // function: "..self.name.." callsite "..id.." delay "..self:getDelay().."\n")
      
    local clocked = {}
    for k,v in pairs(self.assignments) do
      local decl, clk, res = codegen(v.expr, id)
      t = concat(t,decl)
      clocked = concat(clocked,clk)
      if v.dst.kind=="reg" then table.insert(t, declareWire(v.dst.type,callsite..v.dst.name)) end
      table.insert(t, "assign "..callsite..v.dst.name.." = "..res..";\n")
    end
    
    table.insert(t,"always @ (posedge CLK) begin\n")
    t = concat(t, clocked)
    table.insert(t,"end\n")
  end

  if self.pure then
    table.insert(t,"endmodule\n\n")
  else
    table.insert(t," // merge: "..self.name.."\n")
    for id,v in pairs(callsites) do
      for k,v in pairs(self.assignments) do
        if id==1 then
          if v.dst.kind=="reg" then
            table.insert(t, "always @ (posedge CLK) begin "..v.dst.varname.." <= C"..id.."_"..v.dst.varname.."; end\n")
          elseif v.dst.kind=="output" then
            --          table.insert(t, "assign "..v.dst.varname.." = C"..id.."_"..v.dst.varname..";\n")
          else
            print("VDK",v.dst.kind)
            assert(false)
            --        table.insert(t, "assign "..v.dst.varname.." = C"..id.."_"..v.dst.varname..";\n")
          end
        end
      end
    end
  end

  return t
end

local function typecheck(ast)
  assert(darkroom.ast.isAST(ast))

  if ast.kind=="readram128" then
    local t = ast:shallowcopy()
    t.type=darkroom.type.bool()
    return systolicAST.new(t):copyMetadataFrom(ast)
  elseif ast.kind=="readreg" then
    local t = ast:shallowcopy()
    t.type = t.reg.type
    return systolicAST.new(t):copyMetadataFrom(ast)
  else
    return darkroom.typedAST.typecheckAST(ast, filter(ast,function(n) return systolicAST.isSystolicAST(n) end), systolicAST.new )
  end
end

local function convert(ast)
  if getmetatable(ast)==systolicASTMT then
    return ast
  elseif type(ast)=="number" then
    local t = darkroom.ast.new({kind="value", value=ast}):setLinenumber(0):setOffset(0):setFilename("")
    return typecheck(t)
  else
    print(type(ast))
    assert(false)
  end
end

local function binop(lhs, rhs, op)
  lhs = convert(lhs)
  rhs = convert(rhs)
  return typecheck(darkroom.ast.new({kind="binop",op=op,lhs=lhs,rhs=rhs,type=lhs.type}):copyMetadataFrom(lhs))
end

systolic._callsites = {}
systolicInstance = {}
function systolicInstance.new(tab)
  return setmetatable(tab,systolicInstanceMT)
end

function systolicInstance.isSystolicInstance(tab)
  return getmetatable(tab)==systolicInstanceMT
end

function systolicInstanceFunctions:getDefinition()
  local t = {}

  table.insert(t,"module "..self.name.."(input CLK")
  
  for fnname,v in pairs(self.callsites) do

    for id,vv in pairs(v) do
      local fn = self.module.functions[fnname]
      assert(fn~=nil)
      
      table.insert(t,", input C"..id.."_"..fnname.."_valid")
      
      for _,input in pairs(fn.inputs) do
        table.insert(t,", input ["..(input.type:sizeof()*8-1)..":0] C"..id.."_"..input.varname)
      end
      
      if type(fn.outputs)=="table" then
        for _,output in pairs(fn.outputs) do
          table.insert(t,", output ["..(output.type:sizeof()*8-1)..":0] C"..id.."_"..output.varname)
        end
      end
    end
  end
  table.insert(t,");\n")

  table.insert(t," // state\n")
  
  for k,v in pairs(self.module.regs) do
    assert(v.kind=="reg")
    table.insert(t, declareReg(v.type, v.name, v.initial))
  end

  for k,v in pairs(self.module.functions) do
    t=concat(t,v:getDefinition(self.callsites[v.name]))
  end

  table.insert(t,"endmodule\n\n")

  return t
end

function systolicInstanceFunctions:read(addr)
  if self.kind=="reg" then
    assert(addr==nil)
    return typecheck(darkroom.ast.new({kind="readreg",reg=self}):setLinenumber(0):setOffset(0):setFilename(""))
  elseif self.kind=="ram128" then
    addr = convert(addr)
    return typecheck(darkroom.ast.new({kind="readram128",addr=addr,ram128=self}):setLinenumber(0):setOffset(0):setFilename(""))
  else
    assert(false)
  end
end

systolicInstanceMT={
__index = function(tab,key)
  local v = rawget(tab, key)
  if v ~= nil then return v end
  v = systolicInstanceFunctions[key]

  if v==nil and rawget(tab,"kind")=="module" then
    -- try to find key in function tab
    local fn = rawget(tab,"module").functions[key]
    if fn~=nil then
      return function(self, args)
        assert(type(args)=="table" or args==nil)
        
        tab.callsites[fn.name] = tab.callsites[fn.name] or {}
        table.insert(tab.callsites[fn.name],1)
        
        local t = {kind="call",inst=self,func=fn,functionname=fn.name,callsiteid = #tab.callsites[fn.name]}
        
        checkArgList( fn, args, t )
        
        return systolicAST.new(t):setLinenumber(0):setOffset(0):setFilename("")
             end
    end
    
  end
  return v
end}

systolicASTFunctions = {}
setmetatable(systolicASTFunctions,{__index=IRFunctions})
systolicASTMT={__index = systolicASTFunctions,
__add=function(l,r) return binop(l,r,"+") end, 
__eq=function(l,r) return binop(l,r,"==") end, 
__sub=function(l,r) return binop(l,r,"-") end}

-- ops
function systolic.index( expr, idx )
  assert(systolicAST.isSystolicAST(expr))
  assert(type(idx)=="table")
  local t = {kind="index", expr=expr}
  map(idx, function(n,i) t["index"..i] = convert(n);  end)
  return typecheck(darkroom.ast.new(t):copyMetadataFrom(expr))
end

function systolic.cast( expr, ty )
  assert(systolicAST.isSystolicAST(expr))
  assert(darkroom.type.isType(ty))
  return typecheck(darkroom.ast.new({kind="cast",expr=expr,type=ty}):copyMetadataFrom(expr))
end

function systolic.array( expr )
  assert(type(expr)=="table")
  assert(#expr>0)
  local t = {kind="array"}
  for k,v in ipairs(expr) do
    assert(systolicAST.isSystolicAST(v))
    t["expr"..k] = v
  end
  return typecheck(darkroom.ast.new(t):copyMetadataFrom(expr[1]))
end

function systolic.select(cond,a,b)
  cond, a, b = convert(cond), convert(a), convert(b)
  return typecheck(darkroom.ast.new({kind="select",cond=cond,a=a,b=b}):copyMetadataFrom(cond))
end

function systolic.le(lhs, rhs) return binop(lhs,rhs,"<=") end
function systolic.lt(lhs, rhs) return binop(lhs,rhs,"<") end
function systolic.ge(lhs, rhs) return binop(lhs,rhs,">=") end
function systolic.gt(lhs, rhs) return binop(lhs,rhs,">") end
function systolic.__or(lhs, rhs) return binop(lhs,rhs,"or") end
function systolic.__and(lhs, rhs) return binop(lhs,rhs,"and") end

function systolicASTFunctions:cname(c)
  return self:name().."_c"..c
end


function systolicASTFunctions:internalDelay()
  if self.kind=="binop" or self.kind=="select" or self.kind=="readram128" then
    return 1
  elseif self.kind=="input" or self.kind=="reg" or self.kind=="value" or self.kind=="index" or self.kind=="cast" or self.kind=="array" or self.kind=="readreg" then
    return 0
  else
    print(self.kind)
    assert(false)
  end
end

function systolicASTFunctions:toVerilog(options)
  local decl, clocked, finalOut = codegen(self)

  local t = decl
  if #clocked>0 then
    table.insert(t,"always @ (posedge CLK) begin\n")
    t = concat(t,clocked)
    table.insert(t,"end\n")
  end
  
  return t, finalOut
end

systolicAST = {}
function systolicAST.isSystolicAST(ast)
  return getmetatable(ast)==systolicASTMT
end

function systolicAST.new(tab)
  assert(type(tab)=="table")
  if tab.scaleN1==nil then tab.scaleN1=0 end
  if tab.scaleD1==nil then tab.scaleD1=0 end
  if tab.scaleN2==nil then tab.scaleN2=0 end
  if tab.scaleD2==nil then tab.scaleD2=0 end
  assert(darkroom.type.isType(tab.type))
  darkroom.IR.new(tab)
  return setmetatable(tab,systolicASTMT)
end

function systolic.reg( name, ty, initial )
  assert(type(name)=="string")
  ty = darkroom.type.fromTerraType(ty, 0, 0, "")
  return systolicInstance.new({kind="reg",name=name,initial=initial,type=ty})
end

function systolic.ram128( name )
  assert(type(name)=="string")
  return systolicInstance.new({kind="ram128",name=name,type=darkroom.type.null()})
end

function systolic.output(name, ty)
  assert(type(name)=="string")
  ty = darkroom.type.fromTerraType(ty, 0, 0, "")
  return setmetatable({kind="output",varname=name,type=ty},systolicASTMT)
end

function systolic.input(name, ty)
  assert(type(name)=="string")
  ty = darkroom.type.fromTerraType(ty, 0, 0, "")
  return systolicAST.new({kind="input",varname=name,type=ty,scaleN1=0,scaleD1=0,scaleN2=0,scaleD2=0}):setLinenumber(0):setOffset(0):setFilename("")
end

return systolic