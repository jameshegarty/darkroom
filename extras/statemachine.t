local statemachine={}

stateMachineModuleFunctions = {}
stateMachineModuleMT={__index=stateMachineModuleFunctions}

stateMachineBlockFunctions = {}
stateMachineBlockMT={__index=stateMachineBlockFunctions}

function statemachine.isStateMachine(n)
  return getmetatable(n) == stateMachineModuleMT
end

function statemachine.isBlock(n)
  return getmetatable(n) == stateMachineBlockMT
end

function statemachine.module( name, inputs, outputs, mainblock )
  assert(type(name)=="string")
  assert(statemachine.isBlock(mainblock))
  local t = {name=name, mainblock = mainblock, inputs=inputs, outputs=outputs, instances={}, instanceMap={}, usedInstanceNames = {}}

  map(inputs, function(v) assert(v.kind=="input"); assert(t.usedInstanceNames[v]==nil); t.instanceMap[v]=1; t.usedInstanceNames[v]=1 end)
  map(outputs, function(v) assert(v.kind=="output"); assert(t.usedInstanceNames[v]==nil); t.instanceMap[v]=1; t.usedInstanceNames[v]=1 end)

  return setmetatable(t, stateMachineModuleMT)
end

function statemachine.block(name, predicate)
  assert(type(name)=="string")
  assert(systolicAST.isSystolicAST(predicate))
  local t = {name=name,  statements = {}, branches={}, predicate = predicate}
  return setmetatable(t,stateMachineBlockMT)
end

function stateMachineModuleFunctions:add( inst )
  assert(systolicInstance.isSystolicInstance(inst))

  if self.usedInstanceNames[inst.name]~=nil then
    print("Error, inst name",inst.name,"used twice!")
    assert(false)
  end

  table.insert(self.instances, inst)
  self.instanceMap[inst] = 1
  self.usedInstanceNames[inst.name] = 1
end

function stateMachineModuleFunctions:toVerilog()
  local t = {}

  table.insert(t,"module "..self.name.."(input CLK")
  for _,input in pairs(self.inputs) do
    table.insert(t,", "..declarePort( input.type, input.name, true))
  end
  
  for _,output in pairs(self.outputs) do
    table.insert(t,", "..declarePort( output.type, output.name, false))
  end
  table.insert(t,");\n")

  table.insert(t,"  // instances\n")
  for k,v in pairs(self.instances) do
    table.insert(t, v:toVerilog() )
  end

  for k,v in ipairs(self.mainblock.statements) do
    if v.kind=="iflaunch" then
      table.insert(t," // if launch\n")
      table.insert(t," // if "..typedASTPrintPrettys(v.cond).." then\n")
      table.insert(t," // "..typedASTPrintPrettys(v.launch).."\n")
      local launchstat, launchvar = v.launch:toVerilog({valid=v.cond},{self})
      t = concat(t,launchstat)
    elseif v.kind=="assign" then
      table.insert(t," // assign\n")
      local exprstat, exprvar,_,totalDelay = v.expr:toVerilog({valid=true},{self})

      if totalDelay~=0 then
        print("Assignment has nonzero delay ",totalDelay,"dst",v.dst.name)
        assert(false)
      end

      t = concat(t,exprstat)
      
      if v.dst.kind=="output" then
        table.insert( t, "assign "..v.dst.name.." = "..exprvar..";\n" )
      else
        assert(false)
        table.insert( t, v.dst.name.." <= "..exprvar..";\n" )
      end
    else
      assert(false)
    end
  end

  table.insert(t,"endmodule\n\n")

  local defn = systolic.getDefinitions()
  t = concat({defn},t)

  return t
end

-- get the implicit valid bit
function stateMachineModuleFunctions:valid()
  return systolicAST.new({kind="validbit",type=darkroom.type.bool()}):setLinenumber(0):setOffset(0):setFilename("")
end

function stateMachineBlockFunctions:addIfLaunch( cond, launch )
  assert(systolicAST.isSystolicAST(cond))
  assert(systolicAST.isSystolicAST(launch))

  if cond.type:isBool()==false then
    assert( darkroom.type.checkImplicitCast( cond.type, darkroom.type.bool(), cond ) )
    cond = systolic.cast(cond, darkroom.type.bool())
  end

  table.insert(self.statements,{kind="iflaunch",cond=cond,launch=launch})
end

function stateMachineBlockFunctions:addAssign( dst, expr )
  assert(systolicInstance.isSystolicInstance(dst))
  assert(dst.kind=="output")
  assert(systolicAST.isSystolicAST(expr))

  if dst.type~=expr.type then
    if darkroom.type.checkImplicitCast( expr.type, dst.type, expr )==false then
      print("Error, could not cast ",expr.type,"to",dst.type)
      assert(false)
    end

    expr = systolic.cast( expr, dst.type )
  end

  table.insert(self.statements,{kind="assign",dst=dst,expr=expr})
end

return statemachine