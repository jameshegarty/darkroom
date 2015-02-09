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
  map(inputs, function(v) assert(v.kind=="input") end)
  map(outputs, function(v) assert(v.kind=="output") end)
  assert(statemachine.isBlock(mainblock))
  return setmetatable({name=name, mainblock = mainblock, inputs=inputs, outputs=outputs, instances={}}, stateMachineModuleMT)
end

function statemachine.block(name, predicate)
  assert(type(name)=="string")
  assert(systolicAST.isSystolicAST(predicate))
  local t = {name=name,  statements = {}, branches={}, predicate = predicate}
  return setmetatable(t,stateMachineBlockMT)
end

function stateMachineModuleFunctions:add( inst )
  assert(systolicInstance.isSystolicInstance(inst))
  table.insert(self.instances, inst)
end

function stateMachineModuleFunctions:toVerilog()
  local t = {}

  table.insert(t,"module "..self.name.."(input CLK")
  for _,input in pairs(self.inputs) do
    table.insert(t,", input ["..(input.type:sizeof()*8-1)..":0] "..input.name)
  end
  
  for _,output in pairs(self.outputs) do
    table.insert(t,", output ["..(output.type:sizeof()*8-1)..":0] "..output.name)
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
      local launchstat, launchvar = v.launch:toVerilog({validbit=systolic.__and(v.cond, self.mainblock.predicate)})
      t = concat(t,launchstat)
    elseif v.kind=="assign" then
      table.insert(t," // assign\n")
      local predstat, predvar = self.mainblock.predicate:toVerilog({pipeline=false})
      t = concat(t,predstat)
      local exprstat, exprvar = v.expr:toVerilog({pipeline=false})
      t = concat(t,exprstat)
--      table.insert( t, "if("..predvar..") begin "..v.dst.name.." <= "..exprvar.."; end\n" )
      table.insert( t, v.dst.name.." <= "..exprvar..";\n" )
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
    assert( darkroom.type.checkImplicitCast( expr.type, dst.type, expr ) )
    expr = systolic.cast( expr, dst.type )
  end

  table.insert(self.statements,{kind="assign",dst=dst,expr=expr})
end

return statemachine