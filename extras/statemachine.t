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

function statemachine.module(name,inputs, outputs)
  assert(type(name)=="string")
  map(inputs, function(v) assert(v.kind=="input") end)
  map(outputs, function(v) assert(v.kind=="output") end)

  return setmetatable({name=name, blocks={}, inputs=inputs, outputs=outputs, instances={}}, stateMachineModuleMT)
end

function stateMachineModuleFunctions:addBlock(name)
  local t = {name=name,  statements = {}}
  table.insert(self.blocks, t)
  return setmetatable(t,stateMachineBlockMT)
end

function stateMachineModuleFunctions:add( inst )
  assert(systolicAST.isSystolicAST(inst))
  assert(inst.kind=="instance")
  table.insert(self.instances, inst)
end

function stateMachineModuleFunctions:toVerilog()
  local t = {}

  table.insert(t,"module "..self.name.."(input CLK")
  for _,input in pairs(self.inputs) do
    table.insert(t,", input ["..(input.type:sizeof()*8-1)..":0] "..input.varname)
  end
  
  for _,output in pairs(self.outputs) do
    table.insert(t,", output ["..(output.type:sizeof()*8-1)..":0] "..output.varname)
  end
  table.insert(t,");\n")

  table.insert(t,"endmodule\n\n")

  return t
end

-- get the implicit valid bit
function stateMachineModuleFunctions:valid()
  return systolicAST.new({kind="validbit",type=darkroom.type.bool()}):setLinenumber(0):setOffset(0):setFilename("")
end

function stateMachineBlockFunctions:addIfLaunch( cond, launch )
  assert(systolicAST.isSystolicAST(cond))
  assert(systolicAST.isSystolicAST(launch))
  table.insert(self.statements,{kind="iflaunch",cond=cond,launch=launch})
end

function stateMachineBlockFunctions:addAssign( dst, expr )
  assert(systolicAST.isSystolicAST(dst))
  assert(dst.kind=="output")
  assert(systolicAST.isSystolicAST(expr))
  table.insert(self.statements,{kind="assign",dst=dst,expr=expr})
end

return statemachine