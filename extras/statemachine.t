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

  return setmetatable({name=name, blocks={}, inputs=inputs, outputs=outputs}, stateMachineModuleMT)
end

function stateMachineModuleFunctions:addBlock(name)
  local t = {name=name,  statements = {}}
  table.insert(self.blocks, t)
  return setmetatable(t,stateMachineBlockMT)
end

function stateMachineBlockFunctions:addIfLaunch(cond,launch)
  assert(systolicAST.isSystolicAST(cond))
  assert(systolicAST.isSystolicAST(launch))
  table.insert(self.statements,{kind="iflaunch",cond=cond,launch=launch})
end

return statemachine