local statemachine={}

stateMachineModuleFunctions = {}
stateMachineModuleMT={__index=stateMachineModuleFunctions}

stateMachineFunctionFunctions = {}
stateMachineFunctionMT={__index=stateMachineFunctionFunctions}


function statemachine.module(name)
  assert(type(name)=="string")
  return setmetatable({name=name, functions={}}, stateMachineModuleMT)
end

function stateMachineModuleFunctions:addFunction(name, inputs, outputs)
  map(inputs, function(v) assert(v.kind=="input") end)
  map(outputs, function(v) assert(v.kind=="output") end)

  local t = {name=name, inputs=inputs, outputs=outputs, statements = {}}
  assert(self.functions[name]==nil)
  self.functions[name]=t
  return setmetatable(t,stateMachineFunctionMT)
end

return statemachine