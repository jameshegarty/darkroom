
-- graph is just required to have some function 
-- :minUse(input) and :maxUse(input) declared on it, that return
-- the smallest and largest use offset that this node accesses.
--
-- It returns a map from node -> shift
function schedule(graph)
  local shifts = {}
  graph:S("*"):traverse(
    function(node)
      shifts[node] = 0
      for _,input in node:inputs() do
        local s = input:maxUse(input) + shifts[input]
        if s>shifts[node] then shifts[node]=s end
      end
    end)

  return shifts
end