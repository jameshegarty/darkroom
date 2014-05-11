
-- graph is just required to have some function 
-- :minUse(input) and :maxUse(input) declared on it, that return
-- the smallest and largest use offset that this node accesses.
--
-- It returns a map from node -> shift
function schedule(graph)
  assert(orion.kernelGraph.isKernelGraph(graph))

  local shifts = {}
  graph:S("*"):traverse(
    function(node) 
      if node.kernel~=nil then 
        shifts[node] = 0
        for k,v in node:inputs() do
          local s = node:maxUse(v) + shifts[v]
          if s > shifts[node] then shifts[node] = s end
        end
      end 
    end)

  print("shifts:")
  for k,v in pairs(shifts) do print(k:name(),v) end
  return shifts
end

function shift(graph, shifts)
  assert(orion.kernelGraph.isKernelGraph(graph))

  local oldToNewRemap = {}
  local newToOldRemap = {}
  return graph:S("*"):process(
    function(kernelGraphNode, orig)
      if kernelGraphNode.kernel~=nil then
        local newKernelGraphNode = kernelGraphNode:shallowcopy()
        
        -- eliminate transforms
        -- this is wildly inefficient in general. But its only slow in corner cases
        newKernelGraphNode.kernel = kernelGraphNode.kernel:S("transformBaked"):process(
          function(n)
            return n.expr:S("load"):process(
              function(nn)
                local r = nn:shallowcopy()
                r.relX = r.relX + n.translate1
                r.relY = r.relY + n.translate2
                if type(nn.from)=="table" then r.from = oldToNewRemap[nn.from]; assert(r.from~=nil) end
                return orion.typedAST.new(r):copyMetadataFrom(nn)
              end)
          end)

        -- apply shift
        newKernelGraphNode.kernel = newKernelGraphNode.kernel:S("load"):process(
          function(nn)
            if type(nn.from)=="table" then
            local r = nn:shallowcopy()
            r.relY = r.relY - shifts[orig] + shifts[newToOldRemap[nn.from]]
            if type(nn.from)=="table" then r.from = oldToNewRemap[nn.from]; assert(r.from~=nil) end
            return orion.typedAST.new(r):copyMetadataFrom(nn)
            end
          end)

        local res = orion.kernelGraph.new(newKernelGraphNode):copyMetadataFrom(kernelGraphNode)
        oldToNewRemap[orig] = res
        oldToNewRemap[res] = res -- remember, we might touch nodes multiple times
        newToOldRemap[res] = orig
        newToOldRemap[orig] = orig
        return res
      end
    end)
end