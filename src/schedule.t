
-- graph is just required to have some function 
-- :minUse(dim,input) and :maxUse(dim,input) declared on it, that return
-- the smallest and largest use offset that this node accesses.
--
-- It returns a map from node -> shift
function schedule(graph, largestScaleY, HWWidth)
  assert(darkroom.kernelGraph.isKernelGraph(graph))
  assert(type(largestScaleY)=="number")

  local shifts = {}
  graph:S("*"):traverse(
    function(node) 
      if node.kernel~=nil then 
        shifts[node] = 0
        for k,v in node:inputs() do
          local s
          if type(HWWidth)=="number" then
            s = node:maxUse(2,v)*HWWidth + node:maxUse(1,v) + shifts[v]

            if s<0 and node:maxUse(1,v)>0 then
              -- HACK: our current linebuffer design assumes that the max
              -- x pixel is zero. We add extra delay here to make sure
              -- that's the case, even though the delay is unnecessary!
              -- eg if max x = 10, max y = -7
              s = node:maxUse(1,v)
            end
          else
            s = node:maxUse(2,v)*looprate(v.kernel.scaleN2,v.kernel.scaleD2,largestScaleY) + shifts[v]
          end

          if s > shifts[node] then shifts[node] = s end
        end
      end 
    end)

  return shifts
end

function synthRel(rel,t)
  if type(t)=="number" and type(rel)=="number" then
    return darkroom.ast.new({kind="value",value=rel+t})
  elseif type(rel)=="number" and darkroom.ast.isAST(t) then
    local v = darkroom.ast.new({kind="value",value=rel}):copyMetadataFrom(t)
    return darkroom.ast.new({kind="binop", lhs=t, rhs=v, op="+"}):copyMetadataFrom(t)
  elseif darkroom.ast.isAST(rel) and type(t)=="number" then
    local v = darkroom.ast.new({kind="value",value=t}):copyMetadataFrom(rel)
    return darkroom.ast.new({kind="binop", lhs=rel, rhs=v, op="+"}):copyMetadataFrom(rel)
  elseif darkroom.ast.isAST(rel) and darkroom.ast.isAST(t) then
    return darkroom.ast.new({kind="binop", lhs=rel, rhs=t, op="+"}):copyMetadataFrom(rel)
  else
    print(type(rel),type(t))
    assert(false)
  end
end

function shift(graph, shifts, largestScaleY, HWWidth)
  assert(darkroom.kernelGraph.isKernelGraph(graph))
  assert(type(largestScaleY)=="number")

  local oldToNewRemap = {}
  local newToOldRemap = {}
  local newShifts = {} -- other compile stages use the shifts later

  local newGraph = graph:S("*"):process(
    function(kernelGraphNode, orig)
      if kernelGraphNode.kernel~=nil then
        local newKernelGraphNode = kernelGraphNode:shallowcopy()
        
        -- eliminate transforms
        -- this is wildly inefficient in general. But its only slow in corner cases b/c it is uncommon to have transforms within kernel graph nodes
        newKernelGraphNode.kernel = kernelGraphNode.kernel:S("transformBaked"):process(
          function(n)
            return n.expr:S(function(n) return n.kind=="load" or n.kind=="position" end):process(
              function(nn)
                if nn.kind=="load" then
                  local r = nn:shallowcopy()                  

                  r.relX = synthRel(r.relX, n.translate1):optimize()
                  r.relY = synthRel(r.relY, n.translate2):optimize()
                  r.scaleN1 = n.scaleN1
                  r.scaleN2 = n.scaleN2
                  r.scaleD1 = n.scaleD1
                  r.scaleD2 = n.scaleD2

                  if type(nn.from)=="table" then r.from = oldToNewRemap[nn.from]; assert(r.from~=nil) end
                  return darkroom.typedAST.new(r):copyMetadataFrom(nn)
                elseif nn.kind=="position" then

                  local res = {kind="binop", lhs=nn, type = nn.type, op="+", scaleN1=nn.scaleN1, scaleD1=nn.scaleD1, scaldN2=nn.scaleN2, scaleD2=nn.scaleD2}

                  if nn.coord=="x" then
                    res.rhs = darkroom.typedAST._toTypedAST(n.translate1)
                  elseif nn.coord=="y" then
                    res.rhs = darkroom.typedAST._toTypedAST(n.translate2)
                  else
                    assert(false)
                  end

                  return darkroom.typedAST.new(res):copyMetadataFrom(nn)
                else
                  assert(false)
                end
              end)
          end)

        -- apply shift
        newKernelGraphNode.kernel = newKernelGraphNode.kernel:S(function(n) return n.kind=="load" or n.kind=="crop" or n.kind=="position" end):process(
          function(nn)
            if nn.kind=="load" then

              if type(nn.from)=="table" then
                local r = nn:shallowcopy()
                
                if type(HWWidth)=="number" then
                  local s = shifts[newToOldRemap[nn.from]]-shifts[orig]
                  assert(s<=0) -- I don't think we shift things into the future?
                  local sy = -math.floor(-s/HWWidth)
                  local sx = s-sy*HWWidth
                  r.relY = synthRel(r.relY, sy):optimize()
                  r.relX = synthRel(r.relX, sx):optimize()
                else
                  local inputKernel = newToOldRemap[nn.from]
                  local sy = math.floor( (shifts[inputKernel]-shifts[orig])/looprate(inputKernel.kernel.scaleN2,inputKernel.kernel.scaleD2,largestScaleY))
                  r.relY = synthRel(r.relY, sy):optimize()
                end

                if type(nn.from)=="table" then r.from = oldToNewRemap[nn.from]; assert(r.from~=nil) end

                return darkroom.typedAST.new(r):copyMetadataFrom(nn)
              end
            elseif nn.kind=="crop" then
              local r = nn:shallowcopy()

              if type(HWWidth)=="number" then
                local sy = math.floor(shifts[orig]/HWWidth)
                r.shiftY = r.shiftY + sy
                r.shiftX = r.shiftX + (shifts[orig]-sy*HWWidth)
              else
                r.shiftY = r.shiftY + math.floor(shifts[orig]/looprate(orig.kernel.scaleN2,orig.kernel.scaleD2,largestScaleY))
              end

              return darkroom.typedAST.new(r):copyMetadataFrom(nn)
            elseif nn.kind=="position" then
              if nn.coord=="y" and shifts[orig]~=0 then
                local v = darkroom.typedAST.new({kind="value", value=-math.floor(shifts[orig]/looprate(orig.kernel.scaleN2,orig.kernel.scaleD2,largestScaleY)), type=nn.type}):copyMetadataFrom(nn)
                return darkroom.typedAST.new({kind="binop", lhs=nn, rhs = v, type = nn.type, op="+"}):copyMetadataFrom(nn)
              end
            else
              print(nn.kind)
              assert(false)
            end
          end)

        local res = darkroom.kernelGraph.new(newKernelGraphNode):copyMetadataFrom(kernelGraphNode)
        oldToNewRemap[orig] = res
        oldToNewRemap[res] = res -- remember, we might touch nodes multiple times
        newToOldRemap[res] = orig
        newToOldRemap[orig] = orig
        newShifts[res] = shifts[orig]
        return res
      end
    end)

  return newGraph, newShifts
end