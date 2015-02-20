
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
            -- this is an impossible situation - our math won't work anymore if this is the case
            assert(node:maxUse(1,v)<HWWidth)
            s = node:maxUse(2,v)*HWWidth*looprate(v.kernel.scaleN2,v.kernel.scaleD2,1) + node:maxUse(1,v)*looprate(v.kernel.scaleN1,v.kernel.scaleD1,1) + shifts[v]

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

        -- fixup round: it's possible that in the process of calculating multiple shifts,
        -- some of them pushed X before 0. Go through and fixup.
        -- does this terminate? probably not
        if type(HWWidth)=="number" then
          for k,v in node:inputs() do
            local s = shifts[node]-shifts[v]
            local sy = -math.floor(s/HWWidth)
            local sx = -(s+sy*HWWidth)
            if node:maxUse(1,v)+sx>0 then
               shifts[node] = shifts[node] + node:maxUse(1,v)+sx
            end
          end

          -- verify
          for k,v in node:inputs() do
            local s = shifts[node]-shifts[v]
            local sy = -math.floor(s/HWWidth)
            local sx = -(s+sy*HWWidth)
            assert(node:maxUse(1,v)+sx <= 0)
          end
        end
      end 
    end)

  return shifts
end

function synthRel(rel,t)
  assert(darkroom.typedAST.isTypedAST(rel))
  assert(darkroom.typedAST.isTypedAST(t))
  assert(rel.type:isInt() or rel.type:isUint())
  assert(t.type:isInt() or t.type:isUint())

  local constLow, constHigh
  if rel.constLow~=nil then constLow = rel.constLow+t.constLow end
  if rel.constHigh~=nil then constHigh = rel.constHigh+t.constHigh end

  return darkroom.typedAST.new({kind="binop", lhs=rel, rhs=t, op="+", type=rel.type, constLow=constLow, constHigh=constHigh}):copyMetadataFrom(rel)
end

function shift(graph, shifts, largestScaleY, HWWidth)
  assert(darkroom.kernelGraph.isKernelGraph(graph))
  assert(type(largestScaleY)=="number")

  local oldToNewRemap = {}
  local newToOldRemap = {}
  local newShifts = {} -- other compile stages use the shifts later

  local newGraph = graph:S("*"):process(
    function(kernelGraphNode, orig)
      if kernelGraphNode.kernel~=nil and kernelGraphNode.isInputImage==nil then
        local newKernelGraphNode = kernelGraphNode:shallowcopy()
        
        -- eliminate transforms
        -- the reason we do this as an n^2 algorithm, is that when you transform a node multiple ways inside a kernel
        -- you really do want to create multiple copies of the children of the transform.
        -- We do check for the case of noop transforms, but otherwise if there is a transform here, we 
        -- probably really do want to make a copy.
        newKernelGraphNode.kernel = kernelGraphNode.kernel:S("transformBaked"):process(
          function(n)
            local dsStride1, usStride1 = calculateStride(n.expr.scaleN1, n.expr.scaleD1, n.scaleN1, n.scaleD1)
            local dsStride2, usStride2 = calculateStride(n.expr.scaleN2, n.expr.scaleD2, n.scaleN2, n.scaleD2)
            if (n.translate1.constLow==n.translate1.constHigh) and (n.translate2.constLow==n.translate2.constHigh) and 
              n.translate1.constLow==0 and n.translate2.constLow==0 
              and dsStride1==1 and usStride1==1 and dsStride2==1 and usStride2==1 then
              -- noop
              -- it's actually important that we detect this case, b/c the loop invariant code motion algorithm
              -- will expect nodes that are translated by 0 to be identical, so if we go and make a copy here it's a problem
              return n.expr
            end

            return n.expr:S(function(n) return n.kind=="load" or n.kind=="position" end):process(
              function(nn)
                if nn.kind=="load" then
                  local r = nn:shallowcopy()                  

                  r.relX = synthRel(r.relX, n.translate1)
                  r.relY = synthRel(r.relY, n.translate2)
                  assert(r.relX.type:isInt() or r.relX.type:isUint())
                  assert(r.relY.type:isInt() or r.relY.type:isUint())
                  if r.maxX~=nil then
                    r.maxX = synthRel(r.maxX, n.translate1)
                    r.minX = synthRel(r.minX, n.translate1)
                    r.maxY = synthRel(r.maxY, n.translate2)
                    r.minY = synthRel(r.minY, n.translate2)
                  end

                  r.scaleN1 = n.scaleN1
                  r.scaleN2 = n.scaleN2
                  r.scaleD1 = n.scaleD1
                  r.scaleD2 = n.scaleD2

                  if type(nn.from)=="table" then r.from = oldToNewRemap[nn.from]; assert(r.from~=nil) end
                  return darkroom.typedAST.new(r):copyMetadataFrom(nn)
                elseif nn.kind=="position" then

                  local res = {kind="binop", lhs=nn, type = nn.type, op="+", scaleN1=nn.scaleN1, scaleD1=nn.scaleD1, scaldN2=nn.scaleN2, scaleD2=nn.scaleD2}

                  if nn.coord=="x" then
                    res.rhs = n.translate1
                  elseif nn.coord=="y" then
                    res.rhs = n.translate2
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

              -- only apply shift to calculated images. Input images are read a different way
              if nn.from.isInputImage==nil then
                local r = nn:shallowcopy()
                
                if type(HWWidth)=="number" then
                  local inputKernel = newToOldRemap[nn.from]
                  local s = shifts[newToOldRemap[nn.from]]-shifts[orig]
                  assert(s<=0) -- I don't think we shift things into the future?
                  local sy = -math.floor(-s/HWWidth)
                  local sx = s-sy*HWWidth

                  sy = sy / looprate(inputKernel.kernel.scaleN2,inputKernel.kernel.scaleD2,1)
                  assert(sy==math.floor(sy)) -- only powers of 2 supported
                  sy = darkroom.typedAST.new({kind="value",value=sy,type=r.relY.type,constLow=sy,constHigh=sy}):copyMetadataFrom(nn)

                  sx = sx / looprate(inputKernel.kernel.scaleN1,inputKernel.kernel.scaleD1,1)
                  assert(sx==math.floor(sx)) -- only powers of 2 supported
                  sx = darkroom.typedAST.new({kind="value",value=sx,type=r.relY.type,constLow=sx,constHigh=sx}):copyMetadataFrom(nn)

                  r.relY = synthRel(r.relY, sy)
                  r.relX = synthRel(r.relX, sx)
                  if r.maxX~=nil then
                    r.maxX = synthRel(r.maxX, sx)
                    r.minX = synthRel(r.minX, sx)
                    r.maxY = synthRel(r.maxY, sy)
                    r.minY = synthRel(r.minY, sy)
                  end

                else
                  local inputKernel = newToOldRemap[nn.from]
                  local sy = math.floor( (shifts[inputKernel]-shifts[orig])/looprate(inputKernel.kernel.scaleN2,inputKernel.kernel.scaleD2,largestScaleY))
                  sy = darkroom.typedAST.new({kind="value",value=sy,type=r.relY.type,constLow=sy,constHigh=sy}):copyMetadataFrom(nn)
                  r.relY = synthRel(r.relY, sy)
                  if r.maxY~=nil then
                    r.maxY = synthRel(r.maxY, sy)
                    r.minY = synthRel(r.minY, sy)
                  end
                end

                if nn.from.isInputImage==nil then r.from = oldToNewRemap[nn.from]; assert(r.from~=nil) end

                return darkroom.typedAST.new(r):copyMetadataFrom(nn)
              end
            elseif nn.kind=="crop" then
              local r = nn:shallowcopy()

              if type(HWWidth)=="number" then
                local sy = math.floor(shifts[orig]/HWWidth)
                local sx = shifts[orig]-sy*HWWidth

                sy = sy / looprate(orig.kernel.scaleN2, orig.kernel.scaleD2,1)
                assert(sy==math.floor(sy)) -- only powers of 2 supported

                sx = sx / looprate(orig.kernel.scaleN1, orig.kernel.scaleD1,1)
                assert(sx==math.floor(sx)) -- only powers of 2 supported

                r.shiftY = r.shiftY + sy
                r.shiftX = r.shiftX + sx
              else
                r.shiftY = r.shiftY + math.floor(shifts[orig]/looprate(orig.kernel.scaleN2,orig.kernel.scaleD2,largestScaleY))
              end

              return darkroom.typedAST.new(r):copyMetadataFrom(nn)
            elseif nn.kind=="position" then
              if type(HWWidth)=="number" then
                local sy = math.floor(shifts[orig]/HWWidth)
                local sx = shifts[orig]-sy*HWWidth

                if nn.coord=="y" and sy~=0 then
                  local v = darkroom.typedAST.new({kind="value", value=-sy, type=nn.type}):copyMetadataFrom(nn)
                  return darkroom.typedAST.new({kind="binop", lhs=nn, rhs = v, type = nn.type, op="+"}):copyMetadataFrom(nn)
                elseif nn.coord=="x" and sx~=0 then
                  local v = darkroom.typedAST.new({kind="value", value=-sx, type=nn.type}):copyMetadataFrom(nn)
                  return darkroom.typedAST.new({kind="binop", lhs=nn, rhs = v, type = nn.type, op="+"}):copyMetadataFrom(nn)
                end
              else
                if nn.coord=="y" and shifts[orig]~=0 then
                  local v = darkroom.typedAST.new({kind="value", value=-shifts[orig], type=nn.type}):copyMetadataFrom(nn)
                  local res = darkroom.typedAST.new({kind="binop", lhs=nn, rhs = v, type = nn.type, op="+"}):copyMetadataFrom(nn)

                  local lr = looprate(orig.kernel.scaleN2,orig.kernel.scaleD2,largestScaleY)
                  if lr~=1 then
                    local lrv = darkroom.typedAST.new({kind="value", value=lr, type=nn.type}):copyMetadataFrom(nn)
                    res = darkroom.typedAST.new({kind="binop", lhs=res, rhs = lrv, type = nn.type, op="floorDivide"}):copyMetadataFrom(nn)
                  end

                  return res
                end
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