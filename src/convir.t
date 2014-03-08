-- this is basically the equivilant of the loopir, but for the convolution engine
-- engine. It's an IR that's extremely close to the code we generate

convIRFunctions={}
setmetatable(convIRFunctions,{__index=IRFunctions})
convIRMT={__index=convIRFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}

orion.convIR = {}

function convIRFunctions:expectedKeycount()
  return 3 -- stencil, kernel, validRegion
end

function convIRFunctions:checkfn()
  assert(orion.flatIR.isFlatIR(self.kernel))
  self.kernel:check()
  assert(Stencil.isStencil(self.stencil))
  assert(orion.cropIR.isCropIR(self.validRegion))
end

-- validRegion is the valid region of this kernel
function orion.convIR.cropWorkaround(validRegion,kernel)
  assert(orion.cropIR.isCropIR(validRegion))
  assert(orion.flatIR.isFlatIR(kernel))

  -- The conv engine only produces images of size and location w=[0,W], h=[0,H]
  -- but our thing can produce images anywhere. Check that the sizes match,
  -- and offset the loads so that the end result is the same

  return kernel:S("loadConv"):process(
    function(n)
      local ow = n.from.conv1.validRegion:getWidth()
      local tw = validRegion:getWidth()
      local oh = n.from.conv1.validRegion:getHeight()
      local th = validRegion:getHeight()

      if ow ~= tw or
        oh ~= th then
        orion.error("only cropSame is supported for conv engine. mismatched width or height "..n.from.conv1:name(),kernel:linenumber(),kernel:offset(),kernel:filename())
      end

      local xoff = validRegion:getLeft() - n.from.conv1.validRegion:getLeft()
      local yoff = validRegion:getBottom() - n.from.conv1.validRegion:getBottom()

      local newnode = n:shallowcopy()
      newnode.x = n.x+xoff
      newnode.y = n.y+yoff
      return orion.flatIR.new(newnode):copyMetadataFrom(n)
    end)

end

function orion.convIR.convert(scheduledIR)
  -- we need to update the load nodes in the kernels to point
  -- to the new scheduledIR tree
  local convRemap = {}

  local scheduledConvIR = scheduledIR:S("single"):process(
    function(n, origNode)

      local kernel = n.kernel

      -- extract the crop
      local validRegion
      if kernel.kind=="cropBaked" then
        validRegion = kernel.crop
        kernel = kernel.expr
      else
        kernel:printpretty()
        orion.error("For conv engine, only cropSame is supported! somehow you have an inf crop",kernel:linenumber(),kernel:offset())
      end

      local convir = {}
      convir.kernel = orion.flatIR.toFlatIR(kernel)
      convir.kernel = convir.kernel:S("loadConv"):process(
        function(n)
          local new = n:shallowcopy()
          assert(convRemap[new.from]~=nil)
          new.from = convRemap[new.from]
          return orion.flatIR.new(new):copyMetadataFrom(n)
        end)
      convir.kernel = orion.convIR.cropWorkaround(validRegion,convir.kernel)

      convir.kernel = convir.kernel:S("position"):process(
        function(pnode)
          local res
          if pnode.coord=="x" then
            res = {kind="binop", lhs=pnode, type = pnode.type, op="+"}
            res.rhs = orion.flatIR.new({kind="value",value=validRegion:getLeft(),type=pnode.type}):copyMetadataFrom(pnode)
          elseif pnode.coord=="y" then
            local tmp = {kind="binop", rhs=pnode, type = pnode.type, op="-"}
            tmp.lhs = orion.flatIR.new({kind="value",value=(validRegion:getHeight()-1),type=pnode.type}):copyMetadataFrom(pnode)
            tmp = orion.flatIR.new(tmp):copyMetadataFrom(pnode)

            res = {kind="binop", lhs=tmp, type = pnode.type, op="+"}
            res.rhs = orion.flatIR.new({kind="value",value=validRegion:getBottom(),type=pnode.type}):copyMetadataFrom(pnode)
          else
            assert(false)
          end

          return orion.flatIR.new(res):copyMetadataFrom(pnode)
        end)

      assert(convir.kernel:S("gatherConcrete"):count()==0)
      convir.kernel = convir.kernel:S("gather"):process(
        function(node)
          -- need to flip y axis.
          -- afaik, in gather nodes the x,y are just passed through, so we don't
          -- need to correct them other than this. When they are converted to
          -- gatherConcrete they are offset to account for the shift from the CSE algorithm

          local res = node:shallowcopy()
          res.y = orion.flatIR.new({kind="unary",op="-",expr=res.y,type=res.y.type}):copyMetadataFrom(res.y)
          return orion.flatIR.new(res):copyMetadataFrom(node)
        end)

      -- we have to calc this by hand b/c flatIR doesn't really
      -- have a stencil method
      convir.stencil = Stencil.new()
      convir.kernel:S("loadConv"):traverse(
        function(n)
          convir.stencil:add(n.x,n.y,0)
        end)

      convir.kernel:S("specialConv"):traverse(
        function(n)
          convir.stencil:add(n.x,n.y,0)
        end)

      -- by john's rules we can't ever have a negative centroid. our
      -- centroid is located at 0,0, so we have to make sure we always include that
      -- in the centroid.
      convir.stencil:add(0,0,0)

      convir.validRegion = validRegion
      convir = orion.convIR.new(convir):setLinenumber(0):setOffset(0):setFilename("null_convir")

      local newnode = {kind="conv", conv1=convir}
      for i=1,n:arraySize("child") do newnode["child"..i] = n["child"..i] end
      local res = orion.scheduledIR.new(newnode):copyMetadataFrom(n)
      convRemap[origNode] = res
      return res
    end)

  scheduledConvIR:check()

  return scheduledConvIR
end

function orion.convIR.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,convIRMT)
end


function orion.convIR.isConvIR(ast) return getmetatable(ast)==convIRMT end
