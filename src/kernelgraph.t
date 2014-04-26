kernelGraphFunctions={}
setmetatable(kernelGraphFunctions,{__index=IRFunctions})
kernelGraphMT={__index=kernelGraphFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}

-- this is supposed to be an IR that makes it easy to manipulate scheduling decisions.
-- this is common to Conv engine and CPU/GPU
-- your scheduling heuristics will operate on this IR

orion.kernelGraph = {}


-- call init on all the inputs
function kernelGraphFunctions:initInputImages(loopid)
  local res = self:map("inputImage",function(arrayItem)  arrayItem:init(loopid) end)
end

function kernelGraphFunctions:declareInputImages(loopid)

  local res = self:foldl("inputImage",function(tab,arrayItem) 
                           assert(type(tab)=="table")
                           assert(orion.imageWrapper.isImageWrapper(arrayItem))
                           table.insert(tab,arrayItem:declare(loopid)) 
                           return tab
                                                   end,{})
  assert(#res==self:arraySize("inputImage"..loopid.."_"))
  return res
end

function kernelGraphFunctions:setInputImagePositions(loopid,x,y,core,stripId,stripCount, stripList, stripSymbolCache,retime)
  assert(type(loopid)=="number")
  assert(type(stripList)=="table")
  assert(type(stripSymbolCache)=="table")
  assert(type(retime)=="number")

  local res = self:foldl("inputImage",function(tab,arrayItem) 
                           appendTable(tab,arrayItem:setPosition(loopid,x,y,core,stripId,stripCount, stripList, stripSymbolCache,retime)) 
                      return tab
                                                   end,{})
  return res
end

function kernelGraphFunctions:inputImagesNext(loopid,v)
  assert(type(loopid)=="number")
  assert(type(v)=="number")
  return self:foldl("inputImage",function(tab,arrayItem) 
                      table.insert(tab,arrayItem:next(loopid,v)) 
                      return tab
                                              end,{})
end


function kernelGraphFunctions:init()
  setmetatable(self,nil)
  orion.kernelGraph.new(self)
end

function kernelGraphFunctions:irType()
  return "kernelGraph"
end

function kernelGraphFunctions:makeNewName()
  return "kernelGraphNode"
end

function kernelGraphFunctions:isARoot(root)
  if self:parentCount(root) == 0 then
    return true
  end

  return false
end


function kernelGraphFunctions:root()
  if self.kind=="single" then
    return self.kernel
  else
    assert(false)
  end
end


-- scheduleNodes is a list of nodes in the internalIR that will
-- becomed kernelGraph nodes (basically, they are the nodes that are
-- interesting to manipulate when we are making scheduling decisions)
function orion.kernelGraph.internalIRToKernelGraph(internalIR, scheduleNodes, cropRegion, base1, base2)
  assert(orion.internalIR.isInternalIR(internalIR))
  assert(type(scheduleNodes)=="table")

  if orion.verbose or orion.printstage then 
    print("To Scheduled IR", keycount(scheduleNodes), internalIR:S("*"):count()) 
  end

  -- root has to be in the list
  assert(scheduleNodes[internalIR])

  -- translation from the new kernel we generate to the original version
  local origKernel = {}

  -- We do this with a traverse instead of a process b/c we're converting
  -- from one type of AST to another
  local kernelGraph = internalIR:S(
    function(node) 
      return scheduleNodes[node]~=nil
    end)
  :traverse(
    function(node,args)
      local newnode = {kind="single"}
      if node.kind=="outputs" then newnode.kind="outputs" end

      -- go through and replace all the references to input nodes with loads
      local childCount = 1
      
      local query = node:F(
        function(n) -- figure out if this node was replaced by a kernelGraph node
          local res = false
          for _,child in pairs(args) do
            res = res or (n==origKernel[child.kernel])
          end
          return res end)
      assert(query:count()==#args)
      
      local kernel = query:process(
        function(n,origNode)
          local child
          
          for _,pchild in pairs(args) do
            if origNode==origKernel[pchild.kernel] or
              (pchild.kind=="toSOA" and origNode==origKernel[pchild]) then 
              child = pchild 
            end
          end
          assert(child~=nil)
          
          newnode["child"..childCount] = child
          childCount = childCount+1
          
          local res = orion.internalIR.new({kind="load",from=child,type=n.type}):copyMetadataFrom(n)
          return res
        end)
      
      newnode.kernel = kernel
      
      
      local nr = node:neededRegion(internalIR,cropRegion,base1,base2,scheduleNodes)
      newnode.neededRegion = nr:copyMetadataFrom(node)
      
      origKernel[kernel] = node
      return orion.kernelGraph.new(newnode):copyMetadataFrom(node)
    end)

  -- our algorithm returns a list of nodes
  -- the root should be a list of 1
  assert(#kernelGraph==1)
  kernelGraph = kernelGraph[1]

  kernelGraph:check()

  if orion.verbose or orion.printstage then
    print("Scheduled IR Done --------------------------------------")
  end

  if orion.verbose then
    kernelGraph:printpretty()
    kernelGraph:printprettyJSON("debug/scheduledir.json")
  end

  return kernelGraph
end

function orion.kernelGraph.isKernelGraph(ast) return getmetatable(ast)==kernelGraphMT end

function orion.kernelGraph.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,kernelGraphMT)
end

-- collect all the output images, and apply fn to them as a map, and return result
function orion.kernelGraph.outputImageMap(kernelGraph, fn)
  return {}
end
