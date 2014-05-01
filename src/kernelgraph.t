kernelGraphFunctions={}
setmetatable(kernelGraphFunctions,{__index=IRFunctions})
kernelGraphMT={__index=kernelGraphFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}

-- We take the typedAST, and break it up into 'kernels'. We
-- break into kernels iif a linebuffer will be inserted.
-- format of kernel graph is: {children={}, kernel=typedAST or number}
-- for the root, kernel will be nil, and children will be the list of inputs
-- for inputs, kernel will be a number (the position of the input in the argument list), and children will be nil


orion.kernelGraph = {}

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

function kernelGraphFunctions:maxUse(input)
  return self.kernel:stencil(input):max(2)
end

function orion.kernelGraph.typedASTToKernelGraph(typedAST, options)
  assert(orion.typedAST.isTypedAST(typedAST))
  assert(type(options)=="table")

  if options.verbose or options.printstage then 
    print("To Kernel Graph", typedAST:S("*"):count()) 
  end

  -- translation from the new kernel we generate to the original version
  local origKernel = {}

  -- We do this with a traverse instead of a process b/c we're converting
  -- from one type of AST to another
  local kernelGraph = typedAST:S(
    function(node) 
      return node.kind=="crop" or node:parentCount(typedAST)==0
    end)
  :traverse(
    function(node,args)
      local newnode = {kind="single"}

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
          
          local res = orion.typedAST.new({kind="load",from=child,type=n.type}):copyMetadataFrom(n)
          return res
        end)
      
      newnode.kernel = kernel
      
      origKernel[kernel] = node
      return orion.kernelGraph.new(newnode):copyMetadataFrom(node)
    end)

  -- our algorithm returns a list of nodes
  -- the root should be a list of 1
  assert(#kernelGraph==1)
  kernelGraph = kernelGraph[1]

  if options.verbose then
    print("Kernel Graph Done --------------------------------------")
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
