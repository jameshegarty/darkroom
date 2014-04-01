cstdlib = terralib.includec("stdlib.h")
cmath = terralib.includec("math.h")


cropIRFunctions={}
setmetatable(cropIRFunctions,{__index=IRFunctions})
cropIRMT={__index=cropIRFunctions, 
  __newindex = function(table, key, value)
                    orion.error("Attempt to modify ast node")
                  end
}

orion.cropIR = {}
orion.cropIR.optimize = true

terra orion.cropIR.min(a:int, b:int) if a<b then return a else return b end end
terra orion.cropIR.max(a:int, b:int) if a>b then return a else return b end end

-- we cache width and height b/c we need these for array accesses
-- left,bottom : inclusive
-- right,top : exclusive
-- if width==0 and height==0, then the region is empty, and left,right,top,bottom are bogus!
--     but we do guarantee that left==right and top==bottom
-- if inf, then all values are bogus!
struct Crop { left:int; 
              right:int; 
              bottom:int; 
              top:int; 
              width:int, 
              height:int; 
              inf:bool}

-- cropIR node -> terra quote to generate var
-- notice that this cache is shared accross compiles... it turns out
-- the cropIR is idependent of the code we're compiling
orion.cropIR._cache = setmetatable({}, {__mode="k"}) -- stripped (bool) -> cropIR node -> crop table
orion.cropIR._stripRuntimeCache = setmetatable({}, {__mode="k"})
orion.cropIR._stripRuntimeExpr = setmetatable({}, {__mode="k"})

terra Crop:assertXMod(N:int)
  if self.inf then return end

  if self.left % N ~= 0 or self.right % N ~= 0 then
    cstdio.printf("Crop mod error")
    cstdlib.exit(1)
  end
end

-- if infinite, returns 0
terra Crop:area() : int
  if self.inf then return 0 end
  return self.width*self.height
end

terra Crop:print()
  if self.inf then
    cstdio.printf("inf\n")
  else
    cstdio.printf("l=%d r=%d b=%d t=%d w=%d h=%d\n",self.left,self.right,self.bottom,self.top,self.width,self.height)
  end
end


terra orion.cropIR.upToNearest(roundto : int,x: int)
  --if orion.debug and x < 0 then
  --  cstdio.printf("x < 0\n")
  --  cstdlib.exit(1)
  --end

  if x % roundto == 0 or roundto==0 then return x end
  
  var ox : int
  if x < 0 then
    ox = x - (x%roundto)
  else
    ox = x + (roundto-x%roundto)
  end

  if orion.debug and (ox <= x or (ox % roundto)~=0) then
    cstdio.printf("upround error\n")
    cstdlib.exit(1)
  end

  return ox
end


terra orion.cropIR.downToNearest(roundto:int,x:int)

  --if orion.debug and x < 0 then
  --  cstdio.printf("x < 0\n")
  --  cstdlib.exit(1)
  --end

  if x % roundto == 0 or roundto == 0 then return x end

  var ox :int
  if x < 0 then
    ox = x - (roundto+x%roundto)
  else
    ox = x - x%roundto
  end

  if orion.debug and (ox >= x or (ox % roundto)~=0) then
    cstdio.printf("upround error\n")
    cstdlib.exit(1)
  end


  return ox
end

terra Crop:growToNearestX(v : int)

  if self.width==0 or self.height==0 then
    var c : Crop
    c.left  = self.left
    c.right  = self.right
    c.top  = self.top
    c.bottom  = self.bottom
    c.width  = self.width
    c.height  = self.height
    c.inf  = self.inf
    return c
  else
    var c : Crop
    c.right = orion.cropIR.upToNearest(v,self.right)
    c.left = orion.cropIR.downToNearest(v,self.left)
    c.top = self.top
    c.bottom = self.bottom
    c.height = self.height
    c.width = c.right-c.left
    c.inf = self.inf
    
    return c
  end
end

terra Crop:shrinkToNearestX(v : int)

  var c : Crop
  c.right = orion.cropIR.downToNearest(v,self.right)
  c.left = orion.cropIR.upToNearest(v,self.left)
  c.top = self.top
  c.bottom = self.bottom
  c.height = self.height
  c.width = c.right-c.left
  c.inf = self.inf

  return c
end


terra Crop:unionWith( other : Crop)

  if self.width==0 then
    var c : Crop
    c.left  = other.left
    c.right  = other.right
    c.top  = other.top
    c.bottom  = other.bottom
    c.width  = other.width
    c.height  = other.height
    c.inf  = other.inf
    return c
  elseif other.width==0 then
    var c : Crop
    c.left  = self.left
    c.right  = self.right
    c.top  = self.top
    c.bottom  = self.bottom
    c.width  = self.width
    c.height  = self.height
    c.inf  = self.inf
    return c
  else
    var c : Crop
    c.left = orion.cropIR.min(self.left,other.left)
    c.right = orion.cropIR.max(self.right,other.right)
    c.bottom = orion.cropIR.min(self.bottom,other.bottom)
    c.top = orion.cropIR.max(self.top,other.top)
    c.height = c.top-c.bottom
    c.width = c.right-c.left
    c.inf = self.inf or other.inf
    return c
  end
end

terra Crop:intersection( other : Crop)

  var c : Crop
    
  if self.inf and other.inf then
    c.inf = true
  elseif self.inf then
    c.left = other.left
    c.right = other.right
    c.bottom = other.bottom
    c.top = other.top
    c.height = other.height
    c.width = other.width
    c.inf = other.inf
  elseif other.inf then
    c.left = self.left
    c.right = self.right
    c.bottom = self.bottom
    c.top = self.top
    c.height = self.height
    c.width = self.width
    c.inf = self.inf
  elseif self.width==0  or other.width==0 then
    c.left = 0
    c.right = 0
    c.top = 0
    c.bottom = 0
    c.width = 0
    c.height = 0
    c.inf = false
  else
    var l = orion.cropIR.max(self.left,other.left)
    var r = orion.cropIR.min(self.right,other.right)
    var t = orion.cropIR.min(self.top,other.top)
    var b = orion.cropIR.max(self.bottom,other.bottom)

    if r-l <=0 or t-b<=0 then
      l=0
      r=0
      t=0
      b=0
    end

    c.left = l
    c.right = r
    c.bottom = b
    c.top = t
    c.height = t-b
    c.width = r-l
    c.inf = false
  end
  
  return c
end

terra Crop:strip(stripId:int, stripCount:int)
  orionAssert(stripCount>0, "stripCount <=0")
  orionAssert(stripId>=0, "stripId <0")

  -- more fancy partitioning schemes that allowed for a non-even number of strips
  -- per core yielded suboptimal performancs
  orionAssert( stripCount % orion.tune.cores == 0, "There must be an even number of strips per core!")

  if self.inf then
    assert(false)
  elseif self.width==0 or self.height==0 then
    -- it's empty
    return Crop {left = self.left,
                 right = self.right,
                 top = self.top,
                 bottom = self.bottom, 
                 width = self.width,
                 height = self.height,
                 inf=self.inf
                }
  else
    orionAssert(stripId < stripCount, "stripId >= stripCount")

    var stripWidth : int = cmath.ceil(float(self.width)/float(stripCount))
    
    var l = self.left+stripId*stripWidth
    var r = orion.cropIR.min(self.left+(stripId+1)*stripWidth, self.right)
 
    return Crop {left = l,
                 right = r,
                 bottom = self.bottom,
                 top = self.top,
                 width = r-l,
                 height = self.height,
                 inf = false
                }
  end
end

terra Crop:eq( other : Crop)

  orionAssert(self.left==other.left,"Crop:eq left")
  orionAssert(self.right==other.right,"Crop:eq right")
  orionAssert(self.bottom==other.bottom,"Crop:eq bottom")
  orionAssert(self.top==other.top,"Crop:eq top")
  orionAssert(self.width==other.width,"Crop:eq width")
  orionAssert(self.height==other.height,"Crop:eq height")
  orionAssert(self.inf==other.inf,"Crop:eq inf")

  var c : Crop 
  c.left = self.left
  c.right = self.right
  c.top = self.top
  c.bottom = self.bottom
  c.width = self.width
  c.height = self.height
  c.inf = self.inf

  return c
end

terra cropWrap(a:Crop)
  return a
end

terra Crop:grow(left:int, right:int, bottom:int, top:int)
  if self.width==0 then
    -- don't grow empty regions
    var c : Crop
    c.left = self.left
    c.right = self.right
    c.top = self.top
    c.bottom = self.bottom
    c.width = self.width
    c.height = self.height
    c.inf = self.inf
    return c
  else
    var c : Crop
    c.left = self.left+left
    c.right = self.right+right
    c.top = self.top+top
    c.bottom = self.bottom+bottom
    c.width = self.width-left+right
    c.height = self.height-bottom+top
    c.inf = self.inf
    return c
  end
end

terra Crop:outputDefault()
  --return self
--  var c : Crop = @self
--  return self

  var c : Crop
  c.left = self.left
  c.right = self.right
  c.top = self.top
  c.bottom = self.bottom
  c.width = self.width
  c.height = self.height
  c.inf = self.inf
  return c
end

terra Crop:outputCentered(base1 : int, base2:int)
  var c : Crop
  c.left = base1
  c.right = base1 + self.width
  c.top = base2 + self.height
  c.bottom = base2
  c.width = self.width
  c.height = self.height
  c.inf = self.inf
  return c
end

terra orion.cropIR.getBoundImageRuntime(id:int)

  var img = orion._boundImagesRuntime:get(id)

  orionAssert(img.active, "Image is not yet bound!")

  var c : Crop
  c.left = 0
  c.right = img.image.width
  c.bottom = 0
  c.top = img.image.height
  c.width = img.image.width
  c.height = img.image.height
  c.inf = false

  return c

end

function cropIRFunctions:extraDebug()
  local all = self:calculate(0,0)
  local res = {}

  for k,v in pairs(all) do
    res["debug_"..k] = v
  end

  for c=0,orion.tune.cores-1 do
    local cc = self:calculate(c,orion.tune.cores)

    for k,v in pairs(cc) do
      res["debug_"..c.."_"..k] = v
    end

  end


  return res
end

function cropIRFunctions:irType()
  return "cropIR"
end

function cropIRFunctions:getLeft()
  assert(self:calculate(0,0).width>0)
  return self:calculate(0,0).left
end


function cropIRFunctions:getBottom()
  assert(self:calculate(0,0).width>0)
  return self:calculate(0,0).bottom
end

function cropIRFunctions:getTop()
  assert(self:calculate(0,0).width>0)
  return self:calculate(0,0).top
end

function cropIRFunctions:getRight()
  assert(self:calculate(0,0).width>0)
  return self:calculate(0,0).right
end

-- returns the maximum width of a strip
function cropIRFunctions:getMaxWidth(stripCount)
  assert(type(stripCount)=="number")

  -- the way we do strips, the 0th strip will always be the max size
  return self:getWidth(0, stripCount)
end

function cropIRFunctions:getWidth(stripId, stripCount)
  if stripId==nil then stripId=0 end
  if stripCount==nil then stripCount=0 end
  return self:calculate( stripId, stripCount ).width
end


function cropIRFunctions:getHeight(stripId, stripCount)
  if stripId==nil then stripId=0 end
  if stripCount==nil then stripCount=0 end
  return self:calculate(stripId,stripCount).height
end

function cropIRFunctions:getArea(stripId, stripCount)
  if stripId==nil then stripId=0 end
  if stripCount==nil then stripCount=0 end

  if self:calculate(stripId,stripCount).inf then return 0 end
  return self:getWidth(stripId,stripCount)*self:getHeight(stripId,stripCount)
end

-- this is the area including recompute regions
-- double counted (which results from stripping).
-- if no strips, this is the same as :getArea()
function cropIRFunctions:getRecomputeArea(stripCount)
  assert(type(stripCount)=="number")
  if self:calculate(0,0).inf then return 0 end

  if self:S("strip"):count()==0 then
return self:getArea()
  end

  local totalArea = 0

  for i=0, stripCount-1 do
    totalArea = totalArea + self:getArea(i,stripCount)
  end

  return totalArea
end


function cropIRFunctions:get(stripId, stripCount)
  if stripId==nil then stripId=0 end
  if stripCount==nil then stripCount=0 end

  local c = self:calculate(stripId, stripCount)
  return `Crop {left = c.left,
                right = c.right,
                bottom = c.bottom,
                top = c.top,
                width = c.width,
                height = c.height,
                inf = c.inf}
end

function cropIRFunctions:calculate(stripId, stripCount)
  assert(type(stripId)=="number")
  assert(type(stripCount)=="number")
  assert(stripId < stripCount or stripCount==0)

  if orion.cropIR._cache[stripCount]==nil or 
    orion.cropIR._cache[stripCount][stripId]==nil or 
    orion.cropIR._cache[stripCount][stripId][self]==nil then

    local result

    if self.kind=="infinite" then
      result = {left=0,
                right=0,
                bottom=0,
                top=0,
                width = 0,
                height = 0,
                inf = true}

    elseif self.kind=="special" then
      assert(type(orion._boundImages[self.id-1].width)=="number")
      assert(type(orion._boundImages[self.id-1].height)=="number")
      result = {left = 0,
                right = orion._boundImages[self.id-1].width,
                bottom = 0,
                top = orion._boundImages[self.id-1].height,
                width = orion._boundImages[self.id-1].width,
                height = orion._boundImages[self.id-1].height,
                inf = false,
                empty = false}
    elseif self.kind=="grow" then
      result = Crop.methods.grow(cropWrap(self.expr:calculate(stripId, stripCount)),
                         self.left,self.right,self.bottom,self.top)
    elseif self.kind=="growToNearestX" then
      result = Crop.methods.growToNearestX(cropWrap(self.expr:calculate(stripId, stripCount)),
                                 self.v)
    elseif self.kind=="strip" or self.kind=="stripOptional" then
      assert(self.kind~="stripOptional")
      if stripCount>0 then

        -- notice how strip operators compose (or rather don't)
        -- the stripping is only applied to the first operator
        -- seen! all strip operators to the first input are
        -- calculated as the full image!
        local tmp = self.expr:calculate(0,0)

        result = Crop.methods.strip(cropWrap(tmp),
                                   stripId, stripCount)
      else
        result = self.expr:calculate(stripId, stripCount)
      end

    elseif self.kind=="output" then
      --local exprsymb = self.expr:symb(globalContext)

      if self.regionMode=="default" then
        result = Crop.methods.outputDefault(cropWrap(self.expr:calculate(stripId, stripCount)))
      elseif self.regionMode=="centered" then
        result = Crop.methods.outputCentered(cropWrap(self.expr:calculate(stripId, stripCount),self.base1,self.base2))
      else
        assert(false)
      end
    elseif self.kind=="intersection" or
      self.kind=="intersectionCropBaked" or
      self.kind=="union" or
      self.kind=="equals" then

      for i=2,self:arraySize("expr") do

        local expr1 = self.expr1:calculate(stripId, stripCount)
        local expr2 = self.expr2:calculate(stripId, stripCount)

        if i>2 then
          expr1 = result
          expr2 = self["expr"..i]:calculate(stripId, stripCount)
        end

        if self.kind=="intersection" or self.kind=="intersectionCropBaked" then
          result = Crop.methods.intersection(cropWrap(expr1),cropWrap(expr2))
        elseif self.kind=="union" then
          result = Crop.methods.unionWith(cropWrap(expr1),cropWrap(expr2))
        elseif self.kind=="equals" then
          result = Crop.methods.eq(cropWrap(expr1),cropWrap(expr2))
        else
          assert(false)
        end
      end
    elseif self.kind=="explicit" then
      assert(self.right > self.left)
      assert(self.top > self.bottom)

      result = {left=self.left,
                right=self.right,
                bottom=self.bottom,
                top=self.top,
                width = (self.right-self.left),
                height = (self.top-self.bottom),
                inf = false}
    elseif self.kind=="empty" then
      result = {left = 0,
                right = 0,
                bottom = 0,
                top = 0,
                width = 0,
                height = 0,
                inf = false}
    else
      print(self.kind)
      assert(false)
    end

    if type(result)=="cdata" then
      result = {left = result.left,
                right =result.right,
                top=result.top,
                bottom=result.bottom,
                width=result.width,
                height=result.height,
                inf = result.inf}
    end

    assert(type(result)=="table")

    if orion.cropIR._cache[stripCount]==nil then
      orion.cropIR._cache[stripCount] = setmetatable({},{__mode="k"})
    end

    if orion.cropIR._cache[stripCount][stripId]==nil then
      orion.cropIR._cache[stripCount][stripId] = setmetatable({},{__mode="k"})
    end

    if result.width<0 then
      print(self.kind)
      assert(false)
    end

    if result.height<0 then
      print(self.kind)
      assert(false)
    end

    if result.width>0 and (result.right<=result.left or result.top<=result.bottom) then
      print(self.kind)
      print(result.width,result.left,result.right)
      assert(false)
    end

    orion.cropIR._cache[stripCount][stripId][self] = result
  end
  
  return orion.cropIR._cache[stripCount][stripId][self]

end

function cropIRFunctions:printprettys()

  local assignments = ""
  local res = self:visitEach(
    function(node, argList)
      local str
      
      if node.kind=="infinite" then
        str = "infinite"
      elseif node.kind=="special" then
        str="special"..node.id
      elseif node.kind=="explicit" then
        str="crop("..node.left..","..node.right..","..node.bottom..","..node.top..")"
      elseif node.kind=="equals" then
        str = "equals("
        node:map("expr",function(n,i) str = str .. argList["expr"..i] .."," end)
        str = str ..")"
      elseif node.kind=="union" then
        str = "union("
        node:map("expr",function(n,i) str = str .. argList["expr"..i] .."," end)
        str = str ..")"
      elseif node.kind=="intersectionCropBaked" then
        str = "intersectionCB("
        node:map("expr",function(n,i) str = str .. argList["expr"..i] .."," end)
        str = str ..")"
      elseif node.kind=="intersection" then
        str = "intersection("
        node:map("expr",function(n,i) str = str .. argList["expr"..i] .."," end)
        str = str ..")"
      elseif node.kind=="grow" then
        str = "grow(".. argList["expr"] ..","..node.left..","..node.right..","..node.bottom..","..node.top..")"
      elseif node.kind=="growToNearestX" then
        str = "growToNearestX(".. argList["expr"] ..","..node.v..")"
      elseif node.kind=="output" then
        -- output holds an expr, but output can actually end up being any value so don't bother printing it
        str = "output(".. argList["expr"] ..")"
        --    return str.."output"
      elseif node.kind=="strip" then
        str = "strip(".. argList["expr"] ..")"
      elseif node.kind=="stripOptional" then
        str = "stripOptional(".. node.nodeId ..","..argList["expr"] ..")"
      elseif node.kind=="empty" then
        str = "empty"
      else
        print(node.kind)
        assert(false)
      end

      if node:parentCount(self)>1 then
        assignments = assignments .. node:name() .. " = "..str.."\n"
        return node:name()
      end

      return str
    end)

--  return assignments..res.."\n["..self:getWidth().."x"..self:getHeight().."]"..self:getWidth(0,orion.tune.cores).." "..self:getMaxWidth(orion.tune.cores).." area:"..self:getArea().." rcarea(stripcount "..orion.tune.cores.."):"..self:getRecomputeArea(orion.tune.cores)
  return assignments..res
end

function cropIRFunctions:expectedKeycount()
  if self.kind=="infinite" then
    return 1
  elseif self.kind=="special" then
    return 2
  elseif self.kind=="grow" then
    return 2+4
  elseif self.kind=="equals" then
    local len = self:arraySize("expr")
    assert(len>=2)
    return 1+len
  elseif self.kind=="union" then
    local len = self:arraySize("expr")
    assert(len>=2)
    return 1+len
  elseif self.kind=="intersection" then
    local len = self:arraySize("expr")
    assert(len>=2)
    return 1+len
  elseif self.kind=="intersectionCropBaked" then
    return 4 -- expr1 expr2, node
  elseif self.kind=="growToNearestX" then
    return 3
  elseif self.kind=="output" then
    return 5 -- expr, regionMode, base1, base2
  elseif self.kind=="explicit" then
    return 5
  elseif self.kind=="empty" then
    return 1
  elseif self.kind=="strip" then
    return 3 -- expr, node
  else
    print("errornode",self.kind)
    assert(false)
  end

end

function cropIRFunctions:checkfn()

  if self.kind=="infinite" then
  elseif self.kind=="empty" then
  elseif self.kind=="special" then
    assert(type(self.id)=="number")
  elseif self.kind=="grow" then
    assert(orion.cropIR.isCropIR(self.expr))
    assert(type(self.left)=="number")
    assert(type(self.right)=="number")
    assert(type(self.top)=="number")
    assert(type(self.bottom)=="number")
  elseif self.kind=="equals" then

    self:map("expr",
             function(n)
               assert(orion.cropIR.isCropIR(n))
             end)

  elseif self.kind=="union" then
    self:map("expr",
             function(n)
               assert(orion.cropIR.isCropIR(n))
             end)
  elseif self.kind=="intersection" or self.kind=="intersectionCropBaked" then
    self:map("expr",
             function(n)
               assert(orion.cropIR.isCropIR(n))
             end)
  elseif self.kind=="growToNearestX" then
    assert(orion.cropIR.isCropIR(self.expr))
    assert(type(self.v)=="number")
  elseif self.kind=="output" then
    assert(orion.cropIR.isCropIR(self.expr))

    assert(type(self.regionMode)=="string")
    assert(type(self.base1)=="number")
    assert(type(self.base2)=="number")
  elseif self.kind=="explicit" then
    assert(type(self.left)=="number")
    assert(type(self.right)=="number")
    assert(type(self.bottom)=="number")
    assert(type(self.top)=="number")
  elseif self.kind=="strip" then
    assert(orion.cropIR.isCropIR(self.expr))
  else
    print("errornode",self.kind)
    assert(false)
  end

end

function cropIRFunctions:printpretty()
  print(self:printprettys())
end

-- :equals is already used by the IR base class
function cropIRFunctions:equalsWith(other)
  -- neigher of these nodes are equals
  return orion.cropIR.new({kind="equals",expr1=self,expr2=other}):copyMetadataFrom(self)
end

function cropIRFunctions:meet(other,mode)
  assert(orion.cropIR.isCropIR(other))
  assert(orion.type.isCropMode(mode))

  -- don't meet with an infinite no matter what mode we're in
  if other.kind=="infinite" then
    return self
  elseif self.kind=="infinite" then
    return other
  end

  if mode==orion.cropSame then
    if self==other then
      return self
    else
      return self:equalsWith(other)
    end
  elseif mode==orion.cropGrow then
    
    return self:unionWith(other)
  elseif mode==orion.cropShrink then
    return self:intersection(other)
  else
    print(mode.name)
    self:printpretty()
    other:printpretty()
    assert(false)
  end

  
end

-- intersect this crop region with a crop that is all pixels in the passed region
-- S is a crop object symbol
function cropIRFunctions:intersectRuntime(S)
  assert(terralib.issymbol(S))
  return `[self:get()]:intersection(S)
end

function cropIRFunctions:stripRuntime(stripId, stripCount, stripList, stripSymbolCache)
  assert(terralib.issymbol(stripId))
  assert(terralib.issymbol(stripCount) or type(stripCount)=="number")
  assert(type(stripList)=="table")
  assert(type(stripSymbolCache)=="table")

  -- make sure we're not accidentally mixing different stripCounts and stripIds
  assert(type(stripSymbolCache[stripCount][stripId])=="table")

  -- note that these keys may be quotes or symbols!
  if stripSymbolCache[stripCount][stripId][self]==nil then

    assert(type(stripCount)=="number")

    local tmp = {}
    local sCache = symbol(Crop[stripCount])
    table.insert(tmp, quote var [sCache] end)
    
    for i=0,stripCount-1 do
      table.insert(tmp,quote sCache[i]=[self:get(i,stripCount)] end)
    end

    local result = quote tmp in sCache[stripId] end

    local sy = symbol(Crop)
    stripSymbolCache[stripCount][stripId][self] = sy
    table.insert(stripList, quote var [sy] = result end)
  end

  return stripSymbolCache[stripCount][stripId][self]
end

-- 
function cropIRFunctions:growToNearestX(v)
  return orion.cropIR.new({kind="growToNearestX",expr=self,v=v}):copyMetadataFrom(self)
end

-- 'union' is a keyword in terra
function cropIRFunctions:unionWith(other)
  return orion.cropIR.new({kind="union",expr1=self,expr2=other}):copyMetadataFrom(self)
end

function cropIRFunctions:strip(node)
  return orion.cropIR.new({kind="strip",node=node,expr=self}):copyMetadataFrom(self)
end

function cropIRFunctions:stripOptional(lbSchedule, nodeId)
  assert(terralib.issymbol(lbSchedule))
  assert(type(nodeId)=="number")
  return orion.cropIR.new({kind="stripOptional",lbSchedule=lbSchedule,nodeId=nodeId,expr=self}):copyMetadataFrom(self)
end

function cropIRFunctions:intersection(other)

  return orion.cropIR.new({kind="intersection",expr1=self,expr2=other}):copyMetadataFrom(self)
end

-- I think that the only difference between this an a regular
-- intersection is that this allows us to attach a node
-- to it so that we can blow it away later depending on
-- the schedule choice
function cropIRFunctions:intersectionCropBaked(other, node)

  return orion.cropIR.new({kind="intersectionCropBaked",node=node,expr1=self,expr2=other}):copyMetadataFrom(self)
end

-- here, translateX means take a crop that was x=[a,b] to
-- x=[a+translateX,b+translateX]
function cropIRFunctions:translate(translateX,translateY)

  assert(type(translateX)=="number")
  assert(type(translateY)=="number")

  if translateX==0 and translateY==0 then
--    print("0 trans")
    return self
  end

  return orion.cropIR.new({kind="grow",expr=self,
                           left=translateX,right=translateX,top=translateY,bottom=translateY}):copyMetadataFrom(self)
end


function cropIRFunctions:grow(l,r,t,b)

  assert(type(l)=="number")
  assert(type(r)=="number")
  assert(type(t)=="number")
  assert(type(b)=="number")

  return orion.cropIR.new({kind="grow",expr=self,
                           left=l,right=r,top=t,bottom=b}):copyMetadataFrom(self)
end

function orion.cropIR.isCropIR(ast) return getmetatable(ast)==cropIRMT end

function orion.cropIR.new(tab)
  assert(type(tab)=="table")
  orion.IR.new(tab)
  return setmetatable(tab,cropIRMT)
end

orion.cropIR._special = {}
function orion.cropIR.special(id)
  if orion.cropIR._special[id]==nil then
    if orion._boundImages[id+1].width~=nil and orion._boundImages[id+1].height~=nil then
      orion.cropIR._special[id] = orion.cropIR.explicit(0,0,orion._boundImages[id+1].width,orion._boundImages[id+1].height)
    else
      orion.cropIR._special[id] = orion.cropIR.new({kind="special", id=id}):setLinenumber(0):setOffset(0):setFilename("null_cropspecial")
    end
  end

  return orion.cropIR._special[id]
end

orion.cropIR._infinite = orion.cropIR.new({kind="infinite"}):setLinenumber(0):setOffset(0):setFilename("null_cropinf")
function orion.cropIR.infinite()
  return orion.cropIR._infinite
end

orion.cropIR._empty = orion.cropIR.new({kind="empty"}):setLinenumber(0):setOffset(0):setFilename("null_cropempty")
function orion.cropIR.empty()
  return orion.cropIR._empty
end

function orion.cropIR.explicit(left, bottom, right, top)
  return orion.cropIR.new({kind="explicit", left = left, right=right, bottom=bottom, top=top}):setLinenumber(0):setOffset(0):setFilename("null_cropexp")
end

-- expr is the size of this node. An output operator
-- modifies this size to fit the output behavior the user
-- requests.
function orion.cropIR.output(expr,regionMode,base1,base2)
  assert(orion.cropIR.isCropIR(expr))
  assert(type(regionMode)=="string")
  assert(type(base1)=="number")
  assert(type(base2)=="number")
  return orion.cropIR.new({kind="output",expr=expr,regionMode=regionMode,base1=base1,base2=base2}):copyMetadataFrom(expr)
end

-- kind of a hack - so that the IR library can shallowcopy and then
-- modify an ast node without having to know its exact type
function cropIRFunctions:init()
  setmetatable(self,nil)
  orion.cropIR.new(self)
end
