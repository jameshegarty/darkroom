cstdlib = terralib.includec("stdlib.h")
cstdio = terralib.includec("stdio.h")

function Vector(ty)
  local struct V { data : &ty, allocatedSize : int, count : int };

  terra V:init()
    self.data=nil
    self.allocatedSize=0
    self.count=0
  end
  
  terra V:get(ix : int) : ty
    return self.data[ix]
  end

  terra V:getPtr(ix : int) : &ty
    return &(self.data[ix])
  end
  
  terra V:count() return self.count end
  terra V:push(inp : ty)
    self:expand(self.count+1)
    self.data[self.count] = inp
    self.count = self.count+1
  end

  -- if id > count, we fill in the empty places with the default
  terra V:set(id : int, inp : ty, default : ty)
    var oldsize : int = self.count
    self:expand(id+1)
    for k=oldsize,id do self.data[k]=default end
    self.data[id] = inp
    if id+1 > self.count then
      self.count = id+1
    end
  end

  terra V:pop() : ty
    var out : ty = self.data[self.count-1]
    self.count = self.count-1
    return out
  end

  -- expand data to fit something sized size
  terra V:expand(size : int)
    -- always alloc at least 10 entries

    if self.data==nil then
      if size < 10 then size = 10 end
      self.data = [&ty](cstdlib.malloc(sizeof(ty)*size))
      self.allocatedSize = size
    elseif self.allocatedSize < size then
      var oldsize = self.allocatedSize
      while oldsize < size do oldsize = oldsize*2 end
      size = oldsize

      if orion.verbose then cstdio.printf("Expand vector %d %d\n", self.allocatedSize, size) end

      -- now alloc a new buffer and copy the old data into it
      var newdata = [&ty](cstdlib.malloc(sizeof(ty)*size))

      for i=0,self.count do
        newdata[i] = self.data[i]
      end

      cstdlib.free(self.data)
      self.data = newdata
      self.allocatedSize = size
    end

  end

  return V
end