Stencil={}
StencilMT = {__index = Stencil}

function Stencil.isStencil(s)
  return getmetatable(s)==StencilMT	 
end

function Stencil.new()
  local ns = {}
  return setmetatable(ns, StencilMT)
end

-- return a 'deep copy' of this stencil
-- note, this is not actually a deep copy, b/c we want to keep the key pointers the same
function Stencil:copy()
  local ns = Stencil.new()

  for k,v in pairs(self) do
    ns:addKey(k)
  end

  return ns
end

-- not in place
-- 'union' is a keyword in terra, so we can't use it
function Stencil:unionWith(b)
  assert(Stencil.isStencil(b))
  local ns = Stencil.new()

  for k,_ in pairs(self) do ns[k] = 1 end
  for k,_ in pairs(b) do ns[k] = 1 end

  return ns
end

-- this basically 'convolves' two stencils
-- not in place
function Stencil:product(b)
  assert(Stencil.isStencil(b))

  local ns = Stencil.new()

  for k,_ in pairs(self) do
    ns = ns:unionWith(b:translate(k[1],k[2],k[3]))
  end

  return ns
end

-- can't use the keyword union in terra files
function Stencil:st_union(b) return self:union(b) end

-- not in place
function Stencil:intersect(b)
  assert(Stencil.isStencil(b))

  local ns = Stencil.new()

  for k,_ in pairs(self) do 
    if b[k] then ns[k] = 1 end
  end

  return ns
end

-- not in place
function Stencil:translate(x,y,z)
  assert(type(x)=="number")
  assert(type(y)=="number")
  assert(type(z)=="number")

  local ns = Stencil.new()

  for k,_ in pairs(self) do
    ns[Stencil.key(k[1]+x,k[2]+y,k[3]+z)] = 1
  end

  return ns
end

-- in place, add (x,y,z) to stencil
function Stencil:add(x,y,z)
  self[Stencil.key(x,y,z)]=1
  return self
end

function Stencil:addKey(k)
  self[k]=1
  return self
end

function Stencil:eq(s)

  if self:area()~=s:area() then return false end

  for k,v in pairs(self) do
    if s[k]==nil then return false end
  end

  -- if areas are the same, and all the keys in self are in s, then they must be the same
  return true
end

Stencil._key={}
function Stencil.key(x,y,z)
  Stencil._key[x]=Stencil._key[x] or {}
  Stencil._key[x][y]=Stencil._key[x][y] or {}
  Stencil._key[x][y][z]=Stencil._key[x][y][z] or {x,y,z}
  return Stencil._key[x][y][z]
end

-- dim is 1,2, or 3
function Stencil:max(dim)
  local m = -100000
  for k,_ in pairs(self) do if k[dim]>m then m=k[dim] end end

  assert(m~=-100000)

  return m
end

function Stencil:min(dim)
  local m = 100000
  for k,_ in pairs(self) do if k[dim]<m then m=k[dim] end end

  assert(m~=100000)

  return m
end

-- the number of pixels the stencil extends beyond 0 in the positive direction
function Stencil:posOverhang(dim)
  local m = -100000
  for k,_ in pairs(self) do if k[dim]>m then m=k[dim] end end

  if m < 0 then return 0 end

  return m
end

-- the number of pixels the stencil extends beyond 0 in the negative direction
function Stencil:negOverhang(dim)
  local m = 100000
  for k,_ in pairs(self) do if k[dim]<m then m=k[dim] end end

  if m>0 then return 0 end
  m = -m

  return m
end

function Stencil:size()
  return {self:max(1)-self:min(1)+1,self:max(2)-self:min(2)+1,self:max(3)-self:min(3)+1}
end

-- exactly the number of pixels in the stencil
function Stencil:area()
  local tot = 0
  for k,v in pairs(self) do tot=tot+1 end
  return tot
end

-- this returns the area of the bounding box that covers the stencil
function Stencil:bbArea()
  if self:area()==0 then return 0 end
  local size = self:size()
  return size[1]*size[2]*size[3]
end

-- if there's one key in the stencil, this will return it
function Stencil:getKey()
  assert(self:area()==1)
  local key = nil
  for k,_ in pairs(self) do key = k end
  return key
end

function Stencil:print()
  for k,_ in pairs(self) do
    print(k[1].." "..k[2].." "..k[3])
  end
end

