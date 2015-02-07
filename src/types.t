darkroom.type={}
TypeFunctions = {}
TypeMT = {__index=TypeFunctions, __tostring=function(ty)
  if ty.type=="bool" then
    return "bool"
  elseif ty.type=="null" then
    return "null"
  elseif ty.type=="int" then
    return "int"..ty.precision
  elseif ty.type=="uint" then
    return "uint"..ty.precision
  elseif ty.type=="float" then
    return "float"..ty.precision
  elseif ty.type=="array" then
    return tostring(ty.over).."["..table.concat(ty.size,",").."]"
  elseif ty.type=="struct" then
    local t = "{"
    for k,v in pairs(ty.kvs) do
      t=t..k.."="..tostring(v)..","
    end
    return t.."}"
  end

  print("Error, typeToString input doesn't appear to be a type, ",ty.type)
  assert(false)
end}

darkroom.type._bool=setmetatable({type="bool"}, TypeMT)
function darkroom.type.bool() return darkroom.type._bool end

darkroom.type._null=setmetatable({type="null"}, TypeMT)
function darkroom.type.null() return darkroom.type._null end

darkroom.type._uint={}
function darkroom.type.uint(prec)
  darkroom.type._uint[prec] = darkroom.type._uint[prec] or setmetatable({type="uint",precision=prec},TypeMT)
  return darkroom.type._uint[prec]
end


darkroom.type._int={}
function darkroom.type.int(prec)
  darkroom.type._int[prec] = darkroom.type._int[prec] or setmetatable({type="int",precision=prec},TypeMT)
  return darkroom.type._int[prec]
end

darkroom.type._float={}
function darkroom.type.float(prec)
  darkroom.type._float[prec] = darkroom.type._float[prec] or setmetatable({type="float",precision=prec},TypeMT)
  return darkroom.type._float[prec]
end

darkroom.type._array={}
darkroom.type._arraySize={}
-- the last index of size is the innermost size
-- {3,4} means 3 arrays of size 4. [x,y] where x in [0,2], y is in [0,3]
-- But we store this in darkroom.type._array in the opposite order to be unambiguous
function darkroom.type.array( _type, size )
  if type(size)=="number" then size={size} end
  assert(type(size)=="table")
  map(size, function(n) assert(type(n)=="number" and n>0) end)
  assert( getmetatable(_type) == TypeMT )

  local ty = _type
  if _type.type=="array" then 
    size = concat(size,_type.size) 
    ty = _type.over
  end
  assert(ty:isArray()==false)

  -- we uniqify the size array
  darkroom.type._arraySize[#size] = darkroom.type._arraySize[#size] or {}
  size = deepsetweak(darkroom.type._arraySize[#size], size, size)

  darkroom.type._array[ty] = darkroom.type._array[ty] or {}
  darkroom.type._array[ty][size] = darkroom.type._array[ty][size] or setmetatable({type="array", over=ty, size=size},TypeMT)
  return darkroom.type._array[ty][size]
end

darkroom.type._struct={}
function darkroom.type.structure( kvs )
  assert(type(kvs)=="table")
  assert(keycount(kvs)>0)

  local s = {}
  for k,v in pairs(kvs) do 
    assert(type(k)=="string")
    assert(darkroom.type.isType(v))
    assert(s.type~="struct")
    table.insert(s,{k=k,v=v}) 
  end
  table.sort(s, function(a,b) return a.k<b.k end)
  local key = ""
  map(s, function(n) key = key..n.k.."-"..tostring(n.v).."-" end)
  print("STRUCT KEY",key)
  assert(keycount(kvs)>0)

  if darkroom.type._struct[key]==nil then
    darkroom.type._struct[key] = setmetatable({type="struct",kvs = kvs}, TypeMT)
  end

  return darkroom.type._struct[key]
end

function darkroom.type.fromTerraType(ty, linenumber, offset, filename)
  if darkroom.type.isType(ty) then return ty end

  assert(terralib.types.istype(ty))

  if ty==int32 then
    return darkroom.type.int(32)
  elseif ty==int16 then
    return darkroom.type.int(16)
  elseif ty==uint8 then
    return darkroom.type.uint(8)
  elseif ty==uint32 then
    return darkroom.type.uint(32)
  elseif ty==int8 then
    return darkroom.type.int(8)
  elseif ty==uint16 then
    return darkroom.type.uint(16)
  elseif ty==float then
    return darkroom.type.float(32)
  elseif ty==double then
    return darkroom.type.float(64)
  elseif ty==bool then
    return darkroom.type.bool()
  elseif ty:isarray() then
    if ty.N <=0 then
      darkroom.error("Array can not have size 0",linenumber,offset,filename)
    end
    return darkroom.type.array(darkroom.type.fromTerraType(ty.type),ty.N)
  end

  print("error, unsupported terra type",ty)
  assert(false)
end

-- given a lua variable, figure out the correct type and
-- least precision that can represent it
function darkroom.type.valueToType(v)

  if v==nil then return nil end
  
  if type(v)=="boolean" then
    return darkroom.type.bool()
  elseif type(v)=="number" then
    local vi, vf = math.modf(v) -- returns the integral bit, then the fractional bit
    
    -- you might be tempted to take things from 0...255 to a uint8 etc, but this is bad!
    -- then if the user write -(5+4) they get a positive number b/c it's a uint8!
    -- similarly, if you take -128...127 to a int8, you also get problems. Then, you
    -- try to meet this int8 with a uint8, and get a int16! (bc this is the only sensible thing)
    -- when really what you wanted is a uint8.
    -- much better to just make the default int32 and have users cast it to what they want
    
    if vf~=0 then
      return darkroom.type.float(32)
    else
      return darkroom.type.int(32)
    end
  elseif type(v)=="table" then
    if keycount(v)~=#v then return nil end
    local tys = {}
    for k,vv in ipairs(v) do
      tys[k] = darkroom.type.valueToType(vv)
      if tys[k]==nil then return nil end
    end
    return darkroom.type.array(darkroom.type.reduce("",tys),#v)
  end
  
  return nil -- fail
end

-- returns resultType, lhsType, rhsType
-- ast is used for error reporting
function darkroom.type.meet( a, b, op, ast)
  assert(darkroom.type.isType(a))
  assert(darkroom.type.isType(b))
  assert(type(op)=="string")
  assert(darkroom.IR.isIR(ast))
  
  assert(getmetatable(a)==TypeMT)
  assert(getmetatable(b)==TypeMT)

  local treatedAsBinops = {["select"]=1, ["vectorSelect"]=1,["array"]=1, ["mapreducevar"]=1, ["dot"]=1, ["min"]=1, ["max"]=1}

  if a:isArray() and b:isArray() then
    if a:arrayLength() ~= b:arrayLength() then
      print("Type error, array length mismatch")
      return nil
    end
    
    if op=="dot" then
      local rettype,at,bt = darkroom.type.meet(a.over,b.over,op,ast)
      local convtypea = darkroom.type.array( at, a:arrayLength() )
      local convtypeb = darkroom.type.array( bt, a:arrayLength() )
      return rettype, convtypea, convtypeb
    elseif darkroom.cmpops[op] then
      -- cmp ops are elementwise
      local rettype,at,bt = darkroom.type.meet(a.over,b.over,op,ast)
      local convtypea = darkroom.type.array( at, a:arrayLength() )
      local convtypeb = darkroom.type.array( bt, a:arrayLength() )
      
      local thistype = darkroom.type.array( darkroom.type.bool(), a:arrayLength() )
      return thistype, convtypea, convtypeb
    elseif darkroom.binops[op] or treatedAsBinops[op] then
      -- do it pointwise
      local thistype = darkroom.type.array( darkroom.type.meet(a.over,b.over,op,ast), a:arrayLength() )
      return thistype, thistype, thistype
    elseif op=="pow" then
      local thistype = darkroom.type.array(darkroom.type.float(32), a:arrayLength() )
      return thistype, thistype, thistype
    else
      print("OP",op)
      assert(false)
    end
      
  elseif a.type=="int" and b.type=="int" then
    local prec = math.max(a.precision,b.precision)
    local thistype = darkroom.type.int(prec)
    
    if darkroom.cmpops[op] then
      return darkroom.type.bool(), thistype, thistype
    elseif darkroom.binops[op] or treatedAsBinops[op] then
      return thistype, thistype, thistype
    elseif op=="pow" then
      local thistype = darkroom.type.float(32)
      return thistype, thistype, thistype
    else
      print("OP",op)
      assert(false)
    end
  elseif a.type=="uint" and b.type=="uint" then
    local prec = math.max(a.precision,b.precision)
    local thistype = darkroom.type.uint(prec)
    
    if darkroom.cmpops[op] then
      return darkroom.type.bool(), thistype, thistype
    elseif darkroom.binops[op] or treatedAsBinops[op] then
      return thistype, thistype, thistype
    elseif op=="pow" then
      local thistype = darkroom.type.float(32)
      return thistype, thistype, thistype
    else
      print("OP2",op)
      assert(false)
    end
  elseif (a.type=="uint" and b.type=="int") or (a.type=="int" and b.type=="uint") then
    
    local ut = a
    local t = b
    if a.type=="int" then ut,t = t,ut end
    
    local prec
    if ut.precision==t.precision and t.precision < 64 then
      prec = t.precision * 2
    elseif ut.precision<t.precision then
      prec = math.max(a.precision,b.precision)
    else
      darkroom.error("Can't meet a "..tostring(ut).." and a "..tostring(t),ast:linenumber(),ast:offset(),ast:filename())
    end
    
    local thistype = darkroom.type.int(prec)
    
    if darkroom.cmpops[op] then
      return darkroom.type.bool(), thistype, thistype
    elseif darkroom.binops[op] or treatedAsBinops[op] then
      return thistype, thistype, thistype
    elseif op=="pow" then
      return thistype, thistype, thistype
    else
      print( "operation " .. op .. " is not implemented for aType:" .. a.type .. " bType:" .. b.type .. " " )
      assert(false)
    end
    
  elseif (a.type=="float" and (b.type=="uint" or b.type=="int")) or 
    ((a.type=="uint" or a.type=="int") and b.type=="float") then
    
    local thistype
    local ftype = a
    local itype = b
    if b.type=="float" then ftype,itype=itype,ftype end
    
    if ftype.precision==32 and itype.precision<32 then
      thistype = darkroom.type.float(32)
    elseif ftype.precision==32 and itype.precision==32 then
      thistype = darkroom.type.float(32)
    elseif ftype.precision==64 and itype.precision<64 then
      thistype = darkroom.type.float(64)
    else
      assert(false) -- NYI
    end
    
    if darkroom.cmpops[op] then
      return darkroom.type.bool(), thistype, thistype
    elseif darkroom.intbinops[op] then
      darkroom.error("Passing a float to an integer binary op "..op,ast:linenumber(),ast:offset())
    elseif darkroom.binops[op] or treatedAsBinops[op] then
      return thistype, thistype, thistype
    elseif op=="pow" then
      local thistype = darkroom.type.float(32)
      return thistype, thistype, thistype
    else
      print("OP4",op)
      assert(false)
    end
    
  elseif a.type=="float" and b.type=="float" then
    
    local prec = math.max(a.precision,b.precision)
    local thistype = darkroom.type.float(prec)
    
    if darkroom.cmpops[op] then
      return darkroom.type.bool(), thistype, thistype
    elseif darkroom.intbinops[op] then
      darkroom.error("Passing a float to an integer binary op "..op,ast:linenumber(),ast:offset())
    elseif darkroom.binops[op] or treatedAsBinops[op] then
      return thistype, thistype, thistype
    elseif op=="pow" then
      local thistype = darkroom.type.float(32)
      return thistype, thistype, thistype
    else
      print("OP3",op)
      assert(false)
    end
    
  elseif a.type=="bool" and b.type=="bool" then
    -- you can combine two bools into an array of bools
    if darkroom.boolops[op]==nil and op~="array" then
      print("Internal error, attempting to meet two booleans on a non-boolean op: "..op,ast:linenumber(),ast:offset())
      return nil
    end
    
    local thistype = darkroom.type.bool()
    return thistype, thistype, thistype
  elseif a:isArray() and b:isArray()==false then
    -- we take scalar constants and duplicate them out to meet the other arguments array length
    local thistype, lhstype, rhstype = darkroom.type.meet( a, darkroom.type.array( b,a :arrayLength() ), op, ast )
    return thistype, lhstype, rhstype
  elseif a:isArray()==false and b:isArray() then
    local thistype, lhstype, rhstype = darkroom.type.meet( darkroom.type.array(a, b:arrayLength() ), b, op, ast )
    return thistype, lhstype, rhstype
  else
    print("Type error, meet not implemented for "..tostring(a).." and "..tostring(b),"line",ast:linenumber(),ast:filename())
    print(ast.op)
    assert(false)
    --os.exit()
  end
  
  assert(false)
  return nil
end

-- convert a string describing a type like 'int8' to its actual type
function darkroom.type.stringToType(s)
  if s=="rgb8" then
    local res = darkroom.type.array(darkroom.type.uint(8),3)
    assert(darkroom.type.isType(res))
    return res
  elseif s=="rgbw8" then
    return darkroom.type.array(darkroom.type.uint(8),4)
  elseif s:sub(1,4) == "uint" then
    if s=="uint" then
      darkroom.error("'uint' is not a valid type, you must specify a precision")
      return nil
    end
    if tonumber(s:sub(5))==nil then return nil end
    return darkroom.type.uint(tonumber(s:sub(5)))
  elseif s:sub(1,3) == "int" then
    if s=="int" then
      darkroom.error("'int' is not a valid type, you must specify a precision")
      return nil
    end
    if tonumber(s:sub(4))==nil then return nil end
    return darkroom.type.int(tonumber(s:sub(4)))
  elseif s:sub(1,5) == "float" then
    if s=="float" then
      darkroom.error("'float' is not a valid type, you must specify a precision")
      return nil
    end
    if tonumber(s:sub(6))==nil then return nil end
    return darkroom.type.float(tonumber(s:sub(6)))
  elseif s=="bool" then
    return darkroom.type.bool()
  else

  end
 
  --print("Error, unknown type "..s)
  return nil
end

-- check if type 'from' can be converted to 'to' (explicitly)
function darkroom.type.checkExplicitCast(from, to, ast)
  assert(from~=nil)
  assert(to~=nil)
  assert(darkroom.ast.isAST(ast))

  if from==to then
    -- obvously can return true...
    return true
  elseif from:isArray() and to:isArray() then
    -- we do allow you to explicitly cast arrays of different shapes but the same total size
    if from:channels()~=to:channels() then
      darkroom.error("Can't change array length when casting "..tostring(from).." to "..tostring(to), ast:linenumber(), ast:offset(), ast:filename() )
    end

    return darkroom.type.checkExplicitCast(from.over, to.over,ast)

  elseif (from:isStruct()==false and from:isArray()==false) and to:isArray() then
    return darkroom.type.checkExplicitCast(from, to.over, ast )

  elseif from:isArray() and (to:isArray()==false and to:isStruct()==false) then
    if from:arrayOver():isBool() and from:channels()==to:sizeof()*8 then
      -- casting an array of bools to a type with the same number of bits is OK
      return true
    end

    darkroom.error("Can't cast an array type to a non-array type. "..tostring(from).." to "..tostring(to), ast:linenumber(), ast:offset(), ast:filename() )
    return false
  elseif from:isStruct() and to:isStruct()==false and keycount(from.kvs)==1 then
    for k,v in pairs(from.kvs) do
      return darkroom.type.checkExplicitCast(v,to,ast)
    end
  elseif from:isStruct() and to:isStruct() and keycount(from.kvs)==keycount(to.kvs) then
    for k,v in pairs(from.kvs) do
      if to.kvs[k]==nil then return false end
      if darkroom.type.checkExplicitCast(v,to.kvs[k],ast)==false then return false end
    end
    return true
  elseif from.type=="uint" and to.type=="uint" then
    return true
  elseif from.type=="int" and to.type=="int" then
    return true
  elseif from.type=="uint" and to.type=="int" then
    return true
  elseif from.type=="float" and to.type=="uint" then
    return true
  elseif from.type=="uint" and to.type=="float" then
    return true
  elseif from.type=="int" and to.type=="float" then
    return true
  elseif from.type=="int" and to.type=="uint" then
    return true
  elseif from.type=="int" and to.type=="bool" then
    darkroom.error("converting an int to a bool will result in incorrect behavior! C makes sure that bools are always either 0 or 1. Terra does not.",ast:linenumber(),ast:offset())
    return false
  elseif from.type=="bool" and (to.type=="int" or to.type=="uint") then
    darkroom.error("converting a bool to an int will result in incorrect behavior! C makes sure that bools are always either 0 or 1. Terra does not.",ast:linenumber(),ast:offset())
    return false
  elseif from.type=="float" and to.type=="int" then
    return true
  elseif from.type=="float" and to.type=="float" then
    return true
  else
    print(from,to)
    assert(false) -- NYI
  end

  return false
end

-- compare this to meet - this is where we can't change the type of 'to',
-- so we just have to see if 'from' can be converted to 'to'
function darkroom.type.checkImplicitCast(from, to, ast)
  assert(darkroom.type.isType(from))
  assert(darkroom.type.isType(to))
  assert(darkroom.IR.isIR(ast))

  if from==to then
    return true -- obviously
  elseif from.type=="uint" and to.type=="uint" then
    if to.precision >= from.precision then
      return true
    end
  elseif from.type=="uint" and to.type=="int" then
    if to.precision > from.precision then
      return true
    end
  elseif from.type=="int" and to.type=="int" then
    if to.precision >= from.precision then
      return true
    end
  elseif from.type=="uint" and to.type=="float" then
    if to.precision >= from.precision then
      return true
    end
  elseif from.type=="int" and to.type=="float" then
    if to.precision >= from.precision then
      return true
    end
  elseif from.type=="float" and to.type=="float" then
    if to.precision >= from.precision then
      return true
    end
  elseif from:isStruct() and to:isStruct()==false and keycount(from.kvs)==1 then
    -- convert a struct of one entry to the type of that entry
    -- terrible, terrible language design
    for k,v in pairs(from.kvs) do
      return darkroom.type.checkImplicitCast(v,to,ast)
    end
  elseif from:isStruct() and to:isStruct() and keycount(from.kvs)==keycount(to.kvs) then
    for k,v in pairs(from.kvs) do
      if to.kvs[k]==nil then print("A");return false end
      local r = darkroom.type.checkImplicitCast(v,to.kvs[k],ast)
      if r==false then return false end
    end
    return true
  end

  return false
end

---------------------------------------------------------------------
-- 'externally exposed' functions

function darkroom.type.isType(ty)
  return getmetatable(ty)==TypeMT
end

-- convert this uint type to an int type with same precision
function darkroom.type.uintToInt(ty)
  assert(darkroom.type.isUint(ty))
  return darkroom.type.int(ty.precision)
end

function TypeFunctions:toC()
  if self.type=="float" and self.precision==32 then
    return "float"
  elseif self.type=="uint" and self.precision==8 then
    return "unsigned char"
  else
    assert(false)
  end
end

function TypeFunctions:isArray()
  return self.type=="array"
end

function TypeFunctions:arrayOver()
  assert(self.type=="array")
  assert(self.over.type~="array")
  return self.over
end

-- returns 0 if not an array
function TypeFunctions:arrayLength()
  if self.type~="array" then return 0 end
  return self.size  
end

-- if pointer is true, generate a pointer instead of a value
-- vectorN = width of the vector [optional]
function TypeFunctions:toTerraType(pointer, vectorN)
  local ttype

  if self==darkroom.type.float(32) then
    ttype = float
  elseif self==darkroom.type.float(64) then
    ttype = double
  elseif self==darkroom.type.uint(8) then
    ttype = uint8
  elseif self==darkroom.type.int(8) then
    ttype = int8
  elseif self==darkroom.type.bool() then
    ttype = bool
  elseif self==darkroom.type.int(32) then
    ttype = int32
  elseif self==darkroom.type.int(64) then
    ttype = int64
  elseif self==darkroom.type.uint(32) then
    ttype = uint32
  elseif self==darkroom.type.uint(16) then
    ttype = uint16
  elseif self==darkroom.type.int(16) then
    ttype = int16
  elseif self:isArray() then
    ttype = self:arrayOver():toTerraType( pointer, vectorN )[self:channels()]
  else
    print(darkroom.type.typeToString(_type))
    print(debug.traceback())
    assert(false)
  end

  if vectorN then
    if pointer then return &vector(ttype,vectorN) end
    return vector(ttype,vectorN)
  else
    if pointer then return &ttype end
    return ttype
  end

  print(darkroom.type.typeToString(_type))
  assert(false)

  return nil
end

function TypeFunctions:sizeof()
  return terralib.sizeof(self:toTerraType())
end

function TypeFunctions:isFloat()
  return self.type=="float"
end

function TypeFunctions:isBool()
  return self.type=="bool"
end

function TypeFunctions:isInt()
  return self.type=="int"
end

function TypeFunctions:isUint()
  return self.type=="uint"
end

function TypeFunctions:isStruct()
  return self.type=="struct"
end

function TypeFunctions:isNumber()
  return self.type=="float" or self.type=="uint" or self.type=="int"
end

function TypeFunctions:channels()
  if self.type~="array" then return 1 end
  local chan = 1
  for k,v in ipairs(self.size) do chan = chan*v end
  return chan
end

function TypeFunctions:baseType()
  if self.type~="array" then return self end
  assert(type(self.over)~="array")
  return self.over
end

-- this calculates the precision of the result of a reduction tree.
-- op is the reduction op
-- typeTable is a list of the types we're reducing over
function darkroom.type.reduce(op,typeTable)
  assert(type(typeTable)=="table")
  assert(#typeTable>=1)
  for k,v in pairs(typeTable) do assert(darkroom.type.isType(v)) end

  return typeTable[1]
end

