
orion.type={}
TypeFunctions = {}
TypeMT = {__index=TypeFunctions}

orion.type._bool=setmetatable({type="bool"}, TypeMT)

function orion.type.bool() return orion.type._bool end

orion.type._uint={}
function orion.type.uint(prec)
  orion.type._uint[prec] = orion.type._uint[prec] or setmetatable({type="uint",precision=prec},TypeMT)
  return orion.type._uint[prec]
end


orion.type._int={}
function orion.type.int(prec)
  orion.type._int[prec] = orion.type._int[prec] or setmetatable({type="int",precision=prec},TypeMT)
  return orion.type._int[prec]
end

orion.type._float={}
function orion.type.float(prec)
  orion.type._float[prec] = orion.type._float[prec] or setmetatable({type="float",precision=prec},TypeMT)
  return orion.type._float[prec]
end

orion.type._array={}
function orion.type.array(_type,size)
  assert(type(size)=="number")
  assert(getmetatable(_type)==TypeMT)
  assert(orion.type.isArray(_type)==false)

  orion.type._array[_type] = orion.type._array[_type] or {}
  orion.type._array[_type][size] = orion.type._array[_type][size] or setmetatable({type="array", over=_type, size=size},TypeMT)
  return orion.type._array[_type][size]
end

-- given a lua variable, figure out the correct type and
-- least precision that can represent it
function orion.type.valueToType(v)

  if v==nil then return nil end
  
  if type(v)=="boolean" then
    return orion.type.bool()
  elseif type(v)=="number" then
    local vi, vf = math.modf(v) -- returns the integral bit, then the fractional bit
    
    -- you might be tempted to take things from 0...255 to a uint8 etc, but this is bad!
    -- then if the user write -(5+4) they get a positive number b/c it's a uint8!
    -- similarly, if you take -128...127 to a int8, you also get problems. Then, you
    -- try to meet this int8 with a uint8, and get a int16! (bc this is the only sensible thing)
    -- when really what you wanted is a uint8.
    -- much better to just make the default int32 and have users cast it to what they want
    
    if vf~=0 then
      return orion.type.float(32)
    else
      return orion.type.int(32)
    end
  end
  
  orion.error("Couldn't convert "..v.." to orion type")
end

function orion.type.cropMeet(a,b)
  assert(orion.type.isCropMode(a))
  assert(orion.type.isCropMode(b))

  if a==b then 
    return a
  elseif a==orion.cropSame then
    return b
  elseif b==orion.cropSame then
    return a
  end

  assert(false)
end

-- returns resultType, lhsType, rhsType
-- ast is used for error reporting
function orion.type.meet( a, b, op, ast)
  assert(orion.type.isType(a))
  assert(orion.type.isType(b))
  assert(type(op)=="string")
  assert(orion.IR.isIR(ast))
  
  assert(getmetatable(a)==TypeMT)
  assert(getmetatable(b)==TypeMT)

  local treatedAsBinops = {["select"]=1, ["vectorSelect"]=1,["array"]=1, ["mapreducevar"]=1, ["dot"]=1, ["min"]=1, ["max"]=1}

    if orion.type.isArray(a) and orion.type.isArray(b) then
      if orion.type.arrayLength(a)~=orion.type.arrayLength(b) then
        print("Type error, array length mismatch")
        return nil
      end
      
      if op=="dot" then
        local rettype,at,bt = orion.type.meet(a.over,b.over,op,ast)
        local convtypea = orion.type.array(at,orion.type.arrayLength(a))
        local convtypeb = orion.type.array(bt,orion.type.arrayLength(a))
        return rettype, convtypea, convtypeb
      elseif orion.cmpops[op] then
        -- cmp ops are elementwise
        local rettype,at,bt = orion.type.meet(a.over,b.over,op,ast)
        local convtypea = orion.type.array(at,orion.type.arrayLength(a))
        local convtypeb = orion.type.array(bt,orion.type.arrayLength(a))

        local thistype = orion.type.array(orion.type.bool(), orion.type.arrayLength(a))
        return thistype, convtypea, convtypeb
      elseif orion.binops[op] or treatedAsBinops[op] then
        -- do it pointwise
        local thistype = orion.type.array(orion.type.meet(a.over,b.over,op,ast),orion.type.arrayLength(a))
        return thistype, thistype, thistype
      elseif op=="pow" then
        local thistype = orion.type.array(orion.type.float(32),orion.type.arrayLength(a))
        return thistype, thistype, thistype
      else
        print("OP",op)
        assert(false)
      end
      
    elseif a.type=="int" and b.type=="int" then
      local prec = math.max(a.precision,b.precision)
      local thistype = orion.type.int(prec)

      if orion.cmpops[op] then
        return orion.type.bool(), thistype, thistype
      elseif orion.binops[op] or treatedAsBinops[op] then
        return thistype, thistype, thistype
      elseif op=="pow" then
        local thistype = orion.type.float(32)
        return thistype, thistype, thistype
      else
        print("OP",op)
        assert(false)
      end
    elseif a.type=="uint" and b.type=="uint" then
      local prec = math.max(a.precision,b.precision)
      local thistype = orion.type.uint(prec)

      if orion.cmpops[op] then
        return orion.type.bool(), thistype, thistype
      elseif orion.binops[op] or treatedAsBinops[op] then
        return thistype, thistype, thistype
      elseif op=="pow" then
        local thistype = orion.type.float(32)
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
      if ut.precision==t.precision and t.precision < 32 then
        prec = t.precision * 2
      elseif ut.precision<t.precision then
        prec = math.max(a.precision,b.precision)
      else
        orion.error("Can't meet a "..ut:str().." and a "..t:str(),ast:linenumber(),ast:offset(),ast:filename())
      end
      
      local thistype = orion.type.int(prec)
      
      if orion.cmpops[op] then
        return orion.type.bool(), thistype, thistype
      elseif orion.binops[op] or treatedAsBinops[op] then
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
        thistype = orion.type.float(32)
      elseif ftype.precision==32 and itype.precision==32 then
        thistype = orion.type.float(32)
      elseif ftype.precision==64 and itype.precision<64 then
        thistype = orion.type.float(64)
      else
        assert(false) -- NYI
      end

      if orion.cmpops[op] then
        return orion.type.bool(), thistype, thistype
      elseif orion.intbinops[op] then
        orion.error("Passing a float to an integer binary op "..op,ast:linenumber(),ast:offset())
      elseif orion.binops[op] or treatedAsBinops[op] then
        return thistype, thistype, thistype
      elseif op=="pow" then
        local thistype = orion.type.float(32)
        return thistype, thistype, thistype
      else
        print("OP4",op)
        assert(false)
      end

    elseif a.type=="float" and b.type=="float" then

      local prec = math.max(a.precision,b.precision)
      local thistype = orion.type.float(prec)

      if orion.cmpops[op] then
        return orion.type.bool(), thistype, thistype
      elseif orion.intbinops[op] then
        orion.error("Passing a float to an integer binary op "..op,ast:linenumber(),ast:offset())
      elseif orion.binops[op] or treatedAsBinops[op] then
        return thistype, thistype, thistype
      elseif op=="pow" then
        local thistype = orion.type.float(32)
        return thistype, thistype, thistype
      else
        print("OP3",op)
        assert(false)
      end

    elseif a.type=="bool" and b.type=="bool" then
      -- you can combine two bools into an array of bools
      if orion.boolops[op]==nil and op~="array" then
        print("Internal error, attempting to meet two booleans on a non-boolean op: "..op,ast:linenumber(),ast:offset())
        return nil
      end
      
      local thistype = orion.type.bool()
      return thistype, thistype, thistype
    elseif orion.type.isArray(a) and orion.type.isArray(b)==false then
      -- we take scalar constants and duplicate them out to meet the other arguments array length
      local thistype, lhstype, rhstype = orion.type.meet(a,orion.type.array(b,orion.type.arrayLength(a)),op,ast)
      return thistype, lhstype, rhstype
    elseif orion.type.isArray(a)==false and orion.type.isArray(b) then
      local thistype, lhstype, rhstype = orion.type.meet(orion.type.array(a,orion.type.arrayLength(b)),b,op,ast)
      return thistype, lhstype, rhstype
    else
      print("Type error, meet not implemented for "..orion.type.typeToString(a).." and "..orion.type.typeToString(b),"line",ast:linenumber(),ast:filename())
      print(ast.op)
      assert(false)
      --os.exit()
    end
    
    assert(false)
  return nil
end

-- convert a string describing a type like 'int8' to its actual type
function orion.type.stringToType(s)
  if s=="rgb8" then
    local res = orion.type.array(orion.type.uint(8),3)
    assert(orion.type.isType(res))
    return res
  elseif s=="rgbw8" then
    return orion.type.array(orion.type.uint(8),4)
  elseif s:sub(1,4) == "uint" then
    if s=="uint" then
      orion.error("'uint' is not a valid type, you must specify a precision")
      return nil
    end
    if tonumber(s:sub(5))==nil then return nil end
    return orion.type.uint(tonumber(s:sub(5)))
  elseif s:sub(1,3) == "int" then
    if s=="int" then
      orion.error("'int' is not a valid type, you must specify a precision")
      return nil
    end
    if tonumber(s:sub(4))==nil then return nil end
    return orion.type.int(tonumber(s:sub(4)))
  elseif s:sub(1,5) == "float" then
    if s=="float" then
      orion.error("'float' is not a valid type, you must specify a precision")
      return nil
    end
    if tonumber(s:sub(6))==nil then return nil end
    return orion.type.float(tonumber(s:sub(6)))
  elseif s=="bool" then
    return orion.type.bool()
  else

  end
 
  --print("Error, unknown type "..s)
  return nil
end

-- check if type 'from' can be converted to 'to' (explicitly)
function orion.type.checkExplicitCast(from, to, ast)
  assert(from~=nil)
  assert(to~=nil)
  assert(orion.ast.isAST(ast))

  if from==to then
    -- obvously can return true...
    return true
  elseif orion.type.isArray(from) and orion.type.isArray(to) then
    if orion.type.arrayLength(from)~=orion.type.arrayLength(to) then
      orion.error("Can't change array length when casting "..from:str().." to "..to:str(),ast:linenumber(),ast:offset(),ast:filename())
    end

    return orion.type.checkExplicitCast(from.over, to.over,ast)

  elseif orion.type.isArray(from)==false and orion.type.isArray(to)  then
    return orion.type.checkExplicitCast(from, to.over,ast)

  elseif orion.type.isArray(from) and orion.type.isArray(to)==false then
    orion.error("Can't cast an array type to a non-array type. "..from:str().." to "..to:str(),ast:linenumber(),ast:offset(),ast:filename())
    return false
  elseif from.type=="uint" and to.type=="uint" then
    if from.precision > to.precision then
      orion.warning("converting "..orion.type.typeToString(from).." to "..orion.type.typeToString(to).." could result in an overflow!",ast:linenumber(),ast:offset())
    end    

    return true
  elseif from.type=="int" and to.type=="int" then
    if from.precision > to.precision then
      orion.warning("converting "..orion.type.typeToString(from).." to "..orion.type.typeToString(to).." could result in an overflow!",ast:linenumber(),ast:offset())
    end    

    return true
  elseif from.type=="uint" and to.type=="int" then
    if from.precision >= to.precision then
      orion.warning("converting "..orion.type.typeToString(from).." to "..orion.type.typeToString(to).." could result in an overflow!",ast:linenumber(),ast:offset())
    end    

    return true
  elseif from.type=="float" and to.type=="uint" then
    orion.warning("Converting "..orion.type.typeToString(from).." to "..orion.type.typeToString(to).." could result in overflow!",ast:linenumber(),ast:offset())
    return true
  elseif from.type=="uint" and to.type=="float" then
    return true
  elseif from.type=="int" and to.type=="float" then
    if from.precision >= to.precision then
      orion.warning("converting "..from:str().." to "..to:str().." could result in loss of precision!",ast:linenumber(),ast:offset(),ast:filename())
    end    

    return true
  elseif from.type=="int" and to.type=="uint" then
    orion.warning("converting "..from:str().." to "..to:str().." could result in loss of negative values!",ast:linenumber(),ast:offset())

    if from.precision > to.precision then
      orion.warning("converting "..orion.type.typeToString(from).." to "..orion.type.typeToString(to).." could result in an overflow!",ast:linenumber(),ast:offset())
    end    
    
    return true
  elseif from.type=="int" and to.type=="bool" then
    orion.error("converting an int to a bool will result in incorrect behavior! C makes sure that bools are always either 0 or 1. Terra does not.",ast:linenumber(),ast:offset())
    return false
  elseif from.type=="bool" and to.type=="int" then
    orion.error("converting a bool to an int will result in incorrect behavior! C makes sure that bools are always either 0 or 1. Terra does not.",ast:linenumber(),ast:offset())
    return false
  elseif from.type=="float" and to.type=="int" then
    return true
  elseif from.type=="float" and to.type=="float" then
    return true
  else
    from:print()
    to:print()
    assert(false) -- NYI
  end

  return false
end

-- compare this to meet - this is where we can't change the type of 'to',
-- so we just have to see if 'from' can be converted to 'to'
function orion.type.checkImplicitCast(from, to, ast)
  assert(from~=nil)
  assert(to~=nil)
  assert(orion.ast.isAST(ast))

  if from.type=="uint" and to.type=="uint" then
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

    if to.precision == from.precision then
      orion.warning("implicitly casting "..from:str().." to "..to:str().." which may result in precision loss",ast:linenumber(),ast:offset())
    end

    if to.precision >= from.precision then
      return true
    end

  elseif from.type=="int" and to.type=="float" then

    if to.precision == from.precision then
      orion.warning("implicitly casting "..from:str().." to "..to:str().." which may result in precision loss",ast:linenumber(),ast:offset())
    end

    if to.precision >= from.precision then
      return true
    end

  elseif from.type=="float" and to.type=="float" then
    if to.precision >= from.precision then
      return true
    end
  end


  return false
end

---------------------------------------------------------------------
-- 'externally exposed' functions

-- this will only work on a typed ast
function orion.getType(ast)
  assert(type(ast)=="table")
  assert(ast.type~=nil)
  return ast.type
end

function orion.type.isFloat(ty)
  assert(getmetatable(ty)==TypeMT)
  return ty.type=="float"
end

function orion.type.astIsFloat(ast)
  return orion.type.isFloat(orion.getType(ast))	 
end

function orion.type.isUint(ty)
  assert(getmetatable(ty)==TypeMT)
  return ty.type=="uint"
end

function orion.type.astIsUint(ast)
  return orion.type.isUint(orion.getType(ast))
end


function orion.type.isInt(ty)
  assert(getmetatable(ty)==TypeMT)
  return ty.type=="int"
end

function orion.type.astIsInt(ast)
  return orion.type.isInt(orion.getType(ast))
end

function orion.type.isNumber(ty)
  assert(getmetatable(ty)==TypeMT)
  return ty.type=="float" or ty.type=="uint" or ty.type=="int"
end

function orion.type.astIsNumber(ast)
  return orion.type.isNumber(orion.getType(ast))	 
end

function orion.type.isBool(ty)
  assert(getmetatable(ty)==TypeMT)
  return ty.type=="bool"
end

function orion.type.astIsBool(ast)
  return orion.type.isBool(orion.getType(ast))
end

function orion.type.isArray(ty)
  assert(getmetatable(ty)==TypeMT)
  return ty.type=="array"
end

function orion.type.astIsArray(ast)
  assert(orion.ast.isAST(ast))
  return orion.type.isArray(orion.getType(ast))
end
-- returns 0 if not an array
function orion.type.arrayLength(ty)
  assert(getmetatable(ty)==TypeMT)
  if ty.type~="array" then return 0 end
  return ty.size  
end

-- returns 0 if ast is not an array
function orion.type.astArrayLength(ast)
  return orion.type.arrayLength(orion.getType(ast))
end


function orion.type.arrayOver(ty)
  assert(getmetatable(ty)==TypeMT)
  return ty.over
end

function orion.type.astArrayOver(ast)
  local ty = orion.getType(ast)
  assert(orion.type.isArray(ty))
  return orion.type.arrayOver(ty)
end

function orion.type.isType(ty)
  return getmetatable(ty)==TypeMT
end

function orion.type.isCropMode(cropMode)
  return cropMode == orion.cropSame or 
    cropMode == orion.cropGrow or 
    cropMode == orion.cropShrink or
    cropMode == orion.cropExplicit

end

-- return precision of this type
function orion.type.precision(ty)
  if orion.type.isUint(ty) then
    return ty.precision
  else
    assert(false)
  end
end

-- convert this uint type to an int type with same precision
function orion.type.uintToInt(ty)
  assert(orion.type.isUint(ty))
  return orion.type.int(orion.type.precision(ty))
end

function orion.type.typeToString(ty)
  assert(orion.type.isType(ty))

  if ty.type=="bool" then
    return "bool"
  elseif ty.type=="int" then
    return "int"..ty.precision
  elseif ty.type=="uint" then
    return "uint"..ty.precision
  elseif ty.type=="float" then
    return "float"..ty.precision
  elseif ty.type=="array" then
    return orion.type.typeToString(ty.over).."["..ty.size.."]"
  end

  print("Error, typeToString input doesn't appear to be a type")
  os.exit()
end

function orion.type.astTypeToString(ast)
  return orion.type.typeToString(orion.getType(ast))
end

-- returns size in bytes
function orion.type.sizeof(ty)
  if ty.type=="float" and ty.precision==32 then
    return 4
  elseif ty.type=="uint" and ty.precision==8 then
    return 1
  end

  print(orion.type.typeToString(ty))

  assert(false)
  return nil
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

function TypeFunctions:print()
  print(orion.type.typeToString(self))
end

function TypeFunctions:str()
 return orion.type.typeToString(self)
end

function TypeFunctions:isArray()
  return self.type=="array"
end

function TypeFunctions:toTerraType()
  return orion.type.toTerraType(self)
end

function TypeFunctions:sizeof()
  return terralib.sizeof(self:toTerraType())
end

function TypeFunctions:isFloat()
  return orion.type.isFloat(self)
end

function TypeFunctions:isBool()
  return orion.type.isBool(self)
end

function TypeFunctions:isInt()
  return orion.type.isInt(self)
end

function TypeFunctions:isNumber()
  return orion.type.isNumber(self)
end

function TypeFunctions:channels()
  if self.type~="array" then return 1 end
  return self.size
end

function TypeFunctions:baseType()
  if self.type~="array" then return self end
  return self.over
end

-- this calculates the precision of the result of a reduction tree.
-- op is the reduction op
-- typeTable is a list of the types we're reducing over
function orion.type.reduce(op,typeTable)
  assert(type(typeTable)=="table")
  assert(#typeTable>=1)
  for k,v in pairs(typeTable) do assert(orion.type.isType(v)) end

  return typeTable[1]
end


-- _type = the orion type
-- if pointer is true, generate a pointer instead of a value
-- vectorN = width of the vector [optional]
function orion.type.toTerraType(_type,pointer,vectorN)
  assert(orion.type.isType(_type))

  local ttype

  if _type==orion.type.float(32) then
    ttype = float
  elseif _type==orion.type.float(64) then
    ttype = double
  elseif _type==orion.type.uint(8) then
    ttype = uint8
  elseif _type==orion.type.int(8) then
    ttype = int8
  elseif _type==orion.type.bool() then
    ttype = bool
  elseif _type==orion.type.int(32) then
    ttype = int32
  elseif _type==orion.type.int(64) then
    ttype = int64
  elseif _type==orion.type.uint(32) then
    ttype = uint32
  elseif _type==orion.type.uint(16) then
    ttype = uint16
  elseif _type==orion.type.int(16) then
    ttype = int16
  elseif orion.type.isArray(_type) then
    local baseType = orion.type.arrayOver(_type)
    local al = orion.type.arrayLength(_type)
    ttype = orion.type.toTerraType( baseType, pointer, vectorN )[al]
  else
    print(orion.type.typeToString(_type))
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

  print(orion.type.typeToString(_type))
  assert(false)

  return nil
end

