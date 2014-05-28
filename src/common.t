local cstdio = terralib.includec("stdio.h")
local cstring = terralib.includec("string.h")
local cstdlib = terralib.includec("stdlib.h")

function table_print (tt, indent, done)
  done = done or {}
  indent = indent or 0
  if type(tt) == "table" then
    local sb = {}
    local first = true
    for key, value in pairs (tt) do
      table.insert(sb, string.rep (" ", indent)) -- indent it
      if type (value) == "table" and not done [value] then
        done [value] = true
	table.insert(sb, key);
	table.insert(sb, "=");
--        table.insert(sb, "{"..tostring(value).."\n");
--        if first then comma="";first=false end
        table.insert(sb, "{\n");
        table.insert(sb, table_print (value, indent + 2, done))
        table.insert(sb, string.rep (" ", indent)) -- indent it
        local comma = ","
        table.insert(sb, "}");
      elseif "number" == type(key) then
        table.insert(sb, string.format("\"%s\"", tostring(value)))
      else
        table.insert(sb, string.format(
            "%s = %s", tostring (key), tostring(value)))
      end
      table.insert(sb,",\n")
    end
    sb[#sb] = nil -- delete comma
    return table.concat(sb)
  else
    return tostring(tt) .. "\n"
  end
end

function to_string( tbl )
    if  "nil"       == type( tbl ) then
        return tostring(nil)
    elseif  "table" == type( tbl ) then
        return tostring(tbl).." "..table_print(tbl)
    elseif  "string" == type( tbl ) then
        return tbl
    else
        return tostring(tbl)
    end
end

function serialize(tbl) print(to_string(tbl)) end


function deepcopy(object)
    local lookup_table = {}
    local function _copy(object)
        if type(object) ~= "table" then
            return object
        elseif lookup_table[object] then
            return lookup_table[object]
        end
        local new_table = {}
        lookup_table[object] = new_table
        for index, value in pairs(object) do
            new_table[_copy(index)] = _copy(value)
        end
        return setmetatable(new_table, getmetatable(object))
    end
    return _copy(object)
end


function explode(div,str) -- credit: http://richard.warburton.it
  if (div=='') then return false end
  local pos,arr = 0,{}
  -- for each divider found
  for st,sp in function() return string.find(str,div,pos,true) end do
    table.insert(arr,string.sub(str,pos,st-1)) -- Attach chars left of current divider
    pos = sp + 1 -- Jump past current divider
  end
  table.insert(arr,string.sub(str,pos)) -- Attach chars right of last divider
  return arr
end

-- append elements in 'src' to 'dest'
-- both have to have only numeric keys
function appendTable(dest,src)
  for k,v in ipairs(src) do
    assert(type(k)=="number")
    table.insert(dest,v)
  end
end

function appendSet(dest,src)
  for k,v in pairs(src) do
    dest[k]=v
  end
end

function keycount(t)
  assert(type(t)=="table")
  local tot = 0
  for k,v in pairs(t) do tot=tot+1 end
  return tot
end

-- takes an array of values to a hash where the values are keys
function invertTable(t)
  for k,v in pairs(t) do assert(type(k)=="number") end

  local out = {}
  for k,v in ipairs(t) do
    assert(out[v]==nil)
    out[v]=k
  end

  return out
end

-- dedup t. no guarantee on the behavior of the keys
function dedup(t)
  local invT = {}
  for k,v in pairs(t) do 
    assert(type(k)=="number") 
    invT[v] = 1
  end

  local res = {}
  for k,_ in pairs(invT) do
    res[#res+1]=k
  end

  return res
end

function pack(...)
  local arg = {...}
  return arg
end


function explode(div,str) -- credit: http://richard.warburton.it
 if (div=='') then return false end
  local pos,arr = 0,{}
  -- for each divider found
  for st,sp in function() return string.find(str,div,pos,true) end do
    table.insert(arr,string.sub(str,pos,st-1)) -- Attach chars left of current divider
    pos = sp + 1 -- Jump past current divider
  end
  table.insert(arr,string.sub(str,pos)) -- Attach chars right of last divider
  return arr
end


function upToNearest(roundto,x)
  assert(type(x)=="number")
  --if x < 0 then orion.error("uptoNearest x<=0 "..x) end

  if x % roundto == 0 or roundto==0 then return x end
  
  local ox

  if x < 0 then
    ox = x + (x%roundto)
  else
    ox = x + (roundto-x%roundto)
  end

  assert(ox > x)
  assert( (ox % roundto) == 0)
  return ox
end

function downToNearest(roundto,x)
  assert(type(x)=="number")
  --assert(x>=0)

  if x % roundto == 0 or roundto == 0 then return x end

  local ox
  if x < 0 then
    ox = x - (roundto+x%roundto)
  else
    ox = x - x%roundto 
  end
  
  assert(ox < x and ox % roundto == 0)
  return ox
end


terra upToNearestTerra(roundto : int,x: int)
  if x % roundto == 0 or roundto==0 then return x end
  
  var ox : int
  if x < 0 then
    ox = x - (x%roundto)
  else
    ox = x + (roundto-x%roundto)
  end

  return ox
end


terra downToNearestTerra(roundto:int,x:int)
  if x % roundto == 0 or roundto == 0 then return x end

  var ox :int
  if x < 0 then
    ox = x - (roundto+x%roundto)
  else
    ox = x - x%roundto
  end

  return ox
end

-- a hack to have assert print a traceback
local oldassert = assert
function assert(x)
  if x==false then print(debug.traceback()) end
  oldassert(x)
end

local Ctmp = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <inttypes.h>

  double CurrentTimeInSeconds() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + tv.tv_usec / 1000000.0;
                                 }

                                   ]]

orion.currentTimeInSeconds = Ctmp.CurrentTimeInSeconds

terra orionAssert(cond : bool, str : &int8)
  if cond==false then
    cstdio.printf("ASSERTT fail %s\n", str)
    cstdlib.exit(1)
  end
end


function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end

-- a % b
-- stupid C mod doesn't treat negative numbers as you'd hope
terra fixedModulus(a : int,b : int)
  while a < 0 do a = a+b end
  return a % b
end

