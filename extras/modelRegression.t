-- Lua implementation of PHP scandir function
function scandir(directory)
  local i, t, popen = 0, {}, io.popen
  for filename in popen('ls -a "'..directory..'"'):lines() do
        i = i + 1
        t[i] = filename
  end
    return t
end

local t = scandir(arg[1])

local res = {}
for k,v in pairs(t) do
  --print(v)
  if v:find(".actual.lua")~=nil then
    local n =v:sub(0,#v-15)
    print("A",v,n)
    res[n] = res[n] or {}
    local l = dofile(arg[1]..v)
    res[n].actualDsps = l.dsps
    res[n].actualLuts = l.luts
    res[n].actualBrams = l.brams
  elseif v:find(".est.lua")~=nil then
    local n = v:sub(0,#v-12)
    print("E",v,n)
    res[n] = res[n] or {}
    local l = dofile(arg[1]..v)
    res[n].estDsps = l.dsps or 0
    res[n].estLuts = l.luts or 0
    res[n].estBrams = l.brams or 0
  end
end

print("file, actualDsps, actualLuts, actualBrams, estDsps, estLuts, estBrams")
for k,v in pairs(res) do
  print(k..", "..v.actualDsps..", "..v.actualLuts..", "..v.actualBrams..", "..v.estDsps..", "..v.estLuts..", "..v.estBrams)
end