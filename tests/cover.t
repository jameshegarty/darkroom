local cover = {} --require("coverageinfo")

exFile = arg[1] or "/Users/research/Documents/orion/src/terracompiler_codegen.t"

function exclude(str)
  local function trim(s)
    return s:match'^%s*(.*%S)' or ''
  end
  local s = trim(str)

  if s=="" or s=="end" or s:sub(1,2)=="--" then
    return true
  end

  return false
end

function scandir(directory)
  local i, t, popen = 0, {}, io.popen
  for filename in popen('ls -a "'..directory..'"'):lines() do
        i = i + 1
        t[i] = filename
  end
    return t
end

for k,v in pairs(scandir("out")) do
--  print(k,v, v:sub(1,12))
  if v:sub(1,12)=="coverageinfo" then
    local c = dofile("out/"..v)
    
    if c[exFile] then
      for kk,vv in pairs(c[exFile]) do
        if cover[kk]==nil then
          cover[kk] = vv
        else
          cover[kk] = cover[kk]+vv
        end
      end
    end
  end

end

io.input(exFile)

local ln = 1
local coveredCount = 0
for line in io.lines() do
--  table.insert(lines, line)
  local cov = cover[ln] or 0

  if cov==0 
    and exclude(line)==false
  then
    print(ln, cov, exclude(line), line)
  else
    coveredCount = coveredCount+1
  end

  ln = ln + 1
end

print("Covered %",coveredCount/ln)