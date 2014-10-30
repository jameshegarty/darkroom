darkroom={}
terralib.require("common")

function extract(filestring, start, endt)
  if filestring:find(start)==nil then return 0 end
  local dsps = explode(start,t)
  dsps = explode(endt,dsps[2])
  dsps = dsps[1]
  return tonumber(dsps)
end

io.input(arg[1])
t = io.read("*all")
local dsps = extract(t,"Number of DSP48E1s","out")
local brams = extract(t,"Number of RAMB18E1s","out")
local luts = extract(t,"Number of Slice LUTS","out")

io.output(arg[2])
io.write("{dsps="..dsps..", luts="..luts..", brams="..brams.."}")
io.close()