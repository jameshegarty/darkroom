import "orion"

terralib.require "common"

searchRadius = 2
windowRadius = 2

searchRadiusFine = 1
windowRadiusFine = 1

clamp = true
maxResampleX = 10
maxResampleY = 10

local C = terralib.includecstring [[
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <inttypes.h>

double CurrentTimeInSecondsT() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + tv.tv_usec / 1000000.0;
}

]]

function makeOF(frame1, frame2)
  assert(orion.ast.isAST(frame1))
  assert(orion.ast.isAST(frame2))

  local SAD = {}

  for i=-searchRadius,searchRadius do
    for j=-searchRadius,searchRadius do
      local idx = (i+searchRadius)*(searchRadius*2+1)+(j+searchRadius)+1
--      SAD[idx] = im(x,y):cropNone
--        map ii=-windowRadius,windowRadius jj=-windowRadius, windowRadius reduce(sum)
--        orion.abs(frame1(x+i+ii,y+j+jj)-frame2(x+ii,y+ii))
      --        end


      local tmp = {}
      for ii=-windowRadius, windowRadius do
        for jj=-windowRadius, windowRadius do
          table.insert(tmp, im(x,y) : cropNone orion.abs(frame1(x+ii,y+jj)-frame2(x+[i+ii],y+[j+jj])) end)
        end
      end

      SAD[idx] = orion.sum(tmp[1], unpack(tmp))
    end
  end

  local theMin = orion.min(SAD[1],unpack(SAD))

  local coordCheck = {}
  local coordCount = {}
  for i=-searchRadius, searchRadius do
    for j=-searchRadius, searchRadius do
      table.insert(coordCheck, im(x,y) : cropNone if [SAD[(i+searchRadius)*(searchRadius*2+1)+(j+searchRadius)+1]]==theMin(x,y) then {i,j} else {0,0} end end)
      table.insert(coordCount, im(x,y) : cropNone if [SAD[(i+searchRadius)*(searchRadius*2+1)+(j+searchRadius)+1]]==theMin(x,y) then {1,1} else {0,0} end end)
    end
  end

  local vectorField = im(x,y) [orion.sum(coordCheck[1],unpack(coordCheck))] end
  --local vectorField = im(x,y) [orion.sum(coordCount[1],unpack(coordCount))] end
  return {vectorField, im(x,y) : uint8[3] {(vectorField(x,y)[0])*50+128, (vectorField(x,y)[1])*50+128, 0} end}
--  return {vectorField, im(x,y) : uint8[3] {(vectorField(x,y)[0]), (vectorField(x,y)[1]), 0} end}
end

function OFFine(frame1, frame2, startVectorField)
  assert(orion.ast.isAST(frame1))
  assert(orion.ast.isAST(frame2))
  assert(orion.ast.isAST(startVectorField))

  local SAD = {}

  for i=-searchRadiusFine,searchRadiusFine do
    for j=-searchRadiusFine,searchRadiusFine do
      local idx = (i+searchRadiusFine)*(searchRadiusFine*2+1)+(j+searchRadiusFine)+1

      local tmp = {}
      for ii=-windowRadiusFine, windowRadiusFine do
        for jj=-windowRadiusFine, windowRadiusFine do

          local im xcoord(x,y) : cropNone  startVectorField(x,y)[0]+(i/[searchRadiusFine+1])+ii end
          local im ycoord(x,y) : cropNone  startVectorField(x,y)[1]+(j/[searchRadiusFine+1])+jj end
      
          local G = im(x,y) [resampleBilinear(clamp,frame2,maxResampleX, maxResampleY,
                                          xcoord,
                                          ycoord)] end

          table.insert(tmp, im(x,y) : cropNone orion.abs(frame1(x+ii,y+jj)-G(x,y)) end)
        end
      end

      SAD[idx] = orion.sum(tmp[1], unpack(tmp))
    end
  end

  local theMin = orion.min(SAD[1],unpack(SAD))

  local coordCheck = {}
  local coordCount = {}
  for i=-searchRadiusFine, searchRadiusFine do
    for j=-searchRadiusFine, searchRadiusFine do
      table.insert(coordCheck, im(x,y) : cropNone if [SAD[(i+searchRadiusFine)*(searchRadiusFine*2+1)+(j+searchRadiusFine)+1]]==theMin(x,y) then {(i/[searchRadiusFine+1])+startVectorField(x,y)[0],(j/[searchRadiusFine+1])+startVectorField(x,y)[1]} else {0,0} end end)
    end
  end

  local vectorField = im(x,y) [orion.sum(coordCheck[1],unpack(coordCheck))] end
  --local vectorField = im(x,y) [orion.sum(coordCount[1],unpack(coordCount))] end
  return {vectorField, im(x,y) : uint8[3] {(vectorField(x,y)[0])*50+128, (vectorField(x,y)[1])*50+128, 0} end}
--  return {vectorField, im(x,y) : uint8[3] {(vectorField(x,y)[0]), (vectorField(x,y)[1]), 0} end}
end

local frame1 = orion.load("frame10.bmp")
local frame2 = orion.load("frame11.bmp")

if arg[1]=="conv" then
  local width = 1024
  local height = 1024

  frame1 = orion.image(orion.type.uint(8),width,height)
  frame2 = orion.image(orion.type.uint(8),width,height)
end

local frame1F = im(x,y):float32 frame1(x,y) end
local frame2F = im(x,y):float32 frame2(x,y) end
local kern = makeOF(frame1F, frame2F)
--kern = OFFine(frame1F, frame2F, kern[1])

if arg[1]=="conv" then
-- multiout not supported: only write out the flo version
-- of the results, not the human readable bmp version
local yfile,yrfile = orion.compile({kern[1]},
                                    {verbose=false,debug=false,platform="convolution"})
  
  local file = io.open("out/bruteof.yml","w")
  file:write(yfile)
  file:close()
  
  local rfile = io.open("out/bruteof_run.yml","w")
  rfile:write(yrfile)
  rfile:close()

  os.exit()
end


  
local ofFn, model = orion.compile(kern,
    {debug=false, 
     verbose=false, 
     calcPerfModel = false,
     printstage = true})

terra run()

  var start = C.CurrentTimeInSecondsT()
  var res,humanres = ofFn()
  var endt = C.CurrentTimeInSecondsT()

--  C.printf("runtime %f model %f fmodel %f\n",(endt-start)/double(1),model.total,model.fast.total)
  C.printf("runtime %f \n",(endt-start)/double(1))

  humanres:save(["out/bruteof.bmp"])
end

run()