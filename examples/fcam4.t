import "orion"

package.path = package.path..";./fcam4/?.t"
terralib.require("settings")
terralib.require("blc")
terralib.require("lsc")
terralib.require("dpc")
terralib.require("xtk")
terralib.require("gcc")
terralib.require("wbg")
terralib.require("dem")
terralib.require("den")
terralib.require("cnr")
terralib.require("srp")
terralib.require("ccm")
terralib.require("ccf")
terralib.require("ccb")

C = terralib.includecstring [[
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

--W = 3264
--H = 2448
W = 2592
H = 1968

settings.ct8 = {}
settings.ct8.inBitRes = 10
settings.ct8.outBitRes = 8

function ct8(inImg)
  return im ct8(x,y) : uint8[3]
    {
      orion.min( ((inImg(x,y)[2] << settings.ct8.outBitRes) + (1 << (settings.ct8.inBitRes-1))) >> settings.ct8.inBitRes, (1 << settings.ct8.outBitRes)-1),
      orion.min( ((inImg(x,y)[1] << settings.ct8.outBitRes) + (1 << (settings.ct8.inBitRes-1))) >> settings.ct8.inBitRes, (1 << settings.ct8.outBitRes)-1),
      orion.min( ((inImg(x,y)[0] << settings.ct8.outBitRes) + (1 << (settings.ct8.inBitRes-1))) >> settings.ct8.inBitRes, (1 << settings.ct8.outBitRes)-1)
    }
  end

end

function makeCampipe(tmp, settings)
  tmp = blc(tmp)
--  tmp = lin(tmp) -- unused
--  tmp = lsc(tmp,W,H) -- a few bits are off in the LSC block
  tmp = dpc(tmp)
  tmp = xtk(tmp) -- modified this slightly to get rid of float
  tmp = den(tmp,W,H)
  tmp = wbg(tmp)
  tmp = dem(tmp)
--  tmp = srp(tmp,W,H)
  tmp = ccm(tmp)
--  tmp = gcc(tmp)
--  tmp = ccf(tmp)
--  tmp = cnr(tmp,W,H)
--  tmp = ccb(tmp)
  tmp = ct8(tmp)
  
  return tmp
end

terra convertRaw()
  cstdio.printf("CONVERT RAW\n")
  var img : Image
  img:initWithRaw("n900.pgm",W,H,10,17,true)
  img:save("n900.jjm")
end
--convertRaw()

--sensor = orion.loadRaw("img3.raw", W, H, 10)
sensor = orion.loadRaw("n900.pgm", W, H, 10, 17, true)
--sensor = orion.load("n900.jjm")
--sensor = orion.load("../siggraph14/fcam_raw/dog_raw.pgm")
--sensor = orion.loadRaw("../siggraph14/fcam_raw/dog_raw.pgm", W, H, 10, 17, true)
--sensor = orion.loadRaw("../siggraph14/fcam_raw/n900dngs/photo2011.11.10_15.23.26.42.pgm", W, H, 10, 17, true)
--sensor = orion.loadRaw("../siggraph14/fcam_raw/n900dngs/photo2012.05.29_19.03.14.22.pgm", W, H, 10, 17, true)

campipe = makeCampipe(sensor, settings)

if arg[1]=="conv" then
  local yfile,yrfile = orion.compile({campipe},{verbose=false,debug=false,platform="convolution",printstage=true,fastmath=true})
  
  local file = io.open("out/fcam4.yml","w")
  file:write(yfile)
  file:close()
  
  local rfile = io.open("out/fcam4_run.yml","w")
  rfile:write(yrfile)
  rfile:close()
  
else
  local runit,model = orion.compile({campipe},{verbose=false,debug=false,debugimages=false,printruntime=true,printstage=true,fastmath=true,calcPerfModel=false,printoptimal=true,schedule="linebufferall",fastPerfModel=false})

  local model = {workingSet=0,total=0}
  print("WS",model.workingSet)

  local benchmarkIter = 2

  terra run()
    var start = C.CurrentTimeInSecondsT()
    var res = runit()

    for i=1,benchmarkIter do
      res = runit()
    end
    var endt = C.CurrentTimeInSecondsT()

    C.printf("runtime %f model %f mp/s %f",(endt-start)/double(benchmarkIter),[model.total],double(5.1*benchmarkIter)/double(endt-start))

    if true then
      res:flip() -- noy's loadRaw function loads Y flipped of what we expect
      res:save("out/fcam4.bmp")
    else
      res:saveRaw("out/fcam4.raw",10)
    end
  end

  run()

end