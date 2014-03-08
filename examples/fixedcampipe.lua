
import "orion"

makeConv = false

if arg[1]=="conv" then
  makeConv = true
end

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

--

--package.path = package.path..";../src/?.lua;../src/?.t"

-- totally fixed function camera pipeline
-- I have no idea why you'd ever actually build something like this,
-- but it's available for testing...


-- input = uint8 nxm
-- output = RGB8 nxm
function bilinearDemosaic(in1)

  -- most sensors are bayer, but some of them might be offset by a pixel in x or y
  -- remember, the origin is at the bottom left, x,y increasing to the right
  local xoff = 1
  local yoff = 1

  return im(x,y)
    let
    out : rgb8 = 0
    
    -- build the red channel
    r_tr_a = in1(x,y+1)
    r_tr_b = in1(x,y-1)
    r_tr = (r_tr_a+r_tr_b)/orion.uint8(2)

    r_bl_a = in1(x-1,y)
    r_bl_b = in1(x+1,y)
    r_bl = (r_bl_a+r_bl_b)/orion.uint8(2)
    
    r_tl_a = in1(x+1,y+1)
    r_tl_b = in1(x-1,y-1)
    r_tl_c = in1(x-1,y+1)
    r_tl_d = in1(x+1,y-1)
    r_tl = (r_tl_a+r_tl_b+r_tl_c+r_tl_d)/orion.uint8(4)

    phase = {x+xoff,y+yoff}%2
    out_r = if orion.arrayAnd(phase=={1,1}) then in1(x,y) else
      if orion.arrayAnd(phase=={0,1}) then r_bl else
      if orion.arrayAnd(phase=={1,0}) then r_tr else
      r_tl end end end

    -- build green
    g_tl_a = in1(x+1,y)
    g_tl_b = in1(x-1,y)
    g_tl_c = in1(x,y+1)
    g_tl_d = in1(x,y-1)
    g_tl = ( g_tl_a + g_tl_b + g_tl_c + g_tl_d )/orion.uint8(4)


    g_br_a = in1(x+1,y)
    g_br_b = in1(x-1,y)
    g_br_c = in1(x,y+1)
    g_br_d = in1(x,y-1)
    g_br = (g_br_a+g_br_b+g_br_c+g_br_d)/orion.uint8(4)

    out_g = if orion.arrayAnd(phase=={0,0}) then g_tl else
      if orion.arrayAnd(phase=={1,1}) then g_br else
        in1(x,y) end end

    -- build blue
    b_bl_a = in1(x,y+1)
    b_bl_b = in1(x,y-1)
    b_bl = (b_bl_a+b_bl_b)/orion.uint8(2)

    b_tr_a = in1(x-1,y)
    b_tr_b = in1(x+1,y)
    b_tr = (b_tr_a+b_tr_b)/orion.uint8(2)
    
    b_br_a = in1(x+1,y+1)
    b_br_b = in1(x-1,y-1)
    b_br_c = in1(x-1,y+1)
    b_br_d = in1(x+1,y-1)
    b_br = (b_br_a+b_br_b+b_br_c+b_br_d)/orion.uint8(4)

    out_b = if orion.arrayAnd(phase=={0,0}) then in1(x,y) else
      if orion.arrayAnd(phase=={0,1}) then b_bl else
      if orion.arrayAnd(phase=={1,0}) then b_tr else
      b_br end end end

    in {out_r, out_g, out_b}
  end
end

-- input = RGB8 demosaiced
-- output = RGB8 color/white balance corrected
ccm={ {255/142,0,0},
{0, 196/255, 0},
{0,0,1}}

function doccm(in1)
  return im(x,y)
  { orion.dot(in1(x,y),[ccm[1]]),
    orion.dot(in1(x,y),[ccm[2]]),
    orion.dot(in1(x,y),[ccm[3]])}
  end
end

function tonemap(in1)

  return im(x,y)
  orion.pow((in1(x,y)/255),0.5)*255
  end

end


function campipe(in1)
  -- do the campipe using floats
  local im out(x,y) : float32, cropNone in1(x,y) end

  local out = bilinearDemosaic(out)
  out = doccm(out) 
  out = tonemap(out)
  return im(x,y) : uint8[3] out(x,y) end
end

sensor = orion.load("300d.bmp")
campipeline = campipe(sensor)

if makeConv then
  local yfile,yrfile = orion.compile({campipeline},{verbose=false,debug=false,platform="convolution"})

  local file = io.open("out/fixedcampipe.yml","w")
  file:write(yfile)
  file:close()
  
  local rfile = io.open("out/fixedcampipe_run.yml","w")
  rfile:write(yrfile)
  rfile:close()
else

  local sched = arg[1]

  local debug = false
  local verbose = false

  if sched==nil then
    debug = true
    verbose = true
  end

  local r, model = orion.compile({campipeline},{verbose=verbose,debug=debug,schedule=arg[1],calcPerfModel=false})

  local terra runit()
    var start = C.CurrentTimeInSecondsT()
    var res = r()
    for i=0,19 do
      res = r()
    end
    var endt = C.CurrentTimeInSecondsT()

    res:save("out/fixedcampipe.bmp")
--    C.printf("runtime %f model %f fmodel %f\n",(endt-start)/double(20),model.total,model.fast.total)
    C.printf("runtime %f\n",(endt-start)/double(20))
  end

  runit()
--  campipeline:save("out/fixedcampipe.bmp")
end