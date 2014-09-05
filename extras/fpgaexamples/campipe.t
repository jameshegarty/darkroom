import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
fpgaEstimate = terralib.require("fpgaEstimate")

-- simple, totally fixed function camera pipeline
function blackLevel( in1, pedestal )
  local rescale = math.floor((255/(255-pedestal))*255)
  return im(x,y) [uint8]((( [uint16](in1-pedestal) << 8)*rescale) >> 8) end
end

-- input = uint8 nxm
-- output = RGB8 nxm
function bilinearDemosaic(in1)

  -- most sensors are bayer, but some of them might be offset by a pixel in x or y
  -- remember, the origin is at the bottom left, x,y increasing to the right
  local xoff = 0
  local yoff = 0

  return im dem(x,y)
    out = [uint8[3]](0)
    
    -- build the red channel
    r_tr_a = in1(x,y+1)
    r_tr_b = in1(x,y-1)
    r_tr = (r_tr_a+r_tr_b) >> 1

    r_bl_a = in1(x-1,y)
    r_bl_b = in1(x+1,y)
    r_bl = (r_bl_a+r_bl_b) >> 1
    
    r_tl_a = in1(x+1,y+1)
    r_tl_b = in1(x-1,y-1)
    r_tl_c = in1(x-1,y+1)
    r_tl_d = in1(x+1,y-1)
    r_tl = (r_tl_a+r_tl_b+r_tl_c+r_tl_d) >> 2

    phase = {x+xoff,y+yoff} and 1
    out_r = if darkroom.arrayAnd(phase=={1,1}) then in1(x,y) else
      if darkroom.arrayAnd(phase=={0,1}) then r_bl else
      if darkroom.arrayAnd(phase=={1,0}) then r_tr else
      r_tl end end end

    -- build green
    g_tl_a = in1(x+1,y)
    g_tl_b = in1(x-1,y)
    g_tl_c = in1(x,y+1)
    g_tl_d = in1(x,y-1)
    g_tl = ( g_tl_a + g_tl_b + g_tl_c + g_tl_d ) >> 2


    g_br_a = in1(x+1,y)
    g_br_b = in1(x-1,y)
    g_br_c = in1(x,y+1)
    g_br_d = in1(x,y-1)
    g_br = (g_br_a+g_br_b+g_br_c+g_br_d) >> 2

    out_g = if darkroom.arrayAnd(phase=={0,0}) then g_tl else
      if darkroom.arrayAnd(phase=={1,1}) then g_br else
        in1(x,y) end end

    -- build blue
    b_bl_a = in1(x,y+1)
    b_bl_b = in1(x,y-1)
    b_bl = (b_bl_a+b_bl_b) >> 1

    b_tr_a = in1(x-1,y)
    b_tr_b = in1(x+1,y)
    b_tr = (b_tr_a+b_tr_b) >> 1
    
    b_br_a = in1(x+1,y+1)
    b_br_b = in1(x-1,y-1)
    b_br_c = in1(x-1,y+1)
    b_br_d = in1(x+1,y-1)
    b_br = (b_br_a+b_br_b+b_br_c+b_br_d) >> 2

    out_b = if darkroom.arrayAnd(phase=={0,0}) then in1(x,y) else
      if darkroom.arrayAnd(phase=={0,1}) then b_bl else
      if darkroom.arrayAnd(phase=={1,0}) then b_tr else
        b_br end end end;

    in {out_r, out_g, out_b}
  end
end

-- input = RGB8 demosaiced
-- output = RGB8 color/white balance corrected
ccm={ {math.floor((255/142)*255),0,0},
      {0, math.floor((196/255)*255), 0},
{0,0,255}}

function doccm(in1)
  return im(x,y)
  [uint8[3]]({ darkroom.dot( [uint16[3]](in1) << 8,[ccm[1]]),
            darkroom.dot( [uint16[3]](in1) << 8,[ccm[2]]),
            darkroom.dot( [uint16[3]](in1) << 8,[ccm[3]])} >> 8)
  end
end

function tonemap(in1, gamma)

  return im(x,y)
--  darkroom.pow((in1(x,y)/255),gamma)*255
  in1
  end

end


function campipe(in1)
  local out = blackLevel(in1, 10)
  out = bilinearDemosaic(out)
  out = doccm(out) 
  out = tonemap(out, 1/2.4)
  return im(x,y) [uint8[3]]( darkroom.vectorSelect(out>255,[uint8[3]](255),out) ) end
end

sensor = darkroomSimple.load("../../examples/300d.bmp")
campipeline = campipe(sensor)

campipeline:save("out/campipe.bmp")

local est = fpgaEstimate.compile({campipeline}, 1920)
io.output("out/campipeEstimate.txt")
io.write(est)
io.close()