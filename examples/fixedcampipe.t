import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")

-- simple, totally fixed function camera pipeline

-- input = uint8 nxm
-- output = RGB8 nxm
function bilinearDemosaic(in1)

  -- most sensors are bayer, but some of them might be offset by a pixel in x or y
  -- remember, the origin is at the bottom left, x,y increasing to the right
  local xoff = 1
  local yoff = 1

  return im dem(x,y)
    out = [uint8[3]](0)
    
    -- build the red channel
    r_tr_a = in1(x,y+1)
    r_tr_b = in1(x,y-1)
    r_tr = (r_tr_a+r_tr_b)/[uint8](2)

    r_bl_a = in1(x-1,y)
    r_bl_b = in1(x+1,y)
    r_bl = (r_bl_a+r_bl_b)/[uint8](2)
    
    r_tl_a = in1(x+1,y+1)
    r_tl_b = in1(x-1,y-1)
    r_tl_c = in1(x-1,y+1)
    r_tl_d = in1(x+1,y-1)
    r_tl = (r_tl_a+r_tl_b+r_tl_c+r_tl_d)/[uint8](4)

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
    g_tl = ( g_tl_a + g_tl_b + g_tl_c + g_tl_d )/[uint8](4)


    g_br_a = in1(x+1,y)
    g_br_b = in1(x-1,y)
    g_br_c = in1(x,y+1)
    g_br_d = in1(x,y-1)
    g_br = (g_br_a+g_br_b+g_br_c+g_br_d)/[uint8](4)

    out_g = if orion.arrayAnd(phase=={0,0}) then g_tl else
      if orion.arrayAnd(phase=={1,1}) then g_br else
        in1(x,y) end end

    -- build blue
    b_bl_a = in1(x,y+1)
    b_bl_b = in1(x,y-1)
    b_bl = (b_bl_a+b_bl_b)/[uint8](2)

    b_tr_a = in1(x-1,y)
    b_tr_b = in1(x+1,y)
    b_tr = (b_tr_a+b_tr_b)/[uint8](2)
    
    b_br_a = in1(x+1,y+1)
    b_br_b = in1(x-1,y-1)
    b_br_c = in1(x-1,y+1)
    b_br_d = in1(x+1,y-1)
    b_br = (b_br_a+b_br_b+b_br_c+b_br_d)/[uint8](4)

    out_b = if orion.arrayAnd(phase=={0,0}) then in1(x,y) else
      if orion.arrayAnd(phase=={0,1}) then b_bl else
      if orion.arrayAnd(phase=={1,0}) then b_tr else
        b_br end end end;

    {out_r, out_g, out_b}
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
  local im out(x,y) [float](in1(x,y)) end

  local out = bilinearDemosaic(out)
  out = doccm(out) 
  out = tonemap(out)
  return im(x,y) [uint8[3]]( out(x,y) ) end
end

sensor = darkroomSimple.load("300d.bmp")
campipeline = campipe(sensor)

campipeline:save("out/fixedcampipe.bmp")