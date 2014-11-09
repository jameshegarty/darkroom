import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")
fpga = terralib.require("fpga")

-- simple, totally fixed function camera pipeline
function blackLevel( in1, pedestal )
  local rescale = math.floor((255/(255-pedestal))*255)
  return im(x,y) 
    shifted = darkroom.max([uint8](pedestal),in1)-[uint8](pedestal)
    in [uint8]((  [uint16](shifted)  * [uint16](rescale)  ) >> [uint16](8))
  end
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
    r_tr = (r_tr_a+r_tr_b) >> [uint8](1)

    r_bl_a = in1(x-1,y)
    r_bl_b = in1(x+1,y)
    r_bl = (r_bl_a+r_bl_b) >> [uint8](1)
    
    r_tl_a = in1(x+1,y+1)
    r_tl_b = in1(x-1,y-1)
    r_tl_c = in1(x-1,y+1)
    r_tl_d = in1(x+1,y-1)
    r_tl = (r_tl_a+r_tl_b+r_tl_c+r_tl_d) >> [uint8](2)

    phase = {x+xoff,y+yoff} and 1
    out_r = if phase[0]==1 and phase[1]==1 then in1(x,y) else
        if phase[0]==0 and phase[1]==1 then r_bl else
        if phase[0]==1 and phase[1]==0 then r_tr else
        r_tl end end end

    -- build green
    g_tl_a = in1(x+1,y)
    g_tl_b = in1(x-1,y)
    g_tl_c = in1(x,y+1)
    g_tl_d = in1(x,y-1)
    g_tl = ( g_tl_a + g_tl_b + g_tl_c + g_tl_d ) >> [uint8](2)


    g_br_a = in1(x+1,y)
    g_br_b = in1(x-1,y)
    g_br_c = in1(x,y+1)
    g_br_d = in1(x,y-1)
    g_br = (g_br_a+g_br_b+g_br_c+g_br_d) >> [uint8](2)

    out_g = if phase[0]==0 and phase[1]==0 then g_tl else
      if phase[0]==1 and phase[1]==1 then g_br else
        in1(x,y) end end

    -- build blue
    b_bl_a = in1(x,y+1)
    b_bl_b = in1(x,y-1)
    b_bl = (b_bl_a+b_bl_b) >> [uint8](1)

    b_tr_a = in1(x-1,y)
    b_tr_b = in1(x+1,y)
    b_tr = (b_tr_a+b_tr_b) >> [uint8](1)
    
    b_br_a = in1(x+1,y+1)
    b_br_b = in1(x-1,y-1)
    b_br_c = in1(x-1,y+1)
    b_br_d = in1(x+1,y-1)
    b_br = (b_br_a+b_br_b+b_br_c+b_br_d) >> [uint8](2)

    out_b = if phase[0]==0 and phase[1]==0 then in1(x,y) else
        if phase[0]==0 and phase[1]==1 then b_bl else
        if phase[0]==1 and phase[1]==0 then b_tr else
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
  [uint8[3]]({ darkroom.sum([uint16](in1[0])*[uint16]([ccm[1][1]]),[uint16](in1[1])*[uint16]([ccm[1][2]]),[uint16](in1[2])*[uint16]([ccm[1][3]])) ,
darkroom.sum([uint16](in1[0])*[uint16]([ccm[2][1]]),[uint16](in1[1])*[uint16]([ccm[2][2]]),[uint16](in1[2])*[uint16]([ccm[2][3]])) ,
darkroom.sum([uint16](in1[0])*[uint16]([ccm[3][1]]),[uint16](in1[1])*[uint16]([ccm[3][2]]),[uint16](in1[2])*[uint16]([ccm[3][3]])) 
} >> 6)
  end
end

function campipe(in1)
  local out = blackLevel(in1, 10)
  out = bilinearDemosaic(out)
  out = doccm(out)

--  return im(x,y) [uint8[3]]( darkroom.vectorSelect(out>255,[uint8[3]](255),out) ) end
  return im(x,y) [uint8[3]]( darkroom.vectorSelect(out>[uint8](255),[uint8[3]](255),out) ) end
end

sensor = darkroomSimple.load("300d.bmp")
campipeline = campipe(sensor)

campipeline:save("out/demosaic.bmp")

print("Build For: "..arg[1])
local v, metadata = fpga.compile({{sensor,"uart","300d.bmp"}},{{campipeline,"uart"}}, 128,64, fpga.util.deviceToOptions(arg[1]))

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/"..s..".metadata.lua", metadata)

--------------
local opt = fpga.util.deviceToOptions(arg[1])
opt.stripWidth=128
opt.stripHeight=64
local v, metadata = fpga.compile({{sensor,"sim","300d.raw"}},{{campipeline,"sim"}}, 128,64, opt)

local s = string.sub(arg[0],1,#arg[0]-2)
io.output("out/"..s..".sim.v")
io.write(v)
io.close()

fpga.util.writeMetadata("out/"..s..".sim.metadata.lua", metadata)