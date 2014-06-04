import "orion"
local C = terralib.includecstring [[
    #include "loadcv.h"
    #include <stdio.h>
    #include <stdlib.h>
    #include <math.h>
]]

local function resize(i,o)
    os.execute(string.format("convert -resize 720x480 %s BMP3:%s",i,o))
end
local function  getpair(img)
    local l,r = "temp_l.bmp", "temp_r.bmp"
    os.execute("rm -f tmp.jpg")
    os.execute(string.format("exiftool '%s' -mpimage2 -b > tmp.jpg",img))
    resize("tmp.jpg",l)
    os.execute("rm -f tmp.jpg")
    os.execute(string.format("exiftool -trailer:all= '%s' -o tmp.jpg",img))
    resize("tmp.jpg",r)
    os.execute("rm -f tmp.jpg")
    return l,r
end

local W,H = 720,405

terralib.loadfile("../../examples/common.t")()

function clamp(i)
    return im(x,y) : int8 if i > 127 then 127 else (if i < -128 then -128 else i end) end end
end 
function RectifyImage(img,remap,height)
--[[
    double _x = i*ir[1] + ir[2] + j * ir[0];
    double _y = i*ir[4] + ir[5] + j * ir[3];
    double _w = i*ir[7] + ir[8] + j * ir[6];
    double w = 1./_w, x = _x*w, y = _y*w;
    double x2 = x*x, y2 = y*y;
    double r2 = x2 + y2, _2xy = 2*x*y;
    double kr = (1 + ((k3*r2 + k2)*r2 + k1)*r2)/(1 + ((k6*r2 + k5)*r2 + k4)*r2);
    *u = fx*(x*kr + p1*_2xy + p2*(r2 + 2*x2)) + u0;
    *v = fy*(y*kr + p1*(r2 + 2*y2) + p2*_2xy) + v0;
]]
   --[[ local ir0,ir1,ir2,ir3,ir4,ir5,ir6,ir7,ir8 = p.ir0,p.ir1,p.ir2,p.ir3,p.ir4,p.ir5,p.ir6,p.ir7,p.ir8
    local k1,k2,p1,p2,k3,k4,k5,k6,u0,v0,fx,fy = p.k1,p.k2,p.p1,p.p2,p.k3,p.k4,p.k5,p.k6,p.u0,p.v0,p.fx,p.fy
    local _x = im(j,i) (height-i-1)*ir1 + ir2 + j*ir0 end
    local _y = im(j,i) (height-i-1)*ir4 + ir5 + j*ir3 end
    local _w = im(j,i) (height-i-1)*ir7 + ir8 + j*ir6 end
    local w  = im(j,i) 1/_w end
    local x,y = im(j,i) _x*w end, im(j,i) _y*w end 
    local x2 = im(j,i) x * x end
    local y2 = im(j,i) y * y end
    local r2 = im(j,i) x2 + y2 end
    local _2xy = im(j,i) 2*x*y end
    local kr = im(j,i)  (1 + ((k3*r2 + k2)*r2 + k1)*r2)/(1 + ((k6*r2 + k5)*r2 + k4)*r2) end
    local u = im(j,i) fx*(x*kr + p1*_2xy + p2*(r2 + 2*x2)) + u0 end
    local v = im(j,i) fy*(y*kr + p1*(r2 + 2*y2) + p2*_2xy) + v0 end
    local u2 = im(j,i) 8*(u - j) end
    local v2 = im(j,i) 8*((height-i-1) - v)  end ]]
    
    local u = im(j,i) : int8 remap[0] - 128 end
    local v = im(j,i) : int8 remap[1] - 128 end
    local r = im(j,i) [resampleBilinearInt(true,img,orion.type.uint(8),8,8, u,v)] end 
    
    --local u2 = im(j,i) (u - j) end
    --local v2 = im(j,i) ((height-i-1) - v)  end
    --local r = im(j,i) [resampleBilinear(true,img,16,16,u2,v2)] end 
    
    return im(j,i) : crop(0,0,720,405), float32[2] {u,v} end, r
end

local function LoadImage(remap)
    local inputImage = orion.image(orion.type.array(orion.type.uint(8),3),720,405)
    local inputImageR = im(x,y) : uint8 inputImage[1] end
    local idxs,r = RectifyImage(inputImageR,remap,H)
    return r,inputImage
end

function makeOF(searchRadius,windowRadius,frame1, frame2)
  frame1 = im(x,y) : int32 frame1 end
  frame2 = im(x,y) : int32 frame2 end
  local SAD = {}
  for i = 0, 2*searchRadius do
      local tmp = {}
      for ii=-windowRadius, windowRadius do
        for jj=-windowRadius, windowRadius do
          table.insert(tmp, im(x,y) : cropNone orion.abs(frame1(x+ii,y+jj)-frame2(x+[i+ii],y+jj)) end)
        end
      end
      table.insert(SAD,orion.sum(tmp[1], unpack(tmp)))
  end

  local theMin = orion.min(SAD[1],unpack(SAD))
  print(#SAD)
  local coordCheck = {}
  local count = 0
  for i = 0,2*searchRadius do
    local isme = im(x,y) : cropNone if [SAD[#coordCheck + 1]] == theMin then i else 2*searchRadius + 1 end end
    table.insert(coordCheck, isme)
    count = count + 1
  end

  local offset = im(x,y) [orion.min(coordCheck[1],unpack(coordCheck))] end
  return im(x,y) : uint8 offset * 255.0/count end
end

local left,leftIn = LoadImage(orion.load("left-remap.bmp"))
local right,rightIn = LoadImage(orion.load("right-remap.bmp"))
local vectors = makeOF(40,4,left,right)

--local vectors  = im(x,y) (left + right)/2 end


terralib.loadfile("../movie.t")()

--vectors:save("lkoutput.bmp")
--left:save("left.bmp")
--right:save("right.bmp")
local fn = orion.compile({vectors},{schedule="materialize",printstage=true})
renderMovie(fn,{{leftIn:id(),"movie/right"},{rightIn:id(),"movie/left"}},"movie2/out")


if false then
    idxs:save("output.flo")
    local bsize =W*H*4*2+12
    (terra()
        var buf  =[&uint8](C.malloc(bsize))
        var f = C.fopen("output.flo","r")
        C.fread(buf,bsize,1,f)
        var s = [&float](&buf[12])
        var maxu,maxv = -1000.0,-1000.0
        var minu,minv = 1000.0,1000.0
        for i = 0,10 do
        for j = 0,10 do
            var u,v = s[i*2*W+2*j],s[i*2*W+2*j+1]
            maxu = C.fmax(u,maxu)
            minu = C.fmin(u,minu)
            maxv = C.fmax(u,maxv)
            minv = C.fmin(u,minv)
            
         if (i == 1 and j == 0 or i == 9 and j == 9) then
            C.printf("%d %d %f %f\n",i,j,u,v)
         end
        end end
        C.printf("%f %f %f %f\n",minu,maxu,minv,maxv)
    end)()
end