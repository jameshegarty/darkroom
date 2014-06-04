

import "orion"

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


local _CONVGEN = os.getenv("CONVGEN")
if not _CONVGEN then
   _CONVGEN = "/horowitz/users/jbrunhav/ConvEngine/Conv-Gen8/Stanford-SEEC-Convolution-Engine/HW"
end

local argSet = {}

terralib.require "movie"
-- Add run wrapper to path 
--terralib.require "buildRun"
--terralib.require "getOpt" 


--sensor = orion.load(arg[1])
sensor = orion.image(orion.type.array(orion.type.uint(8),3),1920,1080)
--sensor:save( "o/inp.bmp" )

if arg[1]=="benchmark" then
  W = 4096
  H = 4096
  sensor = orion.image(orion.type.array(orion.type.uint(8),3),W,H)

  (terra()
     var c = arrayof(uint8,128,128,128)
     orion.bindConstantArray3Uint8([sensor:id()],W,H,c)
  end)()

end

-- #define clip(x, a, b)   (min2(max2((x), (a)), (b)))
function clip(thisast,inp,minV,maxV)
  assert(orion.ast.isAST(inp))
  assert(orion.ast.isAST(minV))
  assert(orion.ast.isAST(maxV))
  return orion.min(thisast,orion.max(thisast,inp,minV),maxV)
end

-- 6 ops 
function convertToIllum( T , in1 )
   return im convertToIllum(x,y) : cropSame , uint16 clip( ( in1(x,y)[0]*T[0] + in1(x,y)[1]*T[1] + in1(x,y)[2]*T[2] ) , 0 , 32767 ) end 
end

-- 7 ops
function convolve_1_5_( T,R , in1 )
   -- TODO -> Convert T to tap 
   local im upCast(x,y) :        cropNone , int32 in1(x,y) end 
   local im convolve_1_5_(x,y) : cropNone , int32 clip( (orion.sum( in1(x,y-2)*T[0],in1(x,y-1)*T[1],in1(x,y)*T[2],in1(x,y+1)*T[3],in1(x,y+2)*T[4] ) >> 8 ) , -32767 , 32767 ) end
   local im downCast(x,y) :      cropSame , int16 convolve_1_5_(x,y) end 
   return downCast
end
-- 7 ops
function convolve_5_1_( T,R , in1 )
   -- TODO -> Convert T to tap 
   local im upCast(x,y) :        cropNone , int32 in1(x,y) end 
   local im convolve_1_5_(x,y) : cropNone , int32 clip( (orion.sum( in1(x-2,y)*T[0],in1(x-1,y)*T[1],in1(x,y)*T[2],in1(x+1,y)*T[3],in1(x+2,y)*T[4] ) >> 8 ) , -32767 , 32767 ) end
   local im downCast(x,y) :      cropSame , int16 convolve_1_5_(x,y) end 
   return downCast
end

-- 18
function sobel( R, in0 )
   -- TODO -> Convert sobel mask to taps 
   local im in1(x,y)  : cropNone , int32 in0(x,y) end
   local im in1y(x,y) : cropNone , int32 clip( orion.sum( -in1(x-1,y-1),-(in1(x,y-1)<<1),-in1(x+1,y-1),in1(x-1,y+1),in1(x,y+1)<<1,in1(x+1,y+1) ) , -32767 , 32767 ) end -- Sobel Y
   local im in1x(x,y) : cropNone , int32 clip( orion.sum( -in1(x-1,y-1),-(in1(x-1,y)<<1),-in1(x-1,y+1),in1(x+1,y-1),in1(x+1,y)<<1,in1(x+1,y+1) ) , -32767 , 32767 ) end -- Sobel X
   local im in1M(x,y) : cropNone , int32 clip( ( in1x(x,y)*in1x(x,y) + in1y(x,y)*in1y(x,y) ) >> 16 , -32767 , 32767 )   end -- Sx^2 + Sy^2
   return im sobel(x,y) : cropSame, int16[3] { in1x(x,y) , in1y(x,y) , in1M(x,y) } end
end

--40sih
function  nms(  T , P1 , P2 , in1 )
   -- 
   --
   
   local tan22 =  math.floor(0.404 * math.pow( 2 , T ))
   local tan66 =  math.floor(2.47  * math.pow( 2 , T ))

   -- W  202 ,  158 , ( -1 ,  0 )    0.404 * -x >  y > -0.404 * -x
   -- NW 158 ,  112 , ( -1 ,  1 )    2.47  * -x >  y >  0.404 * -x 
   -- N  112 ,   66 , (  0 ,  1 )    y > 2.47 * -x or y > 2.47 * x
   -- NE  66 ,   22 , (  1 ,  1 )    2.47  * x >  y >  0.404 * x 
   -- E   22 ,  -22 , (  1 ,  0 )    0.404 * x >  y > -0.404 * x   
   -- SE -22 ,  -66 , (  1 , -1 )   -0.404 * x >  y > -2.47  * x
   -- S  -66 , -112 , (  0 , -1 )    y > -2.47 * -x or y > -2.47 * x
   -- SW -112 , -156 , ( -1 , -1 )   -2.47  * -x >  y >  -0.404 * -x 

   im in1(x,y) : cropNone ,  int32[3] ( { in1(x,y)[0] , in1(x,y)[1]<<T , in1(x,y)[2]  }) end

   local im dir_W(x,y)  : cropNone , bool (  tan22 * -in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan22 * -in1(x,y)[0]) end 
   local im dir_NW (x,y): cropNone , bool (  tan66 * -in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] >  tan22 * -in1(x,y)[0]) end 
   local im dir_NE(x,y) : cropNone , bool (  tan66 *  in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] >  tan22 *  in1(x,y)[0]) end 
   local im dir_E(x,y)  : cropNone , bool (  tan22 *  in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan22 *  in1(x,y)[0]) end 
   local im dir_SE(x,y) : cropNone , bool ( -tan22 *  in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan66 *  in1(x,y)[0]) end 
   local im dir_SW(x,y) : cropNone , bool ( -tan66 * -in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan22 * -in1(x,y)[0]) end 

   local im dir_S(x,y)  : cropNone , bool ( in1(x,y)[1] < 0 and ( in1(x,y)[1] <  tan66 *  in1(x,y)[0]  or in1(x,y)[1] < -tan66 * in1(x,y)[0] ) ) end 
   local im dir_N(x,y)  : cropNone , bool ( in1(x,y)[1] > 0 and ( in1(x,y)[1] >  tan66 * -in1(x,y)[0]  or in1(x,y)[1] >  tan66 * in1(x,y)[0] ) ) end 

   local im dir_N_S(x,y)    : cropNone , bool ( dir_S(x,y)  or dir_N(x,y) ) end
   local im dir_E_W(x,y)    : cropNone , bool ( dir_E(x,y)  or dir_W(x,y) ) end
   local im dir_NE_SW(x,y)  : cropNone , bool ( dir_SW(x,y) or dir_NE(x,y) ) end
   local im dir_SE_NW(x,y)  : cropNone , bool ( dir_SE(x,y) or dir_NW(x,y) ) end

   local im dir_N_S_peak(x,y)   : cropNone , bool (  in1(x,y)[2] > in1(x+1,y  )[2] and in1(x,y)[2] >= in1(x-1,y  )[2] and dir_E_W(x,y)   ) end
   local im dir_E_W_peak(x,y)   : cropNone , bool (  in1(x,y)[2] > in1(x  ,y+1)[2] and in1(x,y)[2] >= in1(x  ,y-1)[2] and dir_N_S(x,y)   ) end
   local im dir_NE_SW_peak(x,y) : cropNone , bool (  in1(x,y)[2] > in1(x-1,y-1)[2] and in1(x,y)[2] >= in1(x+1,y+1)[2] and dir_NE_SW(x,y) ) end
   local im dir_SE_NW_peak(x,y) : cropNone , bool (  in1(x,y)[2] > in1(x+1,y-1)[2] and in1(x,y)[2] >= in1(x-1,y+1)[2] and dir_SE_NW(x,y) ) end

   local im peak(x,y) : cropNone , bool ( dir_N_S_peak(x,y) or dir_E_W_peak(x,y) or dir_NE_SW_peak(x,y) or dir_SE_NW_peak(x,y) ) end 

   local im true_peak(x,y) : cropNone , bool ( (in1(x,y)[2] > P1) and peak(x,y) ) end
   local im hyst_peak(x,y) : cropNone , bool ( (in1(x,y)[2] > P2) and peak(x,y) ) end

   return im nms(x,y) : cropSame , bool[2]  { true_peak(x,y) , hyst_peak(x,y) } end
end

-- 30 ops in total
--function hyst( in1P , in1H , both )
function hyst( in1 , both )
  
      im in1(x,y) : cropNone , bool[2]{ in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) : cropNone , bool[2]{ in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) : cropNone , bool[2]{ in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) : cropNone , bool[2]{ in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) : cropSame , bool[2]{ in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
                                                or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                     in1(x,y)[1] or false }end
   return in1 
end

function convertToChar( T , in1 )
   return im(x,y) : uint8 (in1(x,y) >> T) end
end

function convertTo3Char( T , in1  )
   return im(x,y) : uint8[3] ( in1(x,y) >> T ) end
end


function convertToPeakImage( in1P , in1H )
   return im(x,y) : uint8[3] ( { if in1P(x,y) then 255 else 0 end ,  if in1H(x,y) then 255 else 0 end , 0 }) end
   --return im(x,y) : uint8[3]   ({ in1(x,y)[0] ,  in1(x,y)[1] , 0 }) end
   --return im(x,y) : uint8  if in1P(x,y) then 255 else 0 end end
end
function convertToPeakImage3( in1 )
   return im convertToPeakImage3(x,y) : cropSame,uint8[3] ( { if in1(x,y)[0] then 255 else 0 end ,  if in1(x,y)[1] then 255 else 0 end , 0 }) end
   --return im(x,y) : uint8[3]   ({ in1(x,y)[0] ,  in1(x,y)[1] , 0 }) end
   --return im(x,y) : uint8  if in1P(x,y) then 255 else 0 end end
end
function convertToPeakImage2( in1P )
   --return im(x,y) : uint8[3] ( { if in1P(x,y) then 255 else 0 end ,  if in1H(x,y) then 255 else 0 end , 0 }) end
   --return im(x,y) : uint8[3]   ({ in1(x,y)[0] ,  in1(x,y)[1] , 0 }) end
   return im(x,y) : uint8  if in1P(x,y) then 255 else 0 end end
end


function canny(in1)
   
   local I  = { 54 , 183 , 18 } 
   local G  = {14 , 62 , 104 , 62 , 14}
   local Gh = {{14 , 62 , 104 , 62 , 14}}
   local Gv = {{14},{62},{104},{62},{14}}
   local N  = 10

   local in1P,in1H

   local P = orion.tap(orion.type.int(32), "Peak")
   local H = orion.tap(orion.type.int(32), "Hyst")
   orion.setTap( P , 3000 )
   orion.setTap( H , 1000 ) 


   in1 = convertToIllum( I , in1 )            -- Convert to illuminance, Implied up shift by 8 bits 
   in1 = convolve_1_5_( G , 8 , in1 ) -- 1x5 Gauss
   in1 = convolve_5_1_( G , 8 , in1 ) -- 5x1 Gauss
   in1 = sobel( 8 , in1 )                     -- Sobel
   in1 = nms( 8 , P , H , in1 )  -- Non Maximal Suppression

   for i=1,(N/5)-1 do 
      in1 = hyst( in1 , true )
   end

   --Convert to single kernel that calculates magnitude as well

   -- in1 = convertToChar( 8 , in1 )  -- Down shift by 8 bits and convert to uint8
   -- in1 = convertTo3Char( 8 , in1_t )
   -- in1 = convertToPeakImage( in1P , in1H  )
   -- in1 = convertToPeakImage2( in1P )
   -- in1 = convertToPeakImage2( in1 )

   in1 = convertToPeakImage3( in1 )

   return in1 
end

if arg[1]=="conv" then
  local yfile, yrfile = orion.compile( {canny(sensor)}, 
                                       {platform="convolution", 
                                        debug=true, 
                                        calcPerfModel=false,
                                        straighten=false})

  local file = io.open("out/canny.yml","w")
  file:write(yfile)
  file:close()
  
  local rfile = io.open("out/canny_run.yml","w")
  rfile:write(yrfile)
  rfile:close()
else

local cannyfn,model = orion.compile({canny(sensor)},{printoptimal=true,calcPerfModel=true,schedule="linebufferall",debug=false})

print("WS",model.workingSet)

if arg[1]=="benchmark" then
   local runIter = 10
   (terra()
       var start = C.CurrentTimeInSecondsT()
       var res = cannyfn()
       for i=1,runIter do
          var res = cannyfn()
       end
       var endt = C.CurrentTimeInSecondsT()

       C.printf("runtime %f model %f mp/s %f",(endt-start)/double(runIter), [model.total], double(16*runIter)/double(endt-start) )
    end)()
else
renderMovie(cannyfn,{{sensor:id(),"canny/canny"}},"out/canny/canny")
end

--cannyfn():save("out/canny.bmp")
--buildRun(  canny(sensor) , opts["i"]  )