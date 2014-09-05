import "darkroom"
darkroomSimple = terralib.require("darkroomSimple")


-- #define clip(x, a, b)   (min2(max2((x), (a)), (b)))
function clip(thisast,inp,minV,maxV)
  assert(darkroom.ast.isAST(inp))
  assert(darkroom.ast.isAST(minV))
  assert(darkroom.ast.isAST(maxV))
  return darkroom.min(thisast,darkroom.max(thisast,inp,minV),maxV)
end

-- 6 ops 
function convertToIllum( T , in1 )
  return im convertToIllum(x,y) darkroom.crop([uint16](clip( ( in1(x,y)[0]*T[0] + in1(x,y)[1]*T[1] + in1(x,y)[2]*T[2] ) , 0 , 32767 ))) end 
end

-- 7 ops
function convolve_1_5_( T,R , in1 )
   -- TODO -> Convert T to tap 
  local im upCast(x,y) [int32](in1(x,y)) end 
  local im convolve_1_5_(x,y) [int32](clip( (darkroom.sum( in1(x,y-2)*T[0],in1(x,y-1)*T[1],in1(x,y)*T[2],in1(x,y+1)*T[3],in1(x,y+2)*T[4] ) >> 8 ) , -32767 , 32767 )) end
  local im downCast(x,y) darkroom.crop([int16](convolve_1_5_(x,y))) end 
   return downCast
end
-- 7 ops
function convolve_5_1_( T,R , in1 )
  -- TODO -> Convert T to tap 
  local im upCast(x,y) [int32](in1(x,y)) end 
  local im convolve_1_5_(x,y) [int32](clip( (darkroom.sum( in1(x-2,y)*T[0],in1(x-1,y)*T[1],in1(x,y)*T[2],in1(x+1,y)*T[3],in1(x+2,y)*T[4] ) >> 8 ) , -32767 , 32767 )) end
  local im downCast(x,y) darkroom.crop([int16](convolve_1_5_(x,y))) end 
  return downCast
end

-- 18
function sobel( R, in0 )
   -- TODO -> Convert sobel mask to taps 
  local im in1(x,y)  [int32](in0(x,y)) end
local im in1y(x,y) [int32](clip( darkroom.sum( -in1(x-1,y-1),-(in1(x,y-1)<<1),-in1(x+1,y-1),in1(x-1,y+1),in1(x,y+1)<<1,in1(x+1,y+1) ) , -32767 , 32767 )) end -- Sobel Y
local im in1x(x,y) [int32](clip( darkroom.sum( -in1(x-1,y-1),-(in1(x-1,y)<<1),-in1(x-1,y+1),in1(x+1,y-1),in1(x+1,y)<<1,in1(x+1,y+1) ) , -32767 , 32767 )) end -- Sobel X
local im in1M(x,y) [int32](clip( ( in1x(x,y)*in1x(x,y) + in1y(x,y)*in1y(x,y) ) >> 16 , -32767 , 32767 ))   end -- Sx^2 + Sy^2
return im sobel(x,y) darkroom.crop([int16[3]]({ in1x(x,y) , in1y(x,y) , in1M(x,y) })) end
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

   im in1(x,y) [int32[3]] ( { in1(x,y)[0] , in1(x,y)[1]<<T , in1(x,y)[2]  }) end

   local im dir_W(x,y)   (  tan22 * -in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan22 * -in1(x,y)[0]) end 
   local im dir_NW (x,y) (  tan66 * -in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] >  tan22 * -in1(x,y)[0]) end 
   local im dir_NE(x,y)  (  tan66 *  in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] >  tan22 *  in1(x,y)[0]) end 
   local im dir_E(x,y)   (  tan22 *  in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan22 *  in1(x,y)[0]) end 
   local im dir_SE(x,y)  ( -tan22 *  in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan66 *  in1(x,y)[0]) end 
   local im dir_SW(x,y)  ( -tan66 * -in1(x,y)[0]> in1(x,y)[1] and in1(x,y)[1] > -tan22 * -in1(x,y)[0]) end 

   local im dir_S(x,y)   ( in1(x,y)[1] < 0 and ( in1(x,y)[1] <  tan66 *  in1(x,y)[0]  or in1(x,y)[1] < -tan66 * in1(x,y)[0] ) ) end 
   local im dir_N(x,y)   ( in1(x,y)[1] > 0 and ( in1(x,y)[1] >  tan66 * -in1(x,y)[0]  or in1(x,y)[1] >  tan66 * in1(x,y)[0] ) ) end 

   local im dir_N_S(x,y)    ( dir_S(x,y)  or dir_N(x,y) ) end
   local im dir_E_W(x,y)    ( dir_E(x,y)  or dir_W(x,y) ) end
   local im dir_NE_SW(x,y)  ( dir_SW(x,y) or dir_NE(x,y) ) end
   local im dir_SE_NW(x,y)  ( dir_SE(x,y) or dir_NW(x,y) ) end

   local im dir_N_S_peak(x,y)    (  in1(x,y)[2] > in1(x+1,y  )[2] and in1(x,y)[2] >= in1(x-1,y  )[2] and dir_E_W(x,y)   ) end
   local im dir_E_W_peak(x,y)    (  in1(x,y)[2] > in1(x  ,y+1)[2] and in1(x,y)[2] >= in1(x  ,y-1)[2] and dir_N_S(x,y)   ) end
   local im dir_NE_SW_peak(x,y)  (  in1(x,y)[2] > in1(x-1,y-1)[2] and in1(x,y)[2] >= in1(x+1,y+1)[2] and dir_NE_SW(x,y) ) end
   local im dir_SE_NW_peak(x,y)  (  in1(x,y)[2] > in1(x+1,y-1)[2] and in1(x,y)[2] >= in1(x-1,y+1)[2] and dir_SE_NW(x,y) ) end

   local im peak(x,y)  ( dir_N_S_peak(x,y) or dir_E_W_peak(x,y) or dir_NE_SW_peak(x,y) or dir_SE_NW_peak(x,y) ) end 

   local im true_peak(x,y) ( (in1(x,y)[2] > P1) and peak(x,y) ) end
   local im hyst_peak(x,y)  ( (in1(x,y)[2] > P2) and peak(x,y) ) end

   return im nms(x,y) darkroom.crop([bool[2]]({ true_peak(x,y) , hyst_peak(x,y) })) end
end

-- 30 ops in total
--function hyst( in1P , in1H , both )
function hyst( in1 , both )
  
      im in1(x,y) { in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) { in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) { in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) { in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
	                                                  or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                        in1(x,y)[1] or false }end
      im in1(x,y) { in1(x,y)[1] and (    in1(x+1,y+1)[0] or in1(x+1,y  )[0] or in1(x+1,y-1)[0] 
                                                          or in1(x  ,y+1)[0] or in1(x  ,y-1)[0] or in1(x-1,y+1)[0] 
                                                or in1(x-1,y  )[0] or in1(x-1,y-1)[0] )  , 
                                     in1(x,y)[1] or false }end
   return in1 
end

function convertToChar( T , in1 )
  return im(x,y) [uint8](in1(x,y) >> T) end
end

function convertTo3Char( T , in1  )
  return im(x,y) [uint8[3]]( in1(x,y) >> T ) end
end


function convertToPeakImage3( in1 )
  return im convertToPeakImage3(x,y) darkroom.crop([uint8[3]] ( { if in1(x,y)[0] then 255 else 0 end ,  if in1(x,y)[1] then 255 else 0 end , 0 })) end
end


function canny(in1)
   
   local I  = { 54 , 183 , 18 } 
   local G  = {14 , 62 , 104 , 62 , 14}
   local Gh = {{14 , 62 , 104 , 62 , 14}}
   local Gv = {{14},{62},{104},{62},{14}}
   local N  = 10

   local in1P,in1H

   local P = darkroomSimple.tap( int32, 3000)
   local H = darkroomSimple.tap( int32, 1000)

   in1 = convertToIllum( I , in1 )            -- Convert to illuminance, Implied up shift by 8 bits 
   in1 = convolve_1_5_( G , 8 , in1 ) -- 1x5 Gauss
   in1 = convolve_5_1_( G , 8 , in1 ) -- 5x1 Gauss
   in1 = sobel( 8 , in1 )                     -- Sobel
   in1 = nms( 8 , P , H , in1 )  -- Non Maximal Suppression

   for i=1,(N/5)-1 do 
      in1 = hyst( in1 , true )
   end


   return convertToPeakImage3( in1 )
end

local pipe = canny(darkroomSimple.load("../../examples/color.bmp"))
pipe:save("out/canny.bmp")

fpgaEstimate = terralib.require("fpgaEstimate")
local est = fpgaEstimate.compile({pipe}, 1920)
io.output("out/cannyEstimate.txt")
io.write(est)
io.close()