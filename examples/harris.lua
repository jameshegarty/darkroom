import "darkroom"
local darkroomSimple = terralib.require "darkroomSimple"

sensor = darkroomSimple.load("color.bmp")

-- clip macro
-- #define clip(x, a, b)   (min2(max2((x), (a)), (b)))
function clip(thisast,inp,minV,maxV)
  assert(darkroom.ast.isAST(inp))
  assert(darkroom.ast.isAST(minV))
  assert(darkroom.ast.isAST(maxV))
  return darkroom.min(thisast,darkroom.max(thisast,inp,minV),maxV)
end


function convertToIllum( T , in1 )
  local im convertToIll(x,y) [uint16](clip( darkroom.sum( in1(x,y)[0]*T[0] , in1(x,y)[1]*T[1] , in1(x,y)[2]*T[2] ) , 0 , 32767 )) end 
  return convertToIll
end

function convolve_1_5_( T,R , in1 )
  -- TODO -> Convert T to tap 
  local im upCast(x,y) [int32](in1(x,y)) end 
  local im convolve(x,y)  [int32](clip( (darkroom.sum( in1(x,y-2)*T[0],in1(x,y-1)*T[1],in1(x,y)*T[2],in1(x,y+1)*T[3],in1(x,y+2)*T[4] ) >> 8 ) , -32767 , 32767 )) end
  local im conv_1_5_(x,y) darkroom.crop([int16](convolve(x,y))) end 
  return conv_1_5_
end
function convolve_5_1_( T,R , in1 )
  -- TODO -> Convert T to tap 
  local im upCast(x,y) [int32](in1(x,y)) end 
  local im convolve(x,y) [int32](clip( (darkroom.sum( in1(x-2,y)*T[0],in1(x-1,y)*T[1],in1(x,y)*T[2],in1(x+1,y)*T[3],in1(x+2,y)*T[4] ) >> 8 ) , -32767 , 32767 )) end
  local im conv_5_1_(x,y) darkroom.crop([int16](convolve(x,y))) end 
  return conv_5_1_
end

function sobel( R, in0 )
  -- TODO -> Convert sobel mask to taps 
  local im in1(x,y) [int32](in0(x,y)) end
  local im in1y(x,y) [int32](clip( darkroom.sum( -in1(x-1,y-1),-(in1(x,y-1)<<1),-in1(x+1,y-1),in1(x-1,y+1),in1(x,y+1)<<1,in1(x+1,y+1) ) , -32767 , 32767 )) end -- Sobel Y
  local im in1x(x,y)  [int32](clip( darkroom.sum( -in1(x-1,y-1),-(in1(x-1,y)<<1),-in1(x-1,y+1),in1(x+1,y-1),in1(x+1,y)<<1,in1(x+1,y+1) ) , -32767 , 32767 )) end -- Sobel X
  local im sobel(x,y) [int32[2]]({  in1x(x,y) , in1y(x,y) }) end
  return sobel 
end


function response( T , K , in1 )
  local im Wxx(x,y) [int32](clip( ((in1(x,y)[0] * in1(x,y)[0]) >> (16)) , -32767 , 32767 )) end 
  local im Wyy(x,y) [int32](clip( ((in1(x,y)[1] * in1(x,y)[1]) >> (16)) , -32767 , 32767 )) end 
  local im Wxy(x,y) [int32](clip( ((in1(x,y)[0] * in1(x,y)[1]) >> (16)) , -32767 , 32767 )) end 
  local im Det(x,y) [int32](((Wxx(x,y)*Wyy(x,y))>>1)-((Wxy(x,y)*Wxy(x,y))>>1)) end 
  local im TrSq(x,y) [int32](((Wxx(x,y)+Wyy(x,y))>>1)*((Wxx(x,y)+Wyy(x,y))>>1)>>15) end 
  local im Resp1(x,y) [int32](Det(x,y) - K*TrSq(x,y)) end 
  local im Resp(x,y) darkroom.crop([int16]( clip( Resp1(x,y) , -32767 , 32767 ) )) end
  return Resp
end



function  nms(  T , P1 , in1 )
  local im PN(x,y) [bool](in1(x,y) >  in1(x,y-1)) end 
  local im PS(x,y) [bool](in1(x,y) >= in1(x,y+1)) end 
  local im PE(x,y) [bool](in1(x,y) >  in1(x+1,y)) end 
  local im PW(x,y) [bool]( in1(x,y) >= in1(x-1,y)) end 
  local im P(x,y) [bool](in1(x,y) >  P1) end 
  local im Pk(x,y) [bool](  ( P(x,y) and PW(x,y) and PE(x,y) and PS(x,y) and PN(x,y) )) end
  local im NMS(x,y) darkroom.crop([uint8]( if Pk(x,y) then 255 else 0 end)) end
  return NMS 
end


function harris(in1)
   
   local P  = darkroomSimple.tap(int, 128)
   local K  = darkroomSimple.tap(int32, 1)

   local I  = { 54 , 183 , 18 } 
   local G  = {14 , 62 , 104 , 62 , 14}

   in1 = convertToIllum( I , in1 )    -- Convert to illuminance, Implied up shift by 8 bits 
   in1 = convolve_1_5_( G , 8 , in1 ) -- 1x5 Gauss
   in1 = convolve_5_1_( G , 8 , in1 ) -- 5x1 Gauss
   in1 = sobel( 8 , in1 )             -- Sobel
   in1 = response( 8 , K  , in1 )          -- Calc det/trace
   in1 = nms( 8 , P , in1 )           -- Non Maximal Suppression

   return in1 
end

harris(sensor):save("out/harris.bmp")
