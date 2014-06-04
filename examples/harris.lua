import "darkroom"

sensor = orion.image(orion.type.array(orion.type.uint(8),3),1920,1080)

-- #define clip(x, a, b)   (min2(max2((x), (a)), (b)))
function clip(thisast,inp,minV,maxV)
  assert(orion.ast.isAST(inp))
  assert(orion.ast.isAST(minV))
  assert(orion.ast.isAST(maxV))
  return orion.min(thisast,orion.max(thisast,inp,minV),maxV)
end


function convertToIllum( T , in1 )

   local im convertToIll(x,y) : cropSame , uint16 clip( orion.sum( in1(x,y)[0]*T[0] , in1(x,y)[1]*T[1] , in1(x,y)[2]*T[2] ) , 0 , 32767 ) end 
   return convertToIll

end

function convolve_1_5_( T,R , in1 )
   -- TODO -> Convert T to tap 
   local im upCast(x,y)    : cropNone , int32 in1(x,y) end 
   local im convolve(x,y)  : cropNone , int32 clip( (orion.sum( in1(x,y-2)*T[0],in1(x,y-1)*T[1],in1(x,y)*T[2],in1(x,y+1)*T[3],in1(x,y+2)*T[4] ) >> 8 ) , -32767 , 32767 ) end
   local im conv_1_5_(x,y) : cropSame , int16 convolve(x,y) end 
   return conv_1_5_
end
function convolve_5_1_( T,R , in1 )
   -- TODO -> Convert T to tap 
   local im upCast(x,y)    : cropNone , int32 in1(x,y) end 
   local im convolve(x,y)  : cropNone , int32 clip( (orion.sum( in1(x-2,y)*T[0],in1(x-1,y)*T[1],in1(x,y)*T[2],in1(x+1,y)*T[3],in1(x+2,y)*T[4] ) >> 8 ) , -32767 , 32767 ) end
   local im conv_5_1_(x,y) : cropSame , int16 convolve(x,y) end 
   return conv_5_1_
end

function sobel( R, in0 )
   -- TODO -> Convert sobel mask to taps 
   local im in1(x,y)   : cropNone , int32 in0(x,y) end
   local im in1y(x,y)  : cropNone , int32 clip( orion.sum( -in1(x-1,y-1),-(in1(x,y-1)<<1),-in1(x+1,y-1),in1(x-1,y+1),in1(x,y+1)<<1,in1(x+1,y+1) ) , -32767 , 32767 ) end -- Sobel Y
   local im in1x(x,y)  : cropNone , int32 clip( orion.sum( -in1(x-1,y-1),-(in1(x-1,y)<<1),-in1(x-1,y+1),in1(x+1,y-1),in1(x+1,y)<<1,in1(x+1,y+1) ) , -32767 , 32767 ) end -- Sobel X
   local im sobel(x,y) : cropNone , int32[2] {  in1x(x,y) , in1y(x,y) } end
   return sobel 
end


function response( T , K , in1 )

   local im Wxx(x,y)  : cropNone , int32 clip( ((in1(x,y)[0] * in1(x,y)[0]) >> (16)) , -32767 , 32767 ) end 
   local im Wyy(x,y)  : cropNone , int32 clip( ((in1(x,y)[1] * in1(x,y)[1]) >> (16)) , -32767 , 32767 ) end 
   local im Wxy(x,y)  : cropNone , int32 clip( ((in1(x,y)[0] * in1(x,y)[1]) >> (16)) , -32767 , 32767 ) end 
   local im Det(x,y)  : cropNone , int32 ((Wxx(x,y)*Wyy(x,y))>>1)-((Wxy(x,y)*Wxy(x,y))>>1) end 
   local im TrSq(x,y) : cropNone , int32 ((Wxx(x,y)+Wyy(x,y))>>1)*((Wxx(x,y)+Wyy(x,y))>>1)>>15 end 
   local im Resp1(x,y): cropNone , int32 Det(x,y) - K*TrSq(x,y) end 
   local im Resp(x,y) : cropSame , int16 clip( Resp1(x,y) , -32767 , 32767 ) end
   return Resp

end



function  nms(  T , P1 , in1 )

   local im PN(x,y)  : cropNone , bool in1(x,y) >  in1(x,y-1) end 
   local im PS(x,y)  : cropNone , bool in1(x,y) >= in1(x,y+1) end 
   local im PE(x,y)  : cropNone , bool in1(x,y) >  in1(x+1,y) end 
   local im PW(x,y)  : cropNone , bool in1(x,y) >= in1(x-1,y) end 
   local im P(x,y)   : cropNone , bool in1(x,y) >  P1 end 
   local im Pk(x,y)  : cropNone , bool  ( P(x,y) and PW(x,y) and PE(x,y) and PS(x,y) and PN(x,y) ) end
   local im NMS(x,y) : cropSame , uint8 if Pk(x,y) then 255 else 0 end end

   return NMS 

end


function harris(in1)
   
   local P  = orion.tap(orion.type.int(32), "Peak")
   orion.setTap( P , 128 )
   local K  = orion.tap(orion.type.int(32), "K")
   orion.setTap( K , 1 )
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
