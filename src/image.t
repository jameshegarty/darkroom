local cstdio = terralib.includec("stdio.h")
local cstdlib = terralib.includec("stdlib.h")
local cstring = terralib.includec("string.h")

struct Image { 
  width : int, 
  height : int, 
  stride : int, 
  channels: int, -- this is always stored AoS
  bits : int, 
  floating : bool, -- is this floating point?
  isSigned : bool, -- is this signed?
  data : &opaque,
  -- it's possible that data doesn't point to the start of the array (strided images)
  -- dataPtr points to the start of the array
  dataPtr : &opaque
}

terra Image:init( 
  width : int, 
  height : int, 
  stride : int,
  channels : int, 
  bits : int, 
  floating : bool,
  isSigned : bool,
  data : &opaque,
  dataPtr : &opaque)

  orionAssert(stride>=width, "stride < width")
  orionAssert(data>=dataPtr, "data < dataPtr")

  self.width = width
  self.height = height
  self.stride = stride
  self.channels = channels
  self.bits = bits
  self.floating = floating
  self.isSigned = isSigned
  self.data = data
  self.dataPtr = dataPtr
end

terra Image:initSimple(
  width : int, 
  height : int, 
  channels : int, 
  bits : int, 
  floating : bool,
  isSigned : bool,
  data : &opaque)

  self:init(width, height, width, channels, bits, floating, isSigned, data, data)
end

terra Image:initWithFile(filename : &int8)

  var width : int
  var height : int
  var channels : int
  var bits : int

  var data : &opaque = orion.util.loadImageUC(filename,&width,&height,&channels,&bits)

  self:init(width,height,width,channels,bits,false,false,data,data)
end

terra Image:initWithRaw(filename : &int8, w:int, h:int, bits:int)
  -- loadRaw always returns an int32
  var outBytes : int
  var data : &opaque = orion.util.loadRaw(filename,w,h,bits,0,false,&outBytes)

  self:init(w,h,w,1,outBytes*8,false,false,data,data)
end

-- header: number of header bits to ignore
-- flipEndian: if multibyte, flip the endian ordering
terra Image:initWithRaw(filename : &int8, w:int, h:int, bits:int, header : int, flipEndian : bool)
  -- loadRaw always returns an int32
  var bytesOut : int
  var data : &opaque = orion.util.loadRaw(filename,w,h,bits, header, flipEndian,&bytesOut)

  self:init(w,h,w,1,bytesOut*8,false,false,data,data)
end

terra Image:free()
  cstdlib.free(self.dataPtr)
end

-- this will take the image, do a deep copy, and in the process rearrange 
-- the image to remove the stride
terra Image:deepcopyUnstride()
  orionAssert(self.bits % 8 == 0, "bits must be mod 8")

  var bytes = (self.bits/8)
  var size = self.width*self.height*self.channels*bytes
  --var temp = cstdlib.malloc(size)
  var temp : &uint8
  cstdlib.posix_memalign( [&&opaque](&temp), orion.tune.pageSize, size)

  var channels = self.channels

  for y=0,self.height do
    for x = 0,self.width do
      for c = 0,channels do
        for b=0,bytes do
          temp[channels*bytes*(y*self.width+x)+c*bytes+b] = [&uint8](self.data)[channels*bytes*(y*self.stride+x)+c*bytes+b]
        end
      end
    end
  end
  
  var out : Image
  out.width = self.width
  out.height = self.height
  out.stride = self.width
  out.channels = self.channels
  out.bits = self.bits
  out.floating = self.floating
  out.isSigned = self.isSigned
  out.data = temp
  out.dataPtr = temp

  return out
end

terra Image:save(filename : &int8)
  var ext = filename + cstring.strlen(filename) - 3
  cstdio.printf("EXT %s\n",ext)

  if self.bits==8 and (self.channels==1 or self.channels==3) and self.floating==false and self.isSigned==false then
    if orion.verbose then cstdio.printf("Assuming uint8\n") end
    if cstring.strcmp(ext,"jjm")==0 then
       var us = self:deepcopyUnstride()
       orion.util.saveImageJJM( filename, self.width, self.height, self.width, self.channels, self.bits, self.floating, self.isSigned, us.data ) -- self.data )
       us:free()
    else 
       orion.util.saveImageUC(
	  filename,
	  self.width,
	  self.height,
	  self.stride,
	  self.channels,
	  [&uint8](self.data))
    end 
    return true
  elseif self.bits==32 and self.channels==1 and self.floating then
    if orion.verbose then cstdio.printf("saving a 32 bit 1 channel float\n") end
    orion.util.saveImageAutoLevels( filename, self.width, self.height, self.stride, 1, [&float](self.data) )
    return true
  elseif self.bits==32 and self.channels==1 and self.floating==false and cstring.strcmp(ext,"jjm")>0 then
    if orion.verbose then cstdio.printf("saving a 32 bit 1 channel float\n") end
    orion.util.saveImageI( filename, self.width, self.height, self.stride, 1, [&int](self.data) )
    return true
  elseif self.bits==32 and (self.channels==2 or self.channels==3) and self.floating then
    --cstdio.printf("Assuming that a 32 bit 3 channel image is float!!!!!!!!!\n")
    orion.util.saveImageF( filename, self.width, self.height, self.stride, self.channels, [&float](self.data) )
    return true
  end

  if self.floating then
    cstdio.printf("Error saving, unimplemented float type bits:%d channels:%d\n",self.bits,self.channels)
  elseif cstring.strcmp(ext,"jjm")==0 then
--    cstdio.printf("Error saving, unimplemented int type bits:%d channels:%d\n",self.bits,self.channels)
    var us =self:deepcopyUnstride()
     orion.util.saveImageJJM( filename, self.width, self.height, self.width, self.channels, self.bits, self.floating, self.isSigned, us.data ) -- self.data )
     us:free()
  else
    cstdio.printf("Error saving, unimplemented type float:%d signed:%d bits:%d channels:%d\n",self.floating,self.isSigned,self.bits,self.channels)
  end


  return false
end

terra Image:saveRaw(filename : &int8, bits:int)
  orionAssert(bits <= 32, "saveRaw only works on bits <= 32")
  orionAssert(self.bits == 32, "saveRaw only works on 32 bit images")
  orionAssert(self.floating == false, "saveRaw only works on int")

  orion.util.saveRaw(
    filename, 
    self.width, 
    self.height, 
    self.stride, 
    bits,
    -- unsafe cast, but we asserted this is 32 bit int
    [&int](self.data) )
end

-- flips the image in place
terra Image:flip()
  orionAssert(self.bits % 8 == 0, "bits must be mod 8 to flip")
  var bytes = (self.bits/8)*self.channels

  for y = 0,self.height/2 do
    var flipY = self.height-y-1
    for x = 0,self.stride do
      for b = 0,bytes do
        var tmp = [&uint8](self.data)[y*self.stride*bytes+x*bytes+b]
        [&uint8](self.data)[y*self.stride*bytes+x*bytes+b] = [&uint8](self.data)[flipY*self.stride*bytes+x*bytes+b]
        [&uint8](self.data)[flipY*self.stride*bytes+x*bytes+b] = tmp
      end
    end
  end
end

terra Image:downsample()
  orionAssert(self.bits % 8 == 0, "bits must be mod 8 to downsample")

  var widthd2 = (self.width/2)
  var heightd2 = (self.height/2)

  var chan = self.channels

--  orionAssert(chan==1,"lolol")

  for y= 0, heightd2 do
    for x= 0, widthd2 do

      if self.bits==8 and self.floating==false and self.isSigned==false then
        for c = 0, self.channels do
          var aa = [&uint8](self.data)[y*2*self.stride*chan+x*2*chan+c]
          var bb = [&uint8](self.data)[y*2*self.stride*chan+(x*2+1)*chan+c]
          var cc = [&uint8](self.data)[(y*2+1)*self.stride*chan+x*2*chan+c]
          var dd = [&uint8](self.data)[(y*2+1)*self.stride*chan+(x*2+1)*chan+c]
          
          var comb = float(aa)+float(bb)+float(cc)+float(dd)
          comb = comb/4
          [&uint8](self.data)[y*widthd2*chan+x*chan+c] = comb
        end
        
      else
        cstdio.printf("%d %d %d\n",self.bits,self.floating,self.isSigned)
        orionAssert(false,"unsupported Downsample")
      end

    end
  end

  self.width = widthd2
  self.height = heightd2
  self.stride = widthd2
end

-- can't do this in place for obvious reasons
terra Image:upsample()
  orionAssert(self.bits % 8 == 0, "bits must be mod 8 to upsample")
  orionAssert(self.bits == 32, "bits must be 32 to upsample")
  orionAssert(self.floating, "must be floating to upsample")
  orionAssert(self.isSigned==false, "must be floating to upsample (IS)")

  cstdio.printf("START US\n")
  var width2 = self.width*2
  var height2 = self.height*2

  var chan = self.channels

  var bpc = self.bits / 8
  var size = width2*height2*self.channels*bpc
  var temp : &uint8
  cstdlib.posix_memalign( [&&opaque](&temp), orion.tune.pageSize, size)

  var inF = [&float](self.data)
  var outF = [&float](temp)

  for y = 0, height2 do
    for x = 0, width2 do

        var lx = x/2
        var ly = y/2

        for c = 0, self.channels do

          var xp1 = 1
          var yp1 = 1

          if lx==self.width-1 then xp1=0 end
          if ly==self.height-1 then yp1=0 end

          var aa = inF[ly*self.stride*chan+lx*chan+c]
          var bb = inF[ly*self.stride*chan+(lx+xp1)*chan+c]
          var cc = inF[(ly+yp1)*self.stride*chan+lx*chan+c]
          var dd = inF[(ly+yp1)*self.stride*chan+(lx+xp1)*chan+c]

          var ix = float(x%2)/2
          var iy = float(y%2)/2

          var m1 = (aa*(1-ix)+bb*ix)
          var m2 = (cc*(1-ix)+dd*ix)

          outF[y*width2*chan+x*chan+c] = (m1*(1-iy)+m2*iy)
        end
    end
  end

  self:free()
  self.width = width2
  self.stride = width2
  self.height = height2
  self.data = temp
  self.dataPtr = temp
end


local function makeConvert(fromType, toType, toTypeBits, toTypeFloating)
  assert(type(toTypeBits)=="number")
  assert(type(toTypeFloating)=="boolean")

  return terra(self:&Image)
    orionAssert(sizeof(fromType)*8==self.bits, "unsupported int32 conversion")
    orionAssert(self.stride==self.width, "toInt32 stride not supported")

    var size = self.width*self.height*self.channels*sizeof(toType)
    var temp : &toType
    cstdlib.posix_memalign( [&&opaque](&temp), orion.tune.pageSize, size)

    var inF = [&fromType](self.data)

    var lineWidth = self.width*self.channels
    for y = 0, self.height do
      for x = 0, self.width do
        for c = 0, self.channels do
          var aa = inF[y*lineWidth + x*self.channels + c]
          temp[y*lineWidth + x*self.channels + c] = aa
        end
      end
    end
    
    self:free()
    self.bits = toTypeBits
    self.floating = toTypeFloating
    self.isSigned = false
    self.data = temp
    self.dataPtr = temp
  end
end

Image.methods.uint8ToFloat32 = makeConvert(uint8, float, 32, true)
Image.methods.int32ToFloat32 = makeConvert(int32, float, 32, true)

terra Image:toFloat32()
  orionAssert(self.floating==false, "you can't converting float to float")
  if self.bits==8 and self.isSigned==false then
    self:uint8ToFloat32()
  elseif self.bits==32 and self.isSigned then
    self:int32ToFloat32()
  else
    cstdio.printf("bits %d\n",self.bits)
    orionAssert(false, "unsupported float32 conversion")
  end
end

Image.methods.uint8ToUint32 = makeConvert(uint8,uint32, 32, false)
Image.methods.uint16ToUint32 = makeConvert(uint16,uint32, 32, false)

terra Image:toUint32()

  if self.bits==8 and self.isSigned==false and self.floating==false then
    self:uint8ToUint32()
  elseif self.bits==16 and self.isSigned==false and self.floating==false then
    self:uint16ToUint32()
  else
    cstdio.printf("bits %d\n",self.bits)
    orionAssert(false, "unsupported float32 conversion")
  end
end


Image.methods.float32ToUint8 = makeConvert(float,uint8,8, false)

terra Image:toUint8()

  if self.bits==32 and self.isSigned==false and self.floating==true then
    self:float32ToUint8()
  else
    cstdio.printf("bits %d\n",self.bits)
    orionAssert(false, "unsupported toUint8 conversion")
  end
end
