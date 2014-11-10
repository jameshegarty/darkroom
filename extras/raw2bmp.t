darkroom={}
terralib.require("common")
terralib.require("image")

terra raw2bmp(infile : &int8, outfile : &int8)
  var inp : Image
  inp:loadRaw(infile,128,64,8)
  -- remember: when using the simulator, the raw file you send in has to be upside down (bottom left pixels comes first
  -- thus, image is flipped). It turns out this is the same convention as BMP, so no flip at the end is necessary.
--  inp:flip()
  inp:save(outfile)
end

raw2bmp(arg[1], arg[2])