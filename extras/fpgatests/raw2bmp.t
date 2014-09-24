darkroom={}
terralib.require("common")
terralib.require("image")

terra raw2bmp(infile : &int8, outfile : &int8)
  var inp : Image
  inp:loadRaw(infile,128,64,8)
  inp:flip()
  inp:save(outfile)
end

raw2bmp(arg[1], arg[2])