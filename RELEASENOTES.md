v1.0 7/13/2014
--------------

* The syntax for typecasts and crops have changed. The colon syntax has been removed.

  Previous code like this:

      im a(x,y) : uint8, cropDefault img(x,y)+darkroom.float32(4) end

  Should be rewritten to look like this:

      im a(x,y) [uint8](darkroom.crop(img(x,y)+[float32](4))) end

  This brings darkroom in line with the typecast syntax of Terra.

* The syntax for let statements has changed. 

  Previous code like this:

      im a(x,y) 
        let 
          exp = 4 
        in exp+2 
      end

  Should be rewritten like this:

      im a(x,y)
        exp = 4
        in exp + 2
     end

* The Darkroom API has been consolidated to only include a single darkroom.compile(...) function, which takes in a table of image functions and returns a Terra function that implements that image function. The returned terra function takes input images, output images, and taps as arguments. The compiled function can be saved out and called directly from C. Darkroom is no longer stateful, and no runtime is necessary.

  Previous functionality like darkroom.load(filename), myImgFunc:save(filename) etc has been moved to a convenience library 'darkroomSimple.t'. Refer to the docs.

* The DPDA backend has been moved to the external 'dpda.t' library, and has its own dpda.compile(...) function.