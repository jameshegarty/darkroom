Getting Started with Darkroom
==========================

_James Hegarty <jhegarty@stanford.edu>_

Darkroom is a language for describing image processing pipelines embedded in Terra.


Installation
------------

First you will need to clone and build Terra using the instructions in the [Terra Readme](https://github.com/zdevito/terra). Run the REPL and make sure it installed correctly.

Add the Darkroom language definition to your lua path environment variable. Add this to .profile or .bashrc:

    export DR=[path to darkroom root]
    export TERRADIR=[path to terra root]
    export TERRA_PATH="$TERRA_PATH;./?.t;$DR/?.t;$DR/src/?.t;$DR/extras/?.t;$TERRADIR/tests/lib/?.t"

Darkroom and Terra are tested to work on Linux and Mac OS X. Other platforms are unlikely to work.

Running Darkroom
-------------

Darkroom includes a number of example image processing pipelines that can be used to test that Darkroom is correctly installed.

    cd darkroom/examples
    terra campipe.t

Which should write `darkroom/examples/out/campipe.bmp`.

Darkroom also contains a test suite which you can run to make sure there isn't any strangeness on your platform:

    cd darkroom/tests
    make

All tests should complete without errors. And then examine the resulting images in darkroom/tests/out and make sure they look correct.

Overview
--------

In Darkroom, images are represented as functions from integer (x,y) coordinates to pixel values. Darkroom uses a right-handed coordinate system, with files loaded from disk resting on the XY axis by default:

![Darkroom Coordinate Conventions](http://stanford.edu/~jhegarty/coords.png)

Darkroom is embedded in Lua. While it is possible to write Darkroom code without being familiar with Lua, knowing the basics of Lua will make Darkroom easier to understand and allow you to write more powerful programs. Lua is a simple language - the [Programming in Lua](http://www.lua.org/pil/) tutorial provides a quick introduction.

Hello World
-----------

Run this tutorial by creating and running a .t terra file. Darkroom is not supported in the Terra REPL. For now, run these commands from inside the `darkroom` directory. Recall that you need to include Darkroom with 'import "darkroom"'. The darkroomSimple library provides convenience functions for loading and saving images - we will use it for now to make these examples cleaner.

    import "darkroom"
    darkroomSimple = terralib.require("darkroomSimple")

Before we can perform any image processing, we need to load an input image. DarkroomSimple provides a function that will load simple image formats (bmp, ppm):

    inputImage = darkroomSimple.load("examples/color.bmp")

You can also create 'image functions' that perform calculations to compute an image. Describing an image processing pipeline will consist of writing a number of these 'image functions'.

Let's start by writing an image function that decreases the brightness of the input by 10%:

    im decreaseBrightness(x,y)
      [uint8[3]](inputImage(x,y) * 0.9)
    end

The keyword `im` indicates that we are creating an image function. It is similar to Lua's function definition syntax, but unlike Lua the last expression is the return value. `return` doesn't need to be specified. The `[uint8[3]]` casts the output to a format that our image saving library supports (24 bit color).

The Lua variable `decreaseBrightness` now contains a description of an image processing pipeline. We now need to compile and run this pipeline:

    decreaseBrightness:save("output.bmp")

DarkroomSimple provides the `:save` function, that will compile and run the image function using default settings and save it to a file. Commands for compiling the pipeline with custom settings or generating an ASIC will be discussed later.

You are now ready to explore some of the more advanced features of Darkroom!

Image Functions in Depth
------------------------

In general, Darkroom image functions can be regarded as a subset of Terra functions: They have similar syntax and behavior, but we will restrict the functions that can be written to only those that will yield good performance. This section will cover the restrictions.

Like in Terra/Lua, the image function syntax shown earlier is just syntax sugar for creating a lambda function and assigning it to a lua variable:

    im decreaseBrightness(x,y) inputImage(x,y) * 0.9 end

Is the same as:

    decreaseBrightness = im(x,y) inputImage(x,y) * 0.9 end

It's possible to string together image functions, creating an image processing pipeline. For example, we may want to adjust the gamma of the image after we decrease brightness. This works the same as before when we read from `inputImage`:

    decreaseBrightness = im(x,y) inputImage(x,y) * 0.9 end
    im gammaCorrected(x,y) [uint8[3]](darkroom.pow(decreaseBrightness(x,y)/255, 2.4)*255) end
    gammaCorrected:save("gamma.bmp")

Notice that `decreaseBrightness` is stored in a lua variable, but is being accessed in Darkroom. Similar to Terra, it's possible to use image functions stored anywhere in Lua (i.e. in globals, tables, etc):

    mytable={}
    mytable.fn = im(x,y) inputImage(x,y)*0.9 end
    im output(x,y) [uint8[3]](mytable.fn(x,y)+10) end

### Stencils ###

So far, we have only shown pointwise image functions. But it's also possible to perform 'stencil' accesses into another image. Stencil access means that we only access a local, shift invariant neighborhood of the (x,y) position. In Darkroom, this means that we can only access constant integer offsets of the (x,y) argument:

    im areaFilterX(x,y)
      [uint8[3]](inputImage(x-1,y)/3+inputImage(x,y)/3+inputImage(x+1,y)/3)
    end

If you index into an image in a non-stencil way, it will cause a compile error:

    im fn(x,y) inputImage(x+y,y) end -- compile error: no general affine transforms allowed
    im fn2(x,y) inputImage( input2(x,y), y) -- compile error: no dependent reads allowed

> Tip: The stencil restriction forces you to write efficient code, because it always yields a small, statically-analyzable working set. Many image processing algorithms can be fit into the stencil model. In general, you will get higher performance if you keep your stencils small - e.g. accessing `input(x,y-10)+input(x,y+10)` may perform poorly because it uses a large working set, 20 lines of the image.

### Temporaries ###

Darkroom also lets you define temporaries inside image functions to make it easier to write more complicated functions. Temporaries must come before the last statement in the image function, which is the return value as before:

    im complicated(x,y)
      a = inputa(x-1,y)+inputa(x+1,y)
      b = inputb(x,y-1)+inputb(x,y+1)
      c = 42
      in
      a*b+c
    end

Temporaries must each have a unique name, a form known as Single Static Assignment (SSA). This makes it clear that these temporaries aren't imperative assignments; instead they are like a 'let' statement in functional programming. Temporaries hold values, not image functions, so they can't be indexed.

Types
-----

Darkroom supports the same set of primitive types as the Terra language. These are `int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64 bool float double`. Typecast syntax is the same as Terra:

    im casted(x,y) [uint8](myFloatFn(x,y)) end

Darkroom supports fixed length arrays over primitive types, using the syntax `primitiveType[N]`, e.g. `uint32[5]` for an array of 5 uint32's. 

Array types support the same set of operators as primitive types: with arrays, these operators act elementwise. Darkroom also supports a number of array-specific operators like dot products, discussed later in the 'Standard Library' section.

Operators
---------

Darkroom supports standard operators, similar to C/Terra:

* Unary: `-` 
* Binary: `- + * / %`
* Comparison: `< <= > >= == ~=` (returns bool)
* Conditional: `if switch`
* Logical: `and or not` (bool argument, bool result)
* Bitwise: `and or not ^ << >>`

The `if` operator behaves like the ternary operator in C (an expression), not like an `if` statement. The condition of the select must be a scalar boolean, the results can be vector, but the lengths must match.

    im out(x,y) 
      a = if input(x,y)%2 == 1 then 10 else 20 end
      in if input(x,y) > 100 then a else 255 end
    end

The `switch` operator performs similarly to a switch-case statement in C, but like the `if` operator, it is an expression not a statement:

    switch controlExpression 
      case value1 -> resultExpr1 
      case value2 -> resultExpr2 
      ... 
      default -> defaultExpr 
    end

For example, to create a striped image:

    im striped(x,y)
      switch x%3
        case 0 -> 0
        case 1 -> inputa(x,y)
        default -> 255
      end
    end

Darkroom also includes a number of standard math operators like pow, abs, etc. discussed later in the 'Standard Library' section.

Similar to Terra, if an expression isn't explicitly tagged with a type, then type propagation occurs.

### Mapreduce Operator ###

It's common in image processing to generate a number of intermediates (map), and then combine them (reduce). For example, consider convolution:

    Convolution PSEUDOCODE:

    -- map
    outputlist = []
    for i = -1 to 1, j = -1 to 1 do    
      push input(x+i,y+j)*tap[(j+1)*3+(i+1)] onto outputlist
    end

    -- reduce
    return sum(outputlist) -- a commutative, associative sum

Darkroom contains a combined map-reduce operator that provides this functionality with a terse syntax:

    map [variable(s)] reduce([operator]) [expression] end

For example, the convolution above would be written as:

    im convolve(x,y)
      map i=-1,1 j=-1,1 reduce(sum)
        input(x+i,y+j)*taps[(j+1)*3+(i+1)]
      end
    end

The syntax is meant to be evocative of a for loop.

`[variable(s)]` is a list of mapped variables and the range of values they take on, with `variablename=rangeExpression` syntax. The `[expression]` is evaluated once for each variable set in the cartesian product of all the rangeExpression(s). `rangeExpression` is a pair of `low, high` constant integers.

Supported reduce operators are: `sum min max argmin argmax`

> Tip: Notice that an important feature of our map-reduce operator is that it doesn't imply an order for the reduction operator (it's associative). Darkroom exploits this to reduce the expression in the order that has the highest performance. While you could write out a similar expression by hand, Darkroom's operators aren't associative and commutative, and it's possible you will choose an inefficient order.

`sum min max` evaluate to the type of the `[expression]`. `argmin argmax` return the variable values that attained the highest or lowest expression value, and the extremal value itself. They evalute to the type `int[N+1]`, where N is the number of variables. The last channel (channel N) contains the extremal value. If multiple variable values evaluate to the same extremal value, then the lowest variable value will be chosen.

Metaprogramming
---------------

We have already seen an example of interaction between Lua and Darkroom: Darkroom image functions are stored in Lua variables. But there are actually some more interesting ways we can use Lua combined with Darkroom.

Darkroom is designed to facilitate the design of hardware image processing pipelines. It is often useful to parameterize your algorithm to explore tradeoffs, for example between performance and image quality.

Consider the design for a pipeline that performs convolution:

    im convolve(x,y)
      map i=-1,1 j=-1,1 reduce(sum) input(x+i, y+j)*taps[(j+1)*3+(i+1)] end
    end

But you might decide that you need a 5x5 or 6x6 convolution instead of a 3x3 convolution. Instead of implementing convolve multiple times, we can use Lua to parameterize the design:

    function makeConvolve(N)
      return im(x,y)
        map i=-N,N j=-N,N reduce(sum) input(x+i, y+j)*taps[(j+N)*(2*N+1)+(i+N)] end
      end
    end

    convolve3x3 = makeConvolve(1)
    convolve5x5 = makeConvolve(2)

Notice the behavior of the variable `N` referenced by the image function in `makeConvolve`. The value of the Lua variable at the time that the Darkroom image function is defined becomes a constant in Darkroom. Here `N` is a number, which becomes an Darkroom number, but some conversions aren't allowed (lua strings can't become Darkroom types). A full list of conversion rules is given in the 'Conversion Rules' section.

We can also parameterize other things, like types:

    for i in pairs({uint8, uint16, uint32}) do
      local im tmp(x,y) [i](input(x,y)) end
      local im blurX(x,y) (tmp(x-1,y)+tmp(x,y)+tmp(x+1,y))/3 end
      blurX:save("blur"..i..".bmp")      
    end

### Escapes ###

The escape operator `[e]` evaluates `e` as a lua expression and returns the Lua result converted to an Darkroom type using the 'Conversion Rules' below. For example, you could use this to derive parameters for your algorithm based on user input:

    sumArea = 16
    
    im sum(x,y)
      map i=[-math.sqrt(convArea)],[math.sqrt(convArea)] j=[-math.sqrt(convArea)],[math.sqrt(convArea)] reduce(sum)
        input(x,y)
      end
    end

All escapes are evaluated before the Darkroom code is typechecked. So, as far as darkroom is concerned, `i` and `j` have the range `-4,4`. No square roots will be executed by Darkroom.

Similar to Terra, there are a number of situations where you can omit an explicit unquote. One example that we've already been using throughout this tutorial is variable lookup:

    a = 42 -- lua value
    im test(x,y) input(x,y)*[a] end
    im test(x,y) input(x,y)*a end  -- same as above

### Conversion Rules ###

When using a Lua value in Darkroom:

    Darkroom image function -> Darkroom image function
    Lua number -> Darkroom constant
    Lua string -> error
    Lua nil -> error
    Lua array -> Darkroom array (table with dense integer keys)
    Non-array Lua Table -> error

Darkroom values used in Lua are always Darkroom quotes.

Boundary Conditions
-------------------

While all images in Darkroom are definited on an infinite domain, in practice we are only ever calculating images on a finite domain. It is common in image processing to define behaviour if you read outside the boundaries of the image you are calculating (boundary conditions) to maintain fidelity on the edges.

In darkroom, you can define boundary conditions with the crop operator. Applying the crop operator to an image function keeps it the same within the region the user is calculating (x=0,W y=0,H), but yields zero outside of this region.
   
   im cropped(x,y) darkroom.crop(input(x,y) end

`cropped(-5,-5)` will be `0`, `cropped(5,5)` will be `input(5,5)`.

Currently the only supported boundary condition is zero fill, but more will be supported in the future. While this boundary condition can be implemented inside darkroom, the crop operator is special-cased by the compiler and is much more efficient. It uses no extra compute or buffering except for within (typically small) boundary regions.

All input images have a crop applied to them automatically.

Standard Library
----------------

`darkroom.crop(x)` Apply a 0 boundry condition to `x`.

`darkroom.exp(x)` e^x

`darkroom.abs(x)` absolute value

`darkroom.floor(x)` floor

`darkroom.ceil(x)` ceil

`darkroom.cos(x)` cos

`darkroom.sin(x)` sin

`darkroom.arrayAnd(x)` x must be an array of bools. ANDs each element of the array together and yields a bool.

`darkroom.pow(x,a)` x^a

`darkroom.sum(a,b,c, ...)`. Commutative, associative sum

`darkroom.min(a,b,c, ...)`. min of inputs

`darkroom.max(a,b,c, ...)`. max of inputs

`darkroom.dot(a,b)` dot product of a and b

`darkroom.cross(a,b)` a,b must be 3 element arrays.

`darkroom.vectorSelect(cond,a,b)` Does an elementwise select. cond, a, and b must be arrays of the same length.

`darkroom.gather( input, X, Y, xRange, yRange )` Performs a gather on the argument `input`. This is essentially equivilant to writing `im(x,y) input(x+X,y+Y) end`, however `X` and `Y` can be arbitrary darkroom expressions instead of constants, which is not typically allowed. `X` must be within the range `[-xRange, xRange]` and Y within the range `[-yRange,yRange]`, and they both must be integers. if `X` or `Y` are outside of the specified range, then behaviour is undefined (potentially could segfault). If the `debug` compile option is enabled than the darkroom raises an error if `X` and `Y` are outside of the specified range.

### Debugging functions ###

`darkroom.assert( expr, printValue, condition )` Raises a runtime exception if `condition` is false, and prints the number `printVal` to the console so that you can identify which assert failed. If `condition` is true, it returns the value `expr`.

`darkroom.print(value)` Prints `value` to the console.

Remember, Darkroom is purely functional, so the result of assert and print must be returned by a function to have an effect. This is very different compared to imperative languages like C:

    im test(x,y)
      tmp = darkroom.assert(input(x,y) == 3) -- never runs, value tmp is unused so is optimized away.
      in darkroom.print(input(x,y))  -- works, prints every pixel of input to the console
    end

Darkroom API
------------

`darkroom.input( type )`
Returns an image function for an input that will be bound at runtime (e.g. the input image you want to process).

### Taps: Read-Only Memory ###

Image processing pipelines typically have a number of runtime configurable parameters, called 'taps' in the image processing community. Examples include the color correction matrix (CCM) or gamma. These are values you want to change every run without recompiling. To create a tap in Darkroom call `darkroom.tap` from Lua:

    theTap = darkroom.tap( tapType )

`tapType` is an type, e.g. `uint8`.

Example code for using a tap to control gamma correction:

     gammaTap = orion.tap( float32 )
     im gammaCorrected(x,y) orion.pow( input(x,y), gammaTap) end

Tap values are passed as arguments to the compiled pipeline.

#### Tap Lookup Tables ####

Recall that Darkroom image functions and arrays cannot be indexed with values calculated at runtime (they can only be indexed with constants). Lookup tables (LUTs) are an array of taps that can be indexed using runtime-calculated values, but their values can't change, and they can only have a compile-time determinable set of entries.

    orion.tapLUT( tapType, count )

A typical use of this functionality would be to implement 'tone mapping', which remaps output pixel values of a pipeline based on a given tone curve (typically includes gamma and contrast). In 8bit, we can implement this with a 256 entry lookup table:

    tonemap = orion.tapLUT( uint8, 256 )
    im finalImage(x,y) tonemap[input(x,y)] end

> Tip: if your lookup tables start to have a large number of entries, it may be more efficient to just calculate their value at runtime from a reduced set of parameters.

### Compiling ###

`darkroom.compile( inputImageFunctionsArray, outputImageFunctionsArray, tapsArray, width, height, compileOptionsTable)` 

Compile takes a lua array of input image functions (returned from `darkroom.input`), an array of outputs to generate, and array of tap inputs, the width and height of the output, and compile options. `darkroom.compile` returns a terra function with this type signature:

  compiledPipeline( input0 : &opaque, input1 : &opaque, ... output1 : &opaque, output2 : &opaque, taps : &TapStruct )

Where inputs, outputs, and taps are in the same order as passed to `darkroom.compile`, and `TapStruct` is a struct with entries in the same order and types as the `tapsArray`. All output images are returned in 'darkroom format'. The data returned matches the type of the darkroom image. In addition, the stride of the output is the `width` rounded up to the nearest vector width `V` passed in the compile options. For example if `width=63` and `V=4` then the stride returned will be 64. Finally, multi-channel images are returned in struct of array form (i.e. all red pixels come before all blue pixels etc). For inputs, the data must be passed in struct of array form, but the stride should equal the output image width (padding the stride to the vector size is not required).

For convenience, you can use [extras/darkroomSimple.t](#extrasdarkroomsimplet), which provides an abstraction on top of this that doesn't require loading images, or use the [extras/image.t](#extrasimaget) class:

    import "darkroom"
    terralib.require("image")

    -- define and compile the pipelne
    inp = darkroom.input( uint8[3] )
    tap = darkroom.tap( uint8 )
    im out(x,y) [uint8](inp[0]/tap + inp[1]/tap + inp[2]/tap) end
    terraPipe = darkroom.compile( {inp}, {out}, {tap}, 128, 64 )
    struct TapStruct {tap:uint8;}

    terra loadAndRun()
      var inp : Image
      inp:load("myfile.bmp"):toDarkroomFormat()

      var out : Image
      out:allocateDarkroomFormat( 128, 64, 4, 1, 8, false, false )

      var tapStruct = TapStruct {3}
      terraPipe( inp.data, out.data, &tapStruct ) -- run pipeline

      out:fromDarkroomFormat()
      out:save("myoutput.bmp")
    end

    loadAndRun()

### Compile Options ###

The compile options table is a lua table with some or all of the following key/value pairs set (values are passed as strings). Option in [] brackets are the default:

`V = [4] number`
Vector width for generated code.

`cores = [4] number`
Number of threads for generated code.

`stripcount = number`
Number for strips for generated code. Defaults to the number of cores. More strips => smaller strip width => less linebuffering, but more overcompute.

`fastmath = [false] true`
Enables a number of math optimizations that don't preserve semantics (but are close). e.g. turning divides into shifts.

`debug = [false] true`
Run a number of extra runtime and compile time checks to make sure the compiler is behaving correctly.

`verbose = [false] true`
Print out a lot of intermediate compiler state.

extras/darkroomSimple.t
-----------------------

`darkroomSimple.t` is a convenience wrapper around Darkroom that reduces the amount of code you have to write if you're using Darkroom in restricted, simple situations. Specifically, it does not allow you to change input images or tap values without recompiling all the image functions. `darkroomSimple` should not be used if you're writing the compiled pipeline out to an object file.

`darkroomSimple` supersedes the standard darkroom API - you can't mix darkroom and darkroomSimple API calls or it will throw an error.

`darkroomSimple.compile( outputImageFunctionList, compilerOptions )` 

Returns a terra function that when executed returns one or more `Image` objects from `image.t`. Note that it is your responsibility to call `:free()` on these objects when you are done with them!

`darkroomSimple.load(filename)` Loads the image file at `filename` and turns it into an image function.

`darkroomSimple.image(img)` 

`darkroomSimple.tap( type, name, value )` 

`darkroomSimple.tapLUT( type, name, valueArray )` 

`darkroomSimple.setImageSize( width, height )` Explicitly set the image size to calculate. This is typically set automatically based on the images you load, but if you made a pipeline with no inputs it must be explicitly set.

`imageFunction:save( filename, compilerOptions )` darkroomSimple installs a `:save` method on every image function that calles compile, run, and save.

extras/image.t
--------------

`image.t` is a pure Terra library provided with Darkroom that provides code for loading and saving a number of simple image formats. The formats supported are `bmp ppm flo raw`. It's not necessary to use `image.t` - any image loading library can be used with Darkroom as long as you pass the data in the expected format (described in the `darkroom.compile` section above).

`Image:init(width : int, height : int, stride : int, channels : int, bits : int, floating : bool, isSigned : bool, SOA : bool, data : &opaque, dataPtr : &opaque)`

`floating` true if the image holds floating point data
`isSigned` true if the image holds signed integer data (eg `int32`)
`SOA` true if the data is stored is struct-of-arrays form. eg, each channel is stored contiguously... all red pixels come before all blue pixels, etc. If false, the image is stored in array-of-structs form (channels interleaved). Typical image formats (eg bmp) store their data in array of structs form.
`data` is a pointer to the first valid pixel in the image. `dataPtr` is a pointer to the data structure that should be freed to free the image. Note that `image.t` takes ownership of the `dataPtr` pointer. Some of its operators can not be done in place, so the original pointer will be freed, so do not expect the original image to remain.

`Image:allocateDarkroomFormat( width : int, height : int, V : int, channels : int, bits : int, floating : bool, isSigned : bool )`
Allocates  memory for the given type in the format that darkroom expects. `V` is the vector width the darkroom function was compiled with (default is 4). Useful for allocating space for the output images.

`Image:load(filename : &int8)` load an image file in a supported format

`Image:loadRaw(filename : &int8, width : int, height : int, bits : int, header : int, flipEndian : bool)` Load any file, and treat it as raw data. `header` is the number of bits at the top of the file to skip before the image data starts. `flipEndian` flips the endianness, if `bits` > 8. Assumes single channel, unsigned integer.

`Image:save(filename : &int8)` save to a supported format.

`Image:saveRaw(filename : &int8, bits : int)`

`Image:flipY()` flip image in Y dimension.

`Image:downsample()`

`Image:upsample()`

`Image:toFloat32()` `Image:toUint32()` `Image:toUint8()`

`Image:toAOS()` `Image:toSOA()`

`Image:toDarkroomFormat()` converts the image to the format that darkroom expects (eg correct stride, SOA).

`Image:toSaveFormat()` converts from the format darkroom returns to a format supported by the file save functions.

`Image:free()`

extras/dpda.t
-------------

This file implements a Darkroom backed for the convolution engine's DPDA code format.

`kernels, config = dpda.compile( inputs, outputs, taps )`
Call this instead of `darkroom.compile` to compile for the convolution engine. `inputs` is a map from darkroom inputs (returned from `darkroom.input) to filenames. `outputs` is a lua array of darkroom images to calculate. `taps` is a map from darkroom taps (returned from `darkroom.tap`) to {name, defaultValue} pairs.

This function returns the strings `kernels` and `config`, the DPDA code and runtime configurating, respectively. These can then be written out to a file, etc.

extras/darkroomDebug.t
----------------------
Including the file installs debug hooks into darkroom that check and print the IR after each compile stage.