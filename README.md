Getting Started with Darkroom
==========================

_James Hegarty <jhegarty@stanford.edu>_

Darkroom is a language for describing image processing pipelines embedded in Lua.


Installation
------------

First you will need to clone and build Terra using the instructions in the [Terra Readme](https://github.com/zdevito/terra). Run the REPL and make sure it installed correctly.

Add the Darkroom language definition to your lua path environment variable. Add this to .profile or .bashrc:

    export LUA_PATH="?.t;[path to Darkroom]/?.t;[path to terra]/tests/lib/?.t"

Darkroom and Terra are tested to work on Linux and Mac OS X. Other platforms are unlikely to work.

Running Darkroom
-------------

Darkroom includes a number of example image processing pipelines that can be used to test that Darkroom is correctly installed.

    cd darkroom/examples
    terra fixedcampipe.lua

Which should write `darkroom/examples/out/fixedcampipe.bmp`.

Darkroom also contains a test suite which you can run to make sure there isn't any strangeness on your platform:

    cd darkroom/tests
    make

All tests should complete without errors. And then examine the resulting images in darkroom/tests/out and make sure they look correct.

Overview
--------

In Darkroom, images are represented as functions from integer (x,y) coordinates to pixel values. Darkroom uses a right-handed coordinate system, with files loaded from disk resting on the XY axis by default:

![Darkroom Coordinate Conventions](http://stanford.edu/~jhegarty/coords.png)

<!--- unlike PAN, halide(?) we only allow positions to be integer coords -->

Darkroom is embedded in Lua. While it is possible to write Darkroom code without being familiar with Lua, knowing the basics of Lua will make Darkroom easier to understand and allow you to write more powerful programs. Lua is a simple language - the [Programming in Lua](http://www.lua.org/pil/) tutorial provides a quick introduction.

Hello World
-----------

You can either run this tutorial from the Terra REPL or by creating and running a .lua script file. For now, run these commands from inside the `darkroom` directory. Recall that you need to include Darkroom with 'import "darkroom"'. The darkroomSimple library provides convenience functions for loading and saving images - we will use it for now to make these examples cleaner.

    terra
    import "darkroom"
    darkroomSimple = terralib.require("darkroomSimple")

Before we can perform any image processing, we need to load an input image. DarkroomSimple provides a function that will load simple image formats (bmp, ppm):

    inputImage = darkroomSimple.load("test.bmp")

You can also create 'image functions' that perform calculations to compute an image. Describing an image processing pipeline will consist of writing a number of these 'image functions'.

Let's start by writing an image function that increases the brightness of the input by 10%:

    im increaseBrightness(x,y)
      inputImage(x,y) * 1.10
    end

The keyword `im` indicates that we are creating an image function. It is similar to Lua's function definition syntax, but unlike Lua the last expression is the return value. `return` doesn't need to be specified.

The Lua variable `increaseBrightness` now contains a description of an image processing pipeline. We now need to compile and run this pipeline:

    increaseBrightness:save("output.bmp")

DarkroomSimple provides the `:save` function, that will compile and run the image function using default settings and save it to a file. Commands for compiling the pipeline with custom settings or generating an ASIC will be discussed later.

You are now ready to explore some of the more advanced features of Darkroom!

Image Functions in Depth
------------------------

In general, Darkroom image functions can be regarded as a subset of Lua functions: They have similar syntax and behavior, but we will restrict the functions that can be written to only those that will yield good performance. This section will cover the restrictions.

Like in Lua, the image function syntax shown earlier is just syntax sugar for creating a lambda function and assigning it to a lua variable:

    im increaseBrightness(x,y) inputImage(x,y) * 1.10 end

Is the same as:

    increaseBrightness = im(x,y) inputImage(x,y) * 1.10 end

It's possible to string together image functions, creating an image processing pipeline. For example, we may want to adjust the gamma of the image after we increase brightness. This works the same as before when we read from `inputImage`:

    increaseBrightness = im(x,y) inputImage(x,y) * 1.10 end
    im gammaCorrected(x,y) darkroom.pow(inputBrightness(x,y), 2.4) end
    gammaCorrected:save("gamma.bmp")

Notice that `increaseBrightness` is stored in a lua variable, but is being accessed in Darkroom. Similar to Terra, it's possible to use image functions stored anywhere in Lua (i.e. in globals, tables, etc):

    mytable={}
    mytable.fn = im(x,y) inputImage(x,y)+10 end
    im output(x,y) mytable.fn(x,y)*2 end

### Stencils ###

So far, we have only shown pointwise image functions. But it's also possible to perform 'stencil' accesses into another image. Stencil access means that we only access a local, shift invariant neighborhood of the (x,y) position. In Darkroom, this means that we can only access constant integer offsets of the (x,y) argument:

    im areaFilterX(x,y)
      (input(x-1,y)+input(x,y)+input(x+1,y))/3
    end

If you index into an image in a non-stencil way, it will cause a compile error:

    im fn(x,y) input(x+y,y) end -- compile error: no general affine transforms allowed
    im fn2(x,y) input1( input2(x,y), y) -- compile error: no dependent reads allowed

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

Types and Operators
-------------------

Darkroom supports the same set of primitive types as the Terra language. These are `int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64 bool float double`. Typecast syntax is the same as Terra:

    im casted(x,y) [uint8](myFloatFn(x,y)) end

Darkroom supports fixed length arrays over primitive types, using the syntax `primitiveType[N]`, e.g. `uint32[5]` for an array of 5 uint32's. 

Array types support the same set of operators as primitive types: with arrays, these operators act elementwise. Darkroom also supports a number of array-specific operators like dot products, discussed later in the 'Standard Library' section.

In image processing, array types are typically used to represent color channels. Darkroom supports syntax sugar for typical use cases, e.g. `rgb8` produces a `uint8[3]`, indexing `a.r` is the same as `a[0]`. The full set of syntax sugar is as follows:

* `rgbN` === `uintN[3]`
* `rgbaN` === `uintN[4]`
* `e.r` === `e.x` === `e[0]`
* `e.g` === `e.y` === `e[1]`
* `e.b` === `e.z` === `e[2]`
* `e.a` === `e.w` === `e[3]`

On these types Darkroom supports standard operators, similar to C/Terra:

* Unary: `-`
* Binary: `- + * / %`
* Comparison: `< <= > >= == ~=` (returns bool)
* Conditional: `if switch`
* Logical: `and or not` (bool argument, bool result)
* Bitwise: `& | ! ^ << >>`

The `if` operator behaves like the ternary operator in C (an expression), not like an `if` statement. The condition of the select must be a scalar boolean, the results can be vector, but the lengths must match.

    im out(x,y) 
      a = if input(x,y)%2 == 1 then 10 else 20 end
      in if input(x,y) > 100 then a else 255 end
    end

The `switch` operator performs similarly to a switch-case statement in C, but like the `if` operator, it is an expression:

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

It's possible to explicitly cast a value to a type that doesn't have enough precision to hold it. But you must explicitely tag it with the behavior you want when it overflows. Darkroom is designed as a hardware description language, so it prevents you from accidentally losing bits:

    im input32(x,y) : uint32 input(x,y) end  -- assume this isn't a downcast
    im downcast(x,y) : uint8 input32(x,y) end -- generates an error
    im downcastFixed(x,y) : uint8 orion.wraparound(input32(x,y)) end -- OK

Mapreduce
----------

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

    return map i=-1,1 j=-1,1 reduce(sum)
      input(x+i,y+j)*[tap[(j+1)*3+(i+1)]]
    end

The syntax is meant to be evocative of a for loop.

`[variable(s)]` is a list of mapped variables and the range of values they take on, with `variablename=rangeExpression` syntax. The `[expression]` is evaluated once for each variable set in the cartesian product of all the rangeExpression(s). `rangeExpression` is a pair of `low, high` constant integers.

Supported reduce operators are: `sum min max argmin argmax`

> Tip: Notice that an important feature of our map-reduce operator is that it doesn't imply an order for the reduction operator (it's commutative and associative). Darkroom exploits this to reduce the expression in the order that has the highest performance. While you could write out a similar expression by hand, Darkroom's operators aren't associative and commutative, and it's possible you will choose an inefficient order.

`sum min max` evaluate to the type of the `[expression]`. `argmin argmax` return the variable values that attained the highest or lowest expression value. They evalute to the type `int[N]`, where N is the number of variables.

Darkroom API
------------



Taps, Lookup Tables, and Statistics
--------------------------

The programming model of Darkroom is quite restrictive - it's purely functional, fixed-function, etc. This section introduces a number of mechanisms that break these restrictions in limited situations and allow you to accomplish a number of typical image processing tasks that would not be otherwise possible.

### Taps: Read-Only Memory ###

Image processing pipelines typically have a number of runtime configurable parameters, called 'taps' in the image processing community. Examples include the color correction matrix (CCM) or gamma. To create a tap in Darkroom call `darkroom.tap` from Lua:

    theTap = darkroom.tap(tapType, name)

`tapType` is an type, e.g. `uint8`. `name` is a string that will identify the tap.

Following declaration of the tap, you must assign it a value. This works similarly to creating and binding images:

    orion.setTap(theTap, 3)

Example code for using a tap to control gamma correction:

     gammaTap = orion.tap(orion.type.float(32), 'gammatap')
     orion.setTap(gammaTap, 2.4)
     im gammaCorrected(x,y) orion.pow( input(x,y), gammaTap) end

#### Tap Lookup Tables ####

Recall that Darkroom image functions and arrays cannot be indexed with values calculated at runtime (they can only be indexed with constants). Lookup tables (LUTs) are an array of taps that can be indexed using runtime-calculated values, but their values can't change, and they can only have a compile-time determinable set of entries.

    orion.tapLUT(tapType, count, name)

Following declaration of the LUT, you must assign it an array of values. This works similarly to creating and binding images:

    orion.setTapLUT(theTapLUT, {1,2,...})

A typical use of this functionality would be to implement 'tone mapping', which remaps output pixel values of a pipeline based on a given tone curve (typically includes gamma and contrast). In 8bit, we can implement this with a 256 entry lookup table:

    tonemap = orion.tapLUT(orion.type.uint(8), 256, "tonemap")
    orion.setTapLUT(tonemap,{1,2,4,...})
    im finalImage(x,y) tonemap[input(x,y)] end

> Tip: if your lookup tables start to have a large number of entries, it may be more efficient to just calculate their value at runtime from a reduced set of parameters.

### Statistics: Write-Only Memory  ###

Darkroom image functions can't have side effects, but Darkroom does allow you to capture information about images using statistics variables. Statistics variables are write-only, commutative and associative. They perform a reduction over the calculated region of the image. Various reductions are supported:

* `orion.statistic.sum(type, imgFn)`
* `orion.statistic.min(type, imgFn)`
* `orion.statistic.max(type, imgFn)`

`type` is an orion type, and `imgFn` is the image the reduction will be performed over. For example `orion.statistic.sum` calculates the sum of all the pixel values in the image. These functions return a 'statistic function' that behaves like an image function, but returns a single value instead of an image. 

For example, to calculate the maximum seen value in an image:

    maxImagePixel = orion.statistic.max(orion.type.uint(8), input)
    print(maxImagePixel())

<!--- this is kind of similar to map-reduce? -->

### Driver Code ###

Now consider how you can combine statistics and taps to create a pipeline that performs a ridiculously simple version of automatic digital gain:
  
    gain = 1
    lastFrameMax = 254

    gaintap = orion.tap(orion.type.float(32),  function()
        if lastFrameMax >= 255 then 
          gain = gain*0.9 
        elseif lastFrameMax < 230 then
          gain = gain*1.1
        end
        return gain
      end)

    im gained(x,y) : uint8 input(x,y)*gaintap end

    while(1) do
      pipeline = orion.compile({gained,orion.statistic.max(gained)})
      out, lastFrameMax = pipeline()
    end

> Note: The 'driver' for an image pipeline manages the feedback loop from statistics outputs of one frame to tap inputs of the next. Darkroom currently supports this functionality by returning statistics outputs to Lua, and allowing Lua to calculate new tap registers on demand. However, we think this is an unsolved problem, and we may vastly change this interface in the future (e.g. to offload driver code to a dedicated core, for example). So beware that this interface is provisional.


Metaprogramming
---------------

We have already seen an example of interaction between Lua and Darkroom: Darkroom image functions are stored in Lua variables. But there are actually some more interesting ways we can use Lua combined with Darkroom.

Darkroom is designed to facilitate the design of hardware image processing pipelines. It is often useful to parameterize your algorithm to explore tradeoffs, for example between performance and image quality.

Consider the design for a pipeline that performs convolution:

    im convolve(x,y)
      map i=-1,1 j=-1,1 reduce(sum) input(x+i, y+j)*tap[i][j] end
    end

But you might decide that you need a 5x5 or 6x6 convolution instead of a 3x3 convolution. Instead of implementing convolve multiple times, we can use Lua to parameterize the design:

    function makeConvolve(N)
      return im(x,y)
        map i=-N,N j=-N,N reduce(sum) input(x+i, y+j)*tap[i][j] end
      end
    end

    convolve3x3 = makeConvolve(1)
    convolve5x5 = makeConvolve(2)

Notice the behavior of the variable `N` referenced by the image function in `makeConvolve`. The value of the Lua variable at the time that the Darkroom image function is defined becomes a constant in Darkroom. Here `N` is a number, which becomes an Darkroom number, but some conversions aren't allowed (lua strings can't become Darkroom types). A full list of conversion rules is given in the 'Conversion Rules' section.

We can also parameterize other things, like types:

    for i=1,32 do
      local imtype = orion.type.uint(i)
      local im tmp(x,y) : imtype input(x,y) end
      local im blurX(x,y) (tmp(x-1,y)+tmp(x,y)+tmp(x+1,y))/3 end
      blurX:save("blur"..i..".bmp")      
    end

<!---
we maybe need to explain this earlier, b/c we've already been using lua variables in orion code since the start of the document (except these lua variables held orion types). Maybe we should just explain this specific thing earlier: "lua variables that hold orion types can be accessed"
-->


### Unquotes and Macros ###

Sometimes it's convenient to call Lua code while compiling a Darkroom image function. This can be accomplished with the unquote operator `[]` and macros. 

The unquote operator `[e]` evaluates `e` as a lua expression and returns the Lua result converted to an Darkroom type using the 'Conversion Rules' below. One example where this might be useful is table lookup:

    luatable = {1,2,3,4,5,6,7,8,9}
    
    im lookup(x,y)
      i = 2
      j = 3
      fail = luatable[i+j] -- fails, (i+j) isn't a constant
      works = [luatable[i+j]] -- works, i+j calculated by Lua
    end

In the example above, `i` and `j` hold Darkroom numbers, which get converted to Lua values in the unquoted code.

Similar to Terra, there are a number of situations where you can omit an explicit unquote. One example that we've already been using is variable lookup:

    a = 42 -- lua value
    im test(x,y) input(x,y)*[a] end
    im test(x,y) input(x,y)*a end  -- same as above

Another automatic unquote happens when you call a Lua function. This is typically called 'macros':

    function double(v) return v*2 end
    im doubleim(x,y) tmp = [double(21)]; input(x,y)*tmp end
    im doubleim(x,y) tmp = double(21); input(x,y)*tmp end   -- same as above

### Conversion Rules ###

When using a Lua value in Darkroom:

    Darkroom image function -> Darkroom image function
    Lua number -> Darkroom constant
    Lua string -> error
    Lua nil -> error
    Lua function -> macro
    Lua array -> Darkroom array (table with dense integer keys)
    Non-array Lua Table -> error

When passing an Darkroom value to Lua:

    Darkroom image function -> Darkroom image function object
    Darkroom number -> Lua number
    Darkroom array -> Lua array

Boundary Conditions
-------------------

Outside of an image functions crop region, Darkroom will calculate the image as a boundary condition. Darkroom supports a number of default boundary conditions:

* `constant(N)`  constant value `N`
* `clamp` (default) clamp to nearest valid pixel in image
* `wrap` wraparound - toroidal

Image functions use the `clamp` boundary condition by default.

As with crop options, boundary conditions are specified along with the type of the image function, and propagate:

    im blurX(x,y) : constant(255) a(x-1,y)+a(x+1,y) end

> Tip: Darkroom doesn't perform particularly well in the boundary section of an image, because this is typically not a common case so it isn't well optimized. Try to avoid reading from the boundary portion of an image.

API
---

The Darkroom system contains a number of Lua functions that interact with the language (e.g. loading images, compiling). This section contains a list of all these functions.

### Inputs/Outputs ###

Darkroom supports creating 'input image functions' which can be bound to input data at runtime.

`orion.image(type, [width], [height]) [lua]` Create an input image function of type `type`, but don't yet bind it to data. Data must be bound before the pipeline is executed, or a compile error will occur. If `width` and `height` are specified, this will bake these values into the pipeline, potentially making it faster (allowing it to perform more optimizations b/c it knows the image bounds).

`orion.bindImage(orionImageDst, orionImageSrc : &Image) [terra]` binds `orionImageSrc` to input image function `orionImageDst`. Useful for feeding the result of one run of the pipeline into the input of the next run. BindImage takes a copy of orionImageSrc, so you should call orionImageSrc:free() after binding the image if you no longer need it.

`orion.bindFile(orionImage, filename) [terra]` Load the image located at 'filename' and bind it to the input image function `orionImage`. Images gets mapped to the area x=[0,W) y=[0,H). bmp, pgm, ppm, flo, and tmp are supported.

`orion.load(filename) [lua]` Convenience function that calls both `orion.image` and `orion.bindFile` and returns an input image function of static size. Useful if you're only planning on running your pipeline on one image.

`orion.bindConstant(orionImage, width, height, constantValue) [terra]` A convenience function that allocates an array of the size of orionImage and fills it with value `constant`. If you just need a runtime configurable constant in your pipeline it's better to use a tap, but this can be used to initialize initial values for example.

`orion.constant(type, width, height, constantValue) [lua]` A convenience function that calles `orion.image` and then `orion.bindConstant` on that image.

<!--- we may want to support taking in images in arbitrary formats (AoS) without reformatting them (bitmaps are often stored AoS) -->

### Taps/Statistics ###

`orion.tap(tapType, name)`

`orion.tapLUT(tapType, count, name)`

`orion.statistic.sum(type, imgFn)`

`orion.statistic.min(type, imgFn)`

`orion.statistic.max(type, imgFn)`

`orion.statistic.histogramSum(type, imgFn)`

### Compiling ###

`orion.compile(imageFunctionsArray, compileOptionsTable)` 
While all image function objects have convenience functions for compiling and running them, you will want to use `orion.compile` when you have a pipeline with multiple outputs. It's likely that the outputs reuse many shared intermediates - compiling them separately will recalculate these intermediates. The function returned from `orion.compile` will return the results in the same order as they are passed:

    im a(x,y) input(x-1,y) end
    im b(x,y) a(x,y)*2 end
    
    a:save("a.bmp")
    b:save("b.bmp")  -- this recalculated a! a was calculated twice

    pipeline = orion.compile({a,b})
    aout,bout = pipeline()
    aout:save("a.bmp")
    bout:save("b.bmp")  -- This reused calculation of a - a was only calculated once

    aout:free() -- if we are done with these images, we need to manually free their memory
    bout:free() 

<!--- we may want to have different compile options for different image functions? nah... -->

### Compile Options ###

The compile options table is a lua table with some or all of the following key/value pairs set (values are passed as strings). Option in [] brackets are the default:

`platform=[cpu] convolution` If platform is `cpu`, the compiler returns a lua function that you can call to calculate the pipeline. If platform is `convolution`, the result of compilation is a string in YAML format that can be saved and sent to the convolution engine framework.

`schedule=[default] fuseall materialize materializeall`
The scheduling algorithm to use (see Halide paper).

* `default` Our best guess for high performance based on our heuristics
* `materialize` materialize all supernodes. No inlining or linebuffering.
* `linebufferall` linebuffer all supernodes.
* `materializeall` materialize _all_ intermediates.

`precision=[arbitrary] or cpu`

`region=[default] centered specify` Specifies the region of the image function that we want to calculate.

* `default` Calculate exactly the crop region of the passed image function(s) - not necessarily starting at (0,0). 
* `centered` Calculate the region x=[0,W) y=[0,H) where W,H is the width and height of the crop region.  See `cropMeet` option below to specify behavior when multiple functions are passed.
* `specify` The function returned by `orion.compile` will take 4 arguments that specify the area to calculate: (lowX,lowY,highX,highY)

If you're compiling a pipeline with multiple outputs, `default` and `crop` apply to each output as if they were the single output (i.e. the outputs may be rendered to different sizes). `specify` sets the size for all outputs.

`debug = [false] true`
Run a number of extra runtime and compile time checks to make sure the compiler is behaving correctly.

`verbose = [false] true`
Print out a lot of intermediate compiler state.

`printruntime = [false] true`
Print runtime statistics.

`printasm = [false] true`
Print assembly of final generated code.

`printloopir = [false] true`
Print loopIR (final IR before code generation)

`printschedule = [false] true`
Print schedule out to stdout in JSON format

`looptimes = [1] number`
number of times to run the inner loop of each kernel. Used to make runtime statistics more accurate.

Standard Library
----------------

This section lists Darkroom functions for a number of standard operations. It typically only makes sense to call these from within Darkroom!

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

`darkroom.gather( input, X, Y, xRange, yRange )` Performs a gather on the argument `input`. This is essentially equivilant to writing `im(x,y) input(x+X,y+Y) end`, however `X` and `Y` can be arbitrary darkroom expressions instead of constants, which is not typically allowed. `X` must be within the range `[-xRange, xRange]` and Y within the range `[-yRange,yRange]`, and they both must be integers.  `clamp` is true, `X` and `Y` are clamped to the range. If `clamp` is false, darkroom throws an assert if `X` or `Y` are out of the range.

### Debugging functions ###

`darkroom.assert( expr, printValue, condition )` Raises a runtime exception if `condition` is false, and prints the number `printVal` to the console so that you can identify which assert failed. If `condition` is true, it returns the value `expr`.

`darkroom.print(value)` Prints `value` to the console.

Remember, Darkroom is purely functional, so the result of assert and print must be returned by a function to have an effect. This is very different compared to imperative languages like C:

    im test(x,y)
      tmp = darkroom.assert(input(x,y) == 3) -- never runs, value tmp is unused so is optimized away.
      in darkroom.print(input(x,y))  -- works, prints every pixel of input to the console
    end

### extras/darkroomSimple.t ###

`darkroomSimple.t` is a convenience wrapper around Darkroom that reduces the amount of code you have to write if you're using Darkroom in restricted, simple situations. Specifically, it does not allow you to change input images or tap values without recompiling all the image functions. `darkroomSimple` should not be used if you're writing the compiled pipeline out to an object file.

`darkroomSimple` supersedes the standard darkroom API - you can't mix darkroom and darkroomSimple API calls or it will throw an error.

`darkroomSimple.compile( imageFunctionList, compilerOptions )` 

`darkroomSimple.load(filename)` Loads the image file at `filename` and turns it into an image function.

`darkroomSimple.image(img)` 

`darkroomSimple.tap( type, name, value )` 

`darkroomSimple.tapLUT( type, name, valueArray )` 

`darkroomSimple.setImageSize( width, height )` Explicitly set the image size to calculate. This is typically set automatically based on the images you load, but if you made a pipeline with no inputs it must be explicitly set.

`imageFunction:save( filename, compilerOptions )` darkroomSimple installs a `:save` method on every image function that calles compile, run, and save.

### extras/image.t ###

`image.t` is a pure Terra library provided with Darkroom that provides code for loading and saving a number of simple image formats. The formats supported are `bmp ppm flo raw`. It's not necessary to use `image.t` - any image loading library can be used with Darkroom as long as you pass the data in the expected format (described in the `darkroom.compile` section above).

`Image:init(width : int, height : int, stride : int, channels : int, bits : int, floating : bool, isSigned : bool, SOA : bool, data : &opaque, dataPtr : &opaque)`

`floating` true if the image holds floating point data
`isSigned` true if the image holds signed integer data (eg `int32`)
`SOA` true if the data is stored is struct-of-arrays form. eg, each channel is stored contiguously... all red pixels come before all blue pixels, etc. If false, the image is stored in array-of-structs form (channels interleaved). Typical image formats (eg bmp) store their data in array of structs form.
`data` is a pointer to the first valid pixel in the image. `dataPtr` is a pointer to the data structure that should be freed to free the image. Note that `image.t` takes ownership of the `dataPtr` pointer. Some of its operators can not be done in place, so the original pointer will be freed, so do not expect the original image to remain.

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

### extras/dpda.t ###

### extras/darkroomDebug.t ###