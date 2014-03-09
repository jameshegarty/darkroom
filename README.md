Getting Started with Orion
==========================

_James Hegarty <jhegarty@stanford.edu>_

Orion is a language for describing image processing pipelines embedded in Lua.


Installation
------------

First you will need to clone and build Terra using the instructions in the [Terra Readme](https://github.com/zdevito/terra). Run the REPL and make sure it installed correctly.

Add the Orion language definition to your lua path environment variable:

    export LUA_PATH="?.t;[path to Stanford-SEEC-Convolution-Engine]/orion/?.t;[path to terra]/tests/lib/?.t"

Orion and Terra are tested to work on Linux and Mac OS X. Other platforms are unlikely to work.

Running Orion
-------------

Orion includes a number of example image processing pipelines that can be used to test that Orion is correctly installed.

    cd Stanford-SEEC-Convolution-Engine/orion/examples
    terra fixedcampipe.lua

Which should write `Stanford-SEEC-Convolution-Engine/orion/examples/fixedcampipeoutput.bmp`.

Orion also contains a test suite which you can run to make sure there isn't any strangeness on your platform:

    cd Stanford-SEEC-Convolution-Engine/orion/tests
    make

And then examine the resulting images in tests/out and make sure they look correct.

Overview
--------

In Orion, images are represented as functions from integer (x,y) coordinates to pixel values. Orion uses a right-handed coordinate system, with files loaded from disk resting on the XY axis by default:

![Orion Coordinate Conventions](http://stanford.edu/~jhegarty/coords.png)

<!--- unlike PAN, halide(?) we only allow positions to be integer coords -->

Orion is embedded in Lua. While it is possible to write Orion code without being familiar with Lua, knowing the basics of Lua will make Orion easier to understand and allow you to write more powerful programs. Lua is a simple language - the [Programming in Lua](http://www.lua.org/pil/) tutorial provides a quick introduction.

Hello World
-----------

You can either run this tutorial from the Terra REPL or by creating and running a .lua script file. For now, run these commands from inside the `Stanford-SEEC-Convolution-Engine` directory. Recall that you need to include Orion with 'import "orion"'

    terra
    import "orion"

Before we can perform any image processing, we need to load an input image. Orion provides a function that will load typical image formats:

    inputImage = orion.load("test.bmp")

You can also create 'image functions' that perform calculations to compute an image. Describing an image processing pipeline will consist of writing a number of these 'image functions'.

Let's start by writing an image function that increases the brightness of the input by 10%:

    im increaseBrightness(x,y)
      inputImage(x,y) * 1.10
    end

<!---
maybe discuss how pixels are sometimes stored in the linear domain, and sometimes in the gamma domain, and how orion is oblivious to this?
-->

The keyword `im` indicates that we are creating an image function. It's syntax should appear similar to Lua's function definition syntax, but unlike Lua the last expression is the return value. `return` doesn't need to be specified.

The Lua variable `increaseBrightness` now contains a description of an image processing pipeline. We now need to compile and run this pipeline:

    increaseBrightness:save("output.bmp")

Calling `:save` on an image function will calculate its output using default settings and save it to a file. Commands for compiling the pipeline with custom settings or generating an ASIC will be discussed later.

You are now ready to explore some of the more advanced features of Orion!

Image Functions in Depth
------------------------

In general, Orion image functions can be regarded as a subset of Lua functions: They have similar syntax and behavior, but we will restrict the functions that can be written to only those that will yield good performance. This section will cover the restrictions.

Like in Lua, the image function syntax shown earlier is just syntax sugar for creating a lambda function and assigning it to a lua variable:

    im increaseBrightness(x,y) inputImage(x,y) * 1.10 end

Is the same as:

    increaseBrightness = im(x,y) inputImage(x,y) * 1.10 end

It's possible to string together image functions, creating an image processing pipeline. For example, we may want to increase the contrast of the image after we increase brightness. This works the same as before when we read from `inputImage`:

    increaseBrightness = im(x,y) inputImage(x,y) * 1.10 end
    im gammaCorrected(x,y) orion.pow(inputBrightness(x,y), 2.4) end
    pipeline = orion.compile(gammaCorrected)

Notice that `increaseBrightness` is stored in a lua variable, but is being accessed in Orion. Similar to Terra, it's possible to use image functions stored anywhere in Lua (i.e. in globals, tables, etc):

    mytable={}
    mytable.fn = im(x,y) inputImage(x,y)+10 end
    im output(x,y) mytable.fn(x,y)*2 end

You can also index into lua tables using Orion values, but the Orion values must be constants:

    luatable = {1,2,3 ...}
    im fancylookup(x,y)
      i = 42
      j = 10
      a = luatable[i] -- ok
      b = luatable[i*j] -- compile error: not a constant
      c = luatable[input(x,y)] -- compile error: not a constant
    end

It is actually also possible to use Lua variables that hold Lua values from within Orion. This will be covered later when we discuss 'metaprogramming'.

### Stencils ###

So far, we have only shown pointwise image functions. But it's also possible to perform 'stencil' accesses into another image. Stencil access means that we only access a local, shift invariant neighborhood of the (x,y) position. In Orion, this means that we can only access constant integer offsets of the (x,y) argument:

    im areaFilterX(x,y)
      (input(x-1,y)+input(x,y)+input(x+1,y))/3
    end

If you index into an image in a non-stencil way, it will cause a compile error:

    im fn(x,y) input(x+y,y) end -- compile error: no general affine transforms allowed
    im fn2(x,y) input1( input2(x,y), y) -- compile error: no dependent reads allowed

> Tip: The stencil restriction forces you to write efficient code, because it always yields a small, statically-analyzable working set. Many image processing algorithms can be fit into the stencil model. In general, you will get higher performance if you keep your stencils small - e.g. accessing `input(x,y-10)+input(x,y+10)` may perform poorly because it uses a large working set, 20 lines of the image.

### Temporaries ###

Orion also lets you define temporaries inside image functions to make it easier to write more complicated functions. Temporaries must come before the last statement in the image function, which is the return value as before:

    im complicated(x,y)
      a = inputa(x-1,y)+inputa(x+1,y)
      b = inputb(x,y-1)+inputb(x,y+1)
      c = 42
      in
      a*b+c
    end

Temporaries must each have a unique name, a form known as Single Static Assignment (SSA). This makes it clear that these temporaries aren't imperative assignments; instead they are like a 'let' statement in functional programming. Temporaries hold values, not image functions, so they can't be indexed. This 'let expression' can actually be used anywhere in Orion:

    im let(x,y) (a=1 b=2 in a+b)+3 end

### Upsampling and Downsampling ###

In Orion we extend the stencil model to support upsampling and downsampling:

    im pointDownsample(x,y) input(x*2,y*2) end
    im nearestNeighborUpsample(x,y) input(x/2,y/2) end

Currently, you are only allowed to multiply or divide (x,y) by integer constants.

> Tip: in hardware, multiplying (x,y) by (A,B) maps to skipping (A,B) pixels. Dividing it by (A,B) maps to outputting a block of AxB pixels for each pixel of input.

You can also access a stencil neighborhood around the downsampled pixels:

    im areaDownsample(x,y)
      ( input(x*2,y*2) + input(x*2+1,y*2) + input(x*2,y*2+1) + input(x*2+1,y*2+1) )/4
    end

We also provide a convenience function that will generate typical resampling patterns:
   
    orion.resample ......

In the future, we may extend Orion to support other more general types of access (e.g. affine).

<!---
We chose to expose the language as pixel-based operators, ie the argument is an (x,y) position, and it produces a value. We could have also exposed this as 'image wide' operators. Where we have image a and b, and then write expressions like 'a+b', which means add image a to image b (instead of add pixels in a to pixels in b). It turns out that there is an isomorphism between these two points of view (if you choose a matching set of operators). But personally, I (and probably other people) found the pixel-oriented POV a lot easier to understand - I didn't find it immediately obvious that the image-oriented POV could do everything that I wanted (but it actually turns out that the code ends up looking very similar). We also considered exposing both ways of doing things, which can make the code more terse, but we concluded this would probably just confuse people...
-->

Types and Operators
-------------------

Orion is a hardware description language, so it supports arbitrary precision types and arithmetic, which will allow you to generate more efficient hardware by not calculating bits that you don't need. But CPUs/GPUs don't support arbitrary precision arithmetic, so the Orion runtime will emulate arbitrary precision arithmetic, which is potentially slow on the CPU.  When high CPU performance is desired, Orion also supports a type system that only allows CPU types (similar to C, Terra). First we will describe the arbitrary precision type system:

### Arbitrary Precision Primitive Types ###

Orion supports 
* Integers: `intI_F`, I is integer precision, F is fractional precision (F can be left off)
* Unsigned integers: `uintI_F`, I is integer precision, F is fractional precision (F can be left off)
* Boolean: `bool`
* Floating Point: `floatF_E`, F is fractional precision (mantissa), E is exponent precision

You can tag image functions with types using a similar syntax to Terra:

    im brightness(x,y) : uint16 
      tmp : uint8 = input(x,y)
      tmp + 10
    end

On these types Orion supports standard operators, similar to C/Terra:

* Unary: `-`
* Binary: `- + * / %`
* Comparison: `< <= > >= == ~=` (returns bool)
* Conditional: `if switch`
* Logical: `and or not` (bool argument, bool result)
* Bitwise: `& | ! ^ << >>`

The `if` operator behaves like the ternary operator in C (an expression), not like an `if` statement. The condition of the select must be a scalar boolean, the results can be vector, but the lengths must match.

    im out(x,y) 
      a = if input(x,y)%2 == 1 then 10 else 20 end
      if input(x,y) > 100 then a else 255 end
    end

<!--- we could instead have 'temporaries' in image functions be assignments, in which case we could have an if statement. But I think this would confuse the functional nature of our language -->

The `switch` operator performs similarly to a switch-case operator in C:

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

Orion also includes a number of standard math operators like pow, abs, etc. discussed later in the 'Standard Library' section.

Similar to Terra, if an expression isn't explicitly tagged with a type, then type propagation occurs. But type propagation in Orion behaves very differently than Terra: intermediate types are sized to be the smallest precision that's guaranteed to not overflow. For example:

    im addAB(x,y)
      a : uint8 = input(x,y)
      b : uint8 = input(x,y)
      a+b
    end

In this case, the output of `addAB` would get type uint9, because that's the smallest type that will hold the sum of two uint8s without overflowing. The type propagation rules are as follows:

    uintN + unintM = uint{max(M,N)+1}
    uintN >> C = uint{N-C}
    uintN >> uintM = uintN
    ...

Notice that some operators actually decrease the precision (i.e. `>>` and `/`).

It's possible to explicitly cast a value to a type that doesn't have enough precision to hold it. But you must explicitely tag it with the behavior you want when it overflows. Orion is designed as a hardware description language, so it prevents you from accidentally loosing bits:

    im input32(x,y) : uint32 input(x,y) end  -- assume this isn't a downcast
    im downcast(x,y) : uint8 input32(x,y) end -- generates an error
    im downcastFixed(x,y) : uint8 orion.wraparound(input32(x,y)) end -- OK

The options for overflow behavior are as follows:

* `orion.clamp(v,low,high)` : Performs saturating cast - `v` clamped to be in range [low,high]
* `orion.wraparound(v)` : `v` outside of precision wraps around - like behavior of CPU types

Note that using `orion.clamp` without explicitly tagging a type will actually cause the type propagation algorithm to choose a the smallest type that will fit a value in the range [low,high].

### CPU Primitive Types ###

When the `precision=cpu` compile option is set, Orion supports the same set of primitive types and propagation rules as Terra:

* Integers: `int` `int8` `int16` `int32` `int64`
* Unsigned integers: `uint` `uint8` `uint16` `uint32` `uint64`
* Boolean: `bool`
* Floating Point: `float32` `float64`

Like in Terra and C, adding a uint32 and uint32 will result in a uint32 (for example).

For convenience, you can write code with arbitrary precision types even if `precision=cpu` is set, and the arbitrary precision types will get clamped to the smallest CPU type that will fit it (i.e. uint18 becomes uint32). An error will occur if no CPU type has enough precision (i.e. uint209). This will allow you to generate a non-bit-accurate simulator, which may have higher performance.

### Arrays ###

Orion supports fixed length arrays over primitive types, using the syntax `primitiveType[N]`, e.g. `uint32[5]` for an array of 5 uint32's. 

Array types support the same set of operators as primitive types: with arrays, these operators act elementwise. Orion also supports a number of array-specific operators like dot products, discussed later in the 'Standard Library' section.

In image processing, array types are typically used to represent color channels. Orion supports syntax sugar for typical use cases, e.g. `rgb8` produces a `uint8[3]`, indexing `a.r` is the same as `a[0]`. The full set of syntax sugar is as follows:

* `rgbN` === `uintN[3]`
* `rgbaN` === `uintN[4]`
* `e.r` === `e.x` === `e[0]`
* `e.g` === `e.y` === `e[1]`
* `e.b` === `e.z` === `e[2]`
* `e.a` === `e.w` === `e[3]`

<!--
This is probably not a good idea b/c this may not map to what the user needs for their application. should make this configurable. otoh this convention forces people to write code that other people will be able to understand immediately (allowing people to change this arbitrarily may make picking up somebody elses codebase difficult)
-->

### Structs ###

Orion doesn't support struct types, but this behavior can be emulated by using Lua tables:

    mystruct={}
    mystruct.color = input
    mystruct.gray = im(x,y) input.r(x,y) end
    im output(x,y) mystruct.color(x,y)+mystruct.gray(x,y) end

Taps, Lookup Tables, and Statistics
--------------------------

The programming model of Orion is quite restrictive - it's purely functional, fixed-function, etc. This section introduces a number of mechanisms that break these restrictions in limited situations and allow you to accomplish a number of typical image processing tasks that wouldn't be otherwise possible.

### Taps: Read-Only Memory ###

Image processing pipelines typically have a number configurable parameters, called 'taps' in the image processing community. Examples include the color correction matrix (CCM) or gamma. To create a tap in Orion call `orion.tap` from Lua:

    theTap = orion.tap(tapType, name)

`tapType` is an Orion type, e.g. returned from `orion.type.uint(8)`. `name` is a string that will identify the tap.

Following declaration of the tap, you must assign it a value. This works similarly to creating and binding images:

    orion.setTap(theTap, 3)

Example code for using a tap to control gamma correction:

     gammaTap = orion.tap(orion.type.float(32), 'gammatap')
     orion.setTap(gammaTap, 2.4)
     im gammaCorrected(x,y) orion.pow( input(x,y), gammaTap) end

#### Tap Lookup Tables ####

Recall that Orion image functions and arrays cannot be indexed with values calculated at runtime (they can only be indexed with constants). Lookup tables (LUTs) are an array of taps that can be indexed using runtime-calculated values, but their values can't change, and they can only have a compile-time determinable set of entries.

    orion.tapLUT(tapType, count, name)

Following declaration of the LUT, you must assign it an array of values. This works similarly to creating and binding images:

    orion.setTapLUT(theTapLUT, {1,2,...})

A typical use of this functionality would be to implement 'tone mapping', which remaps output pixel values of a pipeline based on a given tone curve (typically includes gamma and contrast). In 8bit, we can implement this with a 256 entry lookup table:

    tonemap = orion.tapLUT(orion.type.uint(8), 256, "tonemap")
    orion.setTapLUT(tonemap,{1,2,4,...})
    im finalImage(x,y) tonemap[input(x,y)] end

> Tip: if your lookup tables start to have a large number of entries, it may be more efficient to just calculate their value at runtime from a reduced set of parameters.

### Statistics: Write-Only Memory  ###

Orion image functions can't have side effects, but Orion does allow you to capture information about images using statistics variables. Statistics variables are write-only, commutative and associative. They perform a reduction over the calculated region of the image. Various reductions are supported:

* `orion.statistic.sum(type, imgFn)`
* `orion.statistic.min(type, imgFn)`
* `orion.statistic.max(type, imgFn)`

`type` is an orion type, and `imgFn` is the image the reduction will be performed over. For example `orion.statistic.sum` calculates the sum of all the pixel values in the image. These functions return a 'statistic function' that behaves like an image function, but returns a single value instead of an image. 

For example, to calculate the maximum seen value in an image:

    maxImagePixel = orion.statistic.max(orion.type.uint(8), input)
    print(maxImagePixel())

<!--- this is kind of similar to map-reduce? -->

#### Histograms ####

Similar to lookup tables, you can also create an array of statistic variables that can be dependently indexed:

* `orion.statistic.histogramSum(type, count, indexImgFn, valueImgFn)`

Useful, for example, to create a histogram:

    -- input is a uint8 image function
    hist = orion.statistic.histogramSum(orion.type.uint(32), 256, input, im(x,y) 1 end)

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

> Note: The 'driver' for an image pipeline manages the feedback loop from statistics outputs of one frame to tap inputs of the next. Orion currently supports this functionality by returning statistics outputs to Lua, and allowing Lua to calculate new tap registers on demand. However, we think this is an unsolved problem, and we may vastly change this interface in the future (e.g. to offload driver code to a dedicated core, for example). So beware that this interface is provisional.

Mapreduce
----------

It's common in image processing to generate a number of intermediates (map), and then combine them (reduce). For example, consider convolution:

    Convolution PSEUDOCODE:

    -- map
    outputlist = []
    for i = -1 to 1, j = -1 to 1 do    
      push input(x+i,y+j)*tap[i][j] onto outputlist
    end

    -- reduce
    return sum(outputlist) -- a commutative, associative sum

Orion contains a combined map-reduce operator that provides this functionality with a terse syntax:

    map [variable(s)] reduce([operator]) [expression] end

For example, the convolution above would be written as:

    return map i=-1,1 j=-1,1 reduce(sum)
            input(x+i,y+j)*tap[i][j] 
    end

The syntax is meant to be evocative of a for loop.

`[variable(s)]` is a list of mapped variables and the range of values they take on, with `variablename=rangeExpression` syntax. The `[expression]` is evaluated once for each variable set in the cartesian product of all the rangeExpression(s). Supported `rangeExpression`s are:
* `low,high` integers between low and high, inclusive
* a Lua array of values

Supported reduce operators are: `sum min max`

> Tip: Notice that an important feature of our map-reduce operator is that it doesn't imply an order for the reduction operator (it's commutative and associative). Orion exploits this to reduce the expression in the order that has the highest performance. While you could write out a similar expression by hand, Orion's operators aren't associative and commutative, and it's possible you will choose an inefficient order. (In fact, Orion will throw a performance warning when it sees this).

<!--- in general commutativity doesn't imply associativity, but it might on the types we're dealing with? -->


Metaprogramming
---------------

We have already seen an example of interaction between Lua and Orion: Orion image functions are stored in Lua variables. But there are actually some more interesting ways we can use Lua combined with Orion.

Orion is designed to facilitate the design of hardware image processing pipelines. It is often useful to parameterize your algorithm to explore tradeoffs, for example between performance and image quality.

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

Notice the behavior of the variable `N` referenced by the image function in `makeConvolve`. The value of the Lua variable at the time that the Orion image function is defined becomes a constant in Orion. Here `N` is a number, which becomes an Orion number, but some conversions aren't allowed (lua strings can't become Orion types). A full list of conversion rules is given in the 'Conversion Rules' section.

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

Sometimes it's convenient to call Lua code while compiling a Orion image function. This can be accomplished with the unquote operator `[]` and macros. 

The unquote operator `[e]` evaluates `e` as a lua expression and returns the Lua result converted to an Orion type using the 'Conversion Rules' below. One example where this might be useful is table lookup:

    luatable = {1,2,3,4,5,6,7,8,9}
    
    im lookup(x,y)
      i = 2
      j = 3
      fail = luatable[i+j] -- fails, (i+j) isn't a constant
      works = [luatable[i+j]] -- works, i+j calculated by Lua
    end

In the example above, `i` and `j` hold Orion numbers, which get converted to Lua values in the unquoted code.

Similar to Terra, there are a number of situations where you can omit an explicit unquote. One example that we've already been using is variable lookup:

    a = 42 -- lua value
    im test(x,y) input(x,y)*[a] end
    im test(x,y) input(x,y)*a end  -- same as above

Another automatic unquote happens when you call a Lua function. This is typically called 'macros':

    function double(v) return v*2 end
    im doubleim(x,y) tmp = [double(21)]; input(x,y)*tmp end
    im doubleim(x,y) tmp = double(21); input(x,y)*tmp end   -- same as above

### Conversion Rules ###

When using a Lua value in Orion:

    Orion image function -> Orion image function
    Lua number -> Orion constant
    Lua string -> error
    Lua nil -> error
    Lua function -> macro
    Lua array -> Orion array (table with dense integer keys)
    Non-array Lua Table -> error

When passing an Orion value to Lua:

    Orion image function -> Orion image function object
    Orion number -> Lua number
    Orion array -> Lua array

Cropping
--------

Notice that image functions specify an image that is infinite in extent, but so far whenever we compile and run an image function it produces an image with finite extent. Furthermore, Orion supports boundary conditions for areas 'outside' of the image, but for this to make sense we need a way of defining what we consider to be 'inside' the image. To solve this problem, we will introduce a 'crop', which is a bounding box associated with all image functions that specifies where they're valid.

Leaf nodes of orion expressions come in two forms:

    -- infinite in extent:
    im fortytwo(x,y) 42 end
    
    -- finite, known size:
    input = orion.load('image.png')

We now need to consider what happens when we perform a stencil operation on an image function.

There are 5 mutually-exclusive crop modes for image functions:
* `cropSame` (default) Same crop as parent. Translations do not affect crops. If multiple parents, their size must match or it's an error.
* `cropGrow` grow the crop by the stencil size (outputs touching at least one pixel within parent crop are valid). Translations DO affect crops.
* `cropShrink` shrink crop by stencil size (only outputs totally within the parent crop are valid). Translations DO affect crops.
* `crop(x,y,w,h)` explicitly set a crop
* `cropNone` no boundary condition, infinite extent.

<!---
Technically there are two issues here:
1) are the crops of the input image functions affected by translations? (yes or no)
2) given multiple crops, how are they combined  (same, union, intersection)
We have chosen to reduce these 6 options to 3 reasonable combinations of each, but in the future we could expose all 6.
-->


Summarized in this image (the input is accessed as a non-pointwise stencil):

![Orion Crop Modes](http://stanford.edu/~jhegarty/crop1.png)

We now need to consider what happens when we compose two different image function (e.g. add them together). The `cropGrow` mode takes the union of its inputs. `cropShrink` takes the intersection of its inputs. 

Summarized in this image:

![Orion Crop Meet Modes](http://stanford.edu/~jhegarty/crop2.png)

`cropSame` would raise an error in this case because it requires the all inputs have the same crop.

Crop options are specified along with the type signature of an image function:

    im blurX(x,y) : uint8, cropGrow
      a(x-1,y)+a(x+1,y)+b(x-1,y)+b(x+1,y)
    end

crop modes influence 2 things:
* By default, a compiled image function calculates the area defined by its crop region. There are other options for which region to calculate described later in the 'API' section. Calculating an infinite image function without explicitly setting a crop will yield a compile error.
* Areas outside of the crop region of an image are treated as boundary conditions.

Boundary Conditions
-------------------

Outside of an image functions crop region, Orion will calculate the image as a boundary condition. Orion supports a number of default boundary conditions:

* `constant(N)`  constant value `N`
* `clamp` (default) clamp to nearest valid pixel in image
* `wrap` wraparound - toroidal

Image functions use the `clamp` boundary condition by default.

As with crop options, boundary conditions are specified along with the type of the image function, and propagate:

    im blurX(x,y) : constant(255) a(x-1,y)+a(x+1,y) end

> Tip: Orion doesn't perform particularly well in the boundary section of an image, because this is typically not a common case so it isn't well optimized. Try to avoid reading from the boundary portion of an image.

Standard Library
----------------

This section lists Orion functions for a number of standard operations. It typically only makes sense to call these from within Orion!

`orion.exp(x)` e^x

`orion.abs(x)` absolute value

`orion.floor(x)` floor

`orion.ceil(x)` ceil

`orion.arrayAnd(x)` x must be an array of bools. ANDs each element of the array together and yields a bool.

`orion.pow(x,a)` x^a

`orion.sum(a,b,c, ...)`. Commutative, associative sum

`orion.min(a,b,c, ...)`. min of inputs

`orion.max(a,b,c, ...)`. max of inputs

`orion.dot(a,b)` dot product of a and b

`orion.cross(a,b)` a,b must be 3 element arrays.

`orion.vectorSelect(cond,a,b)` Does an elementwise select. cond,a, and b must be arrays of the same length.

`orion.gather(input, X, Y, xRange, yRange, clamp)` Performs a gather on the argument `input`. This is essentially equivilant to writing `im(x,y) input(x+X,y+Y) end`, however `X` and `Y` can be arbitrary orion expressions instead of constants, which is not typically allowed. `X` must be within the range `[-xRange, xRange]` and Y within the range `[-yRange,yRange]`, and they both must be integers. `clamp` indicates the behavior if `X,Y` fall out of this range. If `clamp` is true, `X` and `Y` are clamped to the range. If `clamp` is false, orion throws an assert if `X` or `Y` are out of the range.

### Debugging functions ###

`orion.assert( expr, printValue, condition)` Raises a runtime exception if `condition` is false, and prints the number `printVal` to the console so that you can identify which assert failed. If `condition` is true, it returns the value `expr`.

`orion.print(value)` Prints `value` to the console.

Remember, Orion is purely functional, so the result of assert and print must be returned by a function to have an effect. This is very different compared to imperative languages like C:

    im test(x,y)
      tmp = orion.assert(input(x,y) == 3) -- never runs, value tmp is unused so is optimized away.
      orion.print(input(x,y))  -- works, prints every pixel of input to the console
    end

API
---

The Orion system contains a number of Lua functions that interact with the language (e.g. loading images, compiling). This section contains a list of all these functions.

### Inputs/Outputs ###

Orion supports creating 'input image functions' which can be bound to input data at runtime.

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

### Types ###

`orion.type.uint(I,F)` or `orion.type.uintI_F` fractional bits `F` can be omitted.

`orion.type.int(I,F)` or `orion.type.intI_F` fractional bits `F` can be omitted.

`orion.type.float(F,E)` or `orion.type.floatF_E` If only one argument is passed, it must be 32 (cpu float type) or 64 (cpu double type).

`orion.type.bool`

### Image Function API ###

`image` is an Orion image function. This could be an image returned from `orion.load()`, an orion image function, or the result of running a compiled image function.


`image:save(filename)`
	If image is an orion image function, this will compile the image function with default settings, run the compiled code, and save it.

`image()`
 Compile and run the image function with default settings, and then return the calculated image.

`image:compile(compileOptionsTable)` Compile with custom compile options, and return the function that calculates the image function.

`image(x,y)` This will compile the image function, and then calculate its value at (x,y). This is not intended to be high-performance, but may be convenient in some circumstances.

`image:width()`
`image:height()`
Return the width/height of the crop area. Returns nil if crop is infinite or undeterminable.

`image:crop()`
Returns a table containing the crop area in `{lowx,lowy,highx,highy}` format. Returns nil if crop is infinite or undeterminable.

`image:type()`
Returns the orion type of the image function.

#### Internal ####

`image:opCount()`

`image:opCost()`
Cost of the ops based on our heuristics for the cost of each op type.

`image:opMix()`
Returns a string of the opmix for this image function.

`image:stencil()`

`image:longestDependencyChain()` # of arithmetic ops in the longest dependency chain in this image function.

### Statistics Function API ###

`stat` is an orion statistics function, returned from `orion.statistic.`. Statistics functions behave like image functions but have a reduced set of options.

`stat()` compile the statistics function, run it, and return its value.

`stat:compile(compileOptionsTable)` Compile with custom compile options, and return the function that calculates the statistics function.

`stat:type()` Return the orion type of the statistics function.