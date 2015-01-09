local cstdio = terralib.includec("stdio.h")
local cstdlib = terralib.includec("stdlib.h")


darkroom={}

terralib.require("common")
terralib.require("frontend")
terralib.require("types")
require "stencil"
require "optimizations"
require "ir"
terralib.require("ast")
require "typedAST"
require "kernelgraph"
require "schedule"
terralib.require("api")
terralib.require("backend_terra")

return {
  name = "darkroom";
  entrypoints = {"im"};
  keywords = {"map","reduce","let","in","switch","default","case","iterate"};
  statement = function(self,lex)
    local imfunc = darkroom.Parser.Parse(darkroom.lang,lex,"imageFunction")

    return 
      function(envfn)
      return darkroom.compileTimeProcess(imfunc,envfn)
      end, {imfunc.identifier}
  end;
  localstatement = function(self,lex)
    local imfunc = darkroom.Parser.Parse(darkroom.lang,lex,"imageFunction")
 
    return 
      function(envfn)
      return darkroom.compileTimeProcess(imfunc,envfn)
      end, {imfunc.identifier}
  end;
  expression = function(self,lex)
    local imfunc = darkroom.Parser.Parse(darkroom.lang,lex,"imageFunction")

    return 
      function(envfn)
      return darkroom.compileTimeProcess(imfunc,envfn)
      end
  end
}
