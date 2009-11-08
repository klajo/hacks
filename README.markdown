This is a collection of various hacks
=====================================

Well, not a very large right now, since there's only one hack here,
but still... :-)

beam_renamer
------------

The idea behind `beam_renamer` is to be able to load an erlang module
(which is already compiled) under a different name.  Normally, there's
an error message if one does that:

    1> {x, Bin, _} = code:get_object_code(x).
    {x,<<...>>,...}
    2> code:load_binary(y, "y.beam", Bin).
    {error,badfile}
    
    =ERROR REPORT==== 8-Nov-2009::22:01:24 ===
    Loading of y.beam failed: badfile
    
    =ERROR REPORT==== 8-Nov-2009::22:01:24 ===
    beam/beam_load.c(1022): Error loading module y:
      module name in object code is x

This is where `beam_renamer` comes in handy.  It'll rename the module
by replacing the module name *within* the beam file.

    1> {x, Bin0, _} = code:get_object_code(x).
    {x,<<...>>,...}
    2> Bin = beam_renamer:rename(Bin0, y).
    <<...>>
    2> code:load_binary(y, "y.beam", Bin).
    {module,y}

See the comments in the source code for more details.
