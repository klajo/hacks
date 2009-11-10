This is a collection of various hacks
=====================================

Well, not a very large collection right now, since there's only few
hacks here, but still... :-)

beam
----

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


stem
----

`stem_en` is an implementation of the [English Porter2 stemming
algorithm](http://snowball.tartarus.org/algorithms/english/stemmer.html).

The implementation has been tested on the sample English vocabulary
and produces an identical result when compared to its stemmed
reference equivalent.  Some performance figures: stems 400,000 words
per second on a 2.53 GHz Core 2 Duo MacBook Pro.

`stem_sv` is its [Swedish
counterpart](http://snowball.tartarus.org/algorithms/swedish/stemmer.html).
Some performance figures: stems 900,000 words per second on a 2.53 GHz
Core 2 Duo MacBook Pro.
