-module(beam_renamer_tests).

-include_lib("eunit/include/eunit.hrl").

replaces_in_atom_table_test() ->
    'x^' = run_literal(x, 'x^', x).

replaces_in_constant_pool_test() ->
    ['x^']       = run_literal(x, 'x^', [x]),
    ['x^', 'x^'] = run_literal(x, 'x^', [x, x]),
    {'x^', 'x^'} = run_literal(x, 'x^', {x, x}),
    {[{'x^'}]}   = run_literal(x, 'x^', {[{x}]}).

run_literal(Name0, Name, Term) ->
    run_with_renamed_module(
      fun() -> Name:f() end, 
      mk_module(Name0, [erl_syntax:abstract(Term)]), 
      Name).

run_with_renamed_module(Fun, BeamBin, Name) -> 
    Bin = beam_renamer:rename(BeamBin, Name),
    unload_module(Name),
    {module, _} = code:load_binary(Name, "dummy.beam", Bin),
    try Fun() 
    after unload_module(Name)
    end.
	    
unload_module(ModName) ->
    code:purge(ModName),
    code:delete(ModName).
    

mk_module(ModName, FuncBody) ->
    {ok, ModName, Bin} = compile:forms(mk_module_forms(ModName, FuncBody)),
    Bin.

mk_module_forms(ModName, FuncBody) ->
    erl_syntax:revert_forms(
      [erl_syntax:attribute(
	 erl_syntax:atom(module), 
	 [erl_syntax:atom(ModName)]),
       erl_syntax:attribute(
	 erl_syntax:atom(compile), 
	 [erl_syntax:atom(export_all)]),
       erl_syntax:function(
 	 erl_syntax:atom(f), 
 	 [erl_syntax:clause([], FuncBody)])]).
    
