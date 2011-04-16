%% Author: David Dossot <david@dossot.net>
%% Created: Oct 25, 2009
%% Description: Generates log_filter based on the log4erl cutoff level.
-module(log_filter_codegen).

-export([reset/0, set_cutoff_level/1]).

reset() ->
    set_cutoff_level(debug).

set_cutoff_level(CutoffLevel) ->
    LogFilterMod = log_filter_mod(CutoffLevel),
    compile_and_load_abstract_form(LogFilterMod).

%% Private functions
compile_and_load_abstract_form(AbsForm) ->
    CompRes = compile:forms(AbsForm),
    {ok, Mod, Code} = CompRes,
    code:purge(Mod),
    code:delete(Mod),
    {module, _} = load_module(Mod, Code),
    ok.

log_filter_mod(CutoffLevel) ->
    [{attribute,1,module,log_filter},
     {attribute,3,export,[{cutoff_level,0}]},
     {function,5,cutoff_level,0,[{clause,5,[],[],[{atom,5,CutoffLevel}]}]},
     {eof,5}].