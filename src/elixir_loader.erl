-module(elixir_loader).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").
-export([undefined_function/3, undefined_lambda/3]).

undefined_lambda(M, F, A) -> error_handler:undefined_lambda(M, F, A).


undefined_function(M, F, A) ->
  case read_file_info(M) of
    {error, _} -> error_handler:undefined_function(M, F, A);
    {ok, #file_info{mtime = Mtime}} -> elixir_method_lookup(atom_to_list(M), F, A, Mtime)
  end.

read_file_info(M) ->
  Name = lists:flatten(io_lib:format("~p.ex", [M])),
  file:read_file_info(Name).


elixir_method_lookup(M, F, A, Mtime) when is_list(M) ->
  application:start(elixir),
  Path = M++".ex",
  try elixir:file(Path) of
    _Mod -> io:format("loaded new file ~p~n", [Path]), ok
  catch 
    error:{module_defined, {Klass1, _Path, _}} -> 
      #elixir_module__{name = ExMod1} = elixir_constants:lookup(Klass1),
      reload_module_if_required(Mtime, ExMod1, Path)
  end,
    
  Klass = string:join([[string:to_upper(H)]++T || [H|T] <- string:tokens(M, "_")], ""),
  #elixir_module__{name = ExMod} = Context = elixir_constants:lookup(list_to_atom(Klass)),
      
  case erlang:function_exported(ExMod, F, length(A) + 1) of
    true -> 
      Result = apply(ExMod, F, [Context|A]),
      process_flag(error_handler, ?MODULE),
      Result;
    false -> erlang:raise(error, undef, [{list_to_atom(M), F, A}])
  end.


reload_module_if_required(Mtime, ExMod, Path) ->
  Options = ExMod:module_info(compile),
  {Y,Mon,D,H,Min,S} = proplists:get_value(time, Options),
  LocalCompileTime = calendar:universal_time_to_local_time({{Y,Mon,D},{H,Min,S}}),
  LifeTime = calendar:datetime_to_gregorian_seconds(LocalCompileTime) - calendar:datetime_to_gregorian_seconds(Mtime),
  if 
    LifeTime < 0 ->
      io:format("Reloading ~p~n", [Path]),
      code:soft_purge(ExMod),
      code:delete(ExMod),
      elixir:file(Path),
      ok;
    true -> 
      ok
  end.
  