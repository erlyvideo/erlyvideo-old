-module(elixir_loader).
-include("../../elixir/include/elixir.hrl").
-export([undefined_function/3, undefined_lambda/3]).

undefined_lambda(M, F, A) -> error_handler:undefined_lambda(M, F, A).


undefined_function(M, F, A) ->
  io:format("Lookup for module ~p~n", [M]),
  Name = lists:flatten(io_lib:format("~p.ex", [M])),
  case file:read_file_info(Name) of
    {error, _} -> error_handler:undefined_function(M, F, A);
    {ok, _} -> elixir_method_lookup(atom_to_list(M), F, A)
  end.

elixir_method_lookup(M, F, A) when is_list(M) ->
  application:start(elixir),
  ZZ = (catch elixir:file(M++".ex")),
  io:format("eval path ~p (~p)~n", [M++".ex", ZZ]),
    
  Klass = string:join([[string:to_upper(H)]++T || [H|T] <- string:tokens(M, "_")], ""),
  io:format("make lookup for module ~p~n", [Klass]),
  case (catch elixir_constants:lookup(list_to_atom(Klass))) of
    #elixir_module__{name = ExMod} ->
      apply(ExMod, F, [[]|A]);
    _Else ->
      io:format("Error for ~p~n", [_Else]),
      error_handler:undefined_function(list_to_atom(M), F, A)
  end.
