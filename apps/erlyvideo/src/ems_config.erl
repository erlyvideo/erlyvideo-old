-module(ems_config).

-export([test/0]).


test() ->
  {ok, Lex, _} = ems_config_lex:string(binary_to_list(element(2,file:read_file("priv/example.conf")))),
  io:format("Lex: ~p~n", [Lex]),
  ems_config_parser:parse(Lex).

