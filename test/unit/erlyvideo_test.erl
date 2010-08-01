#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -name test


main(["ems_media"]) ->
  Node = list_to_atom("ems@"++lists:nth(2, string:tokens(atom_to_list(node()), "@"))),
  rpc:call(Node, ems_media, test, []);


main([Name]) ->
  Module = list_to_atom(Name),
  Module:test().