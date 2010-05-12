#!/usr/bin/env escript
%% -*- erlang -*-


main([Name]) ->
  Module = list_to_atom(Name),
  Module:test().