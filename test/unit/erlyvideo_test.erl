#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin


main([Name]) ->
  Module = list_to_atom(Name),
  Module:test().