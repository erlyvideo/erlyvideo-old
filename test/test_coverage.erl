-module(test_coverage).
-export([analyze/1]).

analyze(Module) ->
  cover:start(),
  cover:compile_module(Module),
  amf3_tests:test(),
  {ok,{amf3,{Covered,UnCovered}}} = cover:analyse(Module,coverage,module),
  {ok,File} = cover:analyse_to_file(Module),
  Total = Covered + UnCovered,
  Coverage = Covered/Total * 100,
  io:fwrite("~n~nCoverage Analysis: ~n",[]),
  io:fwrite("========================== ~n~n",[]),
  io:fwrite("Module: ~w~n",[Module]),
  io:fwrite("Executable Lines: ~w~n",[Total]),
  io:fwrite("Covered Lines: ~w~n",[Covered]),
  io:fwrite("UnCovered Lines: ~w~n",[UnCovered]),
  io:fwrite("Coverage: ~f %~n",[Coverage]),
  io:fwrite("~nFor more details see file: build/~s~n~n",[File]).
  