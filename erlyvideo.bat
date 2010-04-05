@set ERL_LIBS=deps;lib;plugins
erl -make
erl -pa ebin -boot start_sasl -s erlyvideo -sname ems

