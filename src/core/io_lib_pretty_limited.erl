-module(io_lib_pretty_limited).
-author('Max Lapshin <max@maxidoors.ru>').



-export([print/2]).

print(Term, Limit) ->
  print_term(<<>>, Term, Limit).

print_term(_Out, _, Limit) when Limit < 0 ->
  <<"..">>;
  
print_term(Out, Bin, Limit) when is_binary(Bin) andalso size(Bin) >= Limit ->
  <<Out/binary, "<<\"..very big..\">>">>;

print_term(Out, Bin, _Limit) when is_binary(Bin) ->
  <<Out/binary, (list_to_binary(io_lib_pretty:print(Bin)))/binary>>;

print_term(Out, Term, _Limit) when 
  is_atom(Term) orelse is_reference(Term) orelse is_port(Term) orelse is_function(Term) orelse is_number(Term) orelse is_pid(Term) ->
  <<Out/binary, (list_to_binary(io_lib_pretty:print(Term)))/binary>>;

print_term(Out, Atom, _Limit) when is_atom(Atom) ->
  <<Out/binary, (atom_to_binary(Atom,utf8))/binary>>;

print_term(Out, Term, Limit) when is_list(Term) ->
  case io_lib:printable_list(Term) of
    true -> print_printable_list(Out, Term, Limit);
    false -> print_list(<<Out/binary, "[">>, Term, <<"]">>, Limit)
  end;

print_term(Out, Term, Limit) when is_tuple(Term) ->
  print_list(<<Out/binary, "{">>, tuple_to_list(Term), <<"}">>, Limit).


print_printable_list(Out, String, Limit) when length(String) > Limit - 4 ->
  print_printable_list(Out, lists:sublist(String, Limit - 4), Limit);
  
print_printable_list(Out, String, _) ->
  append_characters(<<Out/binary, $">>, String).

append_characters(Out, []) -> <<Out/binary, $">>;
append_characters(Out, [$"|String]) -> append_characters(<<Out/binary, $\\, $">>, String);
append_characters(Out, [C|String]) -> append_characters(<<Out/binary, C>>, String).
  
  

print_list(Out, _, End, Limit) when size(Out) > Limit ->
  <<Out/binary, "...", End/binary>>;
print_list(Out, [], End, _Limit) ->
  <<Out/binary, End/binary>>;  
print_list(Out, [Term], End, Limit) ->
  <<Out/binary, (print_term(<<>>, Term, Limit - size(Out)))/binary, End/binary>>;
print_list(Out, [Term|List], End, Limit) ->
  print_list(<<Out/binary, (print_term(<<>>, Term, Limit - size(Out)))/binary, ",">>, List, End, Limit).


-include_lib("eunit/include/eunit.hrl").


error_formatter_test() ->
  ?assertEqual(<<"{a,\"a\\\"a\",5,<<\"z\">>}">>, print({a,"a\"a", 5, <<"z">>}, 40)).

