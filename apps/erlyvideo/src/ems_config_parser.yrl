Nonterminals servers server module section_name key_values value key_value values.

Terminals '{' '}' 'server_def' 'module_def' 'integer' 'val' 'atom' ';'.

Rootsymbol servers.

servers -> server : ['$1'].
servers -> module : ['$1'].
servers -> key_value : ['$1'].
servers -> server servers : ['$1'] ++ '$2'.
servers -> module servers : ['$1'] ++ '$2'.
servers -> key_value servers : ['$1'] ++ '$2'.


section_name -> atom : unwrap('$1').
server -> server_def section_name '{' key_values '}' : {server, '$2', '$4'}.
module -> module_def section_name '{' key_values '}' : {'$2', '$4'}.


key_values -> key_value : ['$1'].
key_values -> key_value key_values : ['$1'] ++ '$2'.


key_value -> atom values ';' : list_to_tuple([unwrap('$1')] ++ '$2').

values -> value : ['$1'].
values -> value values : ['$1'] ++ '$2'.

value -> val : unwrap('$1').
value -> integer : unwrap('$1').
value -> atom : unwrap('$1').


Erlang code.

unwrap({_,_,V}) -> io:format("Unwrap: ~p~n", [V]), V.
