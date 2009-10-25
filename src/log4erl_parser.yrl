Nonterminals cutoff_level loggers logger appenders appender props prop value.

% 'loger' is missing 'g' not to be confused with Nonterminals 
% check 'log4erl_conf.xrl'
Terminals '{' '}' ',' '=' 'loger' 'default' 'integer' 'val' 'atom'.

Rootsymbol cutoff_level.

cutoff_level -> loggers : '$1'.
cutoff_level -> prop loggers : ['$1'] ++ '$2'.

loggers -> logger : ['$1'].
loggers -> logger loggers : ['$1'] ++ '$2'.

logger -> loger value '{' appenders '}' : {logger, '$2', '$4'}.
logger -> loger '{' appenders '}' : {default_logger, '$3'}.
logger -> loger default '{' appenders '}' : {default_logger, '$4'}.

appenders -> appender : ['$1'].
appenders -> appender appenders : ['$1'] ++ '$2'.

appender -> value value '{' props '}' : {appender, '$1', '$2', '$4'}.

props -> prop : ['$1'].
props -> prop ',' props : ['$1'] ++ '$3'.

prop -> value '=' value : {'$1', '$3'}.

value -> val : unwrap('$1').
value -> integer : unwrap('$1').
value -> atom : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.
