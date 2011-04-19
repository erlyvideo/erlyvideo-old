Definitions.

D   = [0-9]
L   = [A-Z0-9a-z_:]
LS  = [][A-Z0-9a-z_\s%*$#@().><:/\-\+]
WS  = ([\000-\s]|%.*)

SPACE = [\s\t\r\n]+
NAME = ([A-Z0-9a-z_]+)

Rules.

server : {token,{server_def,TokenLine, server}}.
module : {token,{module_def,TokenLine, module}}.
{D}+            : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
%%{L}+_appender   : {token,{apender,TokenLine,list_to_atom(TokenChars)}}.
{L}+	 	: {token,{atom,TokenLine,list_to_atom(TokenChars)}}.
'{LS}+' 	: {token,{val,TokenLine,strip(TokenChars,TokenLen)}}.
"{LS}+" 	: {token,{val,TokenLine,strip(TokenChars,TokenLen)}}.
[{};]     	: {token,{list_to_atom(TokenChars),TokenLine}}.
{WS}+		: skip_token.


Erlang code.

strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).
