-module(log4erl_conf).

-export([conf/1]).

leex(File) ->
    {ok, Bin} = file:read_file(File),
    F = binary_to_list(Bin),
    R = log4erl_lex:string(F),
    case R of
	{ok, Tokens, _} ->
	    Tokens;
	_ ->
	    io:format("Error ~p~n",[R])
    end.

parse(Tokens) ->
    R = log4erl_parser:parse(Tokens),
    case R of
	{ok, T} ->
	    T;
	{error, {Line, _, EMsg}} ->
	    io:format("Error in line ~p with message: ~p~n",[Line, lists:flatten(EMsg)]),
	    throw({parser_error, Line, EMsg})
    end.

conf(File) ->
    application:start(log4erl),
    Tree = parse(leex(File)),
    traverse(Tree).

traverse([]) ->
    ok;
traverse([H|Tree]) ->
    element(H),
    traverse(Tree).

element({cutoff_level, CutoffLevel}) ->
    log_filter_codegen:set_cutoff_level(CutoffLevel);
element({default_logger, Appenders}) ->
    appenders(Appenders);
element({logger, Logger, Appenders}) ->
    log4erl:add_logger(Logger),
    appenders(Logger, Appenders).

appenders([]) ->
    ok;
appenders([H|Apps]) ->
    appender(H),
    appenders(Apps).

appenders(_, []) ->
    ok;
appenders(Logger, [H|Apps]) ->
    appender(Logger, H),
    appenders(Logger, Apps).

appender({appender, App, Name, Conf}) ->
    log4erl:add_appender({App, Name}, {conf, Conf}).

appender(Logger, {appender, App, Name, Conf}) ->
    log4erl:add_appender(Logger, {App, Name}, {conf, Conf}).

