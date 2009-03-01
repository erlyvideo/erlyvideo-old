-module(file_appender).

-author("Ahmed Al-Issaei").
-license("MPL-1.1").

-behaviour(gen_event).

-include("../include/log4erl.hrl").
-include_lib("kernel/include/file.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

%%======================================
%% gen_event callback functions
%%======================================
init({Dir, Fname, {Type, Max}, Rot, Suf, Level})->
    ?LOG("file_appender:init() - with default format~n"),
    init({Dir, Fname, {Type, Max}, Rot, Suf, Level, ?DEFAULT_FORMAT});
%% This one with custom format
init({Dir, Fname, {Type, Max}, Rot, Suf, Level, Pattern} = _Conf) ->
    ?LOG("file_appender:init() - 1~n"),
    File = Dir ++ "/" ++ Fname ++ "." ++ Suf,
    {ok, Fd} = file:open(File, ?FILE_OPTIONS),
    Ltype = #log_type{type = Type, max = Max},
    % Check Rot >= 0
    Rot1 = case Rot < 0 of
	       true ->
		   0;
	       false ->
		   Rot
	   end,
    ?LOG2("To parse format with customer format ~p~n",[Pattern]),
    {ok, Format} = log_formatter:parse(Pattern),
    ?LOG2("Adding format of ~p~n",[Format]),
    State = #file_appender{dir = Dir, file_name = Fname, fd = Fd, counter=0,
			   log_type = Ltype, rotation = Rot1, suffix=Suf,
			   level=Level, format=Format},
    ?LOG2("file_appender:init() with conf ~p~n",[State]),
    {ok, State};
init(Conf) when is_list(Conf) ->
    ?LOG2("file_appender:init() ~p~n",[Conf]),
    case file:consult(Conf) of
	{error, Reason} ->
	    error_logger:error_msg("file_appender: couldn't consult Conf file~n"),
	    {error, file:format_error(Reason)};
	{ok, [Terms]} ->
	    init(Terms)
    end.

handle_event({change_level, Level}, State) ->
    State2 = State#file_appender{level = Level},
    ?LOG2("Changed level to ~p~n",[Level]),
    {ok, State2};
handle_event({log,LLog}, State) ->
    ?LOG2("handl_event:log = ~p~n",[LLog]),
    do_log(LLog, State),
    Res = check_rotation(State),
    {ok, Res}.


handle_call({change_format, Format}, State) ->
    ?LOG2("Old State in file_appender is ~p~n",[State]),
    {ok, Tokens} = log_formatter:parse(Format),
    ?LOG2("Adding format of ~p~n",[Tokens]),
    State1 = State#file_appender{format=Tokens},
    {ok, ok, State1};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    ?LOG2("~w received unknown message: ~p~n", [?MODULE, _Info]),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%======================================
%% internal callback functions
%%======================================
do_log(#log{level = L} = Log,#file_appender{fd = Fd, level=Level, format=Format} = _State) when is_atom(L) ->
    ?LOG2("Formatting ~p~n",[Format]),
    ToLog = log4erl_utils:to_log(L, Level),
    case ToLog of
	true ->
	    M = log_formatter:format(Log, Format),
	    file:write(Fd, M);
	false ->
	    ok
    end;
do_log(_Other, _State) ->
    ?LOG2("unknown level ~p~n",[_Other]),
    ok.

rotate(#file_appender{fd = Fd, dir=Dir,  file_name=Fn, counter=Cntr, rotation=Rot, suffix=Suf, log_type=Ltype, level=Level, format=Format} = _S) ->
    file:close(Fd),
    ?LOG("Starting rotation~n"),
    C = if
	    Rot == 0 ->
		0;
	    Cntr >= Rot ->
		1;
	    true ->
		Cntr+1
	end,
    Src = Dir ++ "/" ++ Fn ++ "." ++ Suf,
    Fname = case C of
		0 ->
		    Dir ++ "/" ++ Fn ++ "." ++ Suf;
		_ ->
		    Dir ++ "/" ++ Fn ++ "_" ++ integer_to_list(C) ++ "." ++ Suf
	    end,
    ?LOG2("Renaming file from ~p to ~p~n",[Src, Fname]),
    file:copy(Src, Fname),
    {ok ,Fd2} = file:open(Src, ?FILE_OPTIONS_ROTATE),
    State2 = #file_appender{dir = Dir, file_name = Fn, fd = Fd2, counter=C, log_type = Ltype, rotation = Rot, suffix=Suf, level=Level, format=Format},
    {ok, State2}.

% Check if the file needs to be rotated
% ignore in case of if log type is set to time instead of size	    
check_rotation(State) ->
    #file_appender{dir=Dir, file_name=Fname, log_type = #log_type{type=T, max=Max}, suffix=Suf} = State,
    case T of
	size ->
	    File = Dir ++ "/" ++ Fname ++  "." ++ Suf,
	    {ok, Finfo} = file:read_file_info(File),
	    Size = Finfo#file_info.size,
	    if
		Size > Max ->
		    {ok, State2} = rotate(State),
		    State2;
		true ->
		    State
	    end;
	%% time-based rotation is not implemented yet
	_ ->
	    State
    end.

