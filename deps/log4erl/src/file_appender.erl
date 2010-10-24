-module(file_appender).

-author("Ahmed Al-Issaei").
-license("MPL-1.1").

-behaviour(gen_event).

-include("../include/log4erl.hrl").
-include_lib("kernel/include/file.hrl").

-import(log4erl_utils, [to_list/1, to_atom/1, to_int/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

%%======================================
%% gen_event callback functions
%%======================================
init({conf, Conf}) when is_list(Conf) ->
    CL = lists:foldl(fun(X, List) ->
			     [proplists:get_value(X,Conf)|List]
		     end,
		     [],
		     [dir, file, type, max, rotation, suffix, level, format]),
    
    %% in case format doesn't exist
    Res = case hd(CL) of
	      undefined ->
		  [_|CL2] = CL,
		  lists:reverse(CL2);
	      _ ->
		  lists:reverse(CL)
	  end,
    init(list_to_tuple(Res));
init({Dir, Fname, {Type, Max}, Rot, Suf, Level})->
    ?LOG("file_appender:init() - with default format~n"),
    init({Dir, Fname, {Type, Max}, Rot, Suf, Level, ?DEFAULT_FORMAT});
%% This one with custom format
init({Dir, Fname, {Type, Max}, Rot, Suf, Level, Pattern} = _Conf) ->
    ?LOG2("file_appender:init() - 1 ~p~n",[_Conf]),
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
% These 2 are for result of reading conf file
init({Dir, Fname, Type, Max, Rot, Suf, Level}) ->
    init({to_list(Dir), to_list(Fname), {to_atom(Type), to_int(Max)}, to_int(Rot), to_list(Suf), to_atom(Level)});
init({Dir, Fname, Type, Max, Rot, Suf, Level, Format}) ->
    init({to_list(Dir), to_list(Fname), {to_atom(Type), to_int(Max)}, to_int(Rot), to_list(Suf), to_atom(Level), to_list(Format)});
% This is for reading from a config file only for one appender
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
    ResState = check_rotation(State),
    do_log(LLog, ResState),
    {ok, ResState}.


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
    file:rename(Src, Fname),
    {ok ,Fd2} = file:open(Src, ?FILE_OPTIONS_ROTATE),
    State2 = #file_appender{dir = Dir, file_name = Fn, fd = Fd2, counter=C, log_type = Ltype, rotation = Rot, suffix=Suf, level=Level, format=Format},
    {ok, State2}.

rotate_daily(#file_appender{fd = Fd, dir=Dir,  file_name=Fn, counter=Cntr, rotation=Rot, suffix=Suf, log_type=Ltype, level=Level, format=Format} = _S, {Year, Month, Day}) ->
    file:close(Fd),
    ?LOG("Starting daily rotation~n"),
    Date = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day])),
    Folder = Dir ++ "/" ++ Fn ++ "." ++ lists:flatten(io_lib:format("~4.10.0B-~2.10.0B", [Year, Month])),
    Src = Dir ++ "/" ++ Fn ++ "." ++ Suf,
    Fname = Folder ++ "/" ++ Fn ++ "." ++ Date ++ "." ++ Suf,
    ?LOG2("Moving file from ~p to ~p~n", [Src, Fname]),

    % each month has it's own log dir
    file:make_dir(Folder),
    file:rename(Src, Fname),

    case erlang:date() of
        {Year, Month, _} -> ok;
        _ ->
            % compress to zip and remove all previous month log files
            RemoveOld = fun() ->
                {ok, _} = zip:create(Folder ++ ".zip", [Folder]),
                {ok, Files} = file:list_dir(Folder),
                lists:foreach(fun(F) -> file:delete(Folder ++ "/" ++ F) end, Files),
                file:del_dir(Folder)
            end,
            spawn(RemoveOld)
    end,

    {ok, Fd2} = file:open(Src, ?FILE_OPTIONS_ROTATE),
    State2 = #file_appender{dir = Dir, file_name = Fn, fd = Fd2, counter=Cntr, log_type = Ltype, rotation = Rot, suffix=Suf, level=Level, format=Format},
    {ok, State2}.



% Check if the file needs to be rotated
% ignore in case of if log type is set to time instead of size	    
check_rotation(State) ->
    #file_appender{dir=Dir, file_name=Fname, log_type = #log_type{type=T, max=Max}, suffix=Suf} = State,
    case T of
	size ->
	    File = Dir ++ "/" ++ Fname ++  "." ++ Suf,
	    case file:read_file_info(File) of
	      {ok, #file_info{size = Size}} when Size > Max ->
	        {ok, State2} = rotate(State),
	        State2;
	      {error, _Error} ->
	        {ok, State2} = rotate(State),
	        State2;
	      _ ->
	        State
	    end;  

	daily ->
	    File = Dir ++ "/" ++ Fname ++  "." ++ Suf,
	    {ok, Finfo} = file:read_file_info(File),
	    {CDate, _CTime} = Finfo#file_info.ctime,
	    case erlang:date() of
		CDate ->
		    State;
		_ ->
		    {ok, State2} = rotate_daily(State, CDate),
		    State2
	    end;

	_ ->
	    State
    end.
