-module(log_formatter).

-compile(export_all).

-include("../include/log4erl.hrl").

%-record(log, {level, msg, data, time}).

test() ->
    Log = #log{level = warn,
	              msg = "logging message for testing purposes ~p",
	              data = [tt],
	              time = calendar:local_time()},
    %Ts = "%j %T [%L] - %l %n",
    Ts = "[%L] %l%n",
    {ok, Tokens} = parse(Ts),
        T = format(Log, Tokens),
        io:format("~s",[T]).

test2(Num) ->
    Ts = "%j %T [%L] - %l",
    {ok, Tokens} = parse(Ts),
    Logs = lists:map(fun(X) ->
			     {X, make_log(warn, "testing a lot", [])}
		     end,
		     lists:seq(1, Num)),
    lists:map(fun(X) ->
		      N = element(1,X),
		      X1 = element(2, X),
		      X2 = format(X1, Tokens),
		      io:format("~p: ~s~n",[N, X2])
	      end, Logs),
    ok.

make_log(Level, Msg, Data) ->
    #log{level = Level,
	 msg = Msg,
	 data = Data,
	 time = calendar:local_time()}.


%%%%%%%%%%%%%%%%%%%
%%  functions
%%%%%%%%%%%%%%%%%%%
format(Log, Tokens) ->
    ?LOG2("log_formatter formatting log: ~p~n",[Log]),
    F = fun(X) ->
		M = get_token_value(X,Log),
		M
	end,
    L = lists:map(F, Tokens),
    L.

get_token_value(date, Log) ->
    D = Log#log.time,
    {{Y, M, Dd},_} = D,
    [C,B,A] = lists:map(
		fun(X) ->
			integer_to_list(X)
		end,
		[Y,M,Dd]),
    Res = A ++ "-" ++ B ++ "-" ++ C,
    Res;
get_token_value(date2, Log) ->
    D = Log#log.time,
    {{Y, M, Dd},_} = D,
    [A,B,C] = lists:map(
		fun(X) ->
			X2 = integer_to_list(X),
			log4erl_utils:return_2columns(X2)
%% 			case string:len(X2) > 1 of
%% 			    false ->
%% 				"0" ++ X2;
%% 			    _ ->
%% 				X2
%% 			end
		end,
		[Y,M,Dd]),
    Res = A ++ "-" ++ B ++ "-" ++ C,
    Res;
get_token_value(time, Log) ->
    D = Log#log.time,
    {_,{H, M, S}} = D,
    [A,B,C] = lists:map(
		fun(X) ->
			integer_to_list(X)
		end,
		[H,M,S]),    
    Res = A ++ ":" ++ B ++ ":" ++ C,
    Res;
get_token_value(time2, Log) ->
    D = Log#log.time,
    Ms = Log#log.millis,
    {_,{H, M, S}} = D,
    [A,B,C,E] = lists:map(
		fun(X) ->
			X2 = integer_to_list(X),
			log4erl_utils:return_2columns(X2)
%% 			case string:len(X2) > 1 of
%% 			    false ->
%% 				"0" ++ X2;
%% 			    _ ->
%% 				X2
%% 			end
		end,
		[H,M,S, Ms]),    
    Res = A ++ ":" ++ B ++ ":" ++ C ++ "." ++ E,
    Res;
get_token_value(year4, Log) ->
    D = Log#log.time,
    {{Y, _,_},_} = D,
    integer_to_list(Y);
get_token_value(year2, Log) ->
    D = Log#log.time,
    {{Y, _,_},_} = D,
    L = integer_to_list(Y),
    string:substr(L,3,2);
get_token_value(month, Log) ->
    {{_, M,_},_} = Log#log.time,
    integer_to_list(M);
get_token_value(month2, Log) ->
    {{_,M,_},_} = Log#log.time,
    log4erl_utils:get_month_name(M);
get_token_value(month3, Log) ->
    {{_, M,_},_} = Log#log.time,
    log4erl_utils:get_month_long_name(M);
get_token_value(day, Log) ->
    D = Log#log.time,
    {{_, _,Dd},_} = D,
    integer_to_list(Dd);
get_token_value(hour, Log) ->
    D = Log#log.time,
    {_,{H, _,_}} = D,
    integer_to_list(H);
get_token_value(minute, Log) ->
    D = Log#log.time,
    {_,{_, M,_}} = D,
    integer_to_list(M);
get_token_value(second, Log) ->
    D = Log#log.time,
    {_,{_, _,S}} = D,
    integer_to_list(S);
get_token_value(millis, Log) ->
    Ms = Log#log.millis,
    integer_to_list(Ms);
get_token_value(log, Log) ->
    Msg = Log#log.msg,
    Data = Log#log.data,
    io_lib:format(Msg, Data);
get_token_value(level, Log) ->
    atom_to_list(Log#log.level);
get_token_value(new_line, _Log) ->
    "\n";
% GMT TZ
get_token_value(iso_format, Log) ->
    [Date] = calendar:local_time_to_universal_time_dst(Log#log.time),
    get_token_value(date2, Log) ++ "T" ++ get_token_value(time2, Log#log{time=Date}) ++ "Z";
% With TZ
get_token_value(iso_format2, Log) ->
    D = Log#log.time,
    [UD] = calendar:local_time_to_universal_time_dst(D),
    Ds = calendar:datetime_to_gregorian_seconds(D),
    UDs = calendar:datetime_to_gregorian_seconds(UD),
    TZ = case Ds-UDs > 0 of
	     true ->
		 {_,{A,B,_}} = calendar:gregorian_seconds_to_datetime(Ds-UDs),
		 A2 = log4erl_utils:return_2columns(integer_to_list(A)),
		 B2 = log4erl_utils:return_2columns(integer_to_list(B)),
		 "+" ++ A2 ++ ":" ++ B2;
	     _ ->
		 {_,{C,D,_}} = calendar:gregorian_seconds_to_datetime(UDs-Ds),
		 C2 = log4erl_utils:return_2columns(integer_to_list(C)),
		 D2 = log4erl_utils:return_2columns(integer_to_list(D)),
		 "-" ++ C2 ++ ":" ++ D2
	 end,
    get_token_value(date2, Log) ++ "T" ++ get_token_value(time2, Log) ++ TZ;
% TZ only
get_token_value(time_zone, Log) ->
    D = Log#log.time,
    [UD] = calendar:local_time_to_universal_time_dst(D),
    Ds = calendar:datetime_to_gregorian_seconds(D),
    UDs = calendar:datetime_to_gregorian_seconds(UD),
    TZ = case Ds-UDs > 0 of
	     true ->
		 {_,{A,B,_}} = calendar:gregorian_seconds_to_datetime(Ds-UDs),
		 A2 = log4erl_utils:return_2columns(integer_to_list(A)),
		 B2 = log4erl_utils:return_2columns(integer_to_list(B)),
		 "+" ++ A2 ++ ":" ++ B2;
	     _ ->
		 {_,{C,D,_}} = calendar:gregorian_seconds_to_datetime(UDs-Ds),
		 C2 = log4erl_utils:return_2columns(integer_to_list(C)),
		 D2 = log4erl_utils:return_2columns(integer_to_list(D)),
		 "-" ++ C2 ++ ":" ++ D2
	 end,
    TZ;
get_token_value(A, _Log) ->
    A.

%%%%%%%%%%%%%%%%%%%
%% parse functions
%%%%%%%%%%%%%%%%%%%
parse(M) ->
    ?LOG2("log_formatter parsing ~p~n",[M]),
    try
	Tokens = parse2(M,[]),
	?LOG2("Received tokens ~p~n",[Tokens]),
	{ok, Tokens}
    catch
	E:R ->
	    {error, {E,R}}
    end.

parse2([], Acc) ->
    lists:reverse(Acc);
parse2([$\% | R], Acc) ->
    [S|R2] = R,
    T = parse_char(S),
    parse2(R2,[T|Acc]);
parse2([S | R], Acc) ->
    parse2(R, [S|Acc]).

parse_char($d) ->
    date;
parse_char($j) ->
    date2;
parse_char($t) ->
    time;
parse_char($T) ->
    time2;
parse_char($y) ->
    year2;
parse_char($Y) ->
    year4;
% 1
parse_char($M) ->
    month;
% Jan
parse_char($b) ->
    month2;
% January
parse_char($B) ->
    month3;
parse_char($D) ->
    day;
parse_char($h) ->
    hour;
parse_char($m) ->
    minute;
parse_char($s) ->
    second;
parse_char($l) ->
    log;
parse_char($L) ->
    level;
parse_char($n) ->
    new_line;
parse_char($i) ->
    millis;
parse_char($I) ->
    iso_format;
parse_char($S) ->
    iso_format2;
parse_char($Z) ->
    time_zone;
parse_char(C) ->
    C.

