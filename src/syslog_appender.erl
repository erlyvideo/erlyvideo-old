-module(syslog_appender).

-include("../include/log4erl.hrl").

-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 514).

init({conf, Conf}) ->
    Level = proplists:get_value(level, Conf,nil),
    Fac = proplists:get_value(facility, Conf,nil),
    Host =  proplists:get_value(host, Conf,nil),
    Port =  proplists:get_value(port, Conf,nil),
    Format =  proplists:get_value(format, Conf,nil),

    Tup = {Level, Fac, Host, Port, Format},

    case Tup of
	{L, F, nil, nil, nil} ->
	    init({L, F});
	{L, F, H, nil, nil} ->
	    init({L, F, H});
	{L, F, H, nil, Fmt} when not (Fmt == nil) ->
	    init({L, F, H, Fmt});
	{L, F, H, P, Fmt} ->
	    init({L,F,H,P,Fmt})
    end;
%% {Level, Facility, Host, Port, Format}
%% 
init({Level, Fac}) ->
    init({Level, Fac, ?DEFAULT_HOST, ?DEFAULT_PORT, ?DEFAULT_FORMAT});
init({Level, Fac, Host}) ->
    init({Level, Fac, Host, ?DEFAULT_PORT, ?DEFAULT_FORMAT});
init({Level, Fac, Host, Format}) ->
    init({Level, Fac, Host, ?DEFAULT_PORT, Format});
init({Level, Fac, Host, Port, Format}) ->
    {ok, Socket} = gen_udp:open(0),
    F = Fac,
    {ok, Form} = log_formatter:parse(Format),
    facility(Fac),
    State = #syslog_appender{level=Level, facility=F, host=Host, port=Port, socket=Socket, format=Form},
    {ok, State};
init(File) when is_list(File) ->
    case file:consult(File) of
	{error, Reason} ->
	    error_logger:error_msg("syslog_appender: couldn't consult Conf file~n"),
	    {error, file:format_error(Reason)};
	{ok, [Terms]} ->
	    init(Terms)
    end.

handle_event({change_level, Level}, State) ->
    State2 = State#syslog_appender{level = Level},
    ?LOG2("Changed level to ~p~n",[Level]),
    {ok, State2};
handle_event({log,LLog}, State) ->
    ?LOG2("handl_event:log = ~p~n",[LLog]),
    do_log(LLog, State),
    {ok, State}.

handle_call({change_format, Format}, State) ->
    ?LOG2("Old State in console_appender is ~p~n",[State]),
    {ok, Tokens} = log_formatter:parse(Format),
    ?LOG2("Adding format of ~p~n",[Tokens]),
    S2 = State#syslog_appender{format=Tokens},
    {ok, ok, S2};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_log(#log{level = L} = Log, #syslog_appender{level=Level} = State) ->
    %% Syslog levels are a little bit differnt
    ToLog = to_log(L, Level),
    case ToLog of
	true ->
	    Pid = State#syslog_appender.socket,
	    Host = State#syslog_appender.host,
	    Port = State#syslog_appender.port,
	    Format = State#syslog_appender.format,
	    Fac = State#syslog_appender.facility,
	    Msg = log_formatter:format(Log, Format),

	    L2 = level(L),
	    do_send(Pid, Host, Port, {Fac, L2, Msg});
	false ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The following code has been shamefully copied from
%% syslog.erl written by Torbjorn Tornkvist and downloaded
%% from the user contribution section in erlang.org
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Convenient routine for specifying levels.
%% modified by ahmed al-issaei

level(all)       -> 0; % to allow 'all' in conf parameters for level
level(emergency) -> 0; % system is unusable
level(emerg)     -> 0; % shortcut for emergency
level(alert)     -> 1; % action must be taken immediately 
level(critical)  -> 2; % critical conditions
level(crit)      -> 2; % shortcut for critical
level(fatal)     -> 2; % another shortcut to critical
level(error)     -> 3; % error conditions
level(warning)   -> 4; % warning conditions
level(warn)      -> 4; % shortcut for warning
level(notice)    -> 5; % normal but significant condition 
level(info)      -> 6; % informational
level(debug)     -> 7; % debug-level messages
level(_)         -> 1. % anything else is alert

%% Convenient routine for specifying facility codes
%% modified by ahmed al-issaei
facility(kern)     -> (0 bsl 3); % kernel messages 
facility(user)     -> (1 bsl 3); % random user-level messages 
facility(mail)     -> (2 bsl 3); % mail system 
facility(daemon)   -> (3 bsl 3); % system daemons 
facility(auth)     -> (4 bsl 3); % security/authorization messages
facility(syslog)   -> (5 bsl 3); % messages generated internally by syslogd 
facility(lpr)      -> (6 bsl 3); % line printer subsystem 
facility(news)     -> (7 bsl 3); % network news subsystem 
facility(uucp)     -> (8 bsl 3); % UUCP subsystem 
facility(cron)     -> (9 bsl 3); % clock daemon 
facility(authpriv) -> (10 bsl 3); % security/authorization messages (private) 
facility(ftp)      -> (11 bsl 3); % ftp daemon
facility(ntp)      -> (12 bsl 3); % ntp daemon
facility(audit)    -> (13 bsl 3); % log audit
facility(alert)    -> (14 bsl 3); % log alert
facility(clock)    -> (15 bsl 3); % clock daemon
facility(local0)   -> (16 bsl 3); % local use 0
facility(local1)   -> (17 bsl 3); % local use 1
facility(local2)   -> (18 bsl 3); % local use 2
facility(local3)   -> (19 bsl 3); % local use 3
facility(local4)   -> (20 bsl 3); % local use 4
facility(local5)   -> (21 bsl 3); % local use 5
facility(local6)   -> (22 bsl 3); % local use 6
facility(local7)   -> (23 bsl 3); % local use 7
facility(_)        -> facility(user). % anything else is user

%% priorities/facilities are encoded into a single 32-bit 
%% quantity, where the bottom 3 bits are the priority (0-7) 
%% and the top 28 bits are the facility (0-big number).    
do_send(S,Host,Port,{Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg ++ "\n",
    gen_udp:send(S,Host,Port,Packet);
do_send(S,Host,Port,{Facil,Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Facil bor Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg ++ "\n",
    gen_udp:send(S,Host,Port,Packet).

i2l(Int) when is_integer(Int) ->
    integer_to_list(Int);
i2l(Int) ->
    Int.

a2l(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
a2l(Atom) ->
    Atom.

to_log(Cur, Level) ->
    level(Cur) > level(Level).
