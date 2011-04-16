-module(smtp_appender).

-include("../include/log4erl.hrl").

-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_TITLE, "log").
-define(DEFAULT_PORT, 25).

init({conf, Conf}) when is_list(Conf) ->
    Level = proplists:get_value(level, Conf),
    Server = lists:foldl(fun(X, List) ->
			     [proplists:get_value(X,Conf)|List]
		     end,
		     [],
		     [ip, port]),
    Auth = lists:foldl(fun(X, List) ->
			     [proplists:get_value(X,Conf)|List]
		     end,
		     [],
		     [username, password, no_auth]),

    SRes = case hd(Server) of
	       undefined ->
		   % in case port is not supplied
		   [_|S2] = Server,
		   list_to_tuple(lists:reverse(S2));
	       _ ->
		   list_to_tuple(lists:reverse(Server))
	   end,
    ARes = case hd(Auth) of
	       undefined ->
		   % in case no_auth not true
		   [_|A2] = Auth,
		   list_to_tuple(lists:reverse(A2));
	       _ ->
		   % no_auth has priority
		   no_auth
	   end,
    MRes = msg_conf(Conf),
    init({Level, SRes, ARes, MRes});
%% Server = {Ip, Port} | {Ip} | Ip
%% Auth = {Uname, Pass} | Uname | {Uname} | no_auth
%% MsgInfo = {From, To, Title, Msg} 
%%         | {To, Title, Msg}
%%         | {To, Msg}
init({Level, Server, Auth, MsgInfo}) ->
    Srvr = check_opts(get_srv_opts(Server)),
    Auth2 = check_opts(get_auth_opts(Auth)),
    MsgI = check_opts(get_msg_opts(MsgInfo)),
    State = #smtp_appender{level=Level, srvr_opts=Srvr, auth_opts=Auth2, msg_opts=MsgI},
    {ok, State};
init(File) when is_list(File) ->
    case file:consult(File) of
	{error, Reason} ->
	    error_logger:error_msg("smtp_appender: couldn't consult Conf file~n"),
	    {error, file:format_error(Reason)};
	{ok, [Terms]} ->
	    init(Terms)
    end.

handle_event({change_level, Level}, State) ->
    State2 = State#smtp_appender{level = Level},
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
    S = State#smtp_appender.msg_opts,
    S2 = S#msg_opts{msg=Tokens},
    State2 = State#smtp_appender{msg_opts=S2},
    {ok, ok, State2};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_log(#log{level = L} = Log, #smtp_appender{level=Level} = State) ->
    ToLog = log4erl_utils:to_log(L, Level),
    case ToLog of
	true ->
	    MsgOpts = State#smtp_appender.msg_opts,
	    
	    From = MsgOpts#msg_opts.from,
	    To = MsgOpts#msg_opts.to,
	    Title = MsgOpts#msg_opts.title,
	    Msg = log_formatter:format(Log, MsgOpts#msg_opts.msg),
	    M = email_msg:simp_msg(From, To, Title, Msg),
	    
	    SrvrOpts = State#smtp_appender.srvr_opts,
	    AuthOpts = State#smtp_appender.auth_opts,
	    
	    Ip = SrvrOpts#srvr_opts.ip,
	    Port = SrvrOpts#srvr_opts.port,
	    Uname = AuthOpts#auth_opts.username,
	    Pass = AuthOpts#auth_opts.password,
	    Pid = init_smtp(Ip, Port, Uname, Pass),
	    
	    smtp_fsm:sendemail(Pid, From, To, M);
	false ->
	    ok
    end.

init_smtp(Ip, Port, Uname, Password) ->
    {ok, Pid} = smtp_fsm:start(Ip, Port),
    smtp_fsm:ehlo(Pid),
    %smtp_fsm:features(Pid),
    case {Uname, Password} of
	{undefined, undefined} ->
	    ok;
	_ ->
	    smtp_fsm:login(Uname, Password)
    end,
    Pid.
	    

get_srv_opts({Ip, Port}) ->
    #srvr_opts{ip=Ip, port=Port};
get_srv_opts({Ip}) when is_list(Ip) ->
    #srvr_opts{ip=Ip, port=?DEFAULT_PORT};
get_srv_opts(Ip) when is_list(Ip) ->
    #srvr_opts{ip=Ip, port=?DEFAULT_PORT};
get_srv_opts(E) ->
    {error, E}.

get_auth_opts({Uname, Pass}) ->
    #auth_opts{username=Uname, password=Pass};
get_auth_opts(no_auth) ->
    #auth_opts{};
get_auth_opts({Uname}) ->
    #auth_opts{username=Uname};
get_auth_opts(Uname) when is_list(Uname) ->
    #auth_opts{username=Uname};
get_auth_opts(E) ->
    {error, E}.

get_msg_opts({From, To, Title, Msg}) ->
    {ok, Mtokens} = log_formatter:parse(Msg),
    #msg_opts{from=From, to=To, title=Title, msg=Mtokens};
get_msg_opts({To, Title, Msg}) ->
    {ok, Mtokens} = log_formatter:parse(Msg),
    #msg_opts{to=To, title=Title, msg=Mtokens};
get_msg_opts({To, Msg}) ->
    {ok, Mtokens} = log_formatter:parse(Msg),
    #msg_opts{to=To, msg=Mtokens};
get_msg_opts(E) ->
    {error, E}.

check_opts(Opts) ->
    case Opts of
	{error, E} ->
	    ?LOG2("error in getting opts with param ~p~n",[Opts]),
	    throw({smtp_appender_opts, E});
	R ->
	    R
    end.

msg_conf(Conf) ->
    To = proplists:get_value(to, Conf),
    Msg = proplists:get_value(msg, Conf),

    case {To, Msg} of
	{undefined, _} ->
	    throw(smtp_to_null);
	{_, undefined} ->
	    throw(smtp_msg_null);
	{_,_} ->
	    ok
    end,

    Title = proplists:get_value(title, Conf),
    From = proplists:get_value(from, Conf),

    case {Title, From} of
	{undefined, _} ->
	    {To, Msg};
	{_, undefined} ->
	    {To, Title, Msg};
	{_, _} ->
	    {From, To, Title, Msg}
    end.
