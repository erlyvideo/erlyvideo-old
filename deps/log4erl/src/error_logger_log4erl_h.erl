%% Initially written by Jakub Odias and later modified by Ahmed Al-Issaei
%% Main addition, add mapping between error_logger msgs and log4erl log levels

%% Code is released as MPL with Jakub's permission

-module(error_logger_log4erl_h).
-behaviour(gen_event).

%% @author Jakub Odias
%% @copyright Proximetry
%% @version 1.0.0
%% @doc Handles events reported by error_logger and forwards it to log4erl
%% @end

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([add_handler/0, add_handler/1]).

-record(elogger_l4e_mappings, {error=error, info_msg=info, warning_msg=warn,
			       error_report=error, info_report=info,
			       warning_report=warn}).

%%======================================
%% gen_event callback functions
%%======================================
init([])->
    {ok, #elogger_l4e_mappings{}};
init(Conf) ->
    io:format("Conf ~p~n",[Conf]),
    {ok, mapping(Conf, #elogger_l4e_mappings{})}.

add_handler(Args) ->
    error_logger:add_report_handler(?MODULE, Args).

add_handler() ->
    error_logger:add_report_handler(?MODULE,[]).

mapping([], M) ->
    M;
mapping([H | C], M) ->
    mapping(C, m(H, M)).

m({error, L}, M) ->
    M#elogger_l4e_mappings{error=L};
m({info_msg, L}, M) ->
    M#elogger_l4e_mappings{info_msg=L};
m({warning_msg, L}, M) ->
    M#elogger_l4e_mappings{warning_msg=L};
m({error_report, L}, M) ->
    M#elogger_l4e_mappings{error_report=L};
m({info_report, L}, M) ->
    M#elogger_l4e_mappings{info_report=L};
m({warning_report, L}, M) ->
    M#elogger_l4e_mappings{warning_report=L};
m(_, M) ->
    M.

handle_event({error, _GLeader, {_PID, Msg, Data}}, #elogger_l4e_mappings{error=L} = State) ->
    R = log4erl:log(L, Msg, Data),
    {R, State};
handle_event({info_msg, _GLeader, {_PID, Msg, Data}}, #elogger_l4e_mappings{info_msg=L} = State) ->
    R = log4erl:log(L, Msg, Data),
    {R, State};
handle_event({warning_msg, _GLeader, {_PID, Msg, Data}}, #elogger_l4e_mappings{warning_msg=L} = State) ->
    R = log4erl:log(L, Msg, Data),
    {R, State};
handle_event({error_report, _GLeader, _}, State) ->
    {ok, State};
handle_event({info_report, _GLeader, _}, State) ->
    {ok, State};
handle_event({warning_report, _GLeader, _}, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
