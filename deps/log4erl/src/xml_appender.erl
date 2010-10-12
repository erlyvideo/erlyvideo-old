-module(xml_appender).

-behaviour(gen_event).

-include("../include/log4erl.hrl").
-include_lib("kernel/include/file.hrl").

-import(log4erl_utils, [to_list/1, to_atom/1, to_int/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_XMLSPEC, [{level, "%L", att},
			  {date, "%j", att},
			  {time, "%T", att},
			  {message, "%l", elem}]).

%% xmlspec is a list of xml_spec records
-record(xml_appender, {dir, file_name, fd, counter, log_type, rotation, suffix="xml", level, xmlspec}).

%% xmlspec is of the form {Name, Format, Type}
%% where Name is the name of the element or attribute
%% Format is the format of the value
%% Type is either 'att' or 'elem'
-record(xml_spec, {name, format, type}).

%%======================================
%% gen_event callback functions
%%======================================
init({conf, Conf}) when is_list(Conf) ->
    CL = lists:foldl(fun(X, List) ->
			     case X of
				 {X1, D} ->
				     [proplists:get_value(X1,Conf,D)|List];
				 _ ->
				     [proplists:get_value(X,Conf)|List]
			     end
		     end,
		     [],
		     [dir, file, type, max, rotation, suffix, level, {xmlspec, ?DEFAULT_XMLSPEC}]),
    
    init(list_to_tuple(lists:reverse(CL)));
init({Dir, Fname, {Type, Max}, Rot, Suf, Level, Spec} = _Conf) ->
    ?LOG2("xml_appender:init() - 1 ~p~n",[_Conf]),
    File = Dir ++ "/" ++ Fname ++ "." ++ Suf,
    Ltype = #log_type{type = Type, max = Max},
    % Check Rot >= 0
    Rot1 = case Rot < 0 of
	       true ->
		   0;
	       false ->
		   Rot
	   end,
    {ok, Fd} = file:open(File, ?FILE_OPTIONS),

    %% Translate {Name, Format, Type} to #xml_spec records
    XmlSpec = lists:map(fun({N, F, T}) ->
				{ok, Tokens} = log_formatter:parse(F),
				#xml_spec{name=N, format=Tokens, type=T}
			end, Spec),

    %% Start xml 
    file:write(Fd, "<?xml>\n<log4erl>"),

    State = #xml_appender{dir = Dir, file_name = Fname, fd = Fd, counter=0,
			   log_type = Ltype, rotation = Rot1, suffix=Suf,
			   level=Level, xmlspec=XmlSpec},
    ?LOG2("xml_appender:init() with conf ~p~n",[State]),
    {ok, State};
% These 2 are for result of reading conf file
init({Dir, Fname, Type, Max, Rot, Suf, Level, Spec}) ->
    init({to_list(Dir), to_list(Fname), {to_atom(Type), to_int(Max)}, to_int(Rot), to_list(Suf), to_atom(Level), Spec}).

handle_event({change_level, Level}, State) ->
    State2 = State#xml_appender{level = Level},
    ?LOG2("Changed level to ~p~n",[Level]),
    {ok, State2};
handle_event({log,LLog}, State) ->
    ?LOG2("handl_event:log = ~p~n",[LLog]),
    do_log(LLog, State),
    Res = check_rotation(State),
    {ok, Res}.

handle_call({change_format, _Format}, State) ->
    ?LOG("Cannot change format in xml_appender~n"),
    {ok, ok, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    ?LOG2("~w received unknown message: ~p~n", [?MODULE, _Info]),
    {ok, State}.

terminate(_Reason, #xml_appender{fd=Fd}) ->
    file:write(Fd, "</log4erl>\n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%======================================
%% internal callback functions
%%======================================
do_log(#log{level = L} = Log,#xml_appender{fd = Fd, level=Level, xmlspec=XmlSpec} = _State) when is_atom(L) ->
    ToLog = log4erl_utils:to_log(L, Level),
    case ToLog of
	true ->
	    M = format_xml(Log, XmlSpec),
	    file:write(Fd, M);
	false ->
	    ok
    end;
do_log(_Other, _State) ->
    ?LOG2("unknown level ~p~n",[_Other]),
    ok.

rotate(#xml_appender{fd = Fd, dir=Dir,  file_name=Fn, counter=Cntr, rotation=Rot, suffix=Suf} = S) ->
    file:write(Fd, "</log4erl>\n"),
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
    file:write(Fd2, "<?xml>\n<log4erl>"),
    State2 = S#xml_appender{dir = Dir, file_name = Fn, fd = Fd2, rotation = Rot, suffix=Suf},
    {ok, State2}.

% Check if the file needs to be rotated
% ignore in case of if log type is set to time instead of size	    
check_rotation(State) ->
    #xml_appender{dir=Dir, file_name=Fname, log_type = #log_type{type=T, max=Max}, suffix=Suf} = State,
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

format_xml(Log, Spec) ->
    Att = lists:filter(fun(#xml_spec{type=T}) ->
			       T == att
		       end,
		       Spec),
    Elems = lists:filter(fun(#xml_spec{type=T}) ->
				 T == elem
			 end,
			 Spec),

    A = attributes(Log, Att),
    B = elements(Log, Elems),

    "<log" ++ A ++ ">\n" ++ B ++ "</log>\n".

attributes(_, []) ->
    [];
attributes(Log, Atts) ->
    attributes(Log, Atts, []).

attributes(_, [], Acc) ->
    lists:reverse(Acc);
attributes(Log, [#xml_spec{name=N,format=F}|Rest], Acc) ->
    S = " " ++ escape_attr(log4erl_utils:to_list(N)) ++ "=\"" ++ escape_attr(log_formatter:format(Log, F)) ++"\"",
    attributes(Log, Rest, [S|Acc]).

elements(_, []) ->
    [];
elements(Log, Elems) ->
    elements(Log, Elems, []).

elements(_, [], Acc) ->
    lists:reverse(Acc);
elements(Log, [#xml_spec{name=N, format=F}|Rest], Acc) ->
    Name = escape_attr(log4erl_utils:to_list(N)),
    L = lists:flatten(log_formatter:format(Log, F)),
    S = "\t<" ++ Name ++ ">" ++ escape(L) ++"</" ++ Name ++ ">\n",
    elements(Log, Rest, [S|Acc]).

%% The following code is copied/pasted from mochiweb's 'mochiweb_html' & 'mochinum' modules
%% See http://code.google.com/p/mochiweb

%% @spec escape(string() | atom() | binary()) -> binary()
%% @doc Escape a string such that it's safe for HTML (amp; lt; gt;).
escape(B) when is_binary(B) ->
    escape(binary_to_list(B), []);
escape(A) when is_atom(A) ->
    escape(atom_to_list(A), []);
escape(S) when is_list(S) ->
    escape(S, []).

%% @spec escape_attr(string() | binary() | atom() | integer() | float()) -> binary()
%% @doc Escape a string such that it's safe for HTML attrs
%%      (amp; lt; gt; quot;).
escape_attr(B) when is_binary(B) ->
    escape_attr(binary_to_list(B), []);
escape_attr(A) when is_atom(A) ->
    escape_attr(atom_to_list(A), []);
escape_attr(S) when is_list(S) ->
    escape_attr(S, []);
escape_attr(I) when is_integer(I) ->
    escape_attr(integer_to_list(I), []);
escape_attr(F) when is_float(F) ->
    escape_attr(mochinum:digits(F), []).

escape([], Acc) ->
    lists:reverse(Acc);
escape("<" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape(">" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape("&" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).

-define(QUOTE, $\").

escape_attr([], Acc) ->
    lists:reverse(Acc);
escape_attr("<" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&lt;", Acc));
escape_attr(">" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&gt;", Acc));
escape_attr("&" ++ Rest, Acc) ->
    escape_attr(Rest, lists:reverse("&amp;", Acc));
escape_attr([?QUOTE | Rest], Acc) ->
    escape_attr(Rest, lists:reverse("&quot;", Acc));
escape_attr([C | Rest], Acc) ->
    escape_attr(Rest, [C | Acc]).

