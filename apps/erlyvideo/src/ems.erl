%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Helper module for some generic things.
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems).
-author('Max Lapshin <max@maxidoors.ru>').
-include("ems.hrl").
-include("log.hrl").

-export([get_var/2, get_var/3, set_var/3, check_app/3, try_method_chain/3, respond_to/3]).
-export([host/1]).

-export([expand_tuple/2, element/2, setelement/3]).
-export([str_split/2, multicall/3, pathjoin/2, pathjoin/1, expand_path/1]).

-export([rebuild/0, reload/0, reload/1, restart/0]).

-export([list_by/1, top_info/1, top_info/2, now/1]).

rebuild() -> erlyvideo:rebuild().
restart() -> erlyvideo:restart().
reload() -> erlyvideo:reload().
reload(App) -> erlyvideo:reload(App).

 

list_by(What) ->
  Processes = [{Pid,erlang:element(2,erlang:process_info(Pid,What))} || Pid <- erlang:processes()],
  lists:sort(fun({_Pid1,Info1}, {_Pid2,Info2}) ->
    Info1 > Info2
  end, Processes).


expand_path(Path) when is_binary(Path) ->
  expand_path(binary_to_list(Path));

expand_path(Path) when is_list(Path) ->
  expand_path(string:tokens(filename:absname(Path), "/"), []);
  
expand_path(_) ->
  undefined.
  
expand_path([], Acc) ->
  "/"++string:join(lists:reverse(Acc), "/");
  
expand_path([_, ".."|Path], Acc) ->
  expand_path(Path, Acc);

expand_path([Part|Path], Acc) ->
  expand_path(Path, [Part|Acc]).

pathjoin(Root, Path) ->
  pathjoin([Root, Path]).
  
pathjoin(Paths) ->
  Root = case Paths of
    ["/"++_|_] -> "/";
    _ -> ""
  end,
  Cleared = lists:foldl(fun(Path, Parts) ->
    Parts ++ [Token || Token <- string:tokens(Path, "/"), Token =/= ".."]
  end, [], Paths),
  Root ++ string:join(Cleared, "/").
  
  
top_info(What) -> top_info(What, 10).

top_info(What, Count) ->
  Top = lists:sublist(list_by(What), Count),
  [process_info(Pid,[memory,status,messages,links,
   reductions,total_heap_size,
   stack_size,suspending,dictionary,priority,initial_call,
   current_function,message_queue_len,garbage_collection]) || {Pid,_} <- Top].


expand_tuple(Tuple, 0) -> Tuple;
expand_tuple(Tuple, N) when size(Tuple) < N ->
  expand_tuple(erlang:append_element(Tuple, undefined), N);

expand_tuple(Tuple, _N) -> Tuple.


element(0, _)	-> undefined;
element(N, Tuple) when size(Tuple) < N -> undefined;
element(N, Tuple) -> erlang:element(N, Tuple).

setelement(0, Tuple, _) -> Tuple;
setelement(N, Tuple, Term) ->
  Tuple1 = expand_tuple(Tuple, N),
  erlang:setelement(N, Tuple1, Term).
	
%%--------------------------------------------------------------------
%% @spec (Opt::atom(), Default::any()) -> any()
%% @doc Gets application enviroment variable. Returns Default if no 
%% varaible named Opt is found. User defined varaibles in .config file
%% override application default varabiles.
%% @end 
%%--------------------------------------------------------------------
get_var(Opt, Default) ->
	case application:get_env(erlyvideo, Opt) of
	{ok, Val} -> Val;
	_ ->
		case init:get_argument(Opt) of
		{ok, [[Val | _] | _]} -> Val;
		error		-> Default
		end
	end.


get_var(Key, Host, Default) ->
  case ets:match_object(vhosts, {{host(Host), Key}, '$1'}) of
    [{{_Hostname, Key}, Value}] -> Value;
    [] -> Default
  end.


set_var(Key, Host, Value) ->
  ets:insert(vhosts, {{host(Host), Key}, Value}).

respond_to(Module, Command, Arity) when is_tuple(Module) -> 
  case code:ensure_loaded(erlang:element(1, Module)) of
    false -> false;
    _ -> erlang:function_exported(erlang:element(1,Module), Command, Arity + 1)
  end;

respond_to(Module, Command, Arity) ->
  case code:ensure_loaded(Module) of
    false -> false;
    _ -> erlang:function_exported(Module, Command, Arity)
  end.
  
  
  
host(Hostname) when is_binary(Hostname) -> host(binary_to_list(Hostname));
host(Hostname) when is_atom(Hostname) -> Hostname;
host(FullHostname) ->
  Hostname = hd(string:tokens(FullHostname, ":")),
  case ets:match_object(vhosts, {Hostname, '$1'}) of
    [{Hostname, Host}] -> Host;
    [] -> default
  end.
  

now(How) ->
  Date = case How of
    utc -> erlang:universaltime();
    local -> erlang:localtime()
  end,
  calendar:datetime_to_gregorian_seconds(Date) - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).
  



%%--------------------------------------------------------------------
%% @spec (Opt::atom(), Command::atom(), Arity::integer()) -> any()
%% @doc Try to launch methods one by one in modules
%% @end 
%%--------------------------------------------------------------------

try_method_chain(Host, Method, Args) when is_atom(Host) ->
  try_method_chain(ems:get_var(rtmp_handlers, Host, [trusted_login]), Method, Args);

try_method_chain([], _Method, _Args) ->
  unhandled;

try_method_chain([Module | Modules], Method, Args) ->
  case respond_to(Module, Method, length(Args)) of
    true -> 
      case apply(Module, Method, Args) of
        unhandled -> try_method_chain(Modules, Method, Args);
        Else -> Else
      end;
    false -> 
      case respond_to(Module, rtmp_method_missing, length(Args)) of
        true -> 
          case apply(Module, rtmp_method_missing, Args) of
            unhandled -> try_method_chain(Modules, Method, Args);
            Else -> Else
          end;
        false -> try_method_chain(Modules, Method, Args)
      end
  end.  


%%--------------------------------------------------------------------
%% @spec (Opt::atom(), Command::atom(), Arity::integer()) -> any()
%% @doc Look whan module in loaded plugins can handle required method
%% @end 
%%--------------------------------------------------------------------

check_app([], _Command, _Arity) ->
  unhandled;

check_app([Module | Applications], Command, Arity) ->
  case respond_to(Module, Command, Arity) of
    true -> {Module, Command};
    false -> 
      case respond_to(Module, rtmp_method_missing, Arity) of
        true -> {Module, rtmp_method_missing};
        false -> check_app(Applications, Command, Arity)
      end
  end;


check_app(Host, Command, Arity) ->
  Modules = ems:get_var(rtmp_handlers, Host, [trusted_login]),
  check_app(Modules, Command, Arity).


str_split(String, Delim) ->
  str_split(String, Delim, []).

str_split([], _, Acc) ->
  lists:reverse([[]|Acc]);

str_split(String, Delim, Acc) ->
  case string:str(String, Delim) of
    0 -> lists:reverse([String|Acc]);
    N -> 
      str_split(string:substr(String, N+1), Delim, [string:substr(String,1,N-1)|Acc])
  end.


multicall(Pids, What, Timeout) ->
  Parent = self(),
  Caller = proc_lib:spawn(fun() -> multicall_helper(Parent, Pids, What) end),
  Ref = erlang:monitor(process, Caller),
  ok = receive
    {multicall_ready, Caller} -> ok;
    {'DOWN', Ref, process, Caller, normal} -> ok;
    {'DOWN', Ref, process, Caller, Reason} -> {error, {caller_down, Reason}}
  after
    Timeout ->  erlang:exit(Caller, shutdown), ok
  end,
  erlang:demonitor(Ref, [flush]),
  collect_multicall_replies(Pids, []).

collect_multicall_replies([], Acc) ->
  lists:reverse(Acc);

collect_multicall_replies([Pid|Pids], Acc) ->
  receive
    {multicall_reply, Pid, Reply} -> collect_multicall_replies(Pids, [{Pid,Reply}|Acc])
  after
    0 -> collect_multicall_replies(Pids, Acc)
  end.

multicall_helper(Parent, Pids, Request) ->
  erlang:monitor(process, Parent),
  Refs = send_multicalls(Pids, Request, []),
  collect_multicalls(Parent, Refs).


send_multicalls([], _Request, Refs) ->
  Refs;

send_multicalls([Pid|Pids], Request, Refs) ->
  Ref = make_ref(),
  erlang:send(Pid, {'$gen_call', {self(), Ref}, Request}),
  send_multicalls(Pids, Request, [{Ref,Pid}|Refs]).

collect_multicalls(Parent, []) ->
  Parent ! {multicall_ready, self()};

collect_multicalls(Parent, Refs) ->
  receive
    {'DOWN', _Ref, process, Parent, _Reason} -> ok;
    {Ref, Reply} ->
      Refs1 = case lists:keytake(Ref, 1, Refs) of
        false -> Refs;
        {value, {Ref, Pid}, Refs1_} ->
          Parent ! {multicall_reply, Pid, Reply},
          Refs1_
      end,
      collect_multicalls(Parent, Refs1)
  end.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
	
str_split1_test() ->
  ?assertEqual(["http:","","ya.ru"], str_split("http://ya.ru", "/")).

str_split2_test() ->
  ?assertEqual(["http:","","ya.ru", ""], str_split("http://ya.ru/", "/")).

