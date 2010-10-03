%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
%%% @doc        Client of erlyvideo license server
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(ems_license_client).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).

-include("log.hrl").

-define(TIMEOUT, 20*60000).

%% External API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([ping/0]).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


ping() ->
  gen_server:call(?MODULE, ping).
  


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([]) ->
  self() ! timeout,
  {ok, state, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(ping, _From, State) ->
  {reply, make_request_internal(), State, ?TIMEOUT};

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(timeout, State) ->
  make_request_internal(),
  {noreply, State, ?TIMEOUT};

handle_info(_Info, State) ->
  {stop, {unknown_info, _Info}, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.





%%%%%%%%%%%%%%%%%%% Make license request logic

make_request_internal() ->
  case file:path_consult(["priv", "/etc/erlyvideo"], "license.txt") of
    {ok, Env, LicensePath} ->
      error_logger:info_msg("Reading license key from ~s", [LicensePath]),
      case proplists:get_value(timeout,Env) of
        Timeout when is_number(Timeout) -> timer:send_after(Timeout, timeout);
        _ -> ok
      end,
      request_licensed(Env);
    {error, enoent} ->
      error_logger:info_msg("No license file found, working in public mode"),
      ok;
    {error, Reason} ->
      error_logger:error_msg("Invalid license key: ~p", [Reason]),
      ok
  end.
  
request_licensed(Env) ->
  LicenseUrl = proplists:get_value(url, Env, "http://license.erlyvideo.tv/license"),
  {_, _Auth, Host, Port, _Path, _Query} = http_uri2:parse(LicenseUrl),
  case gen_tcp:connect(Host, Port, [binary]) of
    {ok, Sock} -> send_license_request(Sock, Env);
    {error, Reason} -> error_logger:error_msg("Couldn't connect to license server ~p: ~p", [LicenseUrl, Reason])
  end.
  
send_license_request(Sock, Env) ->
  Command = "init",
  LicenseUrl = proplists:get_value(url, Env, "http://license.erlyvideo.tv/license"),
  License = proplists:get_value(license, Env),
  {_, _Auth, Host, _Port, Path, _Query} = http_uri2:parse(LicenseUrl),
  inet:setopts(Sock, [{packet,http},{active,false}]),
  Query = io_lib:format("GET ~s?key=~s&command=~s HTTP/1.1\r\nHost: ~s\r\n\r\n", [Path, License, Command, Host]),
  gen_tcp:send(Sock, Query),
  {ok, {http_response, _HTTPVersion, Code, _Status}} = gen_tcp:recv(Sock, 0),
  case Code of
    200 -> ok;
    _ -> erlang:error(unauthenticated_request)
  end,
  Headers = read_headers(Sock, []),
  Length = proplists:get_value('Content-Length', Headers),
  {ok, Bin} = gen_tcp:recv(Sock, Length),
  gen_tcp:close(Sock),
  {reply, Reply} = erlang:binary_to_term(Bin),
  case proplists:get_value(version, Reply) of
    1 ->
      Commands = proplists:get_value(commands, Reply),
      execute_commands_v1(Commands),
      handle_loaded_modules_v1(Commands);
    Version ->
      erlang:error({unknown_license_version, Version})
  end,
  ok.
  
read_headers(Sock, Headers) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, {http_header, _, 'Content-Length' = Header, _, Value}} ->
      read_headers(Sock, [{Header,list_to_integer(Value)}|Headers]);
    {ok, {http_header, _, Header, _, Value}} ->
      read_headers(Sock, [{Header,Value}|Headers]);
    {ok, http_eoh} ->
      inet:setopts(Sock, [{packet,raw}]),
      Headers
  end.
  
  
  
execute_commands_v1([]) -> 
  ok;

execute_commands_v1([{purge,Module}|Commands]) ->
  error_logger:info_msg("Licence purge ~p", [Module]),
  case erlang:function_exported(Module, ems_client_unload, 0) of
    true -> (catch Module:ems_client_unload());
    false -> ok
  end,
  
  case code:is_loaded(Module) of
    true -> code:purge(Module);
    false -> ok
  end,
  execute_commands_v1(Commands);
  
execute_commands_v1([{load,ModInfo}|Commands]) ->
  Module = proplists:get_value(name, ModInfo),
  Code = proplists:get_value(code, ModInfo),
  error_logger:info_msg("Licence load ~p", [Module]),
  code:soft_purge(Module),
  code:load_binary(Module, "license"++atom_to_list(Module)++".erl", Code),
  execute_commands_v1(Commands).

  
handle_loaded_modules_v1([]) ->
  ok;
  
handle_loaded_modules_v1([{load, ModInfo}|Commands]) ->
  Module = proplists:get_value(name, ModInfo),
  case erlang:function_exported(Module, ems_client_load, 0) of
    true -> Module:ems_client_load();
    false -> ok
  end,
  handle_loaded_modules_v1(Commands);
  
handle_loaded_modules_v1([_Command|Commands]) ->
  handle_loaded_modules_v1(Commands).

  


  