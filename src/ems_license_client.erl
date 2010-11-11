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


-record(client, {
  license,
  timeout
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


ping() ->
  ?MODULE ! timeout,
  ok.
  


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
  {ok, #client{timeout = ?TIMEOUT}, ?TIMEOUT}.

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
  make_request_internal(State);

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
make_request_internal(#client{license = OldLicense, timeout = OldTimeout} = State) ->
  {License, Timeout} = case read_license() of
    {OldLicense, _} -> {OldLicense,OldTimeout};
    undefined -> {undefined,OldTimeout};
    {Env, LicensePath} ->
      error_logger:info_msg("Reading license key from ~s", [LicensePath]),
      {Env,proplists:get_value(timeout,Env,OldTimeout)}
  end,
  request_licensed(License),
  {noreply, State#client{license = License, timeout = Timeout}, Timeout}.


read_license() ->
  case file:path_consult(["priv", "/etc/erlyvideo"], "license.txt") of
    {ok, Env, LicensePath} ->
      {Env,LicensePath};
    {error, enoent} ->
      undefined;
    {error, Reason} ->
      error_logger:error_msg("Invalid license key: ~p", [Reason]),
      undefined
  end.

request_licensed(undefined) ->
  ok;
  
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
  Reply = case gen_tcp:recv(Sock, 0) of
    {ok, {http_response, _HTTPVersion, 200, _Status}} -> 
      read_license_response(Sock);
    _ -> 
      {error, unauthenticated_request}
  end,
  gen_tcp:close(Sock),
  Reply.
  
read_license_response(Sock) ->
  Headers = read_headers(Sock, []),
  Length = proplists:get_value('Content-Length', Headers),
  case Length of
    undefined ->
      {error, {no_length_header, Headers}};
    _ when is_number(Length) ->
      {ok, Bin} = gen_tcp:recv(Sock, Length),
      gen_tcp:close(Sock),
      {reply, Reply} = erlang:binary_to_term(Bin),
      case proplists:get_value(version, Reply) of
        1 ->
          Commands = proplists:get_value(commands, Reply),
          Startup = execute_commands_v1(Commands, []),
          handle_loaded_modules_v1(lists:reverse(Startup));
        Version ->
          {error,{unknown_license_version, Version}}
      end
  end.
  
  
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
  
  
  
execute_commands_v1([], Startup) -> 
  Startup;

execute_commands_v1([{purge,Module}|Commands], Startup) ->
  case erlang:function_exported(Module, ems_client_unload, 0) of
    true -> (catch Module:ems_client_unload());
    false -> ok
  end,
  
  case code:is_loaded(Module) of
    true -> error_logger:info_msg("Licence purge ~p", [Module]), code:purge(Module);
    false -> ok
  end,
  execute_commands_v1(Commands, Startup);

execute_commands_v1([{save,Info}|Commands], Startup) ->
  File = proplists:get_value(file, Info),
  Path = proplists:get_value(path, Info),
  CacheDir = ems:get_var(license_cache_dir, "tmp"),
  FullPath = ems:pathjoin(CacheDir, Path),
  code:add_patha(filename:dirname(FullPath)),
  case file:read_file(FullPath) of
    {ok, File} -> ok;
    _ ->
      filelib:ensure_dir(FullPath),
      file:write_file(FullPath, File),
      error_logger:info_msg("License file ~p", [Path])
  end,
  execute_commands_v1(Commands, Startup);
  
execute_commands_v1([{load_app, {application,Name,_Desc} = AppDescr}|Commands], Startup) ->
  case application:load(AppDescr) of
    ok -> error_logger:info_msg("License load application ~p", [Name]);
    _ -> ok
  end,
  execute_commands_v1(Commands, Startup);
  
  
execute_commands_v1([{load,ModInfo}|Commands], Startup) ->
  Code = proplists:get_value(code, ModInfo),
  {ok, {Module, [Version]}} = beam_lib:version(Code),
  case is_new_version(ModInfo) of
    false -> 
      execute_commands_v1(Commands, Startup);
    true -> 
      error_logger:info_msg("Licence load ~p(~p)", [Module, Version]),
      code:soft_purge(Module),
      code:load_binary(Module, "license/"++atom_to_list(Module)++".erl", Code),
      execute_commands_v1(Commands, [Module|Startup])
  end;
  
execute_commands_v1([_Command|Commands], Startup) ->
  error_logger:error_msg("Unknown license server commands"),
  execute_commands_v1(Commands, Startup).


is_new_version(ModInfo) ->
  Code = proplists:get_value(code, ModInfo),
  {ok, {Module, NewVersion}} = beam_lib:version(Code),
  OldVersion = case code:is_loaded(Module) of
    false -> undefined;
    _ -> proplists:get_value(vsn, Module:module_info(attributes))
  end,
  OldVersion =/= NewVersion.

  
handle_loaded_modules_v1([]) ->
  ok;
  
handle_loaded_modules_v1([Module|Startup]) ->
  case erlang:function_exported(Module, ems_client_load, 0) of
    true -> Module:ems_client_load();
    false -> ok
  end,
  handle_loaded_modules_v1(Startup).

  


  