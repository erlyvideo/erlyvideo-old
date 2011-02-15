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

-export([ping/0, ping/1]).


-record(client, {
  license,
  timeout,
  storage_opened = false
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


ping() ->
  ?MODULE ! ping,
  ok.
  
ping([sync]) ->
  gen_server:call(?MODULE, ping, 60000).

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
  % self() ! ping,
  {ok, #client{timeout = ?TIMEOUT}}.

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
handle_call(ping, From, #client{storage_opened = false} = State) ->
  State1 = case filelib:ensure_dir("priv") of
    ok -> 
      {ok, license_storage} = dets:open_file(license_storage, [{file,"priv/license_storage.db"}]),
      State#client{storage_opened = true};
    {error, Reason} -> 
      ems_log:error(license_client, "License client couldn't open license_storage: ~p", [Reason]),
      State
  end,
  handle_call(ping, From, State1);

handle_call(ping, _From, State) ->
  State1 = make_request_internal(State),
  {reply, ok, State1};

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
handle_info(ping, State) ->
  State1 = make_request_internal(State),
  {noreply, State1};

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
  request_licensed(License, State),
  State#client{license = License, timeout = Timeout}.


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

request_licensed(undefined, _State) ->
  ok;
  
request_licensed(Env, State) ->
  Command = "init",
  LicenseUrl = proplists:get_value(url, Env, "http://license.erlyvideo.org/license"),
  License = proplists:get_value(license, Env),
  
  URL = lists:flatten(io_lib:format("~s?key=~s&command=~s", [LicenseUrl, License, Command])),
  case ibrowse:send_req(URL,[],get,[],[{response_format,binary}]) of
    {ok, "200", _ResponseHeaders, Bin} ->
      read_license_response(Bin, State);
    _Else ->
      ?D({license_error, _Else}),
      _Else
  end.    
  
read_license_response(Bin, State) ->
  {reply, Reply} = erlang:binary_to_term(Bin),
  case proplists:get_value(version, Reply) of
    1 ->
      Commands = proplists:get_value(commands, Reply),
      Startup = execute_commands_v1(Commands, [], State),
      handle_loaded_modules_v1(lists:reverse(Startup));
    Version ->
      {error,{unknown_license_version, Version}}
  end.
  
  
execute_commands_v1([], Startup, _State) -> 
  Startup;

execute_commands_v1([{purge,Module}|Commands], Startup, State) ->
  case erlang:function_exported(Module, ems_client_unload, 0) of
    true -> (catch Module:ems_client_unload());
    false -> ok
  end,
  
  case code:is_loaded(Module) of
    true -> error_logger:info_msg("Licence purge ~p", [Module]), code:purge(Module);
    false -> ok
  end,
  execute_commands_v1(Commands, Startup, State);

execute_commands_v1([{save,_Info}|Commands], Startup, #client{storage_opened = false} = State) ->
  execute_commands_v1(Commands, Startup, State);
  

execute_commands_v1([{save,Info}|Commands], Startup, #client{storage_opened = true} = State) ->
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
  execute_commands_v1(Commands, Startup, State);

execute_commands_v1([{save_app, {application,Name,Desc} = AppDescr}|Commands], Startup, #client{storage_opened = CanSave} = State) ->
  Version = proplists:get_value(vsn, Desc),
  case application:load(AppDescr) of
    ok when CanSave == true ->
      save_application(Name,Desc),
      error_logger:info_msg("License save application ~p(~s)", [Name, Version]);
    ok when CanSave == false ->
      error_logger:info_msg("License only load application ~p(~s)", [Name, Version]);
    _ -> ok
  end,
  execute_commands_v1(Commands, Startup, State);
  
execute_commands_v1([{load_app, {application,Name,_Desc} = AppDescr}|Commands], Startup, State) ->
  case application:load(AppDescr) of
    ok -> error_logger:info_msg("License load application ~p", [Name]);
    _ -> ok
  end,
  execute_commands_v1(Commands, Startup, State);
  
  
execute_commands_v1([{load,ModInfo}|Commands], Startup, State) ->
  Code = proplists:get_value(code, ModInfo),
  {ok, {Module, [Version]}} = beam_lib:version(Code),
  case is_new_version(ModInfo) of
    false -> 
      execute_commands_v1(Commands, Startup, State);
    true -> 
      error_logger:info_msg("Licence load ~p(~p)", [Module, Version]),
      code:soft_purge(Module),
      code:load_binary(Module, "license/"++atom_to_list(Module)++".erl", Code),
      execute_commands_v1(Commands, [Module|Startup], State)
  end;
  
execute_commands_v1([_Command|Commands], Startup, State) ->
  error_logger:error_msg("Unknown license server command"),
  execute_commands_v1(Commands, Startup, State).


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

  
save_application(AppName, Desc) ->
  SavedApps = dets:lookup(license_storage, saved_apps),
  ModNames = proplists:get_value(modules, Desc),
  Modules = [begin
    {Name,Bin,_Path} = code:get_object_code(Name),
    {Name,Bin}
  end || Name <- ModNames],
  NewApps = lists:usort([AppName|SavedApps]),
  dets:insert(license_storage, [{saved_apps,NewApps},{{app,AppName},Desc}|Modules]),
  ok.


  