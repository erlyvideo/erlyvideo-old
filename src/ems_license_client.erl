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

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

-define(TIMEOUT, 20*60000).
-define(LICENSE_TABLE, license_storage).
%% External API
-export([list/0, load/0]).

%% gen_server callbacks






list() ->
  Config = read_config(),
  case construct_url(Config,list) of 
    undefined -> [];
    URL ->  
      load_by_url(URL)
  end.
  
%%-------------------------------------------------------------------------
%% @spec () -> ok | {error, Reason}
%% @doc Loads code from storage or from server
%% @end
%% @private
%%-------------------------------------------------------------------------
%% Этот код фактически делает то, что написано на http://dev.erlyvideo.org/projects/commercial/wiki/Система_лицензий
load() ->
  Config = read_config(),
  case load_from_storage(Config) of
    ok -> ok;
    {error, _Error} ->
      error_logger:info_msg("Failed to load from storage: ~p~n", [_Error]),
      case load_from_server(Config) of
        {ok, ServerReply} ->
          load_code(ServerReply),
          save_to_storage(Config, ServerReply);
        {error, Error} ->
          error_logger:error_msg("Failed to load from server: ~p~n", [Error]),
          {error, Error}
    end
  end.


get_config_path() ->
  ConfigPath = case application:get_env(erlyvideo,license_config)of 
    {ok, [Path|FileName]} -> [Path|FileName];
    undefined -> [["priv", "/etc/erlyvideo"],"license.txt"]
  end,
  ConfigPath.

read_config() ->
  [Path|FileName] = get_config_path(),
  case file:path_consult(Path, FileName) of
    {ok, Env, LicensePath} ->
      error_logger:info_msg("Reading license file ~s", [LicensePath]),
      Env;
    {error, enoent} ->
      undefined;
    {error, Reason} ->
      error_logger:error_msg("Invalid license file: ~p", [Reason]),
      undefined
  end.


%%%% load_from_storage
%%%%
load_from_storage(undefined) ->
  {error, config_wrong};

load_from_storage(Config) ->
  StrictVersions = proplists:get_value(projects, Config),
  StoredContent = read_storage(Config),
  case storage_has_versions(StoredContent, StrictVersions) of
    true -> 
      load_code(StoredContent),
      ok;
    false ->
      {error, notfound}
  end.

read_storage(undefined) ->
  [];

read_storage(Config) ->
  case open_license_storage(Config) of
    {ok, Table} ->
      List = case dets:lookup(?LICENSE_TABLE, applications) of
        [{applications, Apps}] -> load_from_dets(Table, Apps, []);
        _ -> []
      end,
      dets:close(Table),
      List;
    {error, _Else} ->
      []
  end.

writeable_cache_dir(undefined) ->
  undefined;

writeable_cache_dir(Config) ->
  case proplists:get_value(license_dir, Config, undefined) of
    undefined -> undefined;
    Path -> 
      filelib:ensure_dir(ems:pathjoin(Path, "license")),
      case file:read_file_info(Path) of
        {ok, #file_info{access = write}} -> Path;
        {ok, #file_info{access = read_write}} -> Path;
        _ -> undefined
      end
  end.


open_license_storage(Config) ->
  case writeable_cache_dir(Config) of
    undefined -> {error, no_cache_dir};
    StorageDir ->
      case dets:open_file(?LICENSE_TABLE, [{file,ems:pathjoin(StorageDir,"license_storage.db")}]) of
        {ok, ?LICENSE_TABLE} -> {ok, ?LICENSE_TABLE};
        {error, Reason} -> {error, Reason} 
      end
  end.


load_from_dets(_Table, [], Acc) -> Acc;

load_from_dets(Table, [AppName|Apps], Acc) ->
  [{app,Desc}] = dets:lookup(Table, {app,AppName}),
  Acc1 = lists:foldl(fun(ModName, AccIn) ->
    [{mod,Module}] = dets:lookup(Table, {mod,ModName}),
    [[{code,Module},{name,ModName}]|AccIn]
  end, [{load_app, {application,AppName,Desc}}|Acc], proplists:get_value(modules,Desc, [])),
  load_from_dets(Table, Apps, Acc1).
  

storage_has_versions(_Stored, []) -> true;
storage_has_versions(Stored, [{AppName, Version} |Versions]) ->
  case storage_has_version(Stored, AppName, Version) of
    true -> storage_has_versions(Stored, Versions);
    false -> false
  end.

storage_has_version(Stored, AppName, Version) ->
  [proplists:get_value(vsn, Desc) || {load_app, {application, Name, Desc}} <- Stored, Name == AppName] == [Version].

%%%% load_from_server
%%%%
load_from_server(Config) ->
  case construct_url(Config, save) of
    undefined -> {error, no_license};
    URL -> load_by_url(URL)
  end.

load_by_url(URL) ->
  case ibrowse:send_req(URL,[],get,[],[{response_format,binary}]) of
    {ok, "200", _Headers, Bin} ->
      unpack_server_response(Bin);
    {ok, "404", _Headers, _Bin} ->
      error_logger:error_msg("No selected versions on server~n"),
      {error, notfound};
    {error, Reason} ->
      {error, Reason};
    Else ->
      {error, Else}
  end.
  

unpack_server_response(Bin) ->
  case erlang:binary_to_term(Bin) of
    {reply, Reply} ->
      case proplists:get_value(version, Reply) of
        1 ->
          Commands = proplists:get_value(commands, Reply),
          {ok, Commands};
        Version ->
          {error,{unknown_license_version, Version}}
      end;
    {error, Reason} ->
      error_logger:error_msg("Couldn't load license key: ~p~n", [Reason]),
      {error, Reason}
  end.


construct_url(undefined,Command) ->
  undefined;

construct_url(Config, Command) ->
  case proplists:get_value(license, Config) of
    undefined -> undefined;
    License ->
      LicenseURL = proplists:get_value(url, Config, "http://license.erlyvideo.tv/license"),
      Versions = [io_lib:format("&version[~s]=~s", [Name,Version]) || {Name,Version} <- proplists:get_value(projects, Config, [])],
      lists:flatten([LicenseURL, io_lib:format("?key=~s&command=~s", [License, Command]), Versions])
  end.
  






%%%% load code
%%%%


load_code(Commands) ->
  Startup = execute_commands_v1(Commands,[]),
  handle_loaded_modules_v1(Startup).
  

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


execute_commands_v1([{Command, {application,Name,Desc} = AppDescr}|Commands], Startup) when Command == save_app orelse Command == load_app ->
  Version = proplists:get_value(vsn, Desc),
  case application:load(AppDescr) of
    ok -> error_logger:info_msg("License load application ~p(~p)", [Name, Version]);
    {error, {already_loaded, AppDescr}} -> error_logger:info_msg("License already loaded application ~p(~p)", [Name, Version]);
    _Else -> error_logger:error_msg("License failed to load application: ~p", [_Else]), ok
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

execute_commands_v1([Command|Commands], Startup) when is_tuple(Command) ->
  error_logger:error_msg("Unknown license server command ~p", [element(1,Command)]),
  execute_commands_v1(Commands, Startup);
  
execute_commands_v1([_Command|Commands], Startup) ->
  error_logger:error_msg("Unknown license server command"),
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


%%%% save_to_storage
%%%%

save_to_storage(Config, Commands) ->
  case open_license_storage(Config) of
    {ok, Table} ->
      AppsToSave = [{Name, Desc} || {save_app, {application, Name, Desc}} <- Commands],
      Modules = [{proplists:get_value(name, Info), proplists:get_value(code, Info)} || {load,Info} <- Commands],
      
      ModulesToSave = lists:flatten([begin
        ModList = proplists:get_value(modules, Desc, []),
        [{{mod,Name}, proplists:get_value(Name, Modules)} || Name <- ModList]
      end || {_AppName, Desc} <- AppsToSave]),
      
      dets:insert(Table, ModulesToSave),
      dets:insert(Table, [{{app,Name},Desc} || {Name,Desc} <- AppsToSave]),
      dets:insert(Table, [{applications, [Name || {Name, _Desc} <- AppsToSave]}]),
      ok;
    {error, Error} ->
      {error, Error}
  end.
  




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Всё что ниже — отрефакторить  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%% Make license request logic
% reload_config(#client{license = OldLicense, timeout = OldTimeout} = State) ->
%   {License, Timeout} = case read_license() of
%     {OldLicense, _} -> {OldLicense,OldTimeout};
%     undefined -> {undefined,OldTimeout};
%     {Env, LicensePath} ->
%       error_logger:info_msg("Reading license key from ~s", [LicensePath]),
%       {Env,proplists:get_value(timeout,Env,OldTimeout)}
%   end,
%   Versions = extract_versions(License),
%   {State#client{license = License, timeout = Timeout},Versions};
% 
% reload_config(State) ->
%   {State,undefined}.  

absent_license_file_read_config_test() ->
  {spawn,{setup,
  fun() -> 
    Path = application:get_env(erlyvideo,license_config),
    application:set_env(erlyvideo,license_config,["test/file/absent.txt"]),
    application:set_env(erlyvideo,license_config_buf,Path)
  end,
  fun(_) -> 
    Path = application:get_env(erlyvideo,license_config_buf),
    application:set_env(erlyvideo,license_config,Path)
  end,
  [
  fun() ->
    Conf = read_config(),
    read_storage(Conf),
    load_from_storage(Conf),
    construct_url(Conf,list),
    writeable_cache_dir(Conf),
    load()
  end]
  }}.

wrong_license_file_read_config_test() ->
  {spawn,{setup,
  fun() -> 
    Path = application:get_env(erlyvideo,license_config),
    application:set_env(erlyvideo,license_config,["test/file/license.txt"]),
    application:set_env(erlyvideo,license_config_buf,Path)
  end,
  fun(_) -> 
    Path = application:get_env(erlyvideo,license_config_buf),
    application:set_env(erlyvideo,license_config,Path)
  end,
  [
  fun() ->
    Conf = read_config(),
    read_storage(Conf),
    load_from_storage(Conf),
    construct_url(Conf,list),
    writeable_cache_dir(Conf),
    load()
  end]
  }}.

unvailable_project_test() ->
  {spawn,{setup,
  fun() -> 
    Path = application:get_env(erlyvideo,license_config),
    application:set_env(erlyvideo,license_config,["test/file/license_unvailable_versions.txt"]),
    application:set_env(erlyvideo,license_config_buf,Path)
  end,
  fun(_) -> 
    Path = application:get_env(erlyvideo,license_config_buf),
    application:set_env(erlyvideo,license_config,Path)
  end,
  [
  fun() ->
    load()
  end]
  }}.

unvalid_project_test() ->
  {spawn,{setup,
  fun() -> 
    Path = application:get_env(erlyvideo,license_config),
    application:set_env(erlyvideo,license_config,["test/file/license_unvalid_project.txt"]),
    application:set_env(erlyvideo,license_config_buf,Path)
  end,
  fun(_) -> 
    Path = application:get_env(erlyvideo,license_config_buf),
    application:set_env(erlyvideo,license_config,Path)
  end,
  [
  fun() ->
    load()
  end]
  }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%
%%%%%%   Tests
%%%%%%
%%%%%%  1) run without license.txt at all
%%%%%%  2) run with empty or broken license.txt
%%%%%%  3) proper license.txt with proper load_app reply
%%%%%%  4) proper license.txt with proper save_app reply   
%%%%%%  5) locked version of unavailable project
%%%%%%  6) locked version of available project, when this version isn't available
%%%%%%  7) locked available version of available project
%%%%%%  8) restore proper version from storage
%%%%%%  9) non-restore version from storage if other is selected
%%%%%%  
%%%%%%  
