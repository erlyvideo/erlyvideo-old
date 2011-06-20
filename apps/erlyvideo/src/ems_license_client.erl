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
-export([list/0, load/0, save/1, afterload/1, read_config/2,read_config/0]).

%% gen_server callbacks



%% TODO: сделать проверку на наличие одного файла


list() ->
  case read_config() of
    undefined -> [];
    Config ->
      case construct_url(Config,list) of 
        undefined -> [];
        URL -> 
          case load_by_url(URL) of
            {ok, Commands} -> {ok, append_current_version(Commands)};
            Else -> Else
          end
      end
  end.


append_current_version(Commands) ->
  lists:map(fun({project, Project}) ->
    Name = proplists:get_value(name, Project),
    case application:get_key(Name, vsn) of
      {ok, Version} ->
        case is_binary(Version) of
 	  true ->
            {project, [{current_version, list_to_binary(Version)}|Project]};
          false -> 
            {project, [{current_version, Version}|Project]}
         end;
      undefined ->
        {project, Project}
    end  
  end, Commands). 

save(Versions) ->
  case read_config() of
    undefined -> {error, no_config};
    Config ->
      Config1 = lists:keystore(projects, 1, Config, {projects, Versions}),
      case load_from_server(Config1) of
        {ok, ServerReply} ->
          load_code(ServerReply),
          save_to_storage(Config1, ServerReply),
          ok;
        {error, Error} ->
          {error, Error}
      end
  end.
  
  
%%-------------------------------------------------------------------------
%% @spec () -> ok | {error, Reason}
%% @doc Loads code from storage or from server
%% @end
%% @private
%%-------------------------------------------------------------------------
%% Этот код фактически делает то, что написано на http://dev.erlyvideo.org/projects/commercial/wiki/Система_лицензий
load() ->
  case read_config() of
    undefined -> 
      error_logger:info_msg("Erlyvideo is booting in non-licensed mode~n"), 
      {ok, []};
    Config -> 
      case proplists:get_value(license, Config) of
        undefined -> 
          error_logger:info_msg("No license in license.txt~n"),
          {ok, []};
        _ ->
          load_from_config(Config)
      end
  end.


afterload(Modules) ->
  [ems_load_module(Module, after_start) || Module <- Modules].
  
load_from_config(Config) ->
  case load_from_storage(Config) of
    {ok, StartupModules} -> 
      error_logger:info_msg("Loaded code from local storage~n"),
      {ok, StartupModules};
    {error, _Error} ->
      error_logger:info_msg("Failed to load from storage: ~p~n", [_Error]),
      case load_from_server(Config) of
        {ok, ServerReply} ->
          StartupModules = load_code(ServerReply),
          save_to_storage(Config, ServerReply),
          {ok, StartupModules};
        {error, Error} ->
          error_logger:error_msg("Failed to load from server: ~p~n", [Error]),
          {error, Error}
    end
  end.


get_config_path() ->
  case application:get_env(erlyvideo,license_config)of 
    {ok, [Paths|FileName]} -> [Paths|FileName];
    undefined -> [["priv", "/etc/erlyvideo", "etc"],"license.txt"]
  end.

read_config() ->
  [Paths|FileName] = get_config_path(),
  read_config(Paths, FileName).

read_config(Paths, FileName) ->
  case file:path_consult(Paths, FileName) of
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
  StrictVersions = versions_of_projects(Config),
  StoredContent = read_storage(Config),
  % ?D(StoredContent),
  % io:format("Hi!!!: ~p ~p~n", [StoredContent, StrictVersions]),
  case storage_has_versions(StoredContent, StrictVersions) of
    ok ->
      StartupModules = load_code(StoredContent),
      {ok, StartupModules};
    {error, Error} ->
      {error, Error}
  end.


read_storage(undefined) ->
  [];

read_storage(Config) ->
  case open_license_storage(Config) of
    {ok, Table} ->
      List = case dets:lookup(?LICENSE_TABLE, applications) of
        [{applications, Apps}] ->
          error_logger:info_msg("License storage has applications: ~p~n", [Apps]),
          load_from_dets(Table, Apps, []);
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
        {ok, ?LICENSE_TABLE} -> ?D(?LICENSE_TABLE), {ok, ?LICENSE_TABLE};
        {error, Reason} -> ?D(Reason), {error, Reason} 
      end
  end.


load_from_dets(_Table, [], Acc) -> Acc;

load_from_dets(Table, [AppName|Apps], Acc) ->
  [{{app,AppName},Desc}] = dets:lookup(Table, {app,AppName}),
  Acc1 = lists:foldl(fun(ModName, AccIn) ->
    [{{mod,ModName},Module}] = dets:lookup(Table, {mod,ModName}),
    [{load, [{code,Module},{name,ModName}]}|AccIn]
  end, [{load_app, {application,AppName,Desc}}|Acc], proplists:get_value(modules,Desc, [])),
  load_from_dets(Table, Apps, Acc1).
  
  
storage_has_versions([], _) -> {error, empty_storage};
storage_has_versions(_Stored, []) -> ok;
storage_has_versions(Stored, [{AppName, Version} |Versions]) ->
  case storage_has_version(Stored, AppName, Version) of
    true -> storage_has_versions(Stored, Versions);
    false -> {error, deprecated_versions}
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
    {ok, "404", _Headers, Bin} ->
      error_logger:error_msg("No selected versions on server: ~p~n", [erlang:binary_to_term(Bin)]),
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


construct_url(Config, Command) when is_list(Config) ->
  case proplists:get_value(license, Config) of
    undefined -> undefined;
    License ->
      LicenseURL = proplists:get_value(url, Config, "http://license.erlyvideo.tv/license"),
      RawVersions = versions_of_projects(Config),
      Versions = [io_lib:format("&version[~s]=~s", [Name,Version]) || {Name,Version} <- RawVersions],
      lists:flatten([LicenseURL, io_lib:format("?key=~s&command=~s", [License, Command]), Versions])
  end.
  

get_versions_from_storage() ->
  case dets:lookup(?LICENSE_TABLE,projects) of
    [{projects, Projects}] -> Projects;
    _ -> []
  end.    

versions_of_projects(Config) ->
  LicensePath = proplists:get_value(license_dir,Config,"./"),
  case proplists:get_value(projects,Config,undefined) of
    undefined -> 
      case dets:open_file(?LICENSE_TABLE,[{file,ems:pathjoin(LicensePath,"license_storage.db")}]) of
        {ok,?LICENSE_TABLE} -> 
          get_versions_from_storage();
        _ -> []
      end;
    Projects -> Projects
  end.
         
              



%%%% load code
%%%%


load_code(Commands) ->
  Startup = execute_commands_v1(Commands,[]),
  handle_loaded_modules_v1(Startup),
  Startup.
  

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
    ok -> error_logger:info_msg("License load application ~p(~p)~n", [Name, Version]);
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
  error_logger:error_msg("Unknown license server command ~p", [_Command]),
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
  ems_load_module(Module, before_start),
  handle_loaded_modules_v1(Startup).


ems_load_module(Module, Command) ->
  case erlang:function_exported(Module, ems_client_load, 1) of
    true -> Module:ems_client_load(Command);
    false -> ok
  end.


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
      
      SavedAppNames = [Name || {Name,_} <- AppsToSave],
      
      error_logger:info_msg("License save applications: ~p~n", [SavedAppNames]),
      
      dets:insert(Table, ModulesToSave),
      dets:insert(Table, [{{app,Name},Desc} || {Name,Desc} <- AppsToSave]),
      dets:insert(Table, [{applications, [Name || {Name, _Desc} <- AppsToSave]}]),
      dets:close(Table),
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

read_config_test_() ->
  [
  ?_assertEqual([], read_config(["test/fixtures"], "license_broken.txt")),
  ?_assertEqual([], read_config(["test/fixtures"], "license_absent.txt")),
  ?_assertEqual([{license,"test-license"},{url,"http://license.erlyvideo.org/license"},{license_dir,"test/fixtures"}], read_config(["test/fixtures"], "license_good.txt"))
  ].

request_functions_test_() ->
  [
  ?_assertMatch({error,_Reason}, load_by_url(["Wrong_url"])),
  fun() ->
    Config = read_config(["test/fixtures"],"license_unavailable_versions.txt"),
    URL = construct_url(Config,save),
    ?assertEqual({error,notfound}, load_by_url(URL))
  end
  ].

storage_has_versions_test_() ->
  [
    ?_assertNot(storage_has_versions([],[]))
  ].

read_storage_test_() ->
  [
  ?_assert(is_list(read_storage(read_config(["test/fixtures"],"license_good.txt"))) =:= true),
  ?_assertEqual([],read_storage(read_config(["test/fixtures"],"lcense_bad_licanse_dir.txt")))
  ].

construct_url_test_() ->
  [
  ?_assertEqual("http://license.erlyvideo.org/license?key=test_key&command=test_command&version[test_application]=test_version",
    construct_url([{license, "test_key"},{url, "http://license.erlyvideo.org/license"},{projects,[{test_application,"test_version"}]}],test_command)),
  ?_assertEqual("http://license.erlyvideo.org/license?key=test_key&command=test_command",
    construct_url([{license, "test_key"},{url, "http://license.erlyvideo.org/license"},{projects,[]}],test_command)),
  ?_assertEqual(undefined, construct_url([{bad_license, "oops"}],test_command))
  ].

web_api_correct_test_() ->
  wrap_license_test_setup("license_good.txt", [
    ?_assertMatch({ok,"200",_Info,_Body},ibrowse:send_req("http://127.0.0.1:8082/erlyvideo/api/licenses",[],get)),
    ?_assertMatch({ok,"200",_Info,_Body},ibrowse:send_req("http://127.0.0.1:8082/erlyvideo/api/licenses",[],post,["Not_empty_body"]))
  ]).
 

web_api_broken_config_test_() ->
  wrap_license_test_setup("lic.txt", [
    ?_assertMatch({ok,"500",_Info,_Body},ibrowse:send_req("http://127.0.0.1:8082/erlyvideo/api/licenses",[],get)),
    ?_assertMatch({ok,"200",_Info,_Body},ibrowse:send_req("http://127.0.0.1:8082/erlyvideo/api/licenses",[],post,["Not_empty_body"]))
  ]).


wrap_license_test_setup(License, Fun) ->
  {spawn, {setup,
  fun() ->
    OldConfig = case application:get_env(erlyvideo,license_config) of
      {ok,ConfigPath} -> ConfigPath;
      undefined -> undefined
    end,
    application:set_env(erlyvideo,license_config,[["test/fixtures"],License]),
    OldConfig
  end,
  fun
    (undefined) -> cleanup_tests(), application:unset_env(erlyvideo, license_config);
    (ConfigPath) -> cleanup_tests(), application:set_env(erlyvideo,license_config,ConfigPath)
  end,
  fun(_) -> Fun end
  }}.

cleanup_tests() ->
  (catch file:delete("test/fixtures/license_storage.db")).

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
