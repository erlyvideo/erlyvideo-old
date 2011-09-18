-module(elixir_tracker).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-include("elixir.hrl").


-define(D(X), error_logger:error_msg("~p", [X])).

-export([start_link/0]).
-export([add_path/1, remove_path/1, paths/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server_ems:start_link({local, ?MODULE}, ?MODULE, [], []).

add_path(Path) when is_list(Path) ->
  gen_server:call(?MODULE, {add_path, Path}).

remove_path(Path) ->
  gen_server:call(?MODULE, {remove_path, Path}).

paths() ->
  gen_server:call(?MODULE, paths).



-record(tracker, {
  paths = [],
  modules = []
}).

-define(TIMEOUT, 3000).

init([]) ->
  application:start(elixir),
  Paths = case application:get_env(elixir, paths) of
    {ok, P} -> P;
    _ -> []
  end,
  {ok, recheck(#tracker{paths = Paths})}.


handle_call({add_path, Path}, _From, #tracker{paths = Paths} = Tracker) ->
  NewPaths = lists:usort([Path|Paths]),
  application:set_env(elixir, paths, NewPaths),
  {reply, ok, Tracker#tracker{paths = NewPaths}};

handle_call({remove_path, Path}, _From, #tracker{paths = Paths} = Tracker) ->
  NewPaths = lists:delete(Path, Paths),
  application:set_env(elixir, paths, NewPaths),
  {reply, ok, Tracker#tracker{paths = NewPaths}};

handle_call(paths, _From, #tracker{paths = Paths} = Tracker) ->
  {reply, Paths, Tracker};

handle_call(Call, _From, Tracker) ->
  {stop, {unknown_call, Call}, Tracker}.



handle_cast(Cast, Tracker) ->
  {stop, {unknown_cast, Cast}, Tracker}.

handle_info(recheck, Tracker) ->
  {noreply, recheck(Tracker)};

handle_info(Info, Tracker) ->
  {stop, {unknown_info, Info}, Tracker}.



terminate(_,_) -> ok.
code_change(_, State, _) -> {ok, State}.


recheck(#tracker{paths = CodePaths, modules = OldModules} = Tracker) ->
  ModulePaths = lists:foldl(fun(Path, List) ->
    List ++ filelib:wildcard(Path++"/*.ex")
  end, [], CodePaths),
  AllModules = lists:usort([list_to_atom(filename:basename(Path, ".ex")) || Path <- ModulePaths]),
  if
    length(AllModules) =/= length(ModulePaths) -> ?D({have_clashed_modules, ModulePaths});
    true -> ok
  end,
  
  RemovedModules = OldModules -- AllModules,
  remove_old_modules(RemovedModules),
  
  
  NewPaths = lists:filter(fun(Path) ->
    ModuleName = mod_name(Path),
    not lists:member(ModuleName, OldModules)
  end, ModulePaths),
  
  NewModules = compile_new_modules(NewPaths),
  
  PathsToCheck = ModulePaths -- NewPaths,
  ReloadedModules = reload_if_required_paths(PathsToCheck),

  timer:send_after(?TIMEOUT, recheck),
  Tracker#tracker{modules = ReloadedModules ++ NewModules}.

remove_old_modules(RemovedModules) ->
  lists:foreach(fun(Module) ->
    code:soft_purge(Module),
    code:delete(Module),

    ExMod = ex_name(Module),
    code:soft_purge(ExMod),
    code:delete(ExMod)
  end, RemovedModules).

compile_new_modules(NewPaths) ->
  [compile_new_module(Path) || Path <- NewPaths].

compile_new_module(Path) ->
  
  Module = mod_name(Path),
  try elixir:file(Path) of
    _ -> ok
  catch
    _Klass:Error -> error_logger:error_msg("Elixir error: ~p~n", [Error])
  end,
  case erlang:module_loaded(ex_name(Module)) of
    true ->
      Forms = proxy_module_text(Module),
      {ok, Module, Bin} = compile:forms(Forms, [binary]),
      code:soft_purge(Module),
      code:load_binary(Module, filename:basename(Path), Bin),
      error_logger:info_msg("Loaded module ~p from ~s", [Module, Path]),
      mod_name(Path);
    false ->
      error_logger:error_msg("Couldn't load module from ~s", [Path]),
      undefined
  end.


proxy_module_text(Module) ->
  ExMod = ex_name(Module),
  Exports = ExMod:'__local_methods__'([]),
  Klass = list_to_atom(klass_name(Module)),

  Line = 0,
  
  Context = {match,Line,
            {var,Line,'Context'},
            {call,Line,
             {remote,Line,{atom,Line,elixir_constants},{atom,Line,lookup}},
             [{atom,Line,Klass}]}},
             
  Args = fun(Arity) ->
    [{var, Line, list_to_atom("Arg"++integer_to_list(I))} || I <- lists:seq(1, Arity)]
  end,
  
  PrependArgs = fun(Arity) ->
    lists:foldl(fun(Arg, Tuple) ->
      {cons, Line, Arg, Tuple}
    end, {nil,Line}, lists:reverse(Args(Arity)))
  end,

  Apply = fun(Fun, Arity) ->
     {call, Line, {atom,Line,apply}, [
    {atom,Line,ExMod}, {atom, Line,Fun}, {cons,Line,{var,22,'Context'},PrependArgs(Arity)}]}
  end,
  [
  {attribute, Line, module, Module},
  {attribute, Line, export, Exports}
  ] ++ lists:map(fun({Fun,Arity}) ->
    {function, Line, Fun, Arity, [
      {clause, Line, Args(Arity), [], [
        Context, Apply(Fun, Arity)
      ]}
    ]}
  end, Exports) ++ [{eof,Line}].


reload_if_required_paths(PathsToCheck) ->
  [reload_module_if_required(Path) || Path <- PathsToCheck].


reload_module_if_required(Path) ->
  {ok, #file_info{mtime = Mtime}} = file:read_file_info(Path),
  Module = mod_name(Path),
  ExMod = ex_name(Module),
  case erlang:module_loaded(ExMod) of
    true ->
      Options = ExMod:module_info(compile),
      {Y,Mon,D,H,Min,S} = proplists:get_value(time, Options),
      LocalCompileTime = calendar:universal_time_to_local_time({{Y,Mon,D},{H,Min,S}}),
      LifeTime = calendar:datetime_to_gregorian_seconds(LocalCompileTime) - calendar:datetime_to_gregorian_seconds(Mtime),
      if
        LifeTime < 0 ->
          % io:format("Reloading ~p~n", [Path]),
          code:soft_purge(ExMod),
          code:delete(ExMod),
          compile_new_module(Path),
          ok;
        true -> 
          ok
      end;
    false ->
      compile_new_module(Path)
  end,
  Module.

mod_name(Path) when is_list(Path) ->
  list_to_atom(filename:basename(Path, ".ex")).

ex_name(Module) when is_atom(Module) ->
  list_to_atom("ex"++klass_name(Module)).

klass_name(Module) ->
  string:join([[string:to_upper(H)]++T || [H|T] <- string:tokens(atom_to_list(Module), "_")], "").
  
