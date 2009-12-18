%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Supervisor module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_app).
-author(max@maxidoors.ru).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, config_change/3]).

%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts ErlMedia Application
%% @end 
%%--------------------------------------------------------------------

start(_Type, _Args) -> 
  application:load(?APPLICATION),
  
  vhosts = ets:new(vhosts, [set, named_table, public]),
  case application:get_env(?APPLICATION, vhosts) of
    {ok, Hosts} when is_list(Hosts) -> init_vhosts(Hosts);
    _ -> ok
  end,
	
  application:start(crypto),
  mnesia:create_schema([node()]),
  mnesia:start(),
  Start = ems_sup:start_link(),
  ok = ems:start_modules(),
  Start.

init_vhosts([]) ->
  ok;

init_vhosts([{Name, Host} | Hosts]) ->
  lists:foreach(fun({Key, Value}) ->
    ets:insert(vhosts, {{Name, Key}, Value})
  end, Host),
  lists:foreach(fun(Hostname) ->
    true = ets:insert(vhosts, {Hostname, Name})
  end, proplists:get_value(hostname, Host, [])),
  init_vhosts(Hosts).



%%--------------------------------------------------------------------
%% @spec (Any::any()) -> any()
%% @doc Stop ErlMedia Application
%% @end 
%%--------------------------------------------------------------------
stop(_S) -> 
  ems:stop_modules(),
  ets:delete(vhosts),
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any()) -> any()
%% @doc Reload ErlMedia Application config
%% @end 
%%--------------------------------------------------------------------
config_change(Changed, New, Remove) ->
  ?D({"Changes", Changed, New, Remove}),
  ok.