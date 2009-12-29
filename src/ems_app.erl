%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        Supervisor module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
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
  
  ems_vhosts:start(),
	
  mnesia:create_schema([node()]),
  mnesia:start(),
  Start = ems_sup:start_link(),
  case ems:get_var(rtsp_port, undefined) of
    undefined -> ok;
    RTSP when is_integer(RTSP) -> rtsp:start_server(RTSP, rtsp_listener1, ems_rtsp)
  end,
  case ems:get_var(rtmp_port, undefined) of
    undefined -> ok;
    RTMP when is_integer(RTMP) -> rtmp_socket:start_server(RTMP, rtmp_listener1, rtmp_session)
  end,
  ok = ems:start_modules(),
  Start.


%%--------------------------------------------------------------------
%% @spec (Any::any()) -> any()
%% @doc Stop ErlMedia Application
%% @end 
%%--------------------------------------------------------------------
stop(_S) ->
  ems:stop_modules(),
  ems_vhosts:stop().
  

%%--------------------------------------------------------------------
%% @spec (Any::any()) -> any()
%% @doc Reload ErlMedia Application config
%% @end 
%%--------------------------------------------------------------------
config_change(Changed, New, Remove) ->
  ?D({"Changes", Changed, New, Remove}),
  ok.