%%% Copyright (c) 2007 Roberto Saccon <rsaccon@gmail.com>
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

%%% Server TCP handling derived from:
%%% http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
%%% Author: Serge Aleynikov <saleyn at gmail.com>
 

-module(erlyvideo_app).
-author('rsaccon@gmail.com').

-behaviour(application).

%% Internal API
-export([start_client/0]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1, get_app_env/2]).

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(RTMP_PORT, 1935).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(erlyvideo_client_sup, []).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    Port = get_app_env(rtmp_port, ?RTMP_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, erlyvideo_fsm]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [                      
       % Erlyvideo Listener
       {   erlyvideo_sup,       % Id       = internal id
           {erlyvideo_listener, % StartFun = {M, F, A}
            start_link, 
            [Port,Module]}, 
           permanent,           % Restart  = permanent | transient | temporary
           2000,                % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,              % Type     = worker | supervisor
           [erlyvideo_listener] % Modules  = [Module] | dynamic
          },
       % Client instance supervisor
       {   erlyvideo_client_sup,
           {supervisor,
            start_link,
            [{local, erlyvideo_client_sup}, ?MODULE, [Module]]},
           permanent,        % Restart  = permanent | transient | temporary
           infinity,         % Shutdown = brutal_kill | int() >= 0 | infinity
           supervisor,       % Type     = worker | supervisor
           []                % Modules  = [Module] | dynamic
          }
      ]
     }
    };

init([Module]) ->
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       % Erlyvideo Client
       {   undefined,        % Id       = internal id
           {Module,          % StartFun = {M, F, A}
            start_link, 
            []},                 
           temporary,        % Restart  = permanent | transient | temporary
           2000,             % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,           % Type     = worker | supervisor
           []                % Modules  = [Module] | dynamic
          }
      ]
     }
    }.

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error       -> Default
            end
    end.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
