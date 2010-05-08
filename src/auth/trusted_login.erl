%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        trusted login
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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
-module(trusted_login).
-author('Max Lapshin <max@maxidoors.ru>').
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-include("../../include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([connect/2]).


%%-------------------------------------------------------------------------
%% @spec connect(Session::rtmp_session(), Funcall::rtmp_funcall()) -> NewState::rtmp_session()
%% @doc Function always accept client, trusting all client information. 
%% It can even subscribe to channels
%% @end
%%-------------------------------------------------------------------------
connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, #rtmp_funcall{args = [_, SessionData, UserId]}) ->
  Session = try json_session:decode(SessionData, undefined) of
    S when is_list(S) -> S;
    _ -> []
  catch
    _Class:_Error ->
      % ems_log:error(Host, "Session decoding: ~p:~p:~p", [_Class, _Error, erlang:get_stacktrace()]),
      ?D({"No session provided:", SessionData}),
      []
  end,
  Channels = proplists:get_value(channels, Session, []),
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{user_id = UserId, session_id = SessionId},
	ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~w trusted_login", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), Channels]),
	rtmp_session:accept_connection(NewState),
  NewState;
  
	
connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, _AMF) ->
  ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~p trusted_login", [Address, Host, undefined, proplists:get_value(pageUrl, PlayerInfo), []]),
	rtmp_session:accept_connection(State),
  State.
	