%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        Generalized RTMP application behavior module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(apps_rtmp).
-author('max@maxiodors.ru').
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../include/ems.hrl").
-include_lib("erlyvideo/include/rtmp_session.hrl").

-export([connect/2]).
-export(['WAIT_FOR_DATA'/2]).


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a connect command and responds
%% @end
%%-------------------------------------------------------------------------
connect(#rtmp_session{socket = Socket, addr = Address} = State, AMF) ->
		
	  [{object, PlayerInfo} | AuthInfo] = AMF#rtmp_funcall.args,
	  _FlashVer = proplists:get_value(flashVer, PlayerInfo),
	  _SwfUrl = proplists:get_value(swfUrl, PlayerInfo),
	  ConnectUrl = proplists:get_value(tcUrl, PlayerInfo),
	  _Fpad = proplists:get_value(fpad, PlayerInfo),
	  _AudioCodecs = round(proplists:get_value(audioCodecs, PlayerInfo, 0)),
	  _VideoCodecs = proplists:get_value(videoCodecs, PlayerInfo),
	  _VideoFunction = proplists:get_value(videoFunction, PlayerInfo),
	  _PageUrl = proplists:get_value(pageUrl, PlayerInfo),

    {ok, UrlRe} = re:compile("(.*)://([^/]+)/?(.*)$"),
    {match, [_, _Proto, HostName, Path]} = re:run(ConnectUrl, UrlRe, [{capture, all, binary}]),
    Host = ems:host(HostName),
    
		NewState1 =	State#rtmp_session{player_info = PlayerInfo, host = Host, path = Path},

    AuthModule = ems:get_var(auth_module, Host, trusted_login),
    NewState2 = AuthModule:client_login(NewState1, AuthInfo),

    ems_log:access(Host, "CONNECT ~p ~s ~p ~s ~p", [Address, Host, NewState2#rtmp_session.user_id, _PageUrl, self()]),
    
    rtmp_session:accept_connection(State, AMF),
    NewState2.



% 
% 
% 
% 'WAIT_FOR_DATA'({invoke, #rtmp_funcall{stream_id = StreamId} = AMF}, #rtmp_session{socket = Socket} = State) ->
%   rtmp_socket:send(Socket, #rtmp_message{channel_id = 3, timestamp = 0, type = invoke, stream_id = StreamId, body = AMF}),
%   {next_state, 'WAIT_FOR_DATA', State};
% 

'WAIT_FOR_DATA'(_Message, #rtmp_session{} =_State) -> {unhandled}.
