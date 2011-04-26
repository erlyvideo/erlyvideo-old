%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Server for registering clients under there SIP ids
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
-module(apps_sip).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("rtmp/include/rtmp.hrl").
-include("../rtmp/rtmp_session.hrl").
-include("../log.hrl").

-export([register/2, ring/2, 'WAIT_FOR_DATA'/2]).

-export([sip_call/3]).

sip_call(RTMP, OutStream, InStream) when is_pid(RTMP) andalso is_binary(OutStream) andalso is_binary(InStream) ->
  gen_fsm:send_event(RTMP, {sip_call, OutStream, InStream}).

register(State, #rtmp_funcall{args = [_, Number]} = AMF) ->
  ?D({sip_register, self(), Number}),
  case ems_sip_flashphone:register(Number, self()) of
    {ok, _Ref} -> rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, true]});
    _Else -> ?D({failed_register,_Else}), rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, false]})
  end,
  State.

ring(State, #rtmp_funcall{args = [_, Number]} = AMF) ->
  case ems_sip_flashphone:call(Number, []) of
    {ok, _Pid} ->
      rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, true]});
    undefined ->
      rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, false]})
  end,
  State.


'WAIT_FOR_DATA'({sip_call, OutStream, InStream}, #rtmp_session{socket = Socket} = State) ->
  % io:format("NetConnection.Message ~s~n", [Message]),
  rtmp_socket:status(Socket, 0, <<"NetConnection.SipCall">>, {object, [{in_stream, InStream},{out_stream,OutStream}]}),
  {next_state, 'WAIT_FOR_DATA', State}.
