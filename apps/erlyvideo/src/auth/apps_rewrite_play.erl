%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Authorization with checking allowed url
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(apps_rewrite_play, [URL]).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").
-include("../../include/rtmp_session.hrl").
-include("../log.hrl").


-export([play/2]).


dump_session_id(SessionId) when is_integer(SessionId) -> integer_to_list(SessionId);
dump_session_id(SessionId) when is_binary(SessionId) -> binary_to_list(SessionId);
dump_session_id(SessionId) when is_list(SessionId) -> SessionId;
dump_session_id(SessionId) -> lists:flatten(io_lib:format("~p",[SessionId])).


play(#rtmp_session{addr = IP, user_id = UserId, session_id = SessionId} = State, #rtmp_funcall{args = [null, FullName | Args]} = AMF) ->
  {Name, Options} = apps_streaming:parse_play(FullName, Args),
  
  Req = lists:flatten(io_lib:format("~s?ip=~s&file=~s&user_id=~p&session_id="++dump_session_id(SessionId), [URL, IP, Name, UserId])),

  ?D({auth_backend_request, Req}),
  {Code, Headers} = case ibrowse:send_req(Req,[],get,[],[{response_format,binary}]) of
    {ok, Code_, Headers_, _Bin} ->
      {Code_, Headers_};
    _Else ->
      erlang:error({http_auth_backend_error, _Else})
  end,
  
  schedule_play_limit_timer(Headers, Options),
  
  case Code of
    "200" ->
      unhandled;
    "302" ->
      case proplists:get_value("X-Location", Headers, proplists:get_value('Location', Headers)) of
        undefined ->
          ?D({"Auth backend replied 302 but no Location or X-Location header:", URL, Headers}),
          State;
        Path ->
          {unhandled, State, AMF#rtmp_funcall{args = [null, list_to_binary(Path) | Args]}}
      end;
    Else ->
      ?D({"Auth backend forbidden play", Else}),
      State
  end.


schedule_play_limit_timer(Headers, Options) ->
  case proplists:get_value("X-Play-Time", Headers) of
    undefined -> ok;
    Timer_ ->
      Timer = list_to_integer(Timer_),
      put(auth_play_limit, Timer),
      case proplists:get_value(start, Options, 0) of
        Num when is_integer(Num) andalso Num >= Timer -> self() ! exit;
        Num when is_integer(Num) andalso Num >= 0 -> timer:send_after(Timer - Num, exit)
      end
  end.

      