%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        erlyvideo rtsp callback
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
-module(ems_rtsp).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([record/3, announce/3, describe/3, play/3]).
-export([start/0, stop/0]).


start() ->
  case ems:get_var(rtsp_port, undefined) of
    undefined ->
      ok;
    RTSP ->
      application:start(rtsp),
      rtsp:start_server(RTSP, rtsp_listener1, ems:get_var(rtsp_callback, ems_rtsp)),
      ok
  end.


stop() ->
  application:stop(rtsp),
  application:unload(rtsp),
  ok.

hostpath(URL) ->
  {HostPort, Path} = http_uri2:extract_path_with_query(URL),
  {ems:host(HostPort), string:strip(Path,both,$/)}.


announce(URL, Headers, MediaInfo) ->
  {Host, Path} = hostpath(URL),
  ?D({"ANNOUNCE", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),

  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    _Session ->
      {ok, Media} = media_provider:open(Host, Path, [{type, live}]),
      ems_media:set_media_info(Media, MediaInfo),
      ems_media:set_source(Media, self()),
      {ok, Media}
  end.


record(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"RECORD", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),

  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    _Else ->
      ems_log:access(Host, "RTSP RECORD ~s ~s", [Host, Path]),
      ok
  end.

describe(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  {Module, Function} = ems:check_app(Host, auth, 3),
  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    _Session ->
      {ok, Media} = media_provider:open(Host, Path),
      {ok, Media}
  end.

play(URL, _Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  % {Module, Function} = ems:check_app(Host, auth, 3),
  ems_log:access(Host, "RTSP PLAY ~s ~s", [Host, Path]),
  {ok, Media} = media_provider:play(Host, Path, [{stream_id,1}, {client_buffer,100}]),
  ems_network_lag_monitor:watch(self()),
  {ok, Media}.

