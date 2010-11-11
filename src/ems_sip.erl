%%%---------------------------------------------------------------------------------------
%%% @author     Maxim Treskin <zerthurd@gmail.com>
%%% @copyright  2010 Max Lapshin
%%% @doc        erlyvideo sip callback
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
-module(ems_sip).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([start/0, stop/0]).

-export([progress/3, play/3]).

start() ->
  ?D("Start EMS SIP"),
  case ems:get_var(?MODULE, undefined) of
    undefined ->
      ok;
    Config ->
      ?D({"Config", Config}),
      application:start(esip, temporary),
      timer:sleep(1000),
      esip:set_config(Config),
      esip:start_transport(),
      ok
  end.


stop() ->
  application:stop(esip),
  application:unload(esip),
  ok.



hostpath(URL) ->
  {match, [Path, HostPort]} = re:run(URL, "sip:([^@]+)@(.*)", [{capture, [1,2], binary}]),
  {ems:host(HostPort), Path}.


progress(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"PROGRESS", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),
  case Module:Function(Host, esip, proplists:get_value('Authorization', Headers)) of
    undefined ->
      ?D({Module, Function}),
      {error, authentication};
    _Session ->
      ?D({Module, Function, _Session}),
      Instream = <<Path/binary, <<"-in">>/binary >>,
      Outstream = << Path/binary, <<"-out">>/binary >>,
      {ok, Media} = media_provider:create(default, Instream, [{type,live},{source_shutdown,shutdown}]),
      case esip_registrator:get(Path) of
        {ok, RTMP} ->
          apps_sip:sip_call(RTMP, Outstream, Instream),
          {ok, Media};
        _ ->
          {error, not_found}
      end
  end.

play(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"PLAY", Host, Path, Headers}),
  ems_log:access(Host, "SIP PLAY ~s ~s", [Host, Path]),
  Outstream = << Path/binary, <<"-out">>/binary >>,
  {ok, Media} = media_provider:play(default, Outstream, [{stream_id,1}]),
  {ok, Media}.
