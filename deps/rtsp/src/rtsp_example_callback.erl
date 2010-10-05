%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTSP example callback module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtsp.
%%% 
%%% erlang-rtsp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtsp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtsp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtsp_example_callback).
-behaviour(rtsp).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

% -export([handle_rtsp_response/2, handle_rtp_packet/2, handle_rtsp_request/2, media/1]).

-export([record/2, recorder/1, announce/3]).

record(URL, _Headers) ->
  Pid = spawn_link(?MODULE, recorder, [URL]),
  {ok, Pid}.
  
  
recorder(URL) ->
  receive
    stop ->
      io:format("TEARDOWN~n"),
      ok;
    Frame ->
      Type = element(2, Frame),
      DTS = round(element(3, Frame)),
      ?D({"F:", [Type, DTS]}),
      recorder(URL)
  end.


announce(_URL, _Headers, _Body) ->
  ok.
