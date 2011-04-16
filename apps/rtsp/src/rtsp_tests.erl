%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTSP decoder module
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
-module(rtsp_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


axis_m1011_test_() ->
  run_camera_test("axis-m1011", 8092).

axis_m1031_w_test_() ->
  run_camera_test("axis-m1031-w", 8092).

beward_test_() ->
  run_camera_test("beward", 8092).

sanyo_hd2100_test_() ->
  run_camera_test("sanyo-hd2100", 8092).

sanyo_hd2500_test_() ->
  run_camera_test("sanyo-hd2500", 8092).

beward1_test_() ->
  run_camera_test("beward_w20100722NS", 8092).

% logitech_750_high_test_() ->
%   run_camera_test("logitech-750-high", 8092).
  
logitech_750_low_test_() ->
  run_camera_test("logitech-750-low", 8092).

capture_output() ->
  receive
    stop -> ok;
    {io_request, From, ReplyAs, _Request} ->
      Reply = ok,
      % io:format("io_request: ~p~n", [Request]),
      From ! {io_reply, ReplyAs, Reply}, capture_output();
    else ->
      ok  
    % Msg -> io:format("msg: ~p~n", [Msg]), capture_output()
  end.
    

test_camera(Name) ->
  % log4erl:change_log_level(error),
  Self = self(),
  Logger = spawn_link(fun() ->
    erlang:monitor(process, Self),
    capture_output()
  end),
  Port = 8092,
  Pid = spawn_link(rtsp_test_client, simulate_camera, [Name, Port]),
  OldLeader = erlang:group_leader(),
  erlang:group_leader(Logger, Pid),
  erlang:group_leader(Logger, self()),
  io:format("Launched ~p~n", [Pid]),
  {ok, P} = media_provider:play(default, "rtsp://localhost:8092/"++Name, [{retry_limit,0},{clients_timeout,0}]),
  timer:send_after(40000, stop),
  Frames = read_frames([]),
  Delta = (hd(lists:reverse(Frames)))#video_frame.dts - (hd(Frames))#video_frame.dts,
  true = Delta >= 20000,
  ems_media:unsubscribe(P),
  (catch unlink(Pid)),
  (catch erlang:exit(Pid)),
  log4erl:change_log_level(debug),
  erlang:group_leader(OldLeader, self()).
  
    

run_camera_test(Name, Port) ->
  {spawn, {setup,
    fun() -> 
      log4erl:change_log_level(error),
      Self = self(),
      Logger = spawn_link(fun() ->
        erlang:monitor(process, Self),
        capture_output()
      end),
      Pid = spawn_link(fun() ->
        case (catch rtsp_test_client:simulate_camera(Name, Port)) of
          ok -> ok;
          {'EXIT',Reason} -> exit({Reason, {rtsp_test_client, simulate_camera, [Name, Port]}})
        end	
      end),
      erlang:group_leader(Logger, Pid),
      Pid
    end,
    fun(_Pid) ->
      log4erl:change_log_level(debug)
    end,
    fun(Pid) ->
      [fun() ->
        erlang:monitor(process, Pid),
        {ok, Media} = media_provider:play(default, "rtsp://localhost:8092/"++Name, [{retry_limit,0},{clients_timeout,0},{dump_traffic,false}]),
        timer:send_after(40000, stop),
        Frames = read_frames([]),
        ?assert(length(Frames) > 40),
        Delta = (hd(lists:reverse(Frames)))#video_frame.dts - (hd(Frames))#video_frame.dts,
        (catch ems_media:stop_stream(Media)),
        ?assert(Delta >= 20000),
        erlang:monitor(process, Pid),
        ?assertEqual(normal, ems_test_helper:wait4(Pid))
      end]
    end
  }}.


read_frames(Acc) ->
  receive
    #video_frame{} = F -> read_frames([F|Acc]);
    stop -> lists:reverse(Acc)
  after
    1000 -> lists:reverse(Acc)
  end.