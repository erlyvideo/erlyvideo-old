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

axis_q7404_test_() ->
  run_camera_test("axis-q7404", 8092, [{acodec,aac}]).

axis_p5534_test_() ->
  run_camera_test("axis-p5534", 8092, [{acodec,aac}]).

axis_m1031_w_test_() ->
  run_camera_test("axis-m1031-w", 8092, [{acodec,aac}]).

beward_test_() ->
  run_camera_test("beward", 8092).

sanyo_hd2100_test_() ->
  run_camera_test("sanyo-hd2100", 8092).

sanyo_hd2500_test_() ->
  run_camera_test("sanyo-hd2500", 8092).

beward_w20100722ns_test_() ->
  run_camera_test("beward_w20100722NS", 8092).

beward_1_test_() ->
  run_camera_test("beward1", 8092, [{tracks,[1]}]).

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
    
test_camera(Name) -> test_camera(Name, []).

test_camera(Name, Options) ->
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
  {ok, P} = media_provider:play(default, "rtsp://localhost:8092/"++Name, [{retry_limit,0},{clients_timeout,0}|Options]),
  timer:send_after(40000, stop),
  Frames = read_frames([]),
  validate_frames(Frames, Options),
  ems_media:unsubscribe(P),
  (catch unlink(Pid)),
  (catch erlang:exit(Pid)),
  log4erl:change_log_level(debug),
  erlang:group_leader(OldLeader, self()).
  
    
run_camera_test(Name, Port) ->
  run_camera_test(Name, Port, []).

run_camera_test(Name, Port, Options) ->
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
        io:format("Camera: ~s~n", [Name]),
        erlang:monitor(process, Pid),
        {ok, Media} = media_provider:play(default, "rtsp://localhost:8092/"++Name, [{retry_limit,0},{source_timeout,0},{clients_timeout,0},{dump_traffic,false}|Options]),
        timer:send_after(40000, stop),
        Frames = read_frames([]),
        (catch ems_media:stop_stream(Media)),
        validate_frames(Frames, Options),
        erlang:monitor(process, Pid),
        ?assertEqual(normal, ems_test_helper:wait4(Pid))
      end]
    end
  }}.


validate_frames(Frames, Options) ->
  ACodec = proplists:get_value(acodec, Options),
  ?assert(length(Frames) > 40),
  Delta = (hd(lists:reverse(Frames)))#video_frame.dts - (hd(Frames))#video_frame.dts,
  ?assert(Delta >= 20000),
  ?assert(length([F || #video_frame{flavor = config, content = video} = F <- Frames]) > 0),
  case ACodec of
    aac -> ?assert(length([F || #video_frame{flavor = config, codec = C} = F <- Frames, C == ACodec]) > 0);
    _ -> ok
  end.
    

read_frames(Acc) ->
  receive
    #video_frame{} = F -> read_frames([F|Acc]);
    stop -> lists:reverse(Acc)
  after
    1000 -> lists:reverse(Acc)
  end.