% @hidden
% Following line was looked after ffmpegX tool. This module repeat it.
% bin/sh -c printf "Encoding started on " && date && /Applications/ffmpegX.app//Contents/Resources/ffmpeg -i /Users/max/Movies/imgp7615.avi -an -f yuv4mpegpipe -croptop 0 -cropbottom 0 -cropleft 0 -cropright 0 -s 640x480 -r 25 -  | /Applications/ffmpegX.app//Contents/Resources/x264 -v -A i4x4 -b 1 --trellis 1 --qpmin 22 --qpmax 51 -B 940 --me umh --threads 2 --level 13 --fps 25 --pass 1 --stats /Users/max/Movies/imgp7615.avi.ff.mp4.log -o /Users/max/Movies/imgp7615.avi.ff.video.mp4 - 640x480 && /Applications/ffmpegX.app//Contents/Resources/ffmpeg -i /Users/max/Movies/imgp7615.avi -an -f yuv4mpegpipe -croptop 0 -cropbottom 0 -cropleft 0 -cropright 0 -s 640x480 -r 25 -  | /Applications/ffmpegX.app//Contents/Resources/x264 -v -A i4x4 -b 1 --trellis 1 --qpmin 22 --qpmax 51 -B 940 --me umh --threads 2 --level 13 --fps 25 --pass 2 --stats /Users/max/Movies/imgp7615.avi.ff.mp4.log -o /Users/max/Movies/imgp7615.avi.ff.video.mp4 - 640x480 && rm /Users/max/Movies/imgp7615.avi.ff.mp4.log && /Applications/ffmpegX.app//Contents/Resources/ffmpeg -i /Users/max/Movies/imgp7615.avi.ff.video.mp4 -i /Users/max/Movies/imgp7615.avi -y -vn -f mp4 -acodec aac -ab 96 -ar 48000 -ac 2 -map 1.1:0.0 /Users/max/Movies/imgp7615.avi.ff.audio.mp4 && /Applications/ffmpegX.app//Contents/Resources/mp4box  -fps 25:1 -add /Users/max/Movies/imgp7615.avi.ff.video.mp4  -add /Users/max/Movies/imgp7615.avi.ff.audio.mp4 -new /Users/max/Movies/imgp7615.avi.ff.mp4 && rm /Users/max/Movies/imgp7615.avi.ff.video.mp4 && rm /Users/max/Movies/imgp7615.avi.ff.audio.mp4 && printf "Encoding completed on " && date && printf "\a"

% ems_encoding:encode("/Users/max/Movies/imgp7615.avi", "/Users/max/Movies/imgp7615.mp4").

% /Applications/VLC.app/Contents/MacOS/VLC -vvv -I dummy  canoneos1000d640x480.flv  ":sout=#transcode{venc=x264{bframes=3,cabac,level=51,keyint=25,bpyramid=none,profile=main,qpmin=20,qpmax=55,partitions=slow},vcodec=x264,vb=1000,width=640,height=480,fps=25,acodec=mp4a,ab=128}:standard{access=file,mux=mp4,dst=\"canon.mp4\"}" --sout-mp4-faststart  vlc://quit
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

-module(ems_encoding).
-include("../include/ems.hrl").
-export([encode/2, status/1]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, code_change/4, handle_event/3, handle_sync_event/4, terminate/3]).
-export([first_pass/2, second_pass/2]).

status(_) -> ok.

-record(encoder, {
  in_file_name,
  out_file_name,
  port
}).

encode(InFileName, OutFileName) ->
  gen_fsm:start_link(?MODULE, {InFileName, OutFileName}, []).

init({InFileName, OutFileName}) ->
  Encoder1 = #encoder{in_file_name = InFileName, out_file_name = OutFileName},
  Encoder2 = run_encoder(Encoder1, "1"),
  {ok, first_pass, Encoder2}.
  


audio_codec("1") -> "acodec=none";
audio_codec("2") -> "acodec=mp4a,ab=96".

run_encoder(#encoder{in_file_name = InFileName, out_file_name = OutFileName} = Encoder, Pass) ->
  X264 = "x264{keyint=120,chroma-me,me-range=8,ref=1,ratetol=1.0,8x8dct,mixed-refs,direct=auto,direct-8x8=-1,non-deterministic,scenecut=50,qpmax=30,qpmin=5,pass="++
    Pass++",stats="++ OutFileName++".log}",
  Output = ":std{access=file,mux=mp4,dst="++OutFileName++"}",
  VBitRate = "1024",
  
  Args = ["vlc ", InFileName, " --sout='#transcode{venc="++X264++",vcodec=h264,vb="++VBitRate++",scale=1,"++audio_codec(Pass)++"}"++ Output++"'", " -I", " dummy", " vlc://quit"],
  Cmd = lists:append(Args),
  
  Port = open_port({spawn, Cmd}, [stream, {line, 1000}, exit_status, binary, stderr_to_stdout, eof]),
  ?D("Running VLC pass "++Pass),
  Encoder#encoder{port = Port}.
  
  
first_pass(Event, State) ->
  ?D({"First pass", Event, State}),
  {next_state, first_pass, State}.

second_pass(Event, State) ->
  ?D({"Second pass", Event, State}),
  {next_state, second_pass, State}.

  
handle_event(Event, StateName, StateData) ->
  ?D({"Unknown event in player", Event, StateName}),
    {stop, {StateName, undefined_event, Event}, StateData}.



handle_sync_event(Event, _From, StateName, StateData) ->
  io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
  {stop, {StateName, undefined_event, Event}, StateData}.


handle_info({Port, {exit_status, 0}}, first_pass, #encoder{port = Port} = Encoder) ->
  ?D("First pass finished"),
  Encoder2 = run_encoder(Encoder, "2"),
  {next_state, second_pass, Encoder2};

handle_info({Port, {exit_status, Status}}, first_pass, #encoder{port = Port} = Encoder) ->
  ?D({"First pass failed:", Status}),
  Encoder1 = run_encoder(Encoder, "1"),
  {next_state, first_pass, Encoder1};

handle_info({Port, {exit_status, 0}}, second_pass, #encoder{port = Port} = Encoder) ->
  ?D("Second pass finished"),
  {stop, normal, Encoder#encoder{port = undefined}};

handle_info({Port, {exit_status, Status}}, second_pass, #encoder{port = Port} = Encoder) ->
  ?D({"Second pass failed:", Status}),
  Encoder2 = run_encoder(Encoder, "2"),
  {next_state, second_pass, Encoder2};

handle_info({Port, {data, {eol, String}}}, StateName, #encoder{port = Port} = Encoder) ->
  % ?D({"Status message", Status})
  io:format("~p~n", [binary_to_list(String)]),
  {next_state, StateName, Encoder};
  
handle_info({_Port, eof}, StateName, Encoder) ->
  ?D({"VLC closed while", StateName}),
  {next_state, StateName, Encoder};

handle_info(_Info, StateName, StateData) ->
  ?D({"Unknown info in encoder", _Info, StateName}),
  {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #encoder{port = _Port} = _State) ->
  ?D("Encoder exit"),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

  