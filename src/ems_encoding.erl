% Following line was looked after ffmpegX tool. This module repeat it.
% bin/sh -c printf "Encoding started on " && date && /Applications/ffmpegX.app//Contents/Resources/ffmpeg -i /Users/max/Movies/imgp7615.avi -an -f yuv4mpegpipe -croptop 0 -cropbottom 0 -cropleft 0 -cropright 0 -s 640x480 -r 25 -  | /Applications/ffmpegX.app//Contents/Resources/x264 -v -A i4x4 -b 1 --trellis 1 --qpmin 22 --qpmax 51 -B 940 --me umh --threads 2 --level 13 --fps 25 --pass 1 --stats /Users/max/Movies/imgp7615.avi.ff.mp4.log -o /Users/max/Movies/imgp7615.avi.ff.video.mp4 - 640x480 && /Applications/ffmpegX.app//Contents/Resources/ffmpeg -i /Users/max/Movies/imgp7615.avi -an -f yuv4mpegpipe -croptop 0 -cropbottom 0 -cropleft 0 -cropright 0 -s 640x480 -r 25 -  | /Applications/ffmpegX.app//Contents/Resources/x264 -v -A i4x4 -b 1 --trellis 1 --qpmin 22 --qpmax 51 -B 940 --me umh --threads 2 --level 13 --fps 25 --pass 2 --stats /Users/max/Movies/imgp7615.avi.ff.mp4.log -o /Users/max/Movies/imgp7615.avi.ff.video.mp4 - 640x480 && rm /Users/max/Movies/imgp7615.avi.ff.mp4.log && /Applications/ffmpegX.app//Contents/Resources/ffmpeg -i /Users/max/Movies/imgp7615.avi.ff.video.mp4 -i /Users/max/Movies/imgp7615.avi -y -vn -f mp4 -acodec aac -ab 96 -ar 48000 -ac 2 -map 1.1:0.0 /Users/max/Movies/imgp7615.avi.ff.audio.mp4 && /Applications/ffmpegX.app//Contents/Resources/mp4box  -fps 25:1 -add /Users/max/Movies/imgp7615.avi.ff.video.mp4  -add /Users/max/Movies/imgp7615.avi.ff.audio.mp4 -new /Users/max/Movies/imgp7615.avi.ff.mp4 && rm /Users/max/Movies/imgp7615.avi.ff.video.mp4 && rm /Users/max/Movies/imgp7615.avi.ff.audio.mp4 && printf "Encoding completed on " && date && printf "\a"

% ems_encoding:encode("/Users/max/Movies/imgp7615.avi", "/Users/max/Movies/imgp7615.mp4").

-module(ems_encoding).
-include("../include/ems.hrl").
-export([encode/2, status/1]).

status(_) -> ok.

encode(InFileName, OutFileName) ->
  FFMpegPath = case os:find_executable("ffmpeg") of
    false -> throw(no_ffmpeg_found);
    Value1 -> Value1
  end,
  X264Path = case os:find_executable("x264") of
    false -> throw(no_x264_found);
    Value2 -> Value2
  end,
  FirstFFMpegArgs = ["-i", InFileName, "-an", "-f","yuv4mpegpipe",
  "-croptop","0", "-cropbottom", "0", "-cropleft", "0", "-cropright", "0", 
  "-s", "640x480", "-r", "25", "-"],
  FirstFFmpeg = open_port({spawn_executable, FFMpegPath}, [exit_status, eof, {args, FirstFFMpegArgs}, binary, stream, out, use_stdio]),
  % FirstFFmpeg = open_port({spawn_executable, FFMpegPath}, [exit_status, eof, binary, stream, out, use_stdio, hide]),
  ?D({"Opening first ffmpeg", FirstFFmpeg}),
  FirstX264Args = ["-v","-A","i4x4", "-b", "1", "--trellis", "1", "--qpmin", "22", "--qpmax", "51", 
  "-B", "940", "--me", "umh", "--threads", "2", "--level", "13", "--fps", "25", "--pass", "1", 
  "--stats", OutFileName++".log", "-o", OutFileName, "-", "640x480"],
  FirstX264 = open_port({spawn_executable, X264Path}, [exit_status, eof, {args, FirstX264Args}, binary, stream, use_stdio, hide]),
  ?D({"Opening first h264", FirstX264}),
  ok.