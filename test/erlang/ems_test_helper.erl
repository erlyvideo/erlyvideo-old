%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2011 Max Lapshin
%%% @doc        test helper module for erlyvideo
%%% 
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_test_helper).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("../../src/log.hrl").

-compile(export_all).

file_dir() ->
  code:lib_dir(erlyvideo, test) ++ "/files".

file_path(File) ->
  filename:join(file_dir(), File).


read_all_frames(Reader, Accessor, Options) ->
  {ok, Media} = Reader:init(Accessor, Options),
  read_all_frames(Media, [], Reader, undefined).

read_all_frames(Media, Frames, Reader, Key) ->
  case Reader:read_frame(Media, Key) of
    #video_frame{next_id = Next} = F -> read_all_frames(Media, [F|Frames], Reader, Next);
    eof -> {ok, Media, lists:reverse(Frames)}
  end.

receive_all_frames() ->
  receive_all_frames([], 10).

receive_all_frames(Timeout) ->
  receive_all_frames([], Timeout).

receive_all_frames(Acc, Timeout) ->
  receive
    #video_frame{} = F -> receive_all_frames([F|Acc], Timeout)
  after
    Timeout -> lists:reverse(Acc)
  end.

set_ticker_timeouts(NoTimeouts) ->
  application:set_env(erlyvideo,no_timeouts, NoTimeouts).


wait4(Pid) ->
  erlang:monitor(process, Pid),
  receive
    {'DOWN', _, _, Pid, Reason} -> Reason
  end.




is_monotonic([#video_frame{dts = DTS1}, #video_frame{dts = DTS2}|_Frames]) when DTS1 > DTS2 -> false;
is_monotonic([#video_frame{}, #video_frame{} = F|Frames]) -> is_monotonic([F|Frames]);
is_monotonic([_F]) -> true;
is_monotonic([]) -> true.


is_interleaved(Frames, Length) ->
  Counts = is_interleaved(Frames),
  % ?D({zz, Counts, [C || #video_frame{content = C} <- Frames]}),
  length([Count || Count <- Counts, Count > Length]) > length(Counts) div 3.

is_interleaved(Frames) ->
  is_interleaved(Frames, 0, []).


is_interleaved([#video_frame{content = Content}, #video_frame{content = Content} = F| Frames], Count, Acc) ->
  is_interleaved([F|Frames], Count+1, Acc);

is_interleaved([#video_frame{content = Content1}, #video_frame{content = Content2} = F| Frames], Count, Acc) when Content1 =/= Content2 ->
  is_interleaved([F|Frames], 0, [Count+1|Acc]);

is_interleaved([#video_frame{}], Count, Acc) ->
  lists:reverse([Count+1|Acc]);

is_interleaved([], 0, Acc) ->
  Acc.



has_small_delta([#video_frame{dts = DTS1}, #video_frame{dts = DTS2}|_Frames], Delta) when abs(DTS2 - DTS1) > Delta -> false;
has_small_delta([#video_frame{}, #video_frame{} = Frame|Frames], Delta) ->
  has_small_delta([Frame|Frames], Delta);

has_small_delta([_], _) -> true;
has_small_delta([], _) -> true.
