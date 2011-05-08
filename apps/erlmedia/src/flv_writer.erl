%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Module to write FLV files
%%%
%%% There are two ways to use it:
%%% 1) passive with state
%%% 2) active with process, consuming #video_frame{}
%%% 
%%% On lower level, lies atomic writer:
%%% <code>
%%% {ok, Writer} = flv_writer:init_file(Filename),
%%% {ok, Writer1} = flv_writer:write_frame(Frame, Writer)
%%% </code>
%%% 
%%% Other way is to launch process, that will accept media frames:
%%% <code>
%%% {ok, Pid} = flv_writer:start_link(Filename),
%%% Pid ! Frame
%%% </code>
%%% 
%%% The code above will open ```Filename''' and write there frames. Also it is possible to
%%% pass writer function:
%%% 
%%% <code>
%%% {ok, File} = file:open("test.flv", [binary, write]),
%%% {ok, Pid} = flv_writer:start_link(fun(Data) -> file:write(File, Data) end),
%%% Pid ! Frame
%%% </code>
%%% 
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlmedia.
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
-module(flv_writer).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").
-include("log.hrl").

-export([start_link/1, start_link/2, init/3, writer/1, writer_no_timeout/1]).

-export([init_file/1, init_file/2, read_frame/2, properties/1, seek/3, can_open_file/1, write_frame/2]).

-define(BUFFER_SIZE, 40).

-record(flv_file_writer, {
  writer,
  buffer_size = 5,
  buffer = [],
  base_dts
}).

%%-------------------------------------------------------------------------
%% @spec (FileName|Writer) -> {ok, Pid}
%% @doc  Starts linked writer process
%% @end
%%-------------------------------------------------------------------------
start_link(FileName) ->
  start_link(FileName, []).
  
start_link(FileName, Options) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [FileName, self(), Options])}.

%% @hidden
init(Writer, Owner, Options) when is_function(Writer) ->
	Writer(flv:header()),
	erlang:monitor(process, Owner),
	SortBuffer = proplists:get_value(sort_buffer, Options, 0),
	?MODULE:writer(#flv_file_writer{writer = Writer, buffer_size = SortBuffer});

init(FileName, Owner, Options) when is_list(FileName) or is_binary(FileName) ->
  case init_file(FileName, Options) of
    {ok, State} ->
      erlang:monitor(process, Owner),
      ?MODULE:writer(State);
		Error ->
		  error_logger:error_msg("Failed to start recording stream to ~p because of ~p", [FileName, Error]),
			exit({flv_file_writer, Error})
  end.


%%-------------------------------------------------------------------------
%% @spec (FileName) -> {ok, Writer}
%% @doc  Creates writer state
%% @end
%%-------------------------------------------------------------------------
init_file(FileName) ->
  init_file(FileName, []).

init_file(FileName, Options) when is_binary(FileName) ->
  init_file(binary_to_list(FileName), Options);
  
init_file(FileName, Options) ->
	ok = filelib:ensure_dir(FileName),
	SortBuffer = proplists:get_value(sort_buffer, Options, 0),
	Mode = proplists:get_value(mode, Options, write),
  case file:open(FileName, [Mode, {delayed_write, 1024, 50}]) of
		{ok, File} when Mode == append ->
		  Duration = flv:duration({file,File}),
    	{ok, #flv_file_writer{writer = fun(Data) ->
    	  file:write(File, Data)
    	end, base_dts = Duration, buffer_size = SortBuffer}};
		{ok, File} when Mode == write ->
    	file:write(File, flv:header()),
    	{ok, #flv_file_writer{writer = fun(Data) ->
    	  file:write(File, Data)
    	end, buffer_size = SortBuffer}};
		Error ->
			Error
  end.

%% @hidden	
writer(FlvWriter) ->
  receive
    Message -> handle_message(Message, FlvWriter)
  after
    500 ->
      % ?D({flush,on_timeout}),
      ?MODULE:writer_no_timeout(flush_messages(FlvWriter, hard))
  end.

%% @hidden
% Called after timeout appeared to block forever till next message
writer_no_timeout(FlvWriter) ->
  receive
    Message -> handle_message(Message, FlvWriter)
  end.

%% External interface for spawned writer
write_frame(#video_frame{} = Frame, FlvWriter) when is_pid(FlvWriter) ->
  FlvWriter ! Frame,
  {ok, FlvWriter};

%% And for embedded writer
write_frame(#video_frame{} = Frame, #flv_file_writer{} = FlvWriter) ->
  store_message(Frame, FlvWriter);


write_frame(eof, FlvWriter) when is_pid(FlvWriter) ->
  FlvWriter ! eof,
  {ok, FlvWriter};

write_frame(eof, #flv_file_writer{} = FlvWriter) ->
  FlvWriter1 = flush_messages(FlvWriter, hard),
  {ok, FlvWriter1}.


%% @hidden
%% Handle message is for inside writer
handle_message(#video_frame{} = Frame, #flv_file_writer{} = FlvWriter) ->
  {ok, FlvWriter1} = store_message(Frame, FlvWriter),
  ?MODULE:writer(FlvWriter1);
  
handle_message(Message, FlvWriter) ->
  flush_messages(FlvWriter, hard),
  Message.
  
store_message(Frame, #flv_file_writer{buffer_size = 0} = Writer) ->
  % ?D({unbuffered_store}),
  dump_frame_in_file(Frame, Writer);
  
store_message(Frame, #flv_file_writer{buffer = Buffer, buffer_size = Size} = FlvWriter) when length(Buffer) >= Size ->
  FlvWriter1 = flush_messages(FlvWriter#flv_file_writer{buffer = [Frame|Buffer]}, soft),
  {ok, FlvWriter1};

store_message(Frame, #flv_file_writer{buffer = Buffer} = FlvWriter) ->
  {ok, FlvWriter#flv_file_writer{buffer = [Frame|Buffer]}}.
  

flush_messages(#flv_file_writer{buffer = Buf1} = FlvWriter, How) ->
  Sorted = lists:keysort(#video_frame.dts, Buf1),

  % Buffer = lists:reverse(Buf1),
  % case Sorted of
  %   Buffer -> ?D({"Frames in order", length(Sorted)});
  %   _ -> ?D({"Frame reordering work", length(Sorted)}),
  %   ?D([round(DTS) || #video_frame{dts = DTS} <- Sorted]),
  %   ?D([round(DTS) || #video_frame{dts = DTS} <- Buffer])
  % end,
  
  {Disk,Mem} = case How of 
    soft -> lists:split(length(Sorted) div 2, Sorted);
    hard -> {Sorted, []}
  end,
  % Disk = Buffer,
  % Mem = [],
  FlvWriter1 = lists:foldl(fun(Frame, Writer) ->
    {ok, Writer1} = dump_frame_in_file(Frame, Writer),
    Writer1
  end, FlvWriter, Disk),
  FlvWriter1#flv_file_writer{buffer = lists:reverse(Mem)}.
  

  
%%-------------------------------------------------------------------------
%% @spec (Frame, Writer) -> {ok, NewWriter}
%% @doc  Writes one flv frame
%% @end
%%-------------------------------------------------------------------------
dump_frame_in_file(#video_frame{dts = DTS} = Frame, #flv_file_writer{base_dts = undefined, writer = Writer} = FlvWriter) ->
  Writer(flv_video_frame:to_tag(Frame#video_frame{dts = 0, pts = 0})),
  {ok, FlvWriter#flv_file_writer{base_dts = DTS}};
  
dump_frame_in_file(#video_frame{dts = DTS, pts = PTS} = Frame, #flv_file_writer{base_dts = BaseDTS, writer = Writer} = FlvWriter) ->
  Writer(flv_video_frame:to_tag(Frame#video_frame{dts = DTS - BaseDTS, pts = PTS - BaseDTS})),
  {ok, FlvWriter}.

read_frame(_, _) -> erlang:error(unsupported).
properties(_) -> [{type, file}].
seek(_,_,_) -> erlang:error(unsupported).
can_open_file(_) -> erlang:error(unsupported).

  

