-module(flv_writer).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").
-include("log.hrl").

-export([start_link/1, init_raw/1, writer/1]).

-export([init/1, read_frame/2, properties/1, seek/3, can_open_file/1, write_frame/2]).

-record(flv_file_writer, {
  writer,
  base_dts
}).


start_link(FileName) ->
  {ok, spawn_link(?MODULE, init_raw, [[FileName]])}.

init_raw([Writer]) when is_function(Writer) ->
	Writer(flv:header()),
	?MODULE:writer(#flv_file_writer{writer = Writer});

init_raw([FileName]) ->
  case init(FileName) of
    {ok, State} ->
      ?MODULE:writer(State);
		Error ->
		  error_logger:error_msg("Failed to start recording stream to ~p because of ~p", [FileName, Error]),
			exit({flv_file_writer, Error})
  end.
  
init(FileName) ->
	ok = filelib:ensure_dir(FileName),
  case file:open(FileName, [write, {delayed_write, 1024, 50}]) of
		{ok, File} ->
    	file:write(File, flv:header()),
    	{ok, #flv_file_writer{writer = fun(Data) ->
    	  file:write(File, Data)
    	end}};
		Error ->
			Error
  end.
	
writer(#flv_file_writer{} = FlvWriter) ->
  receive
    #video_frame{} = Frame ->
      {ok, FlvWriter1} = write_frame(Frame, FlvWriter),
      ?MODULE:writer(FlvWriter1);
    stop ->
      ok;
    Else ->
      ?D({"flv_writer", Else}),
    	?MODULE:writer(FlvWriter)
  end.
  
write_frame(#video_frame{dts = DTS} = Frame, #flv_file_writer{base_dts = undefined, writer = Writer} = FlvWriter) ->
  Writer(flv_video_frame:to_tag(Frame#video_frame{dts = 0, pts = 0})),
  {ok, FlvWriter#flv_file_writer{base_dts = DTS}};
  
write_frame(#video_frame{dts = DTS, pts = PTS} = Frame, #flv_file_writer{base_dts = BaseDTS, writer = Writer} = FlvWriter) ->
  Writer(flv_video_frame:to_tag(Frame#video_frame{dts = DTS - BaseDTS, pts = PTS - BaseDTS})),
  {ok, FlvWriter}.

read_frame(_, _) -> erlang:error(unsupported).
properties(_) -> [{type, file}].
seek(_,_,_) -> erlang:error(unsupported).
can_open_file(_) -> erlang:error(unsupported).

  

