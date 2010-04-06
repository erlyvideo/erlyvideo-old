-module(flv_writer).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([start_link/1, init/1, writer/1]).

-record(flv_file_writer, {
  file,
  base_dts
}).


start_link(FileName) ->
  {ok, spawn_link(?MODULE, init, [[FileName]])}.
  
init([FileName]) ->
	ok = filelib:ensure_dir(FileName),
  case file:open(FileName, [write, {delayed_write, 1024, 50}]) of
		{ok, File} ->
		  Header = flv:header(),
    	file:write(File, Header),
    	?MODULE:writer(#flv_file_writer{file = File});
		Error ->
		  error_logger:error_msg("Failed to start recording stream to ~p because of ~p", [FileName, Error]),
			exit({flv_file_writer, Error})
  end.
	
writer(#flv_file_writer{file = File, base_dts = BaseDTS} = Writer) ->
  receive
    #video_frame{dts = DTS} = Frame ->
      {DTS1, BaseDTS1} = case BaseDTS of
        undefined -> {0, DTS};
        _ -> {DTS - BaseDTS, BaseDTS}
      end,    
    	file:write(File, flv_video_frame:to_tag(Frame#video_frame{dts = DTS1})),
    	?MODULE:writer(Writer#flv_file_writer{base_dts = BaseDTS1});
    Else ->
      ?D({"flv_writer", Else})
  end.
  
