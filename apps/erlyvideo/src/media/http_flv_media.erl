%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        ems_media handler template
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(http_flv_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("ems_media.hrl").
-include("../log.hrl").


-define(FLV_HEADER_LENGTH, 9).

-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

%%%------------------------------------------------------------------------
%%% Callback functions from ems_media
%%%------------------------------------------------------------------------

-record(http_flv, {
  ibrowse_ref,
  buffer
}).

%%----------------------------------------------------------------------
%% @spec (Media::ems_media(), Options::list()) -> {ok, Media::ems_media()} |
%%                                                {stop, Reason}
%%
%% @doc Called by ems_media to initialize specific data for current media type
%% @end
%%----------------------------------------------------------------------

init(Media, Options) ->
  case proplists:get_value(passive, Options) of
    true -> ok;
    _ -> self() ! make_request
  end,
  {ok, Media#ems_media{state = #http_flv{}}}.


%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {ok, State}       |
%%                                        {ok, State, tick} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({source_lost, _Source}, #ems_media{options = Options} = State) ->
  %% Source lost returns:
  %% {ok, State, Source} -> new source is created 
  %% {stop, Reason, State} -> stop with Reason
  case proplists:get_value(passive, Options) of
    true -> ok;
    _ -> self() ! make_request
  end,
  {noreply, State};

handle_control(no_clients, State) ->
  {stop, normal, State};

handle_control({make_request, URL}, #ems_media{state = State} = Media) ->
  case ibrowse:send_req(binary_to_list(URL), [], get, [], [{stream_to, {self(),once}},{response_format,binary},{stream_chunk_size,8192}], infinity) of
    {ibrowse_req_id, Ref} ->
      {noreply, Media#ems_media{state = State#http_flv{ibrowse_ref = Ref}}};
    {error, Error} ->
      {error, Error}
  end;
  
handle_control({set_socket, Socket}, Media) ->
  inet:setopts(Socket, [{active, once},{packet,raw}]),
  {noreply, Media};  
  
handle_control(_Control, State) ->
  {noreply, State}.

%%----------------------------------------------------------------------
%% @spec (Frame::video_frame(), State) -> {reply, Frame, State} |
%%                                        {noreply, State}   |
%%                                        {stop, Reason, State}
%%
%% @doc Called by ems_media to parse frame.
%% @end
%%----------------------------------------------------------------------
handle_frame(Frame, State) ->
  % ?D({Frame#video_frame.codec,Frame#video_frame.flavor,round(Frame#video_frame.dts)}),
  {reply, Frame, State}.


%%----------------------------------------------------------------------
%% @spec (Message::any(), State) ->  {noreply, State}   |
%%                                   {stop, Reason, State}
%%
%% @doc Called by ems_media to parse incoming message.
%% @end
%%----------------------------------------------------------------------
handle_info({ibrowse_async_response, Stream, {error, _Error}}, #ems_media{state = #http_flv{ibrowse_ref = Stream}} = Media) ->
  ?D({http_flv, error, _Error}),
  ems_media:source_is_lost(Media);

handle_info({ibrowse_async_response, Stream, Bin}, #ems_media{state = #http_flv{ibrowse_ref = Stream, buffer = undefined} = State} = Media) ->
  ibrowse:stream_next(Stream),
  <<_:?FLV_HEADER_LENGTH/binary, Buf/binary>> = Bin,
  {noreply, Media#ems_media{state = State#http_flv{buffer = handle_bin(Buf)}}};

handle_info({ibrowse_async_response, Stream, Bin}, #ems_media{state = #http_flv{ibrowse_ref = Stream, buffer = Buf} = State} = Media) ->
  ibrowse:stream_next(Stream),
  {noreply, Media#ems_media{state = State#http_flv{buffer = handle_bin(<<Buf/binary, Bin/binary>>)}}};

handle_info({ibrowse_async_headers, Stream, "200", _Headers}, #ems_media{state = #http_flv{ibrowse_ref = Stream}} = Media) ->
  ibrowse:stream_next(Stream),
  ems_media:source_is_restored(Media);


handle_info({tcp, Socket, Bin}, #ems_media{state = #http_flv{buffer = undefined} = State} = Media) ->
  inet:setopts(Socket, [{active, once}]),
  <<_:?FLV_HEADER_LENGTH/binary, Buf/binary>> = Bin,
  {noreply, Media#ems_media{state = State#http_flv{buffer = handle_bin(Buf)}}};

handle_info({tcp, Socket, Bin}, #ems_media{state = #http_flv{buffer = Buf} = State} = Media) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, Media#ems_media{state = State#http_flv{buffer = handle_bin(<<Buf/binary, Bin/binary>>)}}};



handle_info({ibrowse_async_headers, Stream, Code, _Headers}, #ems_media{state = #http_flv{ibrowse_ref = Stream}} = Media) ->
  ?D({failed_http_flv,Code}),
  ems_media:source_is_lost(Media);
  
handle_info({ibrowse_async_response_end, Stream}, #ems_media{state = #http_flv{ibrowse_ref = Stream}} = Media) ->
  ?D({closed_http_flv}),
  ems_media:source_is_lost(Media);

handle_info(make_request, Media) ->
  {noreply, Media};

handle_info({tcp_closed, _Socket}, #ems_media{state = State} = Media) ->
  ems_media:source_is_lost(Media#ems_media{state = State#http_flv{buffer = undefined}});

handle_info(_Msg, State) ->
  {stop, {error, _Msg}, State}.


handle_bin(<<_PrevTagLength:32, FLV/binary>> = Data) ->
  case flv:read_frame(FLV) of
    {ok, Frame, Rest} ->
      self() ! Frame,
      handle_bin(Rest);
    more ->
      Data;
    {error, Reason} -> erlang:error(Reason)
  end;

handle_bin(FLV) when is_binary(FLV) ->
  FLV.



  
