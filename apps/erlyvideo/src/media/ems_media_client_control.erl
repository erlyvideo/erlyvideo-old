%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Erlyvideo media clients handling
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
-module(ems_media_client_control).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../include/ems_media.hrl").
-include("ems_media_client.hrl").
-include("../log.hrl").

-define(TIMEOUT, 120000).


-export([handle_call/3, handle_info/2, client_count/1]).


handle_call({subscribe, Client, Options}, From, #ems_media{clients_timeout_ref = Ref} = Media) when Ref =/= undefined ->
  timer:cancel(Ref),
  handle_call({subscribe, Client, Options}, From, Media#ems_media{clients_timeout_ref = undefined});


handle_call({subscribe, Client, Options}, _From, #ems_media{module = M, clients = Clients, audio_config = A, media_info = MediaInfo, last_dts = DTS} = Media) ->
  StreamId = proplists:get_value(stream_id, Options),

  DefaultSubscribe = fun(Reply, #ems_media{} = Media1) ->
    Ref = erlang:monitor(process,Client),
    RequireTicker = case Reply of
      tick -> true;
      _ -> proplists:get_value(start, Options) =/= undefined
    end,
    Clients1 = case RequireTicker of
      true ->
        {ok, Ticker} = ems_sup:start_ticker(self(), Client, Options),
        TickerRef = erlang:monitor(process,Ticker),
        Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
        ems_media_clients:insert(Clients, Entry);
      false ->
        HasVideo = length(MediaInfo#media_info.video) > 0,
        
        %
        % It is very important to understand, that we need to send audio config here, because client starts receiving music
        % right after subscribing, but it will wait for video till keyframe
        %
        ClientState = case proplists:get_value(paused, Options, false) of
          true -> paused;
          false when HasVideo == true -> starting;
          false when is_record(A, video_frame) -> Client ! A#video_frame{dts = DTS, pts = DTS, stream_id = StreamId}, active;
          false -> starting
        end,
        ems_media_clients:insert(Clients, #client{consumer = Client, stream_id = StreamId, ref = Ref, state = ClientState, tcp_socket = proplists:get_value(socket, Options), dts = DTS})
    end,
    {reply, ok, Media1#ems_media{clients = Clients1}, ?TIMEOUT}
  end,
  
  case M:handle_control({subscribe, Client, Options}, Media) of
    {stop, Reason, Media1} ->
      ?D({"ems_media failed to subscribe",Client,M,Reason}),
      {stop, Reason, Media1};
    {stop, Reason, Reply, Media1} ->
      {stop, Reason, Reply, Media1};
    {reply, {error, Reason}, Media1} ->
      {reply, {error, Reason}, Media1, ?TIMEOUT};
    {reply, Reply, Media1} ->
      DefaultSubscribe(Reply, Media1);
    {noreply, Media1} ->
      DefaultSubscribe(ok, Media1)
  end;

handle_call({stop, Client}, _From, Media) ->
  unsubscribe_client(Client, Media);

handle_call({unsubscribe, Client}, _From, Media) ->
  unsubscribe_client(Client, Media);

handle_call({start, Client}, From, Media) ->
  handle_call({resume, Client}, From, Media);


handle_call({resume, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{state = passive, ticker = Ticker} ->
      media_ticker:resume(Ticker),
      {reply, ok, Media, ?TIMEOUT};

    #client{state = paused} ->
      Clients1 = ems_media_clients:update_state(Clients, Client, starting),
      {reply, ok, Media#ems_media{clients = Clients1}, ?TIMEOUT};

    #client{} ->
      {reply, ok, Media, ?TIMEOUT};

    undefined ->
      {reply, {error, no_client}, Media, ?TIMEOUT}
  end;

handle_call({pause, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{state = passive, ticker = Ticker} ->
      ?D({"Pausing passive client", Client}),
      media_ticker:pause(Ticker),
      {reply, ok, Media, ?TIMEOUT};

    #client{} ->
      ?D({"Pausing active client", Client}),
      Clients1 = ems_media_clients:update_state(Clients, Client, paused),
      {reply, ok, Media#ems_media{clients = Clients1}, ?TIMEOUT};

    undefined ->
      ?D({"No client to pause", Client}),
      {reply, {error, no_client}, Media, ?TIMEOUT}
  end;

handle_call({seek, _Client, _DTS} = Seek, _From, #ems_media{} = Media) ->
  handle_seek(Seek, Media).


handle_info(no_clients, #ems_media{module = M} = Media) ->
  case client_count(Media) of
    0 ->
      % ?D({"graceful received, handling", self(), Media#ems_media.name}),
      case M:handle_control(no_clients, Media) of
        {noreply, Media1} ->
          {stop, normal, Media1};
        {stop, Reason, _Reply, Media1} ->
          {stop, Reason, Media1};
        {stop, Reason, Media1} ->
          case Reason of
            normal -> ok;
            _ -> ?D({"ems_media is stopping after graceful", M, Reason, Media#ems_media.name})
          end,
          {stop, Reason, Media1};
        {reply, ok, Media1} ->
          ?D({M, "rejected stopping of ems_media due to 0 clients", self(), Media#ems_media.name}),
          {noreply, Media1#ems_media{clients_timeout_ref = undefined}, ?TIMEOUT}
      end;
    _ -> {noreply, Media, ?TIMEOUT}
  end;



handle_info({'DOWN', _Ref, process, Pid, ClientReason} = Msg, #ems_media{clients = Clients, module = M} = Media) ->
  case unsubscribe_client(Pid, Media) of
    {reply, {error, no_client}, Media2, _} ->
      case ems_media_clients:find_by_ticker(Clients, Pid)  of
        #client{consumer = Client, stream_id = StreamId} ->
          case ClientReason of 
            normal -> ok;
            _ -> Client ! {ems_stream, StreamId, play_failed}
          end,
          case unsubscribe_client(Client, Media2) of
            {reply, _Reply, Media3, _} -> {noreply, Media3, ?TIMEOUT};
            {stop, Reason, _Reply, Media3} ->
              ?D({"ems_media is stopping after unsubscribe", M, Client, Reason}),
              {stop, Reason, Media3};
            {stop, Reason, Media3} -> 
              ?D({"ems_media is stopping after unsubscribe", M, Client, Reason}),
              {stop, Reason, Media3}
          end;
        undefined -> 
          case M:handle_info(Msg, Media2) of
            {noreply, Media3} -> {noreply, Media3, ?TIMEOUT};
            {stop, Reason, Media3} -> 
              ?D({"ems_media is stopping after handling info", M, Msg, Reason}),
              {stop, normal, Media3}
          end
      end;
    {reply, _Reply, Media2, _} -> {noreply, Media2, ?TIMEOUT};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {stop, Reason, Media2} -> 
      ?D({"ems_media is stopping after unsubscribe", M, Pid, Reason}),
      {stop, Reason, Media2}
  end.

handle_seek({seek, Client, DTS} = Seek, #ems_media{module = M} = Media) ->
  ?D({"Going to seek", Seek}),
  case M:handle_control(Seek, Media) of
    {noreply, Media1} ->
      ?D({"default seek", Seek}),
      default_ems_media_seek(Seek, Media1);
    {stop, Reason, Media1} ->
      {stop, Reason, Media1};
    {reply, {NewKey, NewDTS}, Media1} ->
      default_seek_reply(Client, {DTS, NewKey, NewDTS}, Media1);
    {reply, Reply, Media1} ->
      default_seek_reply(Client, Reply, Media1)
  end.


default_ems_media_seek(_, #ems_media{format = undefined} = Media) ->
  ?D("no format"),
  {reply, seek_failed, Media, ?TIMEOUT};

default_ems_media_seek({seek, Client, DTS}, #ems_media{format = Format, storage = Storage} = Media) ->
  ?D({"default seek", Client, DTS}),
  case Format:seek(Storage, DTS, []) of
    {NewPos, NewDTS} ->
      ?D({"seek ready", NewPos, NewDTS}),
      default_seek_reply(Client, {DTS, NewPos, NewDTS}, Media);
    undefined ->
      ?D({"no flv seek"}),
      {reply, seek_failed, Media, ?TIMEOUT}
  end.


default_seek_reply(Client, {DTS, _NewPos, NewDTS}, #ems_media{clients = Clients} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{ticker = Ticker, state = passive} ->
      media_ticker:seek(Ticker, DTS),
      {reply, {seek_success, NewDTS}, Media, ?TIMEOUT};

    #client{stream_id = StreamId} = Entry ->
      {ok, Ticker} = ems_sup:start_ticker(self(), Client, [{stream_id, StreamId}]),
      TickerRef = erlang:monitor(process,Ticker),
      media_ticker:seek(Ticker, NewDTS),
      Clients1 = ems_media_clients:update(Clients, Client, Entry#client{ticker = Ticker, ticker_ref = TickerRef, state = passive}),
      {reply, {seek_success, NewDTS}, Media#ems_media{clients = Clients1}, ?TIMEOUT};

    _ ->
      ?D("Client requested seek not in list"),
      {reply, seek_failed, Media, ?TIMEOUT}
  end.



unsubscribe_client(Client, #ems_media{options = Options, clients = Clients, module = M} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{ref = Ref, ticker = Ticker, ticker_ref = TickerRef} ->
      case M:handle_control({unsubscribe, Client}, Media) of
        {noreply, Media1} ->
          case TickerRef of
            undefined -> ok;
            _ ->
              erlang:demonitor(TickerRef, [flush]),
              (catch media_ticker:stop(Ticker))
          end,

          erlang:demonitor(Ref, [flush]),
          Entry = ems_media_clients:find(Clients, Client),
          Host = proplists:get_value(host, Options),
          Stats = [{bytes_sent, Entry#client.bytes}],
          Clients1 = ems_media_clients:delete(Clients, Client),

          ems_event:user_stop(Host, Client, self(), Stats),

          {reply, ok, check_no_clients(Media1#ems_media{clients = Clients1}), ?TIMEOUT};
        {reply, Reply, Media1} ->
          {reply, Reply, Media1, ?TIMEOUT};

        {stop, Reason, Reply, Media1} ->
          {stop, Reason, Reply, Media1};

        {stop, Reason, Media1} ->
          {stop, Reason, Media1}
      end;    

    undefined ->
      {reply, {error, no_client}, check_no_clients(Media), ?TIMEOUT}
  end.


client_count(#ems_media{clients = Clients}) ->
  ems_media_clients:count(Clients).


check_no_clients(#ems_media{clients_timeout = Timeout} = Media) ->
  Count = client_count(Media),
  {ok, TimeoutRef} = case Count of
    0 when is_number(Timeout) -> 
      % ?D({"No clients, sending delayed graceful", Timeout, self(), Media#ems_media.name}), 
      timer:send_after(Timeout, no_clients);
    _ -> 
      {ok, undefined}
  end,
  Media#ems_media{clients_timeout_ref = TimeoutRef}.
