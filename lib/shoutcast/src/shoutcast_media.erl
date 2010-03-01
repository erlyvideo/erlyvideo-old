-module(shoutcast_media).
-author('Max Lapshin <max@maxidoors.ru>').
-export([start_link/2]).
-behaviour(gen_server).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-include("shoutcast.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

-record(shoutcast, {
  socket,
  url,
  audio_config = undefined,
  state,
  sync_count = 0,
  format = aac,
  buffer = <<>>,
  timestamp,
  clients = [],
  headers = [],
  byte_counter = 0
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% AAC+ example
% {ok, Pid1} = ems_sup:start_shoutcast_media("http://91.121.132.237:8052/").
% MP3 example
% {ok, Pid2} = ems_sup:start_shoutcast_media("http://205.188.215.230:8002").

start_link(URL, Opts) ->
  gen_server:start_link(?MODULE, [URL, Opts], []).


init([URL, Opts]) when is_binary(URL)->
  init([binary_to_list(URL), Opts]);
  
init([undefined, _]) ->
  process_flag(trap_exit, true),
  {ok, #shoutcast{state = request}};

init([URL, _Opts]) ->
  process_flag(trap_exit, true),
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}], 4000),
  ?D({Host, Path, Query, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"}),
  gen_tcp:send(Socket, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"),
  ok = inet:setopts(Socket, [{active, once}]),
  
  {ok, #shoutcast{socket = Socket, state = request}}.
  

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------

handle_call({set_socket, Socket}, _From, State) ->
  inet:setopts(Socket, [binary, {active, once}, {packet, raw}]),
  ?D({"Shoutcast received socket"}),
  {reply, ok, State#shoutcast{socket = Socket}};

handle_call({create_player, Options}, _From, #shoutcast{url = URL, clients = Clients} = State) ->
  {ok, Pid} = ems_sup:start_stream_play(self(), Options),
  link(Pid),
  ?D({"Creating media player for", URL, "client", proplists:get_value(consumer, Options), Pid}),
  case State#shoutcast.audio_config of
    undefined -> ok;
    AudioConfig -> Pid ! AudioConfig
  end,
  {reply, {ok, Pid}, State#shoutcast{clients = [Pid | Clients]}};

handle_call(clients, _From, #shoutcast{clients = Clients} = State) ->
  Entries = lists:map(fun(Pid) -> file_play:client(Pid) end, Clients),
  {reply, Entries, State};

handle_call({set_owner, _}, _From, State) ->
  {reply, ok, State};

handle_call(length, _From, State) ->
  {reply, 0, State};

handle_call(Request, _From, State) ->
  ?D({"Undefined call", Request, _From}),
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  ?D({"Undefined cast", _Msg}),
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------

handle_info({tcp, Socket, Bin}, #shoutcast{buffer = <<>>} = State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, decode(State#shoutcast{buffer = Bin})};

handle_info({tcp, Socket, Bin}, #shoutcast{buffer = Buffer} = State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, decode(State#shoutcast{buffer = <<Buffer/binary, Bin/binary>>})};

handle_info(#video_frame{decoder_config = true, type = audio} = Frame, State) ->
  {noreply, send_frame(Frame, State#shoutcast{audio_config = Frame})};

handle_info(#video_frame{} = Frame, State) ->
  {noreply, send_frame(Frame, State)};


handle_info({'EXIT', Client, _Reason}, #shoutcast{clients = Clients} = State) ->
  case {lists:member(Client, Clients), length(Clients)} of
    {true, 1} ->
      {stop, normal, State#shoutcast{clients = []}};
    {true, _} ->
      {noreply, State#shoutcast{clients = lists:delete(Client, Clients)}};
    _ ->
      {stop, {exit, Client, _Reason}, State}
  end;



handle_info({tcp_closed, Socket}, #shoutcast{socket = Socket} = State) ->
  {stop, normal, State#shoutcast{socket = undefined}};
  
handle_info(stop, #shoutcast{socket = Socket} = State) ->
  gen_tcp:close(Socket),
  {stop, normal, State#shoutcast{socket = undefined}};

handle_info(Message, State) ->
  {stop, {unhandled, Message}, State}.



decode(#shoutcast{state = request, buffer = <<"ICY 200 OK\r\n", Rest/binary>>} = State) ->
  decode(State#shoutcast{state = headers, buffer = Rest});

decode(#shoutcast{state = headers, buffer = Buffer, headers = Headers} = State) ->
  case erlang:decode_packet(httph_bin, Buffer, []) of
    {more, undefined} -> 
      State;
    {ok, {http_header, _, Name, _, Value}, Rest} ->
      ?D({Name, Value}),
      decode(State#shoutcast{headers = [{Name, Value} | Headers], buffer = Rest});
    {ok, http_eoh, Rest} ->
      decode(State#shoutcast{state = unsynced_body, format = format(State), buffer = Rest})
  end;

% decode(#shoutcast{state = metadata, buffer = <<Length, Data/binary>>} = State) when size(Data) >= Length*16 ->
%   MetadataLength = Length*16,
%   <<Metadata:MetadataLength/binary, Rest/binary>> = Data,
%   % ?D({"Metadata", Length, Metadata}),
%   decode(State#shoutcast{state = body, buffer = Rest});
% 
% decode(#shoutcast{state = metadata} = State) ->
%   State;
%

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount, format = mp3} = State) when SyncCount == 50 ->
  decode(State#shoutcast{format = mp3, sync_count = SyncCount + 1});

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount, format = aac} = State) when SyncCount == 50 ->
  decode(State#shoutcast{format = aac});

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount}) when SyncCount == 100 ->
  error;

decode(#shoutcast{state = unsynced_body, sync_count = SyncCount, format = mp3, buffer = <<_, Rest/binary>>} = State) ->
  case mp3:decode(State#shoutcast.buffer) of
    {ok, _, _} ->
      ?D({"Sync MP3"}),
      decode(State#shoutcast{state = body, timestamp = 0});
    {more, undefined} ->
      ?D({"Want more MP3 for sync"}),
      State#shoutcast{sync_count = SyncCount + 1};
    {error, unknown} ->
      decode(State#shoutcast{buffer = Rest, sync_count = SyncCount + 1})
  end;


decode(#shoutcast{state = unsynced_body, format = aac, sync_count = SyncCount, buffer = <<_, Rest/binary>>} = State) ->
  case aac:decode(State#shoutcast.buffer) of
    {ok, _Frame, Second} ->
      ?D({"Presync AAC"}),
      case aac:decode(Second) of
        {more, undefined} ->
          ?D({"Want more AAC for second frame"}),
          State;
        {error, unknown} ->
          ?D({"Presync failed"}),
          decode(State#shoutcast{buffer = Rest, sync_count = SyncCount + 1});
        {ok, _, _} ->
          ?D({"Synced AAC"}),
          AudioConfig = #video_frame{       
           	type          = audio,
           	decoder_config = true,
        		dts           = 0,
        		body          = aac:config(Second),
        	  codec_id	    = aac,
        	  sound_type	  = stereo,
        	  sound_size	  = bit16,
        	  sound_rate	  = rate44
        	},
        	send_frame(AudioConfig, State),
          decode(State#shoutcast{buffer = Second, state = body, audio_config = AudioConfig, timestamp = 0})
      end;
    {more, undefined} ->
      ?D({"Want more AAC for first frame"}),
      State;
    {error, unknown} ->
      decode(State#shoutcast{buffer = Rest})
  end;

% decode(#shoutcast{state = unsynced_body, format = aac, buffer = <<16#FFF:12, _:18, Length1:13, _:13, _:Length1, 16#FFF:12, _:18, Length2:13, _:13, _:Length2, _/binary>>} = State) ->
%   {_First, Buffer} = split_binary(State#shoutcast.buffer, 7+Length1),
%   decode(State#shoutcast{state = body, buffer = Buffer});
% 

decode(#shoutcast{state = unsynced_body, buffer = <<>>} = State) ->
  State;

decode(#shoutcast{state = body, format = aac, buffer = Data, timestamp = Timestamp} = State) ->
  % ?D({"Decode"}),
  case aac:decode(Data) of
    {ok, Packet, Rest} ->
      Frame = #video_frame{       
        type          = audio,
        dts           = Timestamp,
        body          = Packet,
    	  codec_id      = aac,
    	  sound_type	  = stereo,
    	  sound_size	  = bit16,
    	  sound_rate	  = rate44
      },
      send_frame(Frame, State),
      decode(State#shoutcast{buffer = Rest, timestamp = Timestamp + 1024});
    {error, unknown} -> 
      <<_, Rest/binary>> = Data,
      ?D({"sync aac"}),
      decode(State#shoutcast{buffer = Rest});
    {more, undefined} -> 
      % ?D(size(Data)),
      State
  end;

decode(#shoutcast{state = body, format = mp3, buffer = Data, timestamp = Timestamp} = State) ->
  % ?D({"Decode"}),
  case mp3:decode(Data) of
    {ok, Packet, Rest} ->
      Frame = #video_frame{       
        type          = audio,
        dts           = Timestamp,
        body          = Packet,
    	  codec_id	    = mp3,
    	  sound_type	  = stereo,
    	  sound_size	  = bit16,
    	  sound_rate	  = rate44
      },
      send_frame(Frame, State),
      decode(State#shoutcast{buffer = Rest, timestamp = Timestamp + 1024});
    {error, unknown} -> 
      <<_, Rest/binary>> = Data,
      ?D({"sync mp3"}),
      decode(State#shoutcast{buffer = Rest});
    {more, undefined} -> 
      State
  end.
      
format(#shoutcast{headers = Headers}) ->
  case proplists:get_value('Content-Type', Headers) of
    <<"audio/mpeg">> -> mp3;
    <<"audio/aacp">> -> aac
  end.



send_frame(Frame, #shoutcast{clients = Clients} = State) ->
  lists:foreach(fun(Client) -> Client ! Frame end, Clients),
  State.



%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ?D({"Shoutcast client terminating", _Reason}),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
