%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
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
-module(ems_media_clients).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/video_frame.hrl").
-include("../log.hrl").
-include("ems_media_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/1, insert/2, find/2, find_by_ticker/2, count/1, make_client_passive/3, update_state/3, delete/2, 
         send_frame/3, increment_bytes/3, list/1, flush/1,
         mark_active_as_starting/1, mark_starting_as_active/1]).
         
-export([init_repeater/3, repeater/2]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(SNDBUF, 4194304).

-define(REPEATER_COUNT, 1).
-define(ACCEL_CLIENTS_LIMIT, 300).

-record(clients, {
  active,
  passive,
  starting,
  repeaters,
  list = [],
  bytes,
  type,
  name,
  send_buffer
}).

-record(cached_entry, {
  pid, 
  socket, 
  dts, 
  stream_id,
  key
}).

init(Options) ->
  Clients = #clients{
    type = proplists:get_value(type, Options),
    name = proplists:get_value(name, Options),
    send_buffer = proplists:get_value(send_buffer, Options, ?SNDBUF)
  },
  case proplists:get_value(stream_mode, Options) of
    accelerated -> init_accel(Clients);
    _ -> Clients
  end.

init_accel(#clients{list = Entries} = Clients) ->
  ?D({"Init accelerated mode for stream", Clients#clients.name}),
  Clients1 = Clients#clients{
    bytes = ets:new(clients, [set, public]),
    active = ets:new(active, [set, public, {keypos, #cached_entry.pid}]),
    passive = ets:new(passive, [set, public, {keypos, #cached_entry.pid}]),
    starting = ets:new(starting, [set, public, {keypos, #cached_entry.pid}])
  },
  [insert_client(Clients1, Entry) || Entry <- Entries],
  Repeaters = [proc_lib:spawn_link(?MODULE, init_repeater, [Clients1, self(), Key]) || Key <- lists:seq(1, ?REPEATER_COUNT)],
  Clients1#clients{repeaters = Repeaters}.
  

init_repeater(#clients{} = Clients, Media, Key) when is_pid(Media) ->
  erlang:monitor(process, Media),
  ?MODULE:repeater(Clients, Key).
  
repeater(#clients{} = Clients, Key) ->
  receive
    {#video_frame{} = Frame, State} -> ?MODULE:repeater(repeater_send_frame(Frame, Clients, State, Key), Key);
    {'DOWN', _, process, _Media, normal} -> ok;
    {inet_reply, _Socket, _Reply} -> ?MODULE:repeater(Clients, Key);
    {flush, Ref, Parent} -> Parent ! {flushed, Ref}, ?MODULE:repeater(Clients, Key);
    Else -> erlang:exit({error, Else})
  end.


flush(#clients{repeaters = undefined}) -> ok;
flush(#clients{repeaters = Repeaters}) ->
  Ref = erlang:make_ref(),
  [Repeater ! {flush, Ref, self()} || Repeater <- Repeaters],
  read_flush_reply(Ref, length(Repeaters)).


read_flush_reply(_Ref, 0) ->
  ok;

read_flush_reply(Ref, Count) ->
  receive
    {flushed, Ref} -> read_flush_reply(Ref, Count - 1)
  after
    3000 -> erlang:exit({timeout_flushing,Count})
  end.

table(#clients{active = Table}, active) -> Table;
table(#clients{passive = Table}, passive) -> Table;
table(#clients{starting = Table}, starting) -> Table.


insert_client(_Clients, #client{state = paused}) ->
  ?D({"Wasn't inserted"}),
  ok;

% No ETS tables for files and low-usage streams
insert_client(#clients{active = undefined}, _Entry)  ->
  ok;
  
insert_client(#clients{bytes = Bytes} = Clients, #client{state = State, consumer = Client, stream_id = StreamId, tcp_socket = Socket, dts = DTS}) ->
  CachedEntry = #cached_entry{pid = Client, socket = Socket, dts = DTS, stream_id = StreamId,
    key = ets:info(table(Clients, State), size) rem ?REPEATER_COUNT + 1
  },
  ets:insert(Bytes, {Client,0}),
  ets:insert(table(Clients, State), CachedEntry),
  ok.


remove_client(#clients{active = undefined}, _Client) ->
  ok;

remove_client(#clients{active = A, passive = P, starting = S, bytes = Bytes}, Client) ->
  ets:delete(Bytes, Client),
  ets:delete(A, Client),
  ets:delete(P, Client),
  ets:delete(S, Client),
  ok.
  
insert(#clients{list = List, type = Type} = Clients, #client{consumer = Client} = Entry) ->
  insert_client(Clients, Entry),
  Clients1 = Clients#clients{list = lists:keystore(Client, #client.consumer, List, Entry#client{connected_at = ems:now(utc)})},
  if
    length(List) > ?ACCEL_CLIENTS_LIMIT andalso Type =/= file -> init_accel(Clients1);
    true -> Clients1
  end.

list(#clients{list = List, bytes = Bytes}) ->
  Now = ems:now(utc),
  [begin
    TimeDelta = Now - ConnectedAt,
    ByteCount = ets:lookup_element(Bytes, Pid, 2),
    % BitRate = 8*ByteCount div TimeDelta,
    [{pid, Pid},
     {bytes, ByteCount},
     {state, atom_to_binary(State,latin1)},
     {connection_time, TimeDelta}
     % {bitrate, BitRate}
   ] 
   end || #client{consumer = Pid, state = State, connected_at = ConnectedAt} <- List].



find(#clients{bytes = Bytes, list = List}, Client) ->
  case lists:keyfind(Client, #client.consumer, List) of
    false -> undefined;
    Entry -> Entry#client{bytes = fetch_bytes(Bytes, Client)}
  end.

fetch_bytes(undefined, _Client) -> 0;
fetch_bytes(Bytes, Client) -> ets:lookup_element(Bytes, Client, 2).
  
find_by_ticker(Storage, Pid) ->
  find(Storage, Pid, #client.ticker).
  
find(#clients{bytes = Bytes, list = List}, Value, Pos) ->
  case lists:keyfind(Value, Pos, List) of
    false -> undefined;
    #client{consumer = Client} = Entry -> Entry#client{bytes = ets:lookup_element(Bytes, Client, 2)}
  end.


count(#clients{active = undefined, list = Entries}) ->
  length(Entries);
  
count(#clients{bytes = Bytes}) ->
  ets:info(Bytes, size).
  

update_state(Storage, Client, State) ->
  update(Storage, Client, #client.state, State).

make_client_passive(Storage, Client, Ticker) ->
  case find(Storage, Client) of
    #client{} = Entry ->
      TickerRef = erlang:monitor(process,Ticker),
      update(Storage, Client, Entry#client{ticker = Ticker, ticker_ref = TickerRef, state = passive});
    _ -> Storage
  end.
  
update(#clients{bytes = Bytes}, Client, #client.bytes, Value) ->
  catch ets:insert(Bytes, {Client, Value});

update(#clients{list = List} = Clients, Client, Pos, Value) ->
  remove_client(Clients, Client),
  case lists:keytake(Client, #client.consumer, List) of
    {value, #client{} = Entry, List1} ->
      insert_client(Clients, Entry),
      Entry1 = setelement(Pos, Entry, Value),
      Clients#clients{list = [Entry1|List1]};
    false ->
      Clients
  end.

update(#clients{list = List, bytes = Bytes} = Clients, Client, #client{bytes = B} = NewEntry) ->
  remove_client(Clients, Client),
  insert_client(Clients, NewEntry),
  ets:insert(Bytes, {Client, B}),
  List1 = lists:keystore(Client, #client.consumer, List, NewEntry),
  Clients#clients{list = List1}.

delete(#clients{list = List} = Clients, Client) ->
  remove_client(Clients, Client),
  Clients#clients{list = lists:keydelete(Client, #client.consumer, List)}.


% send_frame(#video_frame{} = Frame, #clients{} = Clients, starting) ->
%   repeater_send_frame(Frame, Clients, starting, undefined);
send_frame(#video_frame{} = Frame, #clients{repeaters = undefined, list = Entries} = Clients, State) ->
  [Pid ! Frame#video_frame{stream_id = StreamId} || #client{state = State1, consumer = Pid, stream_id = StreamId} <- Entries, State1 == State],
  Clients;

send_frame(#video_frame{} = Frame, #clients{repeaters = Repeaters} = Clients, State) ->
%  case Frame#video_frame.content of
% 	video -> ?D({"video",Frame#video_frame.flavor});
%	Other -> ok
%  end,	
  [Repeater ! {Frame, State} || Repeater <- Repeaters],
  Clients.
  % repeater_send_frame(Frame, Clients, State, undefined).

repeater_send_frame(#video_frame{body = Body} = VideoFrame, #clients{bytes = Bytes} = Clients, State, Key) ->
  FlvFrameGen = flv:rtmp_tag_generator(VideoFrame),
  Table = table(Clients, State),
  Size = case (catch iolist_size(Body)) of
    {'EXIT', _} -> 0;
    BinSize -> BinSize
  end,
  Sender = fun
    (#cached_entry{key = EntryKey}, Frame) when is_number(Key) andalso Key =/= EntryKey ->
      Frame; 
    (#cached_entry{socket = {rtmp, Socket}, pid = Pid, dts = DTS, stream_id = StreamId}, Frame) when is_function(FlvFrameGen)->
      % ?D({accel_send, StreamId, VideoFrame#video_frame.codec,VideoFrame#video_frame.flavor, round(VideoFrame#video_frame.dts - DTS)}),
      case (catch port_command(Socket, FlvFrameGen(DTS, StreamId),[nosuspend])) of
        true -> ok;
        _ -> Pid ! {rtmp_lag, self()}
      end,
      (catch ets:update_counter(Bytes, Pid, Size)),
      Frame;
    (#cached_entry{pid = Pid, stream_id = StreamId}, Frame) ->
      (catch ets:update_counter(Bytes, Pid, Size)),
      % ?D({self(), Pid, Frame}),
      Pid ! Frame#video_frame{stream_id = StreamId},
      Frame
  end,
  
  ets:foldl(Sender, VideoFrame, Table),
  % [begin
  %   Pid ! Frame#video_frame{stream_id = StreamId},
  %   ets:update_counter(Bytes, Pid, Size)
  % end || #client{consumer = Pid, stream_id = StreamId, state = S} <- List, S == State],
  Clients.


mark_active_as_starting(Clients) -> mass_update_state(Clients, active, starting).
mark_starting_as_active(Clients) -> mass_update_state(Clients, starting, active).

mass_update_state(#clients{list = List} = Clients, From, To) ->
  flush(Clients),
  % ?D({update,From,ets:tab2list(table(Clients,From)),To,ets:tab2list(table(Clients,To))}),
  Clients#clients{list = [begin
    S = case State of
      From ->
        remove_client(Clients, Pid),
        insert_client(Clients, Entry#client{state = To}),
        To;
      S1 -> S1
    end,
    Entry#client{state = S}
  end || #client{consumer = Pid, state = State} = Entry <- List]}.
  
increment_bytes(#clients{bytes = Bytes} = Clients, Client, Size) ->
  (catch ets:update_counter(Bytes, Client, Size)),
  Clients.

-include_lib("eunit/include/eunit.hrl").


insert_test_() ->
  [
    fun() ->
      Storage = ?MODULE:init([]),
      Ticker = ticker_pid,
      StreamId = 1,
      Client = client_pid,
      Ref = make_ref(),
      TickerRef = make_ref(),
      Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
      Storage1 = ?MODULE:insert(Storage, Entry),
      ?assertMatch(#client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive}, 
      ?MODULE:find(Storage1, client_pid))
    end
  ].

increment_bytes_test_() ->
  [
    fun() ->
      log4erl:change_log_level(error),
      Storage = ?MODULE:init([{stream_mode,accelerated}]),
      log4erl:change_log_level(debug),
      Ticker = ticker_pid,
      StreamId = 1,
      Client = client_pid,
      Ref = make_ref(),
      TickerRef = make_ref(),
      Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
      Storage1 = ?MODULE:insert(Storage, Entry),
      Storage2 = ?MODULE:increment_bytes(Storage1, client_pid, 100),
      ?assertMatch(#client{bytes = 100}, ?MODULE:find(Storage2, client_pid))
    end
  ].

