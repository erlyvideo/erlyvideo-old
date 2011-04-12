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
-include("../include/ems_media.hrl").
-include("../log.hrl").
-include("ems_media_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/0, insert/2, find/2, find_by_ticker/2, count/1, update/3, update_state/3, delete/2, 
         send_frame/3, mass_update_state/3, increment_bytes/3, list/1, flush/1]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(clients, {
  list = [],
  bytes
}).

init() ->
  #clients{bytes = ets:new(clients, [set,  private])}.
  
  
list(#clients{list = List, bytes = Bytes}) ->
  Now = ems:now(utc),
  [begin
    TimeDelta = Now - ConnectedAt,
    ByteCount = ets:lookup_element(Bytes, Pid, 2),
    BitRate = 8*ByteCount div TimeDelta,
    [{pid, Pid},
     {bytes, ByteCount},
     {state, atom_to_binary(State,latin1)},
     {connection_time, TimeDelta},
     {bitrate, BitRate}
   ] 
   end || #client{consumer = Pid, state = State, connected_at = ConnectedAt} <- List].

flush(_) ->
  ok.
  
insert(#clients{list = List, bytes = Bytes} = Clients, #client{consumer = Client} = Entry) ->
  ets:insert(Bytes, {Client,0}),
  Clients#clients{list = lists:keystore(Client, #client.consumer, List, Entry#client{connected_at = ems:now(utc)})}.


find(#clients{bytes = Bytes, list = List}, Client) ->
  case lists:keyfind(Client, #client.consumer, List) of
    false -> undefined;
    Entry -> Entry#client{bytes = ets:lookup_element(Bytes, Client, 2)}
  end.
  
find_by_ticker(Storage, Pid) ->
  find(Storage, Pid, #client.ticker).
  
find(#clients{bytes = Bytes, list = List}, Value, Pos) ->
  case lists:keyfind(Value, Pos, List) of
    false -> undefined;
    #client{consumer = Client} = Entry -> Entry#client{bytes = ets:lookup_element(Bytes, Client, 2)}
  end.


count(#clients{bytes = Bytes}) ->
  ets:info(Bytes, size).
  

update_state(Storage, Client, State) ->
  update(Storage, Client, #client.state, State).

update(#clients{bytes = Bytes}, Client, #client.bytes, Value) ->
  ets:insert(Bytes, {Client, Value});

update(#clients{list = List} = Clients, Client, Pos, Value) ->
  
  case lists:keytake(Client, #client.consumer, List) of
    {value, #client{} = Entry, List1} ->
      Entry1 = setelement(Pos, Entry, Value),
      Clients#clients{list = [Entry1|List1]};
    false ->
      Clients
  end.

update(#clients{list = List, bytes = Bytes} = Clients, Client, #client{bytes = B} = NewEntry) ->
  ets:insert(Bytes, {Client, B}),
  List1 = lists:keystore(Client, #client.consumer, List, NewEntry),
  Clients#clients{list = List1}.

delete(#clients{list = List, bytes = Bytes} = Clients, Client) ->
  ets:delete(Bytes, Client),
  Clients#clients{list = lists:keydelete(Client, #client.consumer, List)}.
  

send_frame(#video_frame{content = Content} = Frame, #clients{list = List, bytes = Bytes} = Clients, State) ->
  Size = case Content of
    audio -> erlang:iolist_size(Frame#video_frame.body);
    video -> erlang:iolist_size(Frame#video_frame.body);
    _ -> 0
  end,
  [begin
    Pid ! Frame#video_frame{stream_id = StreamId},
    ets:update_counter(Bytes, Pid, Size)
  end || #client{consumer = Pid, stream_id = StreamId, state = S} <- List, S == State],
  Clients.


mass_update_state(#clients{list = List} = Clients, From, To) ->
  Clients#clients{list = [begin
    S = case State of
      From -> To;
      S1 -> S1
    end,
    Entry#client{state = S}
  end || #client{state = State} = Entry <- List]}.
  
increment_bytes(#clients{bytes = Bytes} = Clients, Client, Size) ->
  (catch ets:update_counter(Bytes, Client, Size)),
  Clients.

-include_lib("eunit/include/eunit.hrl").


insert_test_() ->
  [
    fun() ->
      Storage = ?MODULE:init(),
      Ticker = ticker_pid,
      StreamId = 1,
      Client = client_pid,
      Ref = make_ref(),
      TickerRef = make_ref(),
      Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
      Storage1 = ?MODULE:insert(Storage, Entry),
      ?assertEqual(Entry, ?MODULE:find(Storage1, client_pid))
    end
  ].

increment_bytes_test_() ->
  [
    fun() ->
      Storage = ?MODULE:init(),
      Ticker = ticker_pid,
      StreamId = 1,
      Client = client_pid,
      Ref = make_ref(),
      TickerRef = make_ref(),
      Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
      Storage1 = ?MODULE:insert(Storage, Entry),
      Storage2 = ?MODULE:increment_bytes(Storage1, client_pid, 100),
      ?assertEqual(Entry#client{bytes = 100}, ?MODULE:find(Storage2, client_pid))
    end
  ].



