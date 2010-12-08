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

-export([init/0, insert/2, find/2, find_by_ticker/2, count/1, update/3, update_state/3, delete/2, 
         send_frame/3, mass_update_state/3, increment_bytes/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(clients, {
  active,
  passive,
  starting,
  list = [],
  bytes
}).

init() ->
  #clients{
    active = ets:new(active, [set,  private]),
    passive = ets:new(passive, [set,  private]),
    starting = ets:new(starting, [set,  private]),
    bytes = ets:new(clients, [set,  private])
  }.

table(#clients{active = Table}, active) -> Table;
table(#clients{passive = Table}, passive) -> Table;
table(#clients{starting = Table}, starting) -> Table.

insert_client(Clients, State, _Client, Socket, DTS, StreamId) when ((State =/= passive) and (Socket =/= undefined)) ->
  ets:insert(table(Clients, State), {Socket, DTS, StreamId}),
  ok;
insert_client(Clients, State, Client, _Socket, _DTS, StreamId) ->
  ets:insert(table(Clients, State), {Client, StreamId}),
  ok.

insert_client(Clients, State, Client, StreamId) ->
  ets:insert(table(Clients, State), {Client, StreamId}),
  ok.
  
remove_client(#clients{active = A, passive = P, starting = S}, Client) ->
  ets:delete(A, Client),
  ets:delete(P, Client),
  ets:delete(S, Client),
  ok.
  
insert(#clients{list = List, bytes = Bytes} = Clients, #client{state = State, consumer = Client, stream_id = StreamId, tcp_socket = Socket, dts = DTS} = Entry) ->
  ets:insert(Bytes, {Client,0}),
  insert_client(Clients, State, Client, Socket, DTS, StreamId),
  Clients#clients{list = lists:keystore(Client, #client.consumer, List, Entry)}.


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
  remove_client(Clients, Client),
  case lists:keytake(Client, #client.consumer, List) of
    {value, #client{state = State, stream_id = StreamId} = Entry, List1} ->
      insert_client(Clients, State, Client, StreamId),
      Entry1 = setelement(Pos, Entry, Value),
      Clients#clients{list = [Entry1|List1]};
    false ->
      Clients
  end.

update(#clients{list = List, bytes = Bytes} = Clients, Client, #client{bytes = B, state = State, stream_id = StreamId} = NewEntry) ->
  remove_client(Clients, Client),
  insert_client(Clients, State, Client, StreamId),
  ets:insert(Bytes, {Client, B}),
  List1 = lists:keystore(Client, #client.consumer, List, NewEntry),
  Clients#clients{list = List1}.

delete(#clients{list = List, bytes = Bytes} = Clients, Client) ->
  ets:delete(Bytes, Client),
  remove_client(Clients, Client),
  Clients#clients{list = lists:keydelete(Client, #client.consumer, List)}.
  

send_frame(#video_frame{content = Content} = Frame, #clients{bytes = _Bytes} = Clients, State) ->
  Size = case Content of
    audio -> erlang:iolist_size(Frame#video_frame.body);
    video -> erlang:iolist_size(Frame#video_frame.body);
    _ -> 0
  end,
  FrameGen = flv:rtmp_tag_generator(Frame),
  F = fun({{rtmp, Sock}, DTS, StreamId}, Frame) ->
    gen_tcp:send(Sock, FrameGen(DTS, StreamId))
  end,
  %F = fun({Pid, StreamId}, F) ->
    %Pid ! F#video_frame{stream_id = StreamId}
    % ets:update_counter(Bytes, Pid, Size)
  %end,
  
  ets:foldl(F, Frame, table(Clients, State)),
  % [begin
  %   Pid ! Frame#video_frame{stream_id = StreamId},
  %   ets:update_counter(Bytes, Pid, Size)
  % end || #client{consumer = Pid, stream_id = StreamId, state = S} <- List, S == State],
  Clients.


mass_update_state(#clients{list = List} = Clients, From, To) ->
  Clients#clients{list = [begin
    S = case State of
      From ->
        remove_client(Clients, Pid),
        insert_client(Clients, To, Pid, StreamId),
        To;
      S1 -> S1
    end,
    Entry#client{state = S}
  end || #client{consumer = Pid, stream_id = StreamId, state = State} = Entry <- List]}.
  
increment_bytes(#clients{bytes = Bytes} = Clients, Client, Size) ->
  ets:update_counter(Bytes, Client, Size),
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



