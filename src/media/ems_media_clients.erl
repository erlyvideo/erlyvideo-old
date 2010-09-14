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

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include("../include/ems_media.hrl").
-include("../include/ems.hrl").
-include("ems_media_client.hrl").

-export([init/0, insert/2, find/2, find/3, count/1, update/3, update/4, delete/2, select/3, send_frame/3, mass_update/4]).

-define(EMS_MEDIA_ETS,1).

-ifdef(EMS_MEDIA_ETS).

init() ->
  ets:new(clients, [set,  {keypos,#client.consumer}]).

insert(Clients, Entry) ->
  ets:insert(Clients, Entry),
  Clients.

find(Clients, Client) ->
  case ets:lookup(Clients, Client) of
    [Entry] -> Entry;
    _ -> undefined
  end.
  
find(Clients, Value, Pos) ->
  case ets:select(Clients, ets:fun2ms(fun(#client{} = Entry) when element(Pos, Entry) == Value -> Entry end)) of
    [Entry] -> Entry;
    _ -> undefined
  end.


count(Clients) ->
  ets:info(Clients, size).

update(Clients, Client, Pos, Value) ->
  ets:update_element(Clients, Client, {Pos, Value}),
  Clients.

update(Clients, _Client, Entry) ->
  ets:insert(Clients, Entry),
  Clients.

delete(Clients, Client) ->
  ets:delete(Clients, Client),
  Clients.
  
select(Clients, Pos, Value) ->
  MS = ets:fun2ms(fun(#client{} = Entry) when element(Pos, Entry) == Value -> Entry end),
  ets:select(Clients, MS).
  

send_frame(Frame, Clients, State) ->
  MS = ets:fun2ms(fun(#client{state = S, consumer = Client, stream_id = StreamId}) when S == State -> {Client, StreamId} end),
  List = ets:select(Clients, MS),
  [Pid ! Frame#video_frame{stream_id = StreamId} || {Pid,StreamId} <- List].


mass_update(Clients, Pos, From, To) ->
  MS = ets:fun2ms(fun(#client{consumer = Client} = Entry) when element(Pos, Entry) == From -> Client end),
  Starting = ets:select(Clients, MS),
  [ets:update_element(Clients, Client, {Pos, To}) || Client <- Starting],
  Clients.
  
-else.

init() ->
  [].

insert(Clients, #client{consumer = Pid} = Entry) ->
  lists:keystore(Pid, #client.consumer, Clients, Entry).

find(Clients, Pid) ->
  case lists:keyfind(Clients, #client.consumer, Pid) of
    false -> undefined;
    Entry -> Entry
  end.
  
find(Clients, Value, Pos) ->
  case lists:keyfind(Clients, Pos, Value) of
    false -> undefined;
    Entry -> Entry
  end.


count(Clients) ->
  length(Clients).

update(Clients, Client, Pos, Value) ->
  case find(Clients, Client) of
    undefined -> Clients;
    Entry -> insert(Clients, element(Pos, Entry, Value))
  end.

update(Clients, _Client, Entry) ->
  insert(Clients, Entry).

delete(Clients, Client) ->
  lists:keydelete(Client, #client.consumer, Clients).
  
select(Clients, Pos, Value) ->
  [Entry || Entry <- Clients, element(Pos, Entry) == Value].
  

send_frame(Frame, Clients, State) ->
  [Pid ! Frame#video_frame{stream_id = StreamId} || #client{consumer = Pid, stream_id = StreamId, state = S} <- List, S == State].


mass_update(Clients, Pos, From, To) ->
  lists:map(fun
    (#client{} = Entry) when element(Pos, Entry) == From -> setelement(Pos, Entry, To);
    (Entry) -> Entry
  end, Clients).

-endif.