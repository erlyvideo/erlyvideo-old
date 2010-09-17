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
-include_lib("stdlib/include/ms_transform.hrl").
-include("../include/ems_media.hrl").
-include("../ems.hrl").
-include("ems_media_client.hrl").

-export([init/0, insert/2, find/2, find/3, count/1, update/3, update/4, delete/2, 
         select_by_state/2, send_frame/3, mass_update_state/3, increment_bytes/3]).

init() ->
  Clients = ets:new(clients, [set,  {keypos,#client.consumer}, private]),
  Index = ets:new(index, [duplicate_bag, {keypos, #client.state}, private]),
  {Clients, Index}.

insert({Clients, Index}, Entry) ->
  ets:insert(Clients, Entry),
  ets:insert(Index, Entry),
  {Clients, Index}.

find({Clients, _Index}, Client) ->
  case ets:lookup(Clients, Client) of
    [Entry] -> Entry;
    _ -> undefined
  end.
  
find({Clients, _Index}, Value, Pos) ->
  case ets:select(Clients, ets:fun2ms(fun(#client{} = Entry) when element(Pos, Entry) == Value -> Entry end)) of
    [Entry] -> Entry;
    _ -> undefined
  end.


count({Clients, _Index}) ->
  ets:info(Clients, size).
  
delete_from_index(Index, Client) ->
  MS = ets:fun2ms(fun(#client{consumer = Consumer}) when Consumer == Client -> true end),
  ets:select_delete(Index, MS).

update({Clients, Index}, Client, Pos, Value) ->
  case ets:lookup(Clients, Client) of
    [Entry] ->
      ets:update_element(Clients, Client, {Pos, Value}),
      % ets:delete_object(Index, Entry),
      delete_from_index(Index, Client),
      ets:insert(Index, setelement(Pos, Entry, Value));
    _ ->
      undefined
  end,
  {Clients, Index}.

update({Clients, Index}, Client, NewEntry) ->
  case ets:lookup(Clients, Client) of
    [_Entry] ->
      % ets:delete_object(Index, Entry),
      delete_from_index(Index, Client),
      ets:insert(Clients, NewEntry),
      ets:insert(Index, NewEntry);
    _ ->
      undefined
  end, 
  {Clients, Index}.

delete({Clients, Index}, Client) ->
  case ets:lookup(Clients, Client) of
    [Entry] ->
      ets:delete(Clients, Client),
      ets:delete_object(Index, Entry);
    _ ->
      undefined
  end, 
  {Clients, Index}.
  
select_by_state({_Clients,Index}, Value) ->
  ets:lookup(Index, Value).
  

send_frame(Frame, {C,_Index} = Clients, State) ->
  List = select_by_state(Clients, State),
  Bytes = erlang:iolist_size(Frame#video_frame.body),
  [begin
    Pid ! Frame#video_frame{stream_id = StreamId},
    ets:update_counter(C, Pid, {#client.bytes, Bytes})
  end || #client{consumer = Pid, stream_id = StreamId} <- List].


mass_update_state({Clients,Index}, From, To) ->
  List = ets:lookup(Index, From),
  ets:delete(Index, From),
  NewList = [setelement(#client.state, Entry, To) || Entry <- List],
  ets:insert(Index, NewList),
  ets:insert(Clients, NewList),
  {Clients, Index}.
  
increment_bytes({Clients, Index}, Client, Bytes) ->
  ets:update_counter(Clients, Client, {#client.bytes, Bytes}),
  {Clients, Index}.
