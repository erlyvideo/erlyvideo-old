%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        HTTP API for deskshare plugin
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2011 Max Lapshin
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_http_deskshare).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include("../log.hrl").


-include_lib("eunit/include/eunit.hrl").

-define(CAPTURE_START, 0).
-define(CAPTURE_UPDATE, 1).
-define(CAPTURE_END, 2).
-define(MOUSE_LOCATION_EVENT, 3).

-export([http/4]).

b2i(Bin) when is_binary(Bin) -> list_to_integer(binary_to_list(Bin)).

http(_Host, _HTTPMethod, ["deskshare", "tunnel", "screenCapture"], Req) ->
  Info = Req:parse_post(),
  case proplists:get_value("event", Info) of
    undefined ->
      ?D(Req:get(headers)),
      ?D(Req:get(body)),
      Req:stream(ok);
    EventNumber ->
      RoomName = proplists:get_value("room", Info),
      Room = conference:find(RoomName),
      Reply = handle_event(b2i(EventNumber), Room, Info),
      Req:ok([{'Content-type', "application/json"}], [mochijson2:encode(Reply), "\n"])
  end;
  
http(_Host, _Method, _Path, _Req) ->
  unhandled.


seq(Info) ->
  b2i(proplists:get_value("sequenceNumber", Info)).

handle_event(?CAPTURE_START, Room, Info) ->
  ?D({start, Info}),
  {match, [BW, BH]} = re:run(proplists:get_value("blockInfo", Info), "(\\d+)x(\\d+)", [{capture,all_but_first,list}]),
  {match, [W, H]} = re:run(proplists:get_value("screenInfo", Info), "(\\d+)x(\\d+)", [{capture,all_but_first,list}]),
  deskshare:start_capture(Room, [
    {block, {list_to_integer(BW),list_to_integer(BH)}}, 
    {screen, {list_to_integer(W),list_to_integer(H)}},
    {seq, seq(Info)}
  ]),
  true;
  
handle_event(?CAPTURE_UPDATE, Room, Info) ->
  _Keyframe = case proplists:get_value("keyframe", Info) of
    <<"true">> -> true;
    _ -> false
  end,
  Position = b2i(proplists:get_value("position", Info)),
  Data = proplists:get_value("blockdata", Info),
  (catch deskshare:update_capture(Room, Position, Data)),
  true;
  
handle_event(?CAPTURE_END, Room, _Info) ->
  (catch deskshare:stop(Room)),
  true;
  
handle_event(?MOUSE_LOCATION_EVENT, _Room, Info) ->
  X = b2i(proplists:get_value("mousex", Info)),
  Y = b2i(proplists:get_value("mousey", Info)),
  ?D({mouse, X, Y}),
  true.
  