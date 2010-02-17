-module(rtsp).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start_server/3]).

-export([parse/2]).

start_server(Port, Name, Callback) ->
  rtsp_sup:start_rtsp_listener(Port, Name, Callback).
  

parse(ready, <<$$, ChannelId, Length:16, RTP:Length/binary, Rest/binary>>) ->
  {ok, {rtp, ChannelId, RTP}, Rest};

parse(ready, <<$$, _>> = Data) ->
  {more, Data};

parse(ready, <<"RTSP/1.0 ", Response/binary>> = Data) ->
  {ok, Re} = re:compile("(\\d+) ([^\\r]+)\\r\\n(.*)"),
  case re:run(Response, Re, [{capture, all, binary}]) of
    {match, [_, Code, Message, Rest]} ->
      {ok, {response, list_to_integer(binary_to_list(Code)), Message}, Rest};
    nomatch ->
      {more, ready, Data}
  end;

parse(ready, Data) ->
  {ok, Re} = re:compile("([^ ]+) ([^ ]+) RTSP/1.0\\r\\n(.*)"),
  case re:run(Data, Re, [{capture, all, binary}]) of
    {match, [_, Method, URL, Rest]} ->
      {ok, {request, binary_to_atom(Method, latin1), URL}, Rest};
    nomatch ->
      {more, ready, Data}
  end;
  
parse({body, Length}, Data) when size(Data) >= Length ->
  {Body, Rest} = split_binary(Data, Length),
  {ok, {body, Body}, Rest};

parse({body, _Length}, Data) ->
  {more, body, Data};

parse(header, Data) ->
  case erlang:decode_packet(httph_bin, Data, []) of
    {ok, {http_header, _, Key, _, Value}, Rest} ->
      {ok, {header, Key, Value}, Rest};
    {ok, http_eoh, Rest} ->
      {ok, header_end, Rest};
    {more, undefined} ->
      {more, header, Data}
  end;

parse(State, Data) ->
  {error, State, Data}.

-include_lib("eunit/include/eunit.hrl").

parse_rtp_test() ->
  ?assertEqual({ok, {rtp, 0, <<1,2,3,4,5,6>>}, <<7,8>>}, parse(ready, <<$$, 0, 6:16, 1,2,3,4,5,6,7,8>>)),
  ?assertEqual({more, ready, <<$$, 0, 6:16, 1,2,3>>}, parse(ready, <<$$, 0, 6:16, 1,2,3>>)).
  

parse_request_test() ->
  ?assertEqual({more, ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0">>}, 
                parse(ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0">>)),
  ?assertEqual({ok, {request, 'PLAY', <<"rtsp://erlyvideo.org/video">>}, <<"CSeq: 1">>}, 
                parse(ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0\r\nCSeq: 1">>)).

parse_response_test() ->
  ?assertEqual({more, ready, <<"RTSP/1.0 200 OK">>}, 
                parse(ready, <<"RTSP/1.0 200 OK">>)),
  ?assertEqual({ok, {response, 200, <<"OK">>}, <<"Session: 10">>}, 
                parse(ready, <<"RTSP/1.0 200 OK\r\nSession: 10">>)).

  
parse_body_test() ->
  ?assertEqual({more, body, <<1,2,3,4,5>>}, parse({body, 6}, <<1,2,3,4,5>>)),
  ?assertEqual({ok, {body, <<1,2,3,4,5,6>>}, <<7,8>>}, parse({body, 6}, <<1,2,3,4,5,6,7,8>>)).
  
parse_header_test() ->
  ?assertEqual({more, header, <<"Content-Length: 10\r\n">>}, 
          parse(header, <<"Content-Length: 10\r\n">>)),
  ?assertEqual({ok, {header, 'Content-Length', <<"10">>}, <<"\r\nzzz">>}, 
          parse(header, <<"Content-Length: 10\r\n\r\nzzz">>)),
  ?assertEqual({ok, header_end, <<"zzz">>}, 
          parse(header, <<"\r\nzzz">>)).
          
  
  