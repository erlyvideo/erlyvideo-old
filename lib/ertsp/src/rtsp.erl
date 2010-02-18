-module(rtsp).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([start_server/3]).

-export([parse/2, decode/1]).

start_server(Port, Name, Callback) ->
  rtsp_sup:start_rtsp_listener(Port, Name, Callback).
  

parse(ready, <<$$, ChannelId, Length:16, RTP:Length/binary, Rest/binary>>) ->
  {ok, {rtp, ChannelId, RTP}, Rest};

parse(ready, <<$$, _>> = Data) ->
  {more, Data};

parse(ready, <<"RTSP/1.0 ", Response/binary>> = Data) ->
  case erlang:decode_packet(line, Response, []) of
    {ok, Line, Rest} ->
      {ok, Re} = re:compile("(\\d+) ([^\\r]+)"),
      {match, [_, Code, Message]} = re:run(Line, Re, [{capture, all, binary}]),
      {ok, {response, list_to_integer(binary_to_list(Code)), Message}, Rest};
    _ ->
      {more, ready, Data}
  end;

parse(ready, Data) ->
  case erlang:decode_packet(line, Data, []) of
    {ok, Line, Rest} ->
      {ok, Re} = re:compile("([^ ]+) ([^ ]+) RTSP/1.0"),
      {match, [_, Method, URL]} = re:run(Line, Re, [{capture, all, binary}]),
      {ok, {request, binary_to_atom(Method, latin1), URL}, Rest};
    _ ->
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


decode(Data) ->
  case parse(ready, Data) of
    {more, ready, Data} ->
      {more, Data};
    {ok, {request, Method, URL}, Rest} ->
      case decode_headers(Rest, [], undefined) of
        more ->
          {more, Data};
        {ok, Headers, Body, Rest1} ->
          {ok, {request, Method, URL, Headers, Body}, Rest1}
      end;
    {ok, {rtp, _Channel, _Packet}, _Rest} = Reply ->
      Reply;
    {ok, {response, Code, Status}, Rest} ->
      case decode_headers(Rest, [], undefined) of
        more ->
          {more, Data};
        {ok, Headers, Body, Rest1} ->
          {ok, {response, Code, Status, Headers, Body}, Rest1}
      end
  end.

decode_headers(Data, Headers, BodyLength) ->
  case parse(header, Data) of
    
    {ok, {header, 'Content-Length', Length}, Rest} ->
      NewLength = list_to_integer(binary_to_list(Length)),
      decode_headers(Rest, [{'Content-Length', NewLength} | Headers], NewLength);
    {ok, {header, <<"Cseq">>, CSeq}, Rest} ->
      Seq = list_to_integer(binary_to_list(CSeq)),
      decode_headers(Rest, [{'Cseq', Seq} | Headers], BodyLength);
    {ok, {header, Key, Value}, Rest} -> 
      decode_headers(Rest, [{Key, Value}], BodyLength);
      
      
    {ok, header_end, Rest} when BodyLength == undefined ->
      {ok, Headers, undefined, Rest};
    {ok, header_end, Rest} ->
      case parse({body, BodyLength}, Rest) of
        {ok, {body, Body}, Rest1} ->
          {ok, Headers, Body, Rest1};
        {more, body, _} ->
          more
      end;
      
      
    {more, header, Data} ->
      more
  end.

-include_lib("eunit/include/eunit.hrl").

parse_rtp_test() ->
  ?assertEqual({ok, {rtp, 0, <<1,2,3,4,5,6>>}, <<7,8>>}, parse(ready, <<$$, 0, 6:16, 1,2,3,4,5,6,7,8>>)),
  ?assertEqual({more, ready, <<$$, 0, 6:16, 1,2,3>>}, parse(ready, <<$$, 0, 6:16, 1,2,3>>)).
  

parse_request_test() ->
  ?assertEqual({more, ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0">>}, 
                parse(ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0">>)),
  ?assertEqual({ok, {request, 'PLAY', <<"rtsp://erlyvideo.org/video">>}, <<"CSeq: 1\r\nSession: 5\r\n\r\naa">>}, 
                parse(ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0\r\nCSeq: 1\r\nSession: 5\r\n\r\naa">>)).

parse_response_test() ->
  ?assertEqual({more, ready, <<"RTSP/1.0 200 OK">>}, 
                parse(ready, <<"RTSP/1.0 200 OK">>)),
  ?assertEqual({ok, {response, 200, <<"OK">>}, <<"Session: 10\r\nCSeq: 4\r\n\r\n">>}, 
                parse(ready, <<"RTSP/1.0 200 OK\r\nSession: 10\r\nCSeq: 4\r\n\r\n">>)).

  
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
          
  
decode_request_test() ->
  RTP = <<$$, 1, 5:16, 1,2,3,4,5, "RTSP/1.0 200 OK\r\nCseq: 2\r\n\r\n">>,
  Request = <<"ANNOUNCE rtsp://erlyvideo.org RTSP/1.0\r\n",
              "CSeq: 1\r\n"
              "Content-Length: 12\r\n",
              "\r\n",
              "a=fmtp: 96\r\n",
              RTP/binary>>,
  ?assertEqual({more, <<"ANNOUNCE rtsp://erlyvideo.org RTSP/1.0\r\nCSeq: 1\r\n">>},
               decode(<<"ANNOUNCE rtsp://erlyvideo.org RTSP/1.0\r\nCSeq: 1\r\n">>)),
  ?assertEqual({ok, {request, 'ANNOUNCE', <<"rtsp://erlyvideo.org">>, [ {'Content-Length', 12},{'Cseq', 1}],
               <<"a=fmtp: 96\r\n">>}, RTP},
               decode(Request)),
  ?assertEqual({ok, {rtp, 1, <<1,2,3,4,5>>}, <<"RTSP/1.0 200 OK\r\nCseq: 2\r\n\r\n">>}, decode(RTP)),
  ?assertEqual({ok, {response, 200, <<"OK">>, [{'Cseq', 2}], undefined}, <<"">>}, 
               decode(<<"RTSP/1.0 200 OK\r\nCseq: 2\r\n\r\n">>)).
  
  
  