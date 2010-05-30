%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTSP decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtsp).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(application).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([start_server/3, behaviour_info/1]).

-export([parse/2, decode/1]).

-export([edoc/1, edoc/0]).


-export([start/2, stop/1, config_change/3, test_all/0]).

%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTSP library
%% @end 
%%--------------------------------------------------------------------

start(_Type, _Args) -> 
  rtsp_sup:start_link().
  


%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTSP library
%% @end 
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload RTSP config
%% @end 
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.


test_all() ->
  rtsp:test(),
  rtsp_socket:test(),
  sdp:test().



%%-------------------------------------------------------------------------
%% @spec (Callbacks::atom()) -> CallBackList::list()
%% @doc  List of require functions in a video file reader
%% @hidden
%% @end
%%-------------------------------------------------------------------------
behaviour_info(callbacks) -> [{record,2}, {announce,3}];
behaviour_info(_Other) -> undefined.


edoc() ->
  edoc([{dir,"doc/html"}]).
  
edoc(Options) ->
  edoc:application(?MODULE,".",[{packages,false} | Options]).

start_server(Port, Name, Callback) ->
  rtsp_sup:start_rtsp_listener(Port, Name, Callback).
  

parse(ready, <<$$, ChannelId, Length:16, RTP:Length/binary, Rest/binary>>) ->
  {ok, {rtp, ChannelId, RTP}, Rest};

parse(ready, <<$$, _/binary>> = Data) ->
  {more, ready, Data};

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


%%----------------------------------------------------------------------
%% @spec (Data::binary()) -> {more, Data::binary()} |
%%                           {ok, {request, Method::atom, URL::binary(), Headers::list(), Body::binary()}, Rest::binary()} |
%%                           {ok, {rtp, Channel::integer(), Packet::binary()}, Rest::binary()} |
%%                           {ok, {response, Code::integer(), Status::binary(), Headers::list(), Body::binary}, Rest::binary()}
%%
%% @doc Called by {@link rtsp_socket. to decode incoming TCP data}
%%  Can parse RTSP or interleaved RTP
%% @end
%%----------------------------------------------------------------------
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
      decode_headers(Rest, [{'Cseq', CSeq} | Headers], BodyLength);
    {ok, {header, <<"Transport">>, Value}, Rest} ->
      decode_headers(Rest, [{'Transport', Value} | Headers], BodyLength);
    {ok, {header, <<"Session">>, Value}, Rest} ->
      decode_headers(Rest, [{'Session', Value} | Headers], BodyLength);
    {ok, {header, Key, Value}, Rest} -> 
      decode_headers(Rest, [{Key, Value} | Headers], BodyLength);
      
      
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

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

parse_rtp_test() ->
  ?assertEqual({ok, {rtp, 0, <<1,2,3,4,5,6>>}, <<7,8>>}, parse(ready, <<$$, 0, 6:16, 1,2,3,4,5,6,7,8>>)),
  Chunk = <<$$,0,26:16,1,2,3,4,5,6,7,8,9,10,11,12>>,
  ?assertEqual({more, ready, Chunk}, parse(ready, Chunk)).
  

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
  ?assertEqual({ok, {request, 'ANNOUNCE', <<"rtsp://erlyvideo.org">>, [ {'Content-Length', 12},{'Cseq', <<"1">>}],
               <<"a=fmtp: 96\r\n">>}, RTP},
               decode(Request)),
  ?assertEqual({ok, {rtp, 1, <<1,2,3,4,5>>}, <<"RTSP/1.0 200 OK\r\nCseq: 2\r\n\r\n">>}, decode(RTP)),
  ?assertEqual({ok, {response, 200, <<"OK">>, [{'Cseq', <<"2">>}], undefined}, <<"">>}, 
               decode(<<"RTSP/1.0 200 OK\r\nCseq: 2\r\n\r\n">>)),
  ?assertEqual({ok, {response, 200, <<"OK">>, [
                    {'Date', <<"Thu, 18 Feb 2010 06:21:00 GMT">>},
                    {'Transport', <<"RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=FA173C13;mode=\"PLAY\"">>}, 
                    {'Session', <<"94544680; timeout=60">>}, 
                    {'Cseq', <<"2">>}
                ], undefined}, <<>>},
               decode(<<"RTSP/1.0 200 OK\r\nCSeq: 2\r\nSession: 94544680; timeout=60\r\nTransport: RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=FA173C13;mode=\"PLAY\"\r\nDate: Thu, 18 Feb 2010 06:21:00 GMT\r\n\r\n">>)).
  
  
  