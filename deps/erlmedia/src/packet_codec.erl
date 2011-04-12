%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTSP and SIP coder/decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/erlmedia" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlmedia.
%%%
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(packet_codec).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([parse/2, decode/1, encode/1]).
-export([encode_headers/1]).

parse(ready, <<$$, ChannelId, Length:16, RTP:Length/binary, Rest/binary>>) ->
  {ok, {rtp, ChannelId, RTP}, Rest};

parse(ready, <<$$, _/binary>> = Data) ->
  {more, ready, Data};

parse(ready, <<"RTSP/1.0 ", Response/binary>> = Data) ->
  case erlang:decode_packet(line, Response, []) of
    {ok, Line, Rest} ->
      {ok, Re} = re:compile("(\\d+) ([^\\r]+)"),
      {match, [_, Code, Message]} = re:run(Line, Re, [{capture, all, binary}]),
      {ok, {rtsp_response, erlang:list_to_integer(binary_to_list(Code)), Message}, Rest};
    _ ->
      {more, ready, Data}
  end;

parse(ready, <<"SIP/2.0 ", Response/binary>> = Data) ->
  case erlang:decode_packet(line, Response, []) of
    {ok, Line, Rest} ->
      {ok, Re} = re:compile("(\\d+) ([^\\r]+)"),
      {match, [_, Code, Message]} = re:run(Line, Re, [{capture, all, binary}]),
      {ok, {sip_response, erlang:list_to_integer(binary_to_list(Code)), Message}, Rest};
    _ ->
      {more, ready, Data}
  end;

parse(ready, Data) ->
  case erlang:decode_packet(line, Data, []) of
    {ok, Line, Rest} ->
      {ok, Re} = re:compile("([^ ]+)\s+([^ ]+)\s+(RTSP/1\\.0|SIP/2\\.0)"),
      case re:run(Line, Re, [{capture, [1,2,3], binary}]) of
        {match, [Method, URI, <<"RTSP/1.0">>]} ->
          {ok, {rtsp_request, binary_to_atom(Method, latin1), URI}, Rest};
        {match, [Method, URI, <<"SIP/2.0">>]} ->
          {ok, {sip_request, binary_to_atom(Method, latin1), URI}, Rest}
      end;
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
%% @doc Decode incoming TCP data}
%%  Can parse RTSP, SIP or interleaved RTP
%% @end
%%----------------------------------------------------------------------
decode(Data) ->
  case parse(ready, Data) of
    {more, ready, Data} ->
      {more, Data};
    {ok, {rtsp_request, Method, URL}, Rest} ->
      case decode_headers(Rest, [], undefined) of
        more ->
          {more, Data};
        {ok, Headers, Body, Rest1} ->
          {ok, {request, Method, URL, lists:ukeysort(1,Headers), Body}, Rest1}
      end;
    {ok, {rtp, _Channel, _Packet}, _Rest} = Reply ->
      Reply;
    {ok, {rtsp_response, Code, Status}, Rest} ->
      case decode_headers(Rest, [], undefined) of
        more ->
          {more, Data};
        {ok, Headers, Body, Rest1} ->
          {ok, {response, Code, Status, lists:ukeysort(1,Headers), Body}, Rest1}
      end;
    {ok, {sip_request, Method, URI}, Rest} ->
      case decode_headers(Rest, [], undefined) of
        {ok, Headers, Body, _Other} ->
          {ok, {sip_request, Method, URI, lists:ukeysort(1,Headers), Body}};
        more ->
          {more, Data}
      end;
    {ok, {sip_response, Code, Status}, Rest} ->
      case decode_headers(Rest, [], undefined) of
        {ok, Headers, Body, _Other} ->
          {ok, {sip_response, Code, Status, lists:ukeysort(1,Headers), Body}};
        more ->
          {more, Data}
      end
  end.

decode_headers(Data, Headers, BodyLength) ->
  case parse(header, Data) of

    {ok, {header, 'Content-Length', Length}, Rest} ->
      NewLength = list_to_integer(binary_to_list(Length)),
      decode_headers(Rest, [{'Content-Length', NewLength} | Headers], NewLength);
    {ok, {header, HKey, HVal}, Rest} ->
      NewPair =
        case HKey of
          <<"Cseq">> -> {'Cseq', HVal};
          <<"Transport">> -> {'Transport', parse_transport_header(HVal)};
          <<"Rtp-Info">> -> {'Rtp-Info', parse_rtp_info_header(HVal)};
          <<"RTP-Info">> -> {'Rtp-Info', parse_rtp_info_header(HVal)};
          <<"Session">> -> {'Session', HVal};
          <<"Call-Id">> -> {'Call-Id', HVal};
          <<"To">> -> {'To', HVal};
          <<"Subject">> -> {'Subject', HVal};
          <<"Contact">> -> {'Contact', HVal};
          <<"Route">> -> {'Route', HVal};
          <<"Event">> -> {'Event', HVal};
          _ -> {HKey, HVal}
        end,
      decode_headers(Rest, [NewPair | Headers], BodyLength);

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

% <<"RTP/AVP/TCP;unicast;mode=receive;interleaved=2-3">>
parse_transport_header(Header) ->
  Fields = lists:foldl(fun
    ("interleaved="++Interleaved, Opts) -> 
      [Chan0, Chan1] = string:tokens(Interleaved, "-"),
      [{interleaved, {list_to_integer(Chan0), list_to_integer(Chan1)}}|Opts];
    ("RTP/AVP/TCP", Opts) -> [{proto, tcp}|Opts];
    ("RTP/AVP/UDP", Opts) -> [{proto, udp}|Opts];
    ("RTP/AVP", Opts) -> [{proto, udp}|Opts];
    ("mode=record", Opts) -> [{mode, 'receive'}|Opts];
    ("mode=receive", Opts) -> [{mode, 'receive'}|Opts];
    ("mode=\"PLAY\"", Opts) -> [{mode, play}|Opts];
    ("unicast", Opts) -> [{unicast, true}|Opts];
    ("source="++Source, Opts) -> [{source, Source}|Opts];
    ("client_port="++Ports, Opts) ->
      [Port0,Port1] = string:tokens(Ports, "-"),
      [{client_port, {list_to_integer(Port0),list_to_integer(Port1)}}|Opts];
    ("server_port="++Ports, Opts) ->
      [Port0,Port1] = string:tokens(Ports, "-"),
      [{server_port, {list_to_integer(Port0),list_to_integer(Port1)}}|Opts];
    ("ssrc="++SSRC, Opts) -> [{ssrc, erlang:list_to_integer(SSRC, 16)}|Opts];
    (Else, Opts) -> Parts = string:tokens(Else, "="), [{hd(Parts),string:join(tl(Parts),"=")}|Opts]
  end, [], string:tokens(binary_to_list(Header), ";")),
  lists:reverse(Fields).


parse_rtp_info_header(String) when is_binary(String) ->
  parse_rtp_info_header(binary_to_list(String));

parse_rtp_info_header(String) when is_list(String) ->
  {ok, Re} = re:compile(" *([^=]+)=(.*)"),
  F = fun(S) ->
    {match, [_, K, V]} = re:run(S, Re, [{capture, all, list}]),
    Key = list_to_existing_atom(K),
    Value = case Key of
      seq -> list_to_integer(V);
      rtptime -> list_to_integer(V);
      _ -> V
    end,
    {Key, Value}
  end,
  [[F(S1) || S1 <- string:tokens(S, ";")] || S <- string:tokens(String, ",")].
  
  

%%----------------------------------------------------------------------
%% @spec ({rtcp, Channel::integer(), Bin::binary()}) -> Data::binary()
%%
%% @doc Called by {@link rtsp_socket. to encode outcoming RTSP/RTP/RTCP data}
%% @end
%%----------------------------------------------------------------------
encode({Type, Channel, Bin}) when Type =:= rtp;
                                  Type =:= rtcp ->
  <<$$, Channel, (size(Bin)):16, Bin/binary>>.


encode_transport_header(TransportHeader) ->
  encode_transport_header(TransportHeader, []).

encode_transport_header([], Acc) -> string:join(lists:reverse(Acc),";");
encode_transport_header([{proto,tcp}|H], Acc) -> encode_transport_header(H, ["RTP/AVP/TCP"|Acc]);
encode_transport_header([{proto,udp}|H], Acc) -> encode_transport_header(H, ["RTP/AVP"|Acc]);
encode_transport_header([{mode,'receive'}|H], Acc) -> encode_transport_header(H, ["mode=receive"|Acc]);
encode_transport_header([{mode,'play'}|H], Acc) -> encode_transport_header(H, ["mode=play"|Acc]);
encode_transport_header([{unicast,true}|H], Acc) -> encode_transport_header(H, ["unicast"|Acc]);
encode_transport_header([{ssrc,SSRC}|H], Acc) -> encode_transport_header(H, ["ssrc="++erlang:integer_to_list(SSRC, 16)|Acc]);
encode_transport_header([{interleaved,{Chan0,Chan1}}|H], Acc) -> encode_transport_header(H, ["interleaved="++integer_to_list(Chan0)++"-"++integer_to_list(Chan1)|Acc]);
encode_transport_header([{client_port,{P0,P1}}|H], Acc) -> encode_transport_header(H, ["client_port="++integer_to_list(P0)++"-"++integer_to_list(P1)|Acc]);
encode_transport_header([{server_port,{P0,P1}}|H], Acc) -> encode_transport_header(H, ["server_port="++integer_to_list(P0)++"-"++integer_to_list(P1)|Acc]);
encode_transport_header([{Key,Value}|H], Acc) -> encode_transport_header(H, [io_lib:format("~s=~s", [Key,Value])|Acc]).

binarize_header({'Transport', TransportHeader}) ->
  [<<"Transport: ">>, encode_transport_header(TransportHeader), <<"\r\n">>];

binarize_header({Key, Value}) when is_atom(Key) ->
  binarize_header({atom_to_binary(Key, latin1), Value});

binarize_header({Key, Value}) when is_list(Key) ->
  binarize_header({list_to_binary(Key), Value});

binarize_header({Key, Value}) when is_integer(Value) ->
  binarize_header({Key, integer_to_list(Value)});

binarize_header({Key, Value}) ->
  [Key, <<": ">>, Value, <<"\r\n">>];

binarize_header([Key, Value]) ->
  [Key, <<" ">>, Value, <<"\r\n">>].



encode_headers(Headers) ->
  iolist_to_binary([binarize_header({K,V}) || {K,V} <- Headers]).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").



parse_tcp_transport_header_test() ->
  ?assertEqual([{proto,tcp},{unicast,true},{mode,'receive'},{interleaved,{2,3}}],
                 parse_transport_header(<<"RTP/AVP/TCP;unicast;mode=receive;interleaved=2-3">>)).

parse_udp_transport_header_test() ->
  ?assertEqual([{proto,udp},{unicast,true},{client_port,{42276,42277}},{server_port,{2052,2053}}], 
                parse_transport_header(<<"RTP/AVP;unicast;client_port=42276-42277;server_port=2052-2053">>)).

parse_rtp_info_test() ->
  ?assertEqual([[{url,"rtsp://erlyvideo.org/h264/trackID=1"},{seq,60183},{rtptime,4274184387}], [{url,"rtsp://erlyvideo.org/h264/trackID=2"},{seq,51194},{rtptime,1003801948}]], 
    parse_rtp_info_header(<<"url=rtsp://erlyvideo.org/h264/trackID=1;seq=60183;rtptime=4274184387, url=rtsp://erlyvideo.org/h264/trackID=2;seq=51194;rtptime=1003801948">>)).

parse_rtp_test() ->
  ?assertEqual({ok, {rtp, 0, <<1,2,3,4,5,6>>}, <<7,8>>}, parse(ready, <<$$, 0, 6:16, 1,2,3,4,5,6,7,8>>)),
  Chunk = <<$$,0,26:16,1,2,3,4,5,6,7,8,9,10,11,12>>,
  ?assertEqual({more, ready, Chunk}, parse(ready, Chunk)).


parse_request_test() ->
  ?assertEqual({more, ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0">>},
               parse(ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0">>)),
  ?assertEqual({ok, {rtsp_request, 'PLAY', <<"rtsp://erlyvideo.org/video">>}, <<"CSeq: 1\r\nSession: 5\r\n\r\naa">>},
               parse(ready, <<"PLAY rtsp://erlyvideo.org/video RTSP/1.0\r\nCSeq: 1\r\nSession: 5\r\n\r\naa">>)).

parse_response_test() ->
  ?assertEqual({more, ready, <<"RTSP/1.0 200 OK">>},
               parse(ready, <<"RTSP/1.0 200 OK">>)),
  ?assertEqual({ok, {rtsp_response, 200, <<"OK">>}, <<"Session: 10\r\nCSeq: 4\r\n\r\n">>},
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

encode_rtcp_test() ->
  ?assertEqual(<<$$, 1, 5:16, 1,2,3,4,5>>, encode({rtcp, 1, <<1,2,3,4,5>>})).


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
                                               {'Cseq', <<"2">>},
                                               {'Date', <<"Thu, 18 Feb 2010 06:21:00 GMT">>},
                                               {'Session', <<"94544680; timeout=60">>},
                                               {'Transport', [{proto,tcp},{unicast,true},{interleaved,{0,1}},{ssrc, 4195826707},{mode,play}]}
                                              ], undefined}, <<>>},
               decode(<<"RTSP/1.0 200 OK\r\nCSeq: 2\r\nSession: 94544680; timeout=60\r\nTransport: RTP/AVP/TCP;unicast;interleaved=0-1;ssrc=FA173C13;mode=\"PLAY\"\r\nDate: Thu, 18 Feb 2010 06:21:00 GMT\r\n\r\n">>)).



decode_sip_invite_test() ->
  SDP = <<"v=0\r\n",
          "o=StreamingServer 3485077701 1211414520000 IN IP4 129.85.244.160\r\n",
          "s=/evolution/coyne.mov\r\n",
          "u=http:///\r\n",
          "e=admin@\r\n",
          "c=IN IP4 0.0.0.0\r\n",
          "b=AS:876\r\n",
          "t=0 0\r\n",
          "a=control:*\r\n",
          "a=range:npt=0-3973.80667\r\n",
          "m=video 0 RTP/AVP 96\r\n",
          "b=AS:734\r\n",
          "a=rtpmap:96 H264/90000\r\n",
          "a=control:trackID=1\r\n",
          "a=cliprect:0,0,360,480\r\n",
          "a=range:npt=0-3973.8071\r\n",
          "a=fmtp:96 packetization-mode=1;profile-level-id=42E016;sprop-parameter-sets=Z0LAFpZ0DwX/LgCBAAALuwACvyC0YAw4BJd73weEQjU=,aN48gA==\r\n",
          "m=audio 0 RTP/AVP 97\r\n",
          "b=AS:142\r\n",
          "a=rtpmap:97 MP4A-LATM/48000/2\r\n",
          "a=control:trackID=2\r\n",
          "a=range:npt=0-3973.8240\r\n",
          "a=fmtp:97 profile-level-id=15;object=2;cpresent=0;config=400023203FC0\r\n">>,

  Req = <<"INVITE sip:ev@erlyvideo.ru SIP/2.0\r\n",
          "To: sip:ev@erlyvideo.ru\r\n",
          "From: sip:tt@hh\r\n",
          "CSeq: 1234 INVITE\r\n",
          "Date: Tue, 3 Aug 2010 16:00:00 NOVST\r\n"
          "Content-Type: application/sdp\r\n",
          "Content-Length: ",(list_to_binary(integer_to_list(size(SDP))))/binary,"\r\n",
          "\r\n",
          SDP/binary>>,

  _MediaDesc = [{media_desc,video,
    {inet4,"0.0.0.0"},
    "0","96",90.0,"trackID=1",h264,
    <<104,222,60,128>>,
    <<103,66,192,22,150,116,15,5,255,46,0,129,0,0,11,187,0,2,
      191,32,180,96,12,56,4,151,123,223,7,132,66,53>>,
    undefined},
   {media_desc,audio,
    {inet4,"0.0.0.0"},
    "0","97",48.0,"trackID=2","MP4A-LATM",undefined,
    undefined,
    <<64,0,35,32,63,192>>}],
  ?assertEqual({ok,{sip_request,'INVITE',<<"sip:ev@erlyvideo.ru">>,
                    [{'Content-Length',628},
                     {'Content-Type',<<"application/sdp">>},
                     {'Cseq',<<"1234 INVITE">>},
                     {'Date',<<"Tue, 3 Aug 2010 16:00:00 NOVST">>},
                     {'From',<<"sip:tt@hh">>},
                     {'To',<<"sip:ev@erlyvideo.ru">>}],
                    % MediaDesc}},
                    SDP}},
               decode(Req)).

decode_sip_ack_test() ->
  Req = <<"ACK sip:tt@hh SIP/2.0\r\n",
          "To: sip:tt@hh\r\n",
          "From: sip:ev@erlyvideo.ru\r\n",
          "CSeq: 1234 ACK\r\n",
          "Date: Tue, 3 Aug 2010 16:00:00 NOVST\r\n"
          "Content-Length: 0\r\n",
          "\r\n">>,
  ?assertEqual({ok,{sip_request,'ACK',<<"sip:tt@hh">>,
                    [{'Content-Length',0},
                     {'Cseq',<<"1234 ACK">>},
                     {'Date',<<"Tue, 3 Aug 2010 16:00:00 NOVST">>},
                     {'From',<<"sip:ev@erlyvideo.ru">>},
                     {'To',<<"sip:tt@hh">>}],
                    <<>>}},
               decode(Req)).

decode_sip_bye_test() ->
  Req = <<"BYE sip:tt@hh SIP/2.0\r\n",
          "To: sip:tt@hh\r\n",
          "From: sip:ev@erlyvideo.ru\r\n",
          "CSeq: 1234 BYE\r\n",
          "Date: Tue, 3 Aug 2010 16:00:00 NOVST\r\n"
          "Content-Length: 0\r\n",
          "\r\n">>,
  ?assertEqual({ok,{sip_request,'BYE',<<"sip:tt@hh">>,
                    [{'Content-Length',0},
                     {'Cseq',<<"1234 BYE">>},
                     {'Date',<<"Tue, 3 Aug 2010 16:00:00 NOVST">>},
                     {'From',<<"sip:ev@erlyvideo.ru">>},
                     {'To',<<"sip:tt@hh">>}],
                    <<>>}},
               decode(Req)).

decode_sip_cancel_test() ->
  Req = <<"CANCEL sip:tt@hh SIP/2.0\r\n",
          "To: sip:tt@hh\r\n",
          "From: sip:ev@erlyvideo.ru\r\n",
          "CSeq: 1234 CANCEL\r\n",
          "Date: Tue, 3 Aug 2010 16:00:00 NOVST\r\n"
          "Content-Length: 0\r\n",
          "\r\n">>,
  ?assertEqual({ok,{sip_request,'CANCEL',<<"sip:tt@hh">>,
                    [{'Content-Length',0},
                     {'Cseq',<<"1234 CANCEL">>},
                     {'Date',<<"Tue, 3 Aug 2010 16:00:00 NOVST">>},
                     {'From',<<"sip:ev@erlyvideo.ru">>},
                     {'To',<<"sip:tt@hh">>}],
                    <<>>}},
               decode(Req)).
