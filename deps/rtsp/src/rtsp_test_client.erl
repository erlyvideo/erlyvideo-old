%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        RTSP test generator-runner
%%%
%%% 
%%% 1. connect
%%% 2. describe
%%% 3. each setup
%%% 4. play, possible Rtp-Sync
%%% 5. get each packet
%%% 6. decode
%%% 
%%% 
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtsp.
%%%
%%% erlang-rtsp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtsp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtsp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtsp_test_client).
-author('Max Lapshin <max@maxidoors.ru>').
-export([capture_camera/2]).
-define(D(X), io:format("~p:~p ~p~n", [?MODULE,?LINE,X])).

capture_camera(Name, URL) ->
  {rtsp, Auth, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active,false},{packet,line}]),
  CaptureDir = "test/rtsp_capture/"++Name,
  put(capture_dir, CaptureDir),
  io:format("Save session to ~s~n", [CaptureDir]),
  AuthHeader = case Auth of
    [] -> "";
    _ -> "Authorization: Basic "++binary_to_list(base64:encode(Auth))++"\r\n"
  end,
  put(auth, AuthHeader),
  put(seq, 1),
  put(socket, Socket),
  put(url, URL),
  put(interleave, 0),
  {ok, Streams} = capture_camera_describe(),
  [capture_camera_setup(Stream) || Stream <- Streams],
  capture_camera_play(),
  capture_interleaved_data(50),
  ok.

inc_seq() ->
  put(seq, get(seq) + 1).

capture_camera_describe() ->
  {ok, Headers, Body} = send_and_receive("DESCRIBE ~s RTSP/1.0\r\nCseq: ~p\r\n~s\r\n", [get(url), get(seq), get(auth)]),
  SDP = string:tokens(binary_to_list(Body), "\r\n"),
  StreamDesc = lists:dropwhile(fun
    ("m="++_) -> false;
    (_) -> true
  end, SDP),
  ControlDesc = lists:filter(fun
    ("a=control:"++_) -> true;
    (_) -> false
  end, StreamDesc),
  ControlBase = case proplists:get_value('Content-Base', Headers) of
    undefined -> "";
    Else -> Else
  end,
  Controls = [ControlBase ++ Control || "a=control:"++Control <- ControlDesc],
  ?D({Headers, Controls}),
  {ok, Controls}.

capture_camera_setup(Control) ->
  Chan = get(interleave),
  Session = case get(session) of
    undefined -> "";
    Else -> "Session: "++Else++"\r\n"
  end,
  {ok, Headers, _Body} = send_and_receive("SETUP ~s RTSP/1.0\r\nCseq: ~p\r\nTransport: RTP/AVP/TCP;unicast;interleaved-~p-~p\r\n~s~s\r\n", 
                        [Control, get(seq), Chan, Chan+1, get(auth), Session]),
  put(interleave, Chan + 2),
  ok.

capture_camera_play() ->
  {ok, Headers, Body} = send_and_receive("PLAY ~s RTSP/1.0\r\nCseq: ~p\r\nSession: ~s\r\n~s\r\n", [get(url), get(seq), get(session), get(auth)]),
  ok.


send_and_receive(Format, Args) ->
  Out = io_lib:format(Format, Args),
  capture(out, Out),
  gen_tcp:send(get(socket), Out),
  {ok, Headers, Body, Raw} = read_reply(),
  case proplists:get_value("Session", Headers) of
    undefined -> ok;
    Else -> put(session, Else)
  end,
  capture(in, Raw),
  inc_seq(),
  {ok, Headers, Body}.
  
capture(Direction, Data) ->
  Filename = lists:flatten(io_lib:format("~s/~p-~p.txt", [get(capture_dir), get(seq), Direction])),
  filelib:ensure_dir(Filename),
  case Direction of
    out -> io:format(">>>>>>>>>>  OUT >>>>>>>>>\r\n");
    in  -> io:format("<<<<<<<<<<   IN <<<<<<<<<\r\n")
  end,
  io:format("~s", [Data]),
  file:write_file(Filename, Data).


capture_interleaved_data(Timeout) ->
  Ref = timer:send_after(Timeout*1000, stop),
  Filename = lists:flatten(io_lib:format("~s/~p-interleaved-in.txt", [get(capture_dir), get(seq)])),
  filelib:ensure_dir(Filename),
  {ok, F} = file:open(Filename, [binary,write]),
  inet:setopts(get(socket), [{packet,raw},{active,true}]),
  loop_capture(F),
  file:close(F).

loop_capture(F) ->
  receive
    {tcp, _Socket, Bin} ->
      file:write(F, Bin),
      loop_capture(F);
    stop ->
      ok
  end.

read_reply() ->
  Socket = get(socket),
  {ok,<<"RTSP/1.0 200 OK\r\n">>} = gen_tcp:recv(Socket, 0),
  {Headers, Raw} = read_headers([], [<<"RTSP/1.0 200 OK\r\n">>]),
  Body = case proplists:get_value('Content-Length', Headers) of
    undefined -> <<>>;
    "0" -> <<>>;
    Length ->
      inet:setopts(Socket, [{packet,raw}]),
      {ok, Bin} = gen_tcp:recv(Socket, list_to_integer(Length)),
      inet:setopts(Socket, [{packet,line}]),
      Bin
  end,
  {ok, Headers, Body, iolist_to_binary([Raw, Body])}.
  
read_headers(Acc, Raw) ->
  case gen_tcp:recv(get(socket), 0) of
    {ok, <<"\r\n">>} -> {lists:reverse(Acc), lists:reverse([<<"\r\n">>|Raw])};
    {ok, Bin} ->
      {ok, {http_header, _, Key, _, Value}, <<"\r\n">>} = erlang:decode_packet(httph, <<Bin/binary, "\r\n">>, []),
      read_headers([{Key,Value}|Acc], [Bin|Raw])
  end.
