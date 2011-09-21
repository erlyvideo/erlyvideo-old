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

-export([capture_camera/2, capture_camera/3, simulate_camera/2]).
-export([capture_server/4]).


-define(D(X), io:format("~p:~p ~p~n", [?MODULE,?LINE,X])).

-define(CAPTURE_DIR, "test/files/rtsp_capture/").

prepare_connect_socket(URL) ->
  {rtsp, Auth, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active,false},{packet,line}]),
  AuthHeader = case Auth of
    [] -> "";
    _ -> "Authorization: Basic "++binary_to_list(base64:encode(Auth))++"\r\n"
  end,
  put(auth, AuthHeader),
  put(seq, 1),
  put(socket, Socket),
  RawURL = case Auth of
    [] -> URL;
    _ -> re:replace(URL, "rtsp://([^@]+@)", "rtsp://", [{return,list}])
  end,
  put(url, RawURL),
  ok.

capture_camera(Name, URL) ->
  capture_camera(Name, URL, []).
  

capture_camera(Name, URL, Options) ->
  CaptureDir = ?CAPTURE_DIR++Name,
  put(capture_dir, CaptureDir),
  io:format("Save session to ~s~n", [CaptureDir]),
  prepare_connect_socket(URL),
  put(interleave, 0),
  {ok, Streams} = capture_camera_describe(Options),
  [capture_camera_setup(Stream) || Stream <- Streams],
  capture_camera_play(),
  capture_interleaved_data(50),
  ok.

inc_seq() ->
  put(seq, get(seq) + 1).

capture_camera_describe(Options) ->
  {ok, Headers, Body} = send_and_receive("DESCRIBE", "~s RTSP/1.0\r\nCSeq: ~p\r\n~s\r\n", [get(url), get(seq), get(auth)]),
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
  FilteredControls = case proplists:get_value(tracks, Options) of
    undefined -> Controls;
    TrackList -> [lists:nth(N,Controls) || N <- TrackList]
  end,
  ?D({Headers, FilteredControls}),
  {ok, FilteredControls}.

capture_camera_setup(Control) ->
  Chan = get(interleave),
  Session = case get(session) of
    undefined -> "";
    Else -> "Session: "++Else++"\r\n"
  end,
  {ok, _Headers, _Body} = send_and_receive("SETUP", "~s RTSP/1.0\r\nCSeq: ~p\r\nTransport: RTP/AVP/TCP;unicast;interleaved-~p-~p\r\n~s~s\r\n", 
                        [Control, get(seq), Chan, Chan+1, get(auth), Session]),
  put(interleave, Chan + 2),
  ok.

capture_camera_play() ->
  {ok, _Headers, _Body} = send_and_receive("PLAY", "~s RTSP/1.0\r\nCSeq: ~p\r\nSession: ~s\r\n~s\r\n", [get(url), get(seq), get(session), get(auth)]),
  ok.


send_and_receive(Method, Format, Args) ->
  Out = io_lib:format(Method ++ " " ++ Format, Args),
  capture(out, Out, Method),
  gen_tcp:send(get(socket), Out),
  {ok, Headers, Body, Raw} = receive_reply(Method),
  capture(in, Raw, Method),
  inc_seq(),
  {ok, Headers, Body}.

receive_reply(_Method) ->
  {ok, Headers, Body, Raw} = read_reply(),
  case proplists:get_value("Session", Headers) of
    undefined -> ok;
    Else -> put(session, Else)
  end,
  {ok, Headers, Body, Raw}.
  
capture(Direction, Data, Method) ->
  Filename = lists:flatten(io_lib:format("~s/~p-~p-~s.txt", [get(capture_dir), get(seq), Direction, Method])),
  Data1 = case get(url) of
    undefined -> Data;
    URL -> re:replace(Data, URL, "{{URL}}", [{return,binary},global])
  end,
  filelib:ensure_dir(Filename),
  case Direction of
    out -> io:format(">>>>>>>>>>  OUT >>>>>>>>>\r\n");
    in  -> io:format("<<<<<<<<<<   IN <<<<<<<<<\r\n")
  end,
  io:format("~s", [Data]),
  file:write_file(Filename, Data1).


capture_interleaved_data(Timeout) ->
  _Ref = timer:send_after(Timeout*1000, stop),
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
  {ok,<<"RTSP/1.0 200 OK\r\n">>} = gen_tcp:recv(get(socket), 0),
  {Headers, Raw} = read_headers(get(socket), [], [<<"RTSP/1.0 200 OK\r\n">>]),
  Body = read_body(Headers),
  {ok, Headers, Body, iolist_to_binary([Raw, Body])}.
  
read_headers(Socket, Acc, Raw) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, <<"\r\n">>} -> {lists:reverse(Acc), lists:reverse([<<"\r\n">>|Raw])};
    {ok, Bin} ->
      {ok, {http_header, _, Key, _, Value}, <<"\r\n">>} = erlang:decode_packet(httph, <<Bin/binary, "\r\n">>, []),
      read_headers(Socket, [{Key,Value}|Acc], [Bin|Raw])
  end.

read_body(Headers) ->
  Socket = get(socket),
  case proplists:get_value('Content-Length', Headers) of
    undefined -> <<>>;
    "0" -> <<>>;
    Length ->
      inet:setopts(Socket, [{packet,raw}]),
      {ok, Bin} = gen_tcp:recv(Socket, list_to_integer(Length)),
      inet:setopts(Socket, [{packet,line}]),
      Bin
  end.
  

prepare_listen_socket(Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {reuseaddr,true},{active,false},{packet,line}]),
  ?D({accepting, Port}),
  {ok, Socket} = gen_tcp:accept(Listen),
  put(listen_socket, Socket).

simulate_camera(Name, Port) ->
  CaptureDir = ?CAPTURE_DIR++Name,
  put(capture_dir, CaptureDir),
  prepare_listen_socket(Port),
  speak_to_camera_client().

speak_to_camera_client() ->
  case read_and_send_request() of
    {ok, "PLAY", _Url, Headers} -> send_interleaved_reply(list_to_integer(proplists:get_value("Cseq", Headers))+1);
    {ok, "TEARDOWN", _Url, _Headers} -> ok;
    _ -> speak_to_camera_client()
  end.
  
  
  
read_and_send_request() ->
  {ok, Method, URL, Headers, _Body, Raw} = read_request(),
  Seq = list_to_integer(proplists:get_value("Cseq", Headers)),
  Reply = load_capture(Seq, Method, in, URL),

  io:format("<<<<<<<<<<   IN <<<<<<<<<\r\n~s", [Raw]),
  io:format(">>>>>>>>>>  OUT >>>>>>>>>\r\n~s", [Reply]),

  gen_tcp:send(get(listen_socket), Reply),
  {ok, Method, URL, Headers}.
  
send_interleaved_reply(Seq) ->
  Filename = lists:flatten(io_lib:format("~s/~p-interleaved-in.txt", [get(capture_dir), Seq])),
  {ok, Bin} = file:read_file(Filename),
  send_interleaved_reply(Bin, get(listen_socket)).

send_interleaved_reply(<<Data:1540/binary, Bin/binary>>, Socket) ->
  gen_tcp:send(Socket, Data),
  send_interleaved_reply(Bin, Socket);

send_interleaved_reply(Bin, Socket) ->
  gen_tcp:send(Socket, Bin).

load_capture(Num, Method, Direction, URL) ->
  Filename = lists:flatten(io_lib:format("~s/~p-~p-~s.txt", [get(capture_dir), Num, Direction, Method])),
  ?D({request,Method,Num, URL, Filename}),
  case file:read_file(Filename) of
    {ok, Data} -> re:replace(Data, "{{URL}}", URL, [{return,binary},global]);
    _ -> erlang:error({cant_load_capture,Filename})
  end.
  


read_request() ->
  Socket = get(listen_socket),
  {ok, RequestLine} = gen_tcp:recv(Socket, 0),
  {match, [Method, URL]} = re:run(RequestLine, "([^ ]+) ([^ ]+) RTSP/1.0", [{capture,all_but_first,list}]),
  {Headers, Raw} = read_headers(Socket, [], [RequestLine]),
  Body = read_body(Headers),
  {ok, Method, URL, Headers, Body, iolist_to_binary([Raw,Body])}.


capture_server(Name, ListenPort, Host, Port) ->
  put(capture_dir, ?CAPTURE_DIR++Name),
  prepare_listen_socket(ListenPort),
  put(connect_host, Host),
  put(connect_port, Port),
  capture_proxied_server().

capture_proxied_server() ->
  {ok, Method, URL, _Headers, _Body, Request} = read_request(),
  capture(in, Request, Method),
  case get(url) of
    undefined ->
      {_Proto, _Auth, _Addr, _Port, Path, Query} = http_uri2:parse(URL),
      prepare_connect_socket("rtsp://"++get(connect_host)++":"++integer_to_list(get(connect_port))++Path++"?"++Query);
    _ -> ok
  end,
  gen_tcp:send(get(socket), Request),
  {ok, Headers1, _Body1, Reply} = receive_reply(Method),
  capture(out, Reply, Method),
  gen_tcp:send(get(listen_socket), Reply),
  case Method of
    "SETUP" ->
      Transport = proplists:get_value("Transport", Headers1),
      case re:run(Transport, "interleaved") of
        nomatch -> put(transport, udp);
        _ -> put(transport, tcp)
      end,
      capture_proxied_server();
    "PLAY" ->
      case get(transport) of
        tcp ->
          {ok, F} = file:open(get(capture_dir)++"/interleaved.txt", [write, binary]),
          capture_interleave_traffic(F);
        udp ->
          capture_proxied_server()
      end;
    "TEARDOWN" ->
      ok;
    _ -> capture_proxied_server()
  end.

capture_interleave_traffic(F) ->
  case gen_tcp:recv(get(socket), 0) of
    {ok, Bin} -> 
      file:write(F, Bin),
      gen_tcp:send(get(listen_socket), Bin),
      capture_interleave_traffic(F);
    _Else -> ok
  end.


