%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Motion JPEG reader
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
-module(mjpeg_reader).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include("log.hrl").

-define(TIMEOUT, 10000).


%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([is_valid_jpeg/1]).

start_link(URL, Consumer) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [URL, Consumer], []).

-record(reader, {
  url,
  consumer,
  boundary,
  next_length,
  buffer = <<>>,
  state,
  socket
}).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([URL, Consumer]) ->
  erlang:monitor(process, Consumer),
  self() ! connect,
  {ok, #reader{url = URL, consumer = Consumer}}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({'DOWN', _, process, _Client, _Reason}, Server) ->
  {stop, normal, Server};

handle_info(connect, #reader{url = URL} = Server) ->
  {http, UserInfo, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  
  {match, [_, Get]} = re:run(URL, "http://[^\\/]+(/.*)", [{capture, all, list}]),
  

  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, http}, {active, once}], 1000),
  Auth = case UserInfo of
    [] -> "";
    _ -> "Authorization: Basic "++binary_to_list(base64:encode(UserInfo))++"\r\n"
  end,
  Call = io_lib:format("GET ~s HTTP/1.1\r\nConnection: close\r\n"++Auth++"\r\n", [Get]), % Host: ~s\r\n , Host
  gen_tcp:send(Socket, Call),
  ?D({"send mjpeg request", URL}),
  {noreply, Server#reader{socket = Socket}, ?TIMEOUT};

handle_info({http, Socket, {http_response, _, 200, _Status}}, Reader) ->
  ?D({"mjpeg camera responded"}),
  inet:setopts(Socket, [{active, once}]),
  {noreply, Reader, ?TIMEOUT};

handle_info({http, _Socket, {http_response, _, Code, Status}}, Reader) ->
  {stop, {http, Code, Status}, Reader};

handle_info({http, Socket, {http_header, _, 'Content-Type', _, "multipart/x-mixed-replace; boundary="++Boundary}}, Reader) ->
  ?D({set_boundary,Boundary}),
  inet:setopts(Socket, [{active, once}]),
  {noreply, Reader#reader{boundary = list_to_binary(Boundary), state = ready}, ?TIMEOUT};

handle_info({http, Socket, {http_header, _, 'Content-Type', _, "multipart/x-mixed-replace;boundary="++Boundary}}, Reader) ->
  ?D({set_boundary,Boundary}),
  inet:setopts(Socket, [{active, once}]),
  {noreply, Reader#reader{boundary = list_to_binary(Boundary), state = ready}, ?TIMEOUT};

handle_info({http, Socket, {http_header, _, _Key, _, _Value}}, Reader) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, Reader, ?TIMEOUT};

handle_info({http, Socket, http_eoh}, Reader) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  {noreply, Reader, ?TIMEOUT};

handle_info({tcp, Socket, Bin}, #reader{buffer = Buffer, state = ready, boundary = Boundary} = Reader) ->
  Data = <<Buffer/binary, Bin/binary>>,
  Length = size(Boundary),
  case Data of
    <<Boundary:Length/binary, "\r\n", Headers/binary>> ->
      handle_info({tcp, Socket, <<>>}, Reader#reader{buffer = Headers, state = headers});
    <<Boundary:Length/binary, Headers/binary>> ->
      handle_info({tcp, Socket, <<>>}, Reader#reader{buffer = Headers, state = headers});
    <<_, Rest/binary>> when size(Data) >= size(Boundary) ->
      handle_info({tcp, Socket, <<>>}, Reader#reader{buffer = Rest});
    _ ->
      inet:setopts(Socket, [{active, once}, {packet, raw}]),
      {noreply, Reader#reader{buffer = Data}, ?TIMEOUT}
  end;
  
handle_info({tcp, Socket, Bin}, #reader{buffer = Buffer, state = headers} = Reader) ->
  Data = <<Buffer/binary, Bin/binary>>,
  case erlang:decode_packet(httph, Data, []) of
    {ok, {http_header, _, 'Content-Type', _, Value}, Rest} ->
      "image/jpeg" = Value,
      handle_info({tcp, Socket, <<>>}, Reader#reader{buffer = Rest});
    {ok, {http_header, _, 'Content-Length', _, Length}, Rest} ->
      handle_info({tcp, Socket, <<>>}, Reader#reader{buffer = Rest, next_length = list_to_integer(Length)});
    {ok, http_eoh, Rest} ->
      handle_info({tcp, Socket, <<>>}, Reader#reader{buffer = Rest, state = jpeg});
    {more, undefined} ->
      inet:setopts(Socket, [{active, once}, {packet, raw}]),
      {noreply, Reader#reader{buffer = Data}, ?TIMEOUT}
  end;

handle_info({tcp, Socket, Bin}, #reader{buffer = Buffer, state = jpeg, next_length = Length, consumer = Consumer} = Reader) ->
  Data = <<Buffer/binary, Bin/binary>>,
  case Data of
    <<JPEG:Length/binary, Rest/binary>> ->
      case is_valid_jpeg(JPEG) of
        true -> Consumer ! {jpeg, self(), JPEG};
        false -> ?D({invalid, element(1, erlang:split_binary(JPEG, 10))}), ok
      end,
      handle_info({tcp, Socket, <<>>}, Reader#reader{buffer = Rest, state = ready});
    _ ->
      inet:setopts(Socket, [{active, once}, {packet, raw}]),
      {noreply, Reader#reader{buffer = Data}, ?TIMEOUT}
  end;

handle_info(timeout, State) ->
  {stop, timeout, State};

handle_info(_Info, State) ->
  ?D({_Info, State}),
  {noreply, State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
  

is_valid_jpeg(<<16#FF, 16#D8, _JPEG/binary>>) ->
  true;
    
%   ?D(d8),
%   validate_jpeg(JPEG);
% 
% validate_jpeg(<<16#FF, 16#D9, JPEG/binary>>) ->
%   ?D(d9),
%   validate_jpeg(JPEG);
%   
% validate_jpeg(<<16#FF, 16#D:4, 0:1, _N:3, JPEG/binary>>) ->
%   ?D({dn,_N}),
%   validate_jpeg(JPEG);
%   
% validate_jpeg(<<16#FF, 16#DD, _:2, JPEG/binary>>) ->
%   ?D(dd),
%   validate_jpeg(JPEG);
% 
% validate_jpeg(<<16#FF, _Marker, Length:16, _/binary>> = JPEG) when size(JPEG) >= Length + 2 ->
%   <<16#FF, _Marker, _:Length/binary, Rest/binary>> = JPEG,
%   ?D({erlang:integer_to_list(_Marker,16),Length, element(1, erlang:split_binary(JPEG, 20))}),
%   validate_jpeg(Rest);
% 
% validate_jpeg(<<>>) ->
%   true;
%   
is_valid_jpeg(_Else) ->
  false.
  