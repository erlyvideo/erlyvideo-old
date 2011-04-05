%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  201 Max Lapshin
%%% @doc        RTMFP module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%%---------------------------------------------------------------------------------------
-module(rtmfp_server).
-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").


%% External API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([encrypt/2, decrypt/2]).
-export([diff/2]).

start_link(Port) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).
  
-define(DEFAULT_KEY, <<"Adobe Systems 02">>).


-record(server, {
  port,
  socket,
  sessions
}).

-record(session, {
  id = 0,
  socket,
  addr,
  tag,
  server_cookie,
  client_cookie,
  client_public,
  server_public,
  shared_secret,
  
  enc_key = ?DEFAULT_KEY,
  dec_key = ?DEFAULT_KEY
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


init([Port]) ->
  {ok, Socket} = gen_udp:open(Port, [binary,{active,once},{recbuf,65536},inet]),
  inet:setopts(Socket, [{active,true}]),
  {ok, #server{socket = Socket, port = Port, sessions = #session{}}}.

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
  {noreply, Server};
  
handle_info({udp, Socket, IP, Port, Bin}, #server{socket = Socket, sessions = Session} = Server) ->
  <<S1:32, S2:32, S3:32, _/binary>> = Bin,
  SessionId = S1 bxor S2 bxor S3,
  Session1 = case SessionId of
    0 -> ?D(flush_keys), Session#session{dec_key = ?DEFAULT_KEY, enc_key = ?DEFAULT_KEY};
    _ -> Session
  end,
  {ok, Session2} = process(Bin, Session1#session{addr = {IP, Port}, id = SessionId, socket = Socket}),
    
  % inet:setopts(Socket, [{active,once}]),
  {noreply, Server#server{sessions = Session2}};
  

handle_info(_Info, State) ->
  {noreply, State}.

decrypt(Crypted, Key) ->
  crypto:aes_cbc_128_decrypt(Key, <<0:128>>, Crypted).

encrypt(Decrypted, Key) ->
  crypto:aes_cbc_128_encrypt(Key, <<0:128>>, Decrypted).

hexdump(<<>>) ->
  ok;

hexdump(Bin) ->
  {Line, Next} = case Bin of
    <<S:16/binary, Bin1/binary>> -> {S, Bin1};
    _ -> {Bin, <<>>}
  end,
  [io:format("~2.16.0b ", [N]) || N <- binary_to_list(Line)],
  io:format("  "),
  lists:foreach(fun(N) ->
    Print = io_lib:printable_list([N]),
    if 
      N > 32 andalso N < 128 andalso Print == true -> io:format("~c", [N]);
      true -> io:format(".")
    end
  end, binary_to_list(Line)),
  io:format("~n"),
  hexdump(Next).


checksum(Bin) ->
  checksum(Bin, 0).

checksum(<<P:16, Bin/binary>>, Sum) ->
  checksum(Bin, Sum + P);

checksum(<<P:8>>, Sum) ->
  checksum(<<>>, Sum + P);

checksum(<<>>, Sum) ->
  <<S1:16, S2:16>> = <<Sum:32>>,
  S3 = S1 + S2,
  S4 = S3 + (S3 bsr 16),
  bnot S4 band 16#FFFF.
  
  
diff(<<S1:32, N1/binary>>, <<S2:32, N2/binary>>) ->
  diff0(<<S1:32, (decrypt(N1, ?DEFAULT_KEY))/binary>>, <<S2:32, (decrypt(N2, ?DEFAULT_KEY))/binary>>).
  
diff0(<<S1:8/binary, N1/binary>>, <<S2:8/binary, N2/binary>>) ->
  [io:format("~2.16.0b ", [N]) || N <- binary_to_list(S1)],
  io:format("  "),
  [io:format("~2.16.0b ", [N]) || N <- binary_to_list(S2)],
  case S1 of
    S2 -> io:format("=");
    _ -> io:format("+")
  end,
  io:format("~n"),
  diff0(N1, N2);

diff0(<<>>, <<>>) ->
  ok;

diff0(<<S1/binary>>, <<S2/binary>>) ->
  [io:format("~2.16.0b ", [N]) || N <- binary_to_list(S1)],
  [io:format("   ") || _N <- lists:seq(1, 8 - size(S1))],
  io:format("  "),
  [io:format("~2.16.0b ", [N]) || N <- binary_to_list(S2)],
  [io:format("   ") || _N <- lists:seq(1, 8 - size(S2))],
  case S1 of
    S2 -> io:format("=");
    _ -> io:format("+")
  end,
  io:format("~n"),
  ok.
    
process(<<_S:32, Crypted/binary>>, #session{dec_key = DecKey} = Session) ->
  % case DecKey of
  %   ?DEFAULT_KEY -> ok;
  %   _ -> ?D(Crypted)
  % end,
  All = <<CRC:16, Decrypted/binary>> = decrypt(Crypted, DecKey),
  CRC1 = checksum(Decrypted),
  <<Type, DTS:16, ChunkType, Length:16, Message/binary>> = Decrypted,
  case CRC of
    CRC1 ->
      % <<E:1, S:1, R1:1, R2:1, I:1, R:1, T:1, TT:1>> = <<Type>>,
      % ?D({E, S, I, R, T, TT, DTS, ChunkType, Length, size(Message)}),
      ?D({message,Type,ChunkType}),
      process_message(Type, ChunkType, Session, Message, DTS, Length);
    _ ->
      % ?D({crc,CRC,CRC1, Type, ChunkType,size(Crypted)}),
      % hexdump(<<_S:32, Crypted/binary>>),
      % io:format("------~n"),
      hexdump(<<_S:32,All/binary>>),
      % io:format("------~n"),
      % ?D(Crypted),
      % ?D(All),
      % hexdump(Crypted),
      application:stop(rtmp),
      {ok, Session, <<"hi">>}
  end.


process_message(16#0B, 16#30, Session, Message, DTS, Length) -> process_message_0b_30(Session, Message, DTS, Length);
process_message(16#0B, 16#38, Session, Message, DTS, Length) -> process_message_0b_38(Session, Message, DTS, Length);
process_message(16#8D, _, Session, Message, DTS, Length) -> process_message_8d_5e(Session, Message, DTS, Length);
process_message(Type, ChunkType, Session, Message, DTS, Length) ->
  ?D({unknown_message, Type, ChunkType, DTS, Length}),
  io:format("~n~n"),
  hexdump(Message),
  application:stop(rtmp),
  {ok, Session, <<>>}.

process_message_0b_30(Session, Message, _DTS, _Length) ->
  {_Len1, Rest1} = amf3:read_uint29(Message),
  {Len2, Rest2} = amf3:read_uint29(Rest1),
  Len2_ = Len2 - 1,
  <<PunchHole, _URI:Len2_/binary, ClientTag:16/binary, _Rest3/binary>> = Rest2,
  
  % ?D({uri,URI}),
  
  % ServerCookie = crypto:rand_bytes(64),
  ServerCookie = list_to_binary(lists:seq(1,64)),
  
  PunchHole = 16#0A,

  Reply = [ClientTag, ServerCookie, <<10>>, <<16#0E, (server_cert())/binary>>, <<16#15, 16#02>>, <<16#15, 16#05>>, <<16#15, 16#0E>>],
  prepare_for_client(Session#session{tag = ClientTag, server_cookie = ServerCookie},
                 16#0B, 16#70, pack_reply(Reply)).
                 
process_message_0b_38(#session{server_cookie = ServerCookie} = Session, Message, _DTS, _Length) ->
  <<SessionId:32, Rest1/binary>> = Message,
  {64, <<ServerCookie:64/binary, Rest2/binary>>} = amf3:read_uint29(Rest1),
  
  {132, Rest3} = amf3:read_uint29(Rest2),
  {130, <<_Unknown1:16, ClientPublic:128/binary, Rest4/binary>>} = amf3:read_uint29(Rest3),
  {Len3, Rest5} = amf3:read_uint29(Rest4),
  Len3 = 16#4c,
  <<ClientCookie:Len3/binary, _Rest6/binary>> = Rest5,

  {<<DH_KEY_SIZE:32, ServerPublic:DH_KEY_SIZE/binary>>, Private} = crypto:dh_generate_key([rtmpe:p(), rtmpe:g()]),
  SharedSecret = crypto:dh_compute_key(<<(size(ClientPublic)):32, ClientPublic/binary>>, Private, [rtmpe:p(), rtmpe:g()]),
  
  % NewSessionId = random:uniform(16#FFFFFFFF),
  NewSessionId = 42,
  Reply = <<NewSessionId:32, 
  16#81, 16#0B,
    16#03, 16#1a, 16#00, 16#00, 
    16#02, 16#1e, 16#00,
    16#81, 16#02, 16#0d, 16#02, % 81 01 = amf3:write_uint29(size(ServerPublic)+2)
    ServerPublic/binary,
  16#58>>,
  
  % ?D({replied,SessionId,NewSessionId}),
  
  _ClientId = sha2:digest256(ClientPublic),
  _ServerId = sha2:digest256(ServerPublic),
  
  Buf = <<3,16#1a,0,0,2,16#1e,0,16#81,2,16#d,2,ServerPublic/binary>>,
  MD1 = hmac256:digest_bin(Buf, ClientCookie),
  MD2 = hmac256:digest_bin(ClientCookie, Buf),
  <<NewDecKey:16/binary, _/binary>> = hmac256:digest_bin(SharedSecret, MD1),
  <<NewEncKey:16/binary, _/binary>> = hmac256:digest_bin(SharedSecret, MD2),
  
  {ok, Session1} = prepare_for_client(Session#session{
    id = SessionId,
    client_cookie = ClientCookie,
    server_public = ServerPublic,
    client_public = ClientPublic,
    shared_secret = SharedSecret
  }, 16#0B, 16#78, Reply),
  {ok, Session1#session{
    id = NewSessionId,
    enc_key = NewEncKey,
    dec_key = NewDecKey
  }}.
  


process_message_8d_5e(Session, Message, _DTS, _Length) ->
  <<Len:16, Msg:Len/binary, _Rest/binary>> = Message,
  hexdump(Msg),
  application:stop(rtmp),
  {ok, Session, <<>>}.
  % <<Skip:13/binary, RTMPType, StreamId:32, Payload/binary

% dts() ->
%   16#FAFA;

dts() ->
  {_Mega, Sec, USec} = erlang:now(),
  TS = ((Sec rem 1000)*1000 + (USec div 1000)) div 4,
  TS band 16#FFFF.
  
prepare_for_client(#session{id = SessionId, enc_key = EncKey, socket = Socket, addr = {IP, Port}} = Session, Type, ChunkType, Chunk) ->
  Msg1 = <<Type, (dts()):16, ChunkType, (size(Chunk)):16, Chunk/binary>>,
  Msg2 = pad(Msg1),
  0 = (size(Msg2) + 2) rem 16,
  CRC = checksum(Msg2),
  <<S2:32, S3:32, _/binary>> = Msg3 = encrypt(<<CRC:16, Msg2/binary>>, EncKey),
  S1 = S2 bxor S3 bxor SessionId,
  gen_udp:send(Socket, IP, Port, <<S1:32, Msg3/binary>>),
  {ok, Session}.

pad(Bin) when size(Bin) rem 16 == 14 -> Bin;
pad(Bin) -> pad(<<Bin/binary, 16#FF>>).

pack_reply(Reply) ->
  iolist_to_binary([[<<(size(Bin))>>, Bin] || Bin <- Reply]).

server_cert() ->
  <<
  16#F4, 16#E4, 16#53, 16#6A, 16#66, 16#76, 16#EA, 16#EA,
	16#7D, 16#BA, 16#43, 16#5B, 16#B8, 16#82, 16#72, 16#54,
	16#4C, 16#46, 16#DF, 16#91, 16#54, 16#2A, 16#B8, 16#1D,
	16#E0, 16#32, 16#B1, 16#8F, 16#10, 16#D2, 16#5F, 16#1E,
	16#3A, 16#1E, 16#2C, 16#42, 16#AE, 16#68, 16#64, 16#52,
	16#D5, 16#56, 16#87, 16#FE, 16#7B, 16#C4, 16#EA, 16#ED,
	16#14, 16#06, 16#38, 16#9D, 16#27, 16#6F, 16#E6, 16#CE,
	16#9C, 16#38, 16#18, 16#B3, 16#EF, 16#12, 16#06, 16#2A
	>>.

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
