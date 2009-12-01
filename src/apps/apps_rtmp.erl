%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Generalized RTMP application behavior module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
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
-module(apps_rtmp).
-author('max@maxiodors.ru').
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../include/ems.hrl").

-export([connect/2, reply/1, fail/1]).
-export(['WAIT_FOR_DATA'/2]).


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a connect command and responds
%% @end
%%-------------------------------------------------------------------------
connect(AMF, #rtmp_client{window_size = WindowAckSize} = State) ->
    
    Channel = #channel{id = 2, timestamp = 0, msg = <<>>},
		gen_fsm:send_event(self(), {send, {Channel#channel{type = ?RTMP_TYPE_WINDOW_ACK_SIZE}, <<WindowAckSize:32>>}}),
		gen_fsm:send_event(self(), {send, {Channel#channel{type = ?RTMP_TYPE_BW_PEER}, <<0,16#26, 16#25,16#a0, 16#02>>}}),
		
	  [{object, PlayerInfo} | AuthInfo] = AMF#amf.args,
	  _FlashVer = proplists:get_value(flashVer, PlayerInfo),
	  _SwfUrl = proplists:get_value(swfUrl, PlayerInfo),
	  _TcUrl = proplists:get_value(tcUrl, PlayerInfo),
	  _Fpad = proplists:get_value(fpad, PlayerInfo),
	  _AudioCodecs = round(proplists:get_value(audioCodecs, PlayerInfo, 0)),
	  _VideoCodecs = proplists:get_value(videoCodecs, PlayerInfo),
	  _VideoFunction = proplists:get_value(videoFunction, PlayerInfo),
	  _PageUrl = proplists:get_value(pageUrl, PlayerInfo),

    ?D({"CONNECT", _PageUrl}),
		NewState1 =	State#rtmp_client{player_info = PlayerInfo, previous_ack = erlang:now()},

    AuthModule = ems:get_var(auth_module, trusted_login),
    NewState2 = AuthModule:client_login(NewState1, AuthInfo),
    
    NewState3 = case lists:keyfind(objectEncoding, 1, PlayerInfo) of
      {objectEncoding, 0.0} -> NewState2#rtmp_client{amf_version = 0};
      {objectEncoding, 3.0} -> NewState2#rtmp_client{amf_version = 3};
      {objectEncoding, _N} -> 
        error_logger:error_msg("Warning! Cannot work with clients, using not AMF0/AMF3 encoding.
        Assume _connection.objectEncoding = ObjectEncoding.AMF0; in your flash code is used version ~p~n", [_N]),
        throw(invalid_amf3_encoding);
      _ -> NewState2#rtmp_client{amf_version = 0}
    end,
    
    ConnectObj = [{capabilities, 31}, {fmsVer, <<"Erlyvideo 1.0">>}],
    StatusObj = [{code, <<?NC_CONNECT_SUCCESS>>},
                 {level, <<"status">>}, 
                 {description, <<"Connection succeeded.">>},
                 {objectEncoding, NewState3#rtmp_client.amf_version}],
    reply(AMF#amf{args = [{object, ConnectObj}, {object, StatusObj}]}),
    NewState3.


reply(AMF) ->
  gen_fsm:send_event(self(), {invoke, AMF#amf{command = '_result', type = invoke}}).

fail(AMF) ->
  gen_fsm:send_event(self(), {invoke, AMF#amf{command = '_error', type = invoke}}).
  



'WAIT_FOR_DATA'({control, Type, Stream}, State) ->
  gen_fsm:send_event(self(), {send, {#channel{id = 2, timestamp = 0, type = ?RTMP_TYPE_CONTROL}, <<Type:16/big, Stream:32/big>>}}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};


'WAIT_FOR_DATA'({status, Code, Stream, Description}, State) when is_list(Code) ->
  'WAIT_FOR_DATA'({status, list_to_binary(Code), Stream, Description}, State);

'WAIT_FOR_DATA'({status, Code, Stream, Description}, State) when is_list(Description) ->
  'WAIT_FOR_DATA'({status, Code, Stream, list_to_binary(Description)}, State);
  
'WAIT_FOR_DATA'({status, Code, Stream, Description}, State) ->
  AMF = #amf{
      command = 'onStatus',
      type = invoke,
      id = 0,
      stream_id = Stream,
      args = [null, {object, [{code, Code}, 
                              {level, <<"status">>}, 
                              {description, Description}]}]},
  'WAIT_FOR_DATA'({invoke, AMF}, State);

'WAIT_FOR_DATA'({status, Code, Stream}, State) -> 'WAIT_FOR_DATA'({status, Code, Stream, "-"}, State);
'WAIT_FOR_DATA'({status, Code}, State) -> 'WAIT_FOR_DATA'({status, Code, 0, "-"}, State);

'WAIT_FOR_DATA'({invoke, #amf{stream_id = StreamId} = AMF}, #rtmp_client{amf_version = 0} = State) ->
  gen_fsm:send_event(self(), {send, {#channel{id = 16, timestamp = 0, type = ?RTMP_INVOKE_AMF0, stream_id = StreamId}, AMF}}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({invoke, #amf{stream_id = StreamId} = AMF}, #rtmp_client{amf_version = 3} = State) ->
  gen_fsm:send_event(self(), {send, {#channel{id = 16, timestamp = 0, type = ?RTMP_INVOKE_AMF0, stream_id = StreamId}, AMF}}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};


'WAIT_FOR_DATA'(_Message, #rtmp_client{} =_State) -> {unhandled}.
