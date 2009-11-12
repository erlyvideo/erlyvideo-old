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
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../include/ems.hrl").

-export([connect/2]).
-export(['WAIT_FOR_DATA'/2]).


obj_to_integer({number, Int}) -> Int;
obj_to_integer({string, String}) -> 
  {Int, _} = string:to_integer(String),
  Int.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a connect command and responds
%% @end
%%-------------------------------------------------------------------------
connect(AMF, #ems_client{window_size = WindowAckSize} = State) ->
    ?D({"invoke - connect", AMF}),
    
    Channel = #channel{id = 2, timestamp = 0, stream = 0, msg = <<>>},
		gen_fsm:send_event(self(), {send, {Channel#channel{type = ?RTMP_TYPE_WINDOW_ACK_SIZE}, <<WindowAckSize:32>>}}),
		gen_fsm:send_event(self(), {send, {Channel#channel{type = ?RTMP_TYPE_BW_PEER}, <<0,16#26, 16#25,16#a0, 16#02>>}}),
		
	  NewState1 = case AMF#amf.args of
			[{object, PlayerInfo}, {string, Cookie}, UserIdObj] ->
			  UserId = obj_to_integer(UserIdObj),
			  Session = rtmp_session:decode(Cookie),
        ?D({"Session:", Session}),
        
				State#ems_client{player_info = PlayerInfo, user_id = UserId};
	    [{object, PlayerInfo} | _] ->
				State#ems_client{player_info = PlayerInfo, user_id = undefined}
		end,
		
		NewState = NewState1#ems_client{previous_ack = erlang:now()},
    
    case lists:keyfind(objectEncoding, 1, PlayerInfo) of
      {objectEncoding, 0} -> ok;
      {objectEncoding, _N} -> 
        error_logger:error_msg("Warning! Cannot work with clients, using AMF3 encoding.
        Assume _connection.objectEncoding = ObjectEncoding.AMF0; in your flash code"),
        throw(invalid_amf3_encoding);
      _ ->
        error_logger:error_msg("Warning! Client hasnt provided any encoding.
        Assume _connection.objectEncoding = ObjectEncoding.AMF0; in your flash code"),
        throw(invalid_amf_encoding)
    end,
    
    NewAMF = AMF#amf{
        command = '_result', 
        id = 1, %% muriel: dirty too, but the only way I can make this work
        type = invoke,
        args= [
            [{capabilities, 31}, {fmsVer, "RubyIZUMI/0,1,2,0"}],
            [{code, ?NC_CONNECT_SUCCESS},
            {level, "status"}, 
            {description, "Connection succeeded."}]]},
    gen_fsm:send_event(self(), {invoke, NewAMF}),
    NewState.



'WAIT_FOR_DATA'({control, Type, Stream}, State) ->
  gen_fsm:send_event(self(), {send, {#channel{id = 2, timestamp = 0, type = ?RTMP_TYPE_CONTROL, stream = 0}, <<Type:16/big, Stream:32/big>>}}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};


'WAIT_FOR_DATA'({status, Code, Stream, Description}, State) ->
  AMF = #amf{
      command = 'onStatus',
      type = invoke,
      id = 0,
      args = [null, [{code, Code}, 
                     {level, "status"}, 
                     {description, Description}]]},
  'WAIT_FOR_DATA'({invoke, AMF, Stream}, State);

'WAIT_FOR_DATA'({status, Code, Stream}, State) -> 'WAIT_FOR_DATA'({status, Code, Stream, "-"}, State);
'WAIT_FOR_DATA'({status, Code}, State) -> 'WAIT_FOR_DATA'({status, Code, 0, "-"}, State);

'WAIT_FOR_DATA'({invoke, #amf{} = AMF, Stream}, State) ->
  gen_fsm:send_event(self(), {send, {#channel{id = 16, timestamp = 0, type = ?RTMP_INVOKE_AMF0, stream = Stream}, AMF}}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({invoke, #amf{} = AMF}, State) -> 'WAIT_FOR_DATA'({invoke, #amf{} = AMF, 0}, State);


'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.
