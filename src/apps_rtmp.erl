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
-include("../include/ems.hrl").

-export([connect/2]).
-export(['WAIT_FOR_DATA'/2]).


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a connect command and responds
%% @end
%%-------------------------------------------------------------------------
connect(AMF, State) ->
    ?D({"invoke - connect", AMF}),
    case AMF#amf.args of
      [_PlayerInfo, {string, Cookie}, _UserId] ->
        Session = rtmp_session:decode(Cookie),
        ?D({"Session:", Session});
      _ -> ok
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
    State.



'WAIT_FOR_DATA'({control, Type, Stream}, State) ->
  'WAIT_FOR_DATA'({send, {#channel{id = 2, timestamp = 0, type = ?RTMP_TYPE_CONTROL, stream = 0}, <<Type:16/big, Stream:32/big>>}}, State);


'WAIT_FOR_DATA'({status, Code, Stream, Description}, State) ->
  AMF = #amf{
      command = 'onStatus',
      type = invoke,
      id = 0,
      args = [null, [{code, Code}, 
                     {level, "status"}, 
                     {description, Description}]]},
  'WAIT_FOR_DATA'({invoke, AMF, Stream}, State);

'WAIT_FOR_DATA'({status, Code}, State) -> 'WAIT_FOR_DATA'({status, Code, 0, "-"}, State);

'WAIT_FOR_DATA'({invoke, #amf{} = AMF, Stream}, State) ->
  gen_fsm:send_event(self(), {send, {#channel{id = 16, timestamp = 0, type = ?RTMP_TYPE_INVOKE, stream = Stream}, AMF}}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({invoke, #amf{} = AMF}, State) -> 'WAIT_FOR_DATA'({invoke, #amf{} = AMF, 0}, State);


'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.
