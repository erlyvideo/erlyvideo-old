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
-module(apps_recording).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../include/ems.hrl").
-include("../../include/recorder.hrl").

-export([publish/2]).
-export(['WAIT_FOR_DATA'/2]).





'WAIT_FOR_DATA'({publish, #channel{} = Channel}, #rtmp_client{video_player = Recorder} = State) ->
  stream_media:publish(Recorder, Channel),
	{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};	


'WAIT_FOR_DATA'({stop}, #rtmp_client{video_player = #video_recorder{
                                                 device = IoDev, 
                                                 buffer = Buffer,
                                                 timer_ref = TimerRef} = _Recorder} = State) ->
	case Buffer of
		undefined -> ok;
		_ -> file:write(IoDev, lists:reverse(Buffer))
	end,
  case IoDev of
      undefined -> ok;
      _ -> file:close(IoDev)
  end,
  case TimerRef of
      undefined -> ok;
      _ -> gen_fsm:cancel_timer(TimerRef)
  end,
  ?D({"Stopping video recorder"}),
  {next_state, 'WAIT_FOR_DATA', State#rtmp_client{video_player = undefined}, ?TIMEOUT};


'WAIT_FOR_DATA'({stop}, State) ->
  ?D({"Invalid state in STOP", State#rtmp_client.video_player}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a publish command and responds
%% @end
%%-------------------------------------------------------------------------

publish(#amf{args = [{null,null},{string,Name},{string,"record"}]} = _AMF, State) -> 
  ?D({"Publish - Action - record",Name}),
  Recorder = media_provider:create(Name, record),
  ?D({"Recording", Recorder}),
  State#rtmp_client{video_player = Recorder, video_state = publishing};


publish(#amf{args = [{null,null},{string,Name},{string,"append"}]} = _AMF, State) -> 
  ?D({"Publish - Action - append",Name}),
  gen_fsm:send_event(self(), {publish, append, Name}),
  State;


publish(#amf{args = [{null,null},{string,Name},{string,"live"}]} = _AMF, State) -> 
  ?D({"Publish - Action - live",Name}),
  Recorder = media_provider:create(Name, live),
  State#rtmp_client{video_player = Recorder, video_state = publishing};

publish(#amf{args = [{null,null},{string,Name}]} = _AMF, State) -> 
  ?D({"Publish - Action - live",Name}),
  gen_fsm:send_event(self(), {publish, live, Name}),
  State.

