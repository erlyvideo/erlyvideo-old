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

%% rsacon: TODOD move this to ems.hrl ist her only for testing purpose
%% or make it an application confiuration environment variable
-define(VIDEO_WRITE_BUFFER, 20000). 


'WAIT_FOR_DATA'({send, {live, Channel, Recorder}}, #ems_fsm{video_player = Recorder, video_state = publishing} = State) ->
  {next_state, 'WAIT_FOR_DATA', State};


'WAIT_FOR_DATA'({send, {live, #channel{msg = Body} = Channel, Stream}}, State) ->
  % ?D({"Sending live stream", Channel#channel.timestamp}),
  ems_fsm:'WAIT_FOR_DATA'({send, {Channel, Body}}, State);
  % {next_state, 'WAIT_FOR_DATA', State};

'WAIT_FOR_DATA'({publish, record, Name}, State) when is_list(Name) ->
  Recorder = media_provider:open(Name, record),
  media_entry:subscribe(Recorder, self()),
  {next_state, 'WAIT_FOR_DATA', State#ems_fsm{video_player = Recorder, video_state = publishing}};

%% rsaccon: TODO get last timstamp of the exisiting FLV file, without that it won't playback propperly 	
'WAIT_FOR_DATA'({publish, append, Name}, State) when is_list(Name) ->
	FileName = filename:join([file_play:file_dir(), Name]),
	case file:open(FileName, [write, append]) of
		{ok, IoDev} ->
		  Recorder = #video_recorder{type=record_append, device = IoDev, file_name = FileName, ts_prev=0},
			{next_state, 'WAIT_FOR_DATA', State#ems_fsm{video_player = Recorder}, ?TIMEOUT};
	    _ ->
    		{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}
    end;	
    
'WAIT_FOR_DATA'({publish, live, Name}, State) when is_list(Name) ->        
    ems_cluster:broadcast(Name),
    %% hack - empty audio
    StreamId=1,
    Id=ems_play:channel_id(?RTMP_TYPE_AUDIO, StreamId),
    TimeStamp=0,
    Length=0,
    Type=?RTMP_TYPE_AUDIO,
    Rest = <<>>,
    FirstPacket = <<?RTMP_HDR_NEW:2,Id:6,TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>,    
    ems_cluster:broadcast(Name, FirstPacket),
    %% hack end
    Recorder = #video_recorder{type=broadcast, file_name = Name, ts_prev = 0},
	{next_state, 'WAIT_FOR_DATA', State#ems_fsm{video_player = Recorder}, ?TIMEOUT};

'WAIT_FOR_DATA'({publish,Channel}, #ems_fsm{video_player = #video_recorder{
                                              type = broadcast, 
                                              ts_prev = PrevTs,
                                              file_name = Name} = Recorder} = State) when is_record(Channel,channel) ->
	NextTimeStamp = PrevTs + Channel#channel.timestamp,    
%	?D({"Broadcast",Channel#channel.id,Channel#channel.type,size(Channel#channel.msg),NextTimeStamp}),
	Packet = ems_rtmp:encode(Channel#channel{id = ems_play:channel_id(Channel#channel.type,1), timestamp = NextTimeStamp}),        
    ems_cluster:broadcast(Name, Packet),
    {next_state, 'WAIT_FOR_DATA', State#ems_fsm{video_player = Recorder#video_recorder{ts_prev=NextTimeStamp}}, ?TIMEOUT};
    	
'WAIT_FOR_DATA'({publish, #channel{} = Channel}, #ems_fsm{video_player = Recorder} = State) ->
  media_entry:publish(Recorder, Channel),
	{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};	


'WAIT_FOR_DATA'({stop}, #ems_fsm{video_player = #video_recorder{
                                                 device = IoDev, 
                                                 buffer = Buffer,
                                                 file_name = FileName,
                                                 type = Type,
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
  case Type of
      live -> 
          ems_cluster:unsubscribe(FileName, self());
      wait -> 
          ems_cluster:unsubscribe(FileName, self());
      broadcast ->
          ems_cluster:stop_broadcast(FileName);
      _ -> 
          ok
  end,
  ?D({"Stopping video recorder"}),
  {next_state, 'WAIT_FOR_DATA', State#ems_fsm{video_player = undefined}, ?TIMEOUT};


'WAIT_FOR_DATA'({stop}, State) ->
  ?D({"Invalid state in STOP", State#ems_fsm.video_player}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a publish command and responds
%% @end
%%-------------------------------------------------------------------------

publish(#amf{args = [{null,null},{string,Name},{string,"record"}]} = _AMF, State) -> 
  ?D({"Publish - Action - record",Name}),
  gen_fsm:send_event(self(), {publish, record, Name}),
  State;


publish(#amf{args = [{null,null},{string,Name},{string,"append"}]} = _AMF, State) -> 
  ?D({"Publish - Action - append",Name}),
  gen_fsm:send_event(self(), {publish, append, Name}),
  State;


publish(#amf{args = [{null,null},{string,Name},{string,"live"}]} = _AMF, State) -> 
  ?D({"Publish - Action - live",Name}),
  gen_fsm:send_event(self(), {publish, live, Name}),
  State;

publish(#amf{args = [{null,null},{string,Name}]} = _AMF, State) -> 
  ?D({"Publish - Action - live",Name}),
  gen_fsm:send_event(self(), {publish, live, Name}),
  State.

