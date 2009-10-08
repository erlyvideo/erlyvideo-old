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

-export([publish/2]).
-export(['WAIT_FOR_DATA'/2]).

%% rsacon: TODOD move this to ems.hrl ist her only for testing purpose
%% or make it an application confiuration environment variable
-define(VIDEO_WRITE_BUFFER, 20000). 


'WAIT_FOR_DATA'({publish, record, Name}, State) when is_list(Name) ->
	FileName = filename:join([ems_play:file_dir(), Name]),
	Header = ems_flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	case file:open(FileName, [write, {delayed_write, 1024, 50}]) of
		{ok, IoDev} ->
			NextState = State#ems_fsm{video_buffer=[Header],type=record,video_device=IoDev,video_file_name=FileName,video_ts_prev=0},
			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
		_ ->
			{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}
	end;

%% rsaccon: TODO get last timstamp of the exisiting FLV file, without that it won't playback propperly 	
'WAIT_FOR_DATA'({publish, append, Name}, State) when is_list(Name) ->
	FileName = filename:join([ems_play:file_dir(), Name]),
	case file:open(FileName, [write, append]) of
		{ok, IoDev} ->
			NextState = State#ems_fsm{type=record_append,video_device=IoDev,video_file_name=FileName,video_ts_prev=0},
			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
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
    NextState = State#ems_fsm{type=broadcast,video_file_name=Name, video_ts_prev = 0},
	{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};

'WAIT_FOR_DATA'({publish,Channel}, #ems_fsm{type = broadcast, 
                                            video_ts_prev = PrevTs,
                                            video_file_name = Name} = State) when is_record(Channel,channel) ->
	NextTimeStamp = PrevTs + Channel#channel.timestamp,    
%	?D({"Broadcast",Channel#channel.id,Channel#channel.type,size(Channel#channel.msg),NextTimeStamp}),
	Packet = ems_rtmp:encode(Channel#channel{id = ems_play:channel_id(Channel#channel.type,1), timestamp = NextTimeStamp}),        
    ems_cluster:broadcast(Name, Packet),
    NextState = State#ems_fsm{video_ts_prev=NextTimeStamp},
    {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
    	
'WAIT_FOR_DATA'({publish,Channel}, #ems_fsm{video_ts_prev = PrevTs, 
                                            video_device = IoDev, 
                                            video_buffer = Buffer} = State) when is_record(Channel,channel) ->
	?D({"Record",Channel#channel.type,size(Channel#channel.msg),Channel#channel.timestamp,PrevTs}),
	{Tag,NextTimeStamp} = ems_flv:to_tag(Channel,PrevTs),
	FlvChunk = [Tag | Buffer],	
	Size = size(list_to_binary(FlvChunk)),
	NextState = if
		(Size > ?VIDEO_WRITE_BUFFER) ->
			file:write(IoDev, lists:reverse(Buffer)),
			State#ems_fsm{video_buffer=[Tag],video_ts_prev=NextTimeStamp};
		true ->
			State#ems_fsm{video_buffer=FlvChunk,video_ts_prev=NextTimeStamp}
	end,
	{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};	

'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a publish command and responds
%% @end
%%-------------------------------------------------------------------------
publish(AMF, State) -> 
    ?D("invoke - publish"),
    Args = AMF#amf.args,
    case Args of
        [{null,null},{string,Name},{string,Action}] ->
            case list_to_atom(Action) of
                record -> 
                    ?D({"Publish - Action - record",Name}),
                    gen_fsm:send_event(self(), {publish, record, Name});
                append -> 
                     ?D({"Publish - Action - append",Name}),
                     gen_fsm:send_event(self(), {publish, append, Name});
                live -> 
                    ?D({"Publish - Action - live",Name}),
                    gen_fsm:send_event(self(), {publish, live, Name});
                _OtherAction -> 
                    ?D({"Publish Ignoring - ", _OtherAction})
            end;
		[{null,null},{string,Name}] -> % second arg is optional
			?D({"Publish - Action - live",Name}),
            gen_fsm:send_event(self(), {publish, live, Name});
        _ -> ok
    end,
    State.

