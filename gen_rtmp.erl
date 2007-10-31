-module(gen_rtmp).
-author('sjackson@simpleenigma.com').
-include("ems.hrl").

-export([connect/3,createStream/3,play/3,deleteStream/3,closeStream/3,pause/3,stop/3,publish/3,live/3,append/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{createStream,3},{play,3},{stop,3},{pause,3},{deleteStream,3},{closeStream,3},{publish,3},{live,3},{append,3}];
behaviour_info(_Other) -> undefined.




connect(From, AMF, Channel) ->
    ?D("invoke - connect"),   
	NewAMF = AMF#amf{
		command = '_result', 
		args= [null,
			[{level, "status"}, 
			{code, "NetConnection.Connect.Success"}, 
			{description, "Connection succeeded."}]
			  ]},
	gen_fsm:send_event(From, {send, {Channel,NewAMF}}).


createStream(From, AMF, Channel) -> 
    ?D("invoke - createStream"),   
	NewAMF = AMF#amf{
		command = '_result', 
		args= [null, generate_stream_id()]},
	gen_fsm:send_event(From, {send, {Channel,NewAMF}}).


deleteStream(_From, _AMF, _Channel) ->  
    ?D("invoke - deleteStream").

play(From, AMF, Channel) -> 
    ?D("invoke - play"),
	NextChannel = Channel#channel{id=4},
	[_Null,{string,Name}] = AMF#amf.args,
	NewAMF = AMF#amf{
		command = 'onStatus', 
		args= [null,[{level, "status"}, 
                    {code, "NetStream.Play.Reset"}, 
                    {description, "Resetting NetStream."},
                    {details, Name},
                    {clientid, NextChannel#channel.stream}]]},
	gen_fsm:send_event(From, {send, {NextChannel,NewAMF}}),
	NewAMF2 = AMF#amf{
		command = 'onStatus', 
		args= [null,[{level, "status"}, 
                    {code, "NetStream.Play.Start"}, 
                    {description, "Start playing."},
                    {details, Name},
                    {clientid, NextChannel#channel.stream}]]},
	gen_fsm:send_event(From, {send, {NextChannel,NewAMF2}}),
    gen_fsm:send_event(From, {play, filename(Name), NextChannel#channel.stream}).



pause(From, _AMF, _Channel) -> 
    ?D("invoke - pause"),
    gen_fsm:send_event(From, {pause}). 

publish(From, AMF, _Channel) -> 
    ?D("invoke - publish"),
	Args = AMF#amf.args,
	case Args of
		[{null,null},{string,Name},{string,Action}] ->
			case list_to_atom(Action) of
				record -> 
					?D({"Publish - Action - record",Name}),
					gen_fsm:send_event(From, {record,filename(Name)});
				_OtherAction -> ?D({"Publish Ignoring - ", _OtherAction})
			end;
		_ -> ok
	end. 

stop(From, _AMF, _Channel) -> 
    ?D("invoke - stop"),
    gen_fsm:send_event(From, {stop}). 

live(From, _AMF, _Channel) -> 
    ?D("invoke - live"),
    gen_fsm:send_event(From, {stop}). 

append(From, _AMF, _Channel) -> 
    ?D("invoke - append"),
    gen_fsm:send_event(From, {stop}). 



closeStream(From, _AMF, _Channel) ->
    ?D("invoke - closeStream"),
    gen_fsm:send_event(From, {stop}). 



filename(Name) ->
	case filename:extension(Name) of
		".flv" -> Name;
		".FLV" -> Name;
		_      -> Name ++ ".flv"
	end.


generate_stream_id() ->  %% provisory !!!!!!!
   1.  %% TODO: replace by (distrubuted) gen_server to get a unique Id 