-module(ems_demo).
-include("ems.hrl").
-behavior(gen_rtmp).

-export([createStream/3,closeStream/3,deleteStream/3,play/3,pause/3,stop/3,publish/3,live/3,append/3]).
-export([echoString/3,echoBoolean/3,echoNumber/3,echoArray/3,echoObject/3,echoMultiParam/3]).



createStream(From, AMF, Channel) -> gen_rtmp:createStream(From, AMF, Channel).
deleteStream(From, AMF, Channel) -> gen_rtmp:deleteStream(From, AMF, Channel).
closeStream(From, AMF, Channel)  -> gen_rtmp:closeStream(From, AMF, Channel).
play(From, AMF, Channel)         -> gen_rtmp:play(From, AMF, Channel).
pause(From, AMF, Channel)        -> gen_rtmp:pause(From, AMF, Channel).
stop(From, AMF, Channel)         -> gen_rtmp:stop(From, AMF, Channel).
live(From, AMF, Channel)         -> gen_rtmp:live(From, AMF, Channel).
append(From, AMF, Channel)       -> gen_rtmp:append(From, AMF, Channel).
publish(From, AMF, Channel)      -> gen_rtmp:publish(From, AMF, Channel).



echoString(From, AMF, Channel) ->
    ?D({"invoke - echoString",AMF#amf.args}),   
	NewAMF = AMF#amf{
		command = '_result', 
		args = [null, null]},
	gen_fsm:send_event(From, {send, {Channel, NewAMF}}).

echoBoolean(From, AMF, Channel) ->
    ?D({"invoke - echoBoolean",AMF#amf.args}),   
	NewAMF = AMF#amf{
		command = '_result', 
		args = [null, null]},
	gen_fsm:send_event(From, {send, {Channel, NewAMF}}).

echoNumber(From, AMF, Channel) ->
    ?D({"invoke - echoNumber",AMF#amf.args}),   
	NewAMF = AMF#amf{
		command = '_result', 
		args = [null, null]},
	gen_fsm:send_event(From, {send, {Channel, NewAMF}}).

echoArray(From, AMF, Channel) ->
    ?D({"invoke - echoArray",AMF#amf.args}),   
	NewAMF = AMF#amf{
		command = '_result', 
		args = [null, null]},
	gen_fsm:send_event(From, {send, {Channel, NewAMF}}).

echoObject(From, AMF, Channel) ->
    ?D({"invoke - echoObject",AMF#amf.args}),   
	NewAMF = AMF#amf{
		command = '_result', 
		args = [null, null]},
	gen_fsm:send_event(From, {send, {Channel, NewAMF}}).

echoMultiParam(From, AMF, Channel) ->
    ?D({"invoke - echoMultiParam",AMF#amf.args}),   
	NewAMF = AMF#amf{
		command = '_result', 
		args = [null, null]},
	gen_fsm:send_event(From, {send, {Channel, NewAMF}}).


