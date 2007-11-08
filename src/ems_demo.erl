%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Demo RTMP application module
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
-module(ems_demo).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").
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


