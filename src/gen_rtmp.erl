%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @doc        Generalized RTMP applicaiton behavior module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%---------------------------------------------------------------------------------------
-module(gen_rtmp).
-author('sjackson@simpleenigma.com').
-include("../include/ems.hrl").

-export([connect/3,createStream/3,play/3,deleteStream/3,closeStream/3,pause/3,stop/3,publish/3,live/3,append/3]).

-export([behaviour_info/1]).


%%-------------------------------------------------------------------------
%% @spec (Callbacks::atom()) -> CallBackList::list()
%% @doc  List of require funcations in a RTMP application
%% @hidden
%% @end
%%-------------------------------------------------------------------------
behaviour_info(callbacks) -> [{createStream,3},{play,3},{stop,3},{pause,3},{deleteStream,3},{closeStream,3},{publish,3},{live,3},{append,3}];
behaviour_info(_Other) -> undefined.



%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a connect command and responds
%% @end
%%-------------------------------------------------------------------------
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

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a createStream command and responds
%% @end
%%-------------------------------------------------------------------------
createStream(From, AMF, Channel) -> 
    ?D("invoke - createStream"),   
	NewAMF = AMF#amf{
		command = '_result', 
		args= [null, generate_stream_id()]},
	gen_fsm:send_event(From, {send, {Channel,NewAMF}}).


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a deleteStream command and responds
%% @end
%%-------------------------------------------------------------------------
deleteStream(_From, _AMF, _Channel) ->  
    ?D("invoke - deleteStream").

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a play command and responds
%% @end
%%-------------------------------------------------------------------------
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



%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a pause command and responds
%% @end
%%-------------------------------------------------------------------------
pause(From, _AMF, _Channel) -> 
    ?D("invoke - pause"),
    gen_fsm:send_event(From, {pause}). 

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a publish command and responds
%% @end
%%-------------------------------------------------------------------------
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

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a stop command and responds
%% @end
%%-------------------------------------------------------------------------
stop(From, _AMF, _Channel) -> 
    ?D("invoke - stop"),
    gen_fsm:send_event(From, {stop}). 

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a live command and responds
%% @end
%%-------------------------------------------------------------------------
live(From, _AMF, _Channel) -> 
    ?D("invoke - live"),
    gen_fsm:send_event(From, {stop}). 

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a append command and responds
%% @end
%%-------------------------------------------------------------------------
append(From, _AMF, _Channel) -> 
    ?D("invoke - append"),
    gen_fsm:send_event(From, {stop}). 



%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a closeStream command and responds
%% @end
%%-------------------------------------------------------------------------
closeStream(From, _AMF, _Channel) ->
    ?D("invoke - closeStream"),
    gen_fsm:send_event(From, {stop}). 



%%-------------------------------------------------------------------------
%% @spec (Name::string()) -> FileName::string()
%% @doc ensures that the file name ends with a .flv extention
%% @end
%%-------------------------------------------------------------------------
filename(Name) ->
	case filename:extension(Name) of
		".flv" -> Name;
		".FLV" -> Name;
		_      -> Name ++ ".flv"
	end.


%%-------------------------------------------------------------------------
%% @spec () -> any()
%% @doc
%% @end
%%-------------------------------------------------------------------------
generate_stream_id() ->  %% provisory !!!!!!!
   1.  %% TODO: replace by (distrubuted) gen_server to get a unique Id 