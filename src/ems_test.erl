-module(ems_test).
-author('sjackson@simpleenigma.com').
-include("../include/ems.hrl").

-compile(export_all).


s() -> s("/sfe/temp/stream.flv",20).
s(Count) -> s("/sfe/temp/stream.flv",Count).
s(stream,Count)    ->  s("/sfe/temp/stream.flv",Count);
s(funnycats,Count) ->  s("/sfe/temp/funnycats.flv",Count);
s(middle,Count)    ->  s("/sfe/temp/middleagestechsupport.flv",Count);
s(FileName,_Count)  ->
	{ok,Bin} = file:read_file(FileName),
	<<Header:9/binary,Rest/binary>> = Bin,
	io:format("Header ~p~n",[ems_flv:header(Header)]),
	tags(Rest,_Count,0),
%	<<PrevTagSize:32/integer,Type:8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Rest2/binary>> = Rest,
%	<<Body:BodyLength/binary,T:30/binary,_Rest3/binary>> = Rest2,
%	{Meta,_Rest2} = tag(Rest),
%	AMF = ems_flv:parse_meta(Meta#flv_tag.body),
%	io:format("Meta: ~p~n",[AMF]),
%	?D({PrevTagSize,Type,BodyLength,TimeStamp,TimeStampExt,StreamId}),
%	?D({Body,T}),
	ok.

tag(Bin) -> 
	{#flv_tag{type=Type,timestamp=TimeStamp,body_length = Size,timestamp_ext =TimeStampExt,timestamp_abs = TimeStampAbs} = Tag,Next} = ems_flv:tag(Bin),
%	io:format("Tag: ~p~n",[Tag#flv_tag{body = <<>>}]),
	io:format("Type: ~p Size: ~p TimeStamp: ~p Ext: ~p Abs: ~p~n",[Type,Size,TimeStamp,TimeStampExt,TimeStampAbs]),
	{Tag,Next}.

tags(_Bin,Stop,Number) when Number >= Stop -> ok;
tags(Bin,Stop,Number) ->
	{_,Next} = tag(Bin),
	tags(Next,Stop,Number+1).
	


g() -> g("/sfe/temp/pack1.txt").
g(FileName) ->
	{ok,Bin} = file:read_file(FileName),
	{Channel,Rest} = ems_rtmp:header(Bin),
	{Channel,size(Rest)}.
	
m() -> m("/sfe/temp/meta.txt").
m(FileName) ->
	{ok,Bin} = file:read_file(FileName),
	?D(Bin),
	{Type,String,Next} = ems_amf:parse(Bin),
	{Type,Array,_Next} = ems_amf:parse(Next),
	{String,Array}.



















flv() -> flv("/sfe/temp/flv1-packet.txt").
flv(FileName) ->
	{ok,Bin} = file:read_file(FileName),
	{_Channel,_Rest} = ems_rtmp:header(Bin),
%	?D({Channel,size(Rest)}),
	Bin.

rflv() -> rflv("/sfe/sites/castini/htdocs/castinidemo/flv/0_320_high.flv").
rflv(FileName) ->
	{ok, IoDev} = file:open(FileName, [read,read_ahead]),
	{ok, Pos, _Header} = ems_flv:read_header(IoDev),
	{ok,Tag} = ems_flv:read_tag(IoDev,Pos),
%	?D(Header),
	Channel = #channel{id=4,timestamp=0,msg= <<>>,length=size(Tag#flv_tag.body),type=18,stream=1},
%	?D(Tag#flv_tag{body= <<>>}),
	Packet = ems_rtmp:encode(Channel,Tag#flv_tag.body),
	Packet.

f() ->
	Flv1 = flv(),
	Flv2 = rflv(),
	?D({size(Flv1),size(Flv2)}),
	Flv1 =:= Flv2.



d() -> d("/sfe/temp/result2-packet.txt").
d(FileName) ->
	{ok,Bin} = file:read_file(FileName),
	{_Header,Rest} = ems_rtmp:header(Bin),
	ems_amf:decode(Rest).


c() -> c("/sfe/temp/object.txt").
c(FileName) ->
	{ok,Bin} = file:read_file(FileName),
	Channel = decode(Bin,#ems_fsm{}),
	AMF = ems_amf:decode(Channel#channel.msg),
	io:format("~p~n~n~p~n",[Channel#channel.msg,AMF]).



decode(<<>>,State) -> 
	[Channel] = State#ems_fsm.channels,
	Channel;
decode(Bin,State) ->
	{Channel,Rest} = ems_rtmp:header(Bin),
	{NewChannel,NewChannelList} = ems_rtmp:channel_merge(Channel,State),
	ChunkSize = ems_rtmp:chunk_size(Channel,size(Rest)),
	<<Chunk:ChunkSize/binary,Next/binary>> = Rest,
	Msg = NewChannel#channel.msg,
	Message = <<Msg/binary,Chunk/binary>>,
	NextChannel = NewChannel#channel{msg=Message},
	NextState = State#ems_fsm{channels=ems_rtmp:channel_put(NextChannel,NewChannelList)},
	decode(Next,NextState).









t() -> t("/sfe/temp/test.txt","/sfe/temp/output.txt").
t(FileName) -> t(FileName,"/sfe/temp/output.txt").
t(FileName,Dest) ->
	{ok,Bin} = file:read_file(FileName),
	List = clean(binary_to_list(Bin)),
	Tokens = string:tokens(List,[10,13,32]),
	Hex = lists:map(fun(S) -> if length(S) == 2 -> erlang:list_to_integer(S,16); true -> ok end end,Tokens),
	B = list_to_binary(Hex),
	file:write_file(Dest,B),
	B.

clean(File) -> 
	T = string:tokens(File,[10,13]),
	C = lists:map(fun(Line) -> 
		string:sub_string(Line,7,54)
	end,T),
	lists:flatten(C).
	
	
	
amf(null) ->
	E = ems_amf:encode(null),
	{null,D,_} = ems_amf:parse(E),
	{D,E};
amf(true) ->
	E = ems_amf:encode(true),
	{boolean,D,_} = ems_amf:parse(E),
	{D,E};
amf(false) ->
	E = ems_amf:encode(false),
	{boolean,D,_} = ems_amf:parse(E),
	{D,E};
amf([H|_] = String) when not is_tuple(H)->
	E = ems_amf:encode(String),
	{string,D,_} = ems_amf:parse(E),
	{D,E};
amf(object) -> amf({object,[{'TeamMembers',{object,[{devs,{mixed_array,[{0,{string,"John"}}]}}]}}]});
amf({object,_} = Object) ->
	E = ems_amf:encode(Object),
	?D(E),
	{object,D,_} = ems_amf:parse(E),
	{D,E};
amf(mixed_array) -> amf({mixed_array,[{0,"test"},{1,"testing"},{"skey","testing string key"},{-12,"negative key"}]});
amf({mixed_array,List} = Array) when is_list(List) ->
	E = ems_amf:encode(Array),
	{mixed_array,D,_} = ems_amf:parse(E),
	{D,E};
amf(Number) when is_number(Number) -> 
	E = ems_amf:encode(Number),
	{number,D,_} = ems_amf:parse(E),
	{D,E}.


hex_dump(B) when binary(B) ->
    hex_dump(binary_to_list(B));
hex_dump(L) when list(L) ->
	lists:flatten([hex_dump(I) || I <- L]);
hex_dump(I) when I > 16#f ->
	[hex0((I band 16#f0) bsr 4), hex0((I band 16#0f)), 32];
hex_dump(I) -> [$0, hex0(I), 32].

hex0(10) -> $A;
hex0(11) -> $B;
hex0(12) -> $C;
hex0(13) -> $D;
hex0(14) -> $E;
hex0(15) -> $F;
hex0(I) ->  $0 +I.