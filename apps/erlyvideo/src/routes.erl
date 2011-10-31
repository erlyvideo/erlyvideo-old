%%%-------------------------------------------------------------------
%%% @author Ilya Shcherbak <ilya@erlyvideo.org>
%%% Created : 27 Oct 2011 by Ilya Shcherbak <ilya@erlyvideo.org>
%%%-------------------------------------------------------------------
-module(routes).

%% API
-export([handler/1]).

-define(FILE_NAME,"routes.conf").

-record(routes, {
	  method,
	  controller,
	  routes=[],
	  vars=[],
	  options=[]
}).

init([]) ->
  Handlers = 
    case catch(ets:lookup(route_table,route)) of
      {'EXIT',_Reason} ->
	get_file_config();
      []->
	get_file_config();	
      PList ->
	io:format("~p~n",["Load from ETS table route_table.."]),
	proplists:get_value(route,PList)	
    end,
  {ok, #routes{routes=Handlers}}.

handler(URL) ->
   {ok,State}=init([]),
%% Parsing URL
   parse(URL,State).

%%%===================================================================
%%% Main parse function
%%%===================================================================

parse(URL,#routes{routes=Routes}) ->
  ParamList=case re:run(URL,"http://([-_a-zA-Z0-9.]+):*([0-9]*)/*([-_a-zA-Z0-9.=/]*)",[{capture,all_but_first,list}]) of
    {match,[_Host,_Port,RawParams]} ->
      RawParams;
     nomatch ->
      []
  end,
  Route = 
    fun(List)->
	Fun =
	  fun
	    (_F,[])->false;
	    (F,[{X,Controller,Method,Vars}|Tail])->
	      case re:run(ParamList,X,[{capture,all_but_first,list}]) of
		{match,P} ->
		  {Controller,Method,compose(P,Vars)};
		_->
		  F(F,Tail)
	      end
	  end,
	Fun(Fun,List)
    end,
  case Route(Routes) of
    false ->
      [];
    VParams ->
      VParams
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
compose(PList,VList) ->
  compose(PList,VList,[]).

compose([],[],Acc) ->
  Acc;
compose([Param|PTail],[[Var]|VTail],Acc) ->
  compose(PTail,VTail,[{Var,Param}|Acc]).

get_file_config()-> 
  {ok,RList}=case file:read_file(?FILE_NAME) of
    {ok,Data}->
      {ok,erlang:binary_to_list(Data)};
    _ -> errror
  end,
  TList=re:replace(RList,"(\\\n)","",[global,{return,list}]),
  {ok,Tokens,_}=erl_scan:string(TList),
  {ok,List} = erl_parse:parse_term(Tokens),
  get_routes(List).

get_routes(List)->
  catch(ets:new(route_table,[public,named_table])),

 Handlers = 
  [begin
     {URL,Controller,Method,ExList} = 
       case Element of
	 [{RURL,RController,RMethod,RExList}] ->
	   {RURL,RController,RMethod,RExList};
	 [{RURL,RController,RMethod}] ->
	   {RURL,RController,RMethod,[]}
       end,
     VarList = 
       case re:run(URL,"\\(*:([-_a-zA-Z0-9]+)\\)*",[{capture,all_but_first,list},global]) of
	 {match,Vars} ->
	   Vars;
	 _ ->
	   []
       end, 
     NewURL = convert_to_pattern(URL,ExList),
     {NewURL,Controller,Method,VarList} 
   end||Element <-List],

  ets:insert(route_table,{route,Handlers}),Handlers.

convert_to_pattern(URL,ExList)->
  Pattern1=exlist(URL,ExList),
  Pattern2=case re:replace(Pattern1,"(\\(*:[a-zA-Z]+\\)*)","(.*)",[global,{return,list}]) of
    Value2 when is_list(Value2) ->
      Value2;
    nomatch ->
      ""
  end,
  Pattern2.

exlist(URL,[])->
  URL;
exlist(URL,[{Name,Pattern}|ExList]) ->
  List=re:replace(URL,"\\(*"++Name++"*\\)",Pattern,[{return,list}]),
  exlist(List,ExList).


%%%===================================================================
%%% Test functions
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

make_routes_test () ->
  ?assertEqual([{"hds/(.*)/(.*)/Seg(.*)-frag(.*)",hds_handler,manifest}],get_routes([[{"hds/:video/:high/Seg(.*)-frag(.*)",hds_handler,manifest,[{":video","(.*)"}]}]])),
  ?assertEqual([{"hds/(.*)/(.*)/Seg(.*)-frag(.*)",hds_handler,manifest}],get_routes([[{"hds/:video/:high/Seg(.*)-frag(.*)",hds_handler,manifest}]])).

complex_request_test () ->
  ?assertEqual({hds_handler,manifest,["video.mp4","high","0","0"]},parse("http://my_host/hds/video.mp4/high/Seg0-frag0",#routes{routes=get_routes([[{"hds/:video/:high/Seg(.*)-frag(.*)",hds_handler,manifest}]])})).

parse_request_test () ->
  ?assertEqual({hds_handler,manifest,["video.mp4","high","0","0"]},parse("http://my_host/hds/video.mp4/high/Seg0-frag0",#routes{routes=[{"hds/(.*)/(.*)/Seg(.*)-frag(.*)",hds_handler,manifest}]})).

