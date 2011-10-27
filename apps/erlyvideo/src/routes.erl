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
	  options=[]
}).

init([]) ->
  Handlers = 
    case catch(ets:lookup(route_table,route)) of
      {'EXIT',_Reason} ->
	get_routes();
      []->
	get_routes();	
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
  ParamList=case re:run(URL,"http://([a-zA-Z0-9.]+):*([0-9]*)/*([-_a-zA-Z0-9.=/]*)",[{capture,all_but_first,list}]) of
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
	    (F,[{X,Controller,Method}|Tail])->
	      case re:run(ParamList,X,[{capture,all_but_first,list}]) of
		{match,P}->
		  {Controller,Method,P};
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
%%% Internla functions
%%%===================================================================

get_routes()->
  {ok,RList} = case file:read_file(?FILE_NAME) of
    {ok,Data}->
      {ok,erlang:binary_to_list(Data)};
    _ -> errror
  end,
  TList=re:replace(RList,"(s\\\n)","",[global,{return,list}]),
  {ok,Tokens,_}=erl_scan:string(TList),
  {ok,List} = erl_parse:parse_term(Tokens),
  Table=ets:new(route_table,[public,named_table]),
  Handlers = 
  [begin
     NewURL = convert_to_pattern(URL,ExList),
     {NewURL,Controller,Method} 
   end||[{URL,Controller,Method,ExList}]<-List],
  ets:insert(Table,{route,Handlers}),Handlers.

convert_to_pattern(URL,ExList)->
  Pattern1=exlist(URL,ExList),
  Pattern2=case re:replace(Pattern1,"(:[a-zA-Z]+)","(.*)",[global,{return,list}]) of
    Value2 when is_list(Value2) ->
      Value2;
    nomatch ->
      ""
  end,
  Pattern2.

exlist(URL,[])->
  URL;
exlist(URL,[{Name,Pattern}|ExList])->
  List=re:replace(URL,Name,Pattern,[{return,list}]),
  exlist(List,ExList).
