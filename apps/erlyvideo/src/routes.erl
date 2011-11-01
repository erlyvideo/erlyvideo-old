%%%-------------------------------------------------------------------
%%% @author Ilya Shcherbak <ilya@erlyvideo.org>
%%% Created : 27 Oct 2011 by Ilya Shcherbak <ilya@erlyvideo.org>
%%%-------------------------------------------------------------------
-module(routes).
-include_lib("proper/include/proper.hrl").

%% API
-export([init/0,handler/2,prop_tests/0,get_params/1]).

-define(FILE_NAME,"routes.conf").

-record(routes, {
	  method,
	  controller,
	  routes=[],
	  options=[]
}).

init() ->
  Handlers = get_file_config(),
  {ok, #routes{routes=Handlers}}.

handler(URL,State) ->
%% Parsing URL
  parse(URL,State).

%%%===================================================================
%%% Main parse function
%%%===================================================================

get_params(URL) ->
    case re:run(URL,"http://([-_a-zA-Z0-9.]+):*([0-9]*)/*([-_a-zA-Z0-9.=/]*)",[{capture,all_but_first,list}]) of
      {match,[_Host,_Port,RawParams]} ->
	RawParams;
      nomatch ->
	[]
    end.

parse(URL,#routes{routes=Routes}) ->
%%% Get list of parameters
  ParamList=get_params(URL),
%%% Implementation of parsing lambda 
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

%%% Calling parsing lambda
  case Route(Routes) of
    false ->
      {error,route_not_found};
    RouteList ->
      RouteList
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
compose(PList,VList) ->
  compose(PList,VList,[]).

compose([],[],Acc) ->
  lists:reverse(Acc);

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
       {ok,NewURL} = convert_to_pattern(URL,ExList),
       {NewURL,Controller,Method,VarList} 
     end||Element <-List].

convert_to_pattern(URL,ExList)->
  Convert = 
    fun() ->
	Pattern1=exlist(URL,ExList),
	  case re:replace(Pattern1,"(\\(*:[a-zA-Z]+\\)*)","(.*)",[global,{return,list}]) of
	    Value2 when is_list(Value2) ->
	      {ok,Value2};
	    nomatch ->
	      ""
	  end
    end,
  case re:run(URL,"(.*\\([^:]+)",[{capture,all_but_first,list},global]) of
    nomatch ->
      Convert();
    {match,Error} ->
      io:format("Route rule error near ~p~n",[Error]),
      error
  end.

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
  ?assertEqual([{"hds/(.*)/(.*)/Seg(.*)-frag(.*)",hds_handler,manifest,[["video"],["high"],["segment"],["fragment"]]}],get_routes([[{"hds/:video/:high/Seg(:segment)-frag(:fragment)",hds_handler,manifest,[{":video","(.*)"}]}]])),
  ?assertEqual([{"hds/(.*)/(.*)/Seg(.*)-frag(.*)",hds_handler,manifest,[["video"],["high"],["segment"],["fragment"]]}],get_routes([[{"hds/:video/:high/Seg(:segment)-frag(:fragment)",hds_handler,manifest}]])).


complex_request_test () ->
  ?assertEqual({hds_handler,manifest,[{"video","video.mp4"},{"quality","high"},{"segment","0"},{"fragment","1"}]},parse("http://my_host/hds/video.mp4/high/Seg0-frag1",#routes{routes=get_routes([[{"hds/:video/:quality/Seg(:segment)-frag(:fragment)",hds_handler,manifest}]])})).

parse_request_test () ->
  ?assertEqual({hds_handler,manifest,[{"video","video.mp4"},{"quality","high"},{"segment","0"},{"fragment","0"}]},parse("http://my_host/hds/video.mp4/high/Seg0-frag0",#routes{routes=[{"hds/(.*)/(.*)/Seg(.*)-frag(.*)",hds_handler,manifest,[["video"],["quality"],["segment"],["fragment"]]}]})).


hostname_head_char() ->
  oneof([choose($a, $z), choose($A, $Z), choose($0, $9)]).

hostname_char() ->
  oneof([choose($a, $z), choose($A, $Z), choose($0, $9), $-]).

hostname_label() ->
  ?SUCHTHAT(Label, [hostname_head_char()|list(hostname_char())],
	    length(Label) < 64).

hostname() ->
  ?SUCHTHAT(Hostname,
	    ?LET(Labels, list(hostname_label()), string:join(Labels, ".")),
	    length(Hostname) > 0 andalso length(Hostname) =< 255).

port_number() ->
  choose(1, 16#ffff).

port_str() ->
  oneof(["", ?LET(Port, port_number(), ":" ++ integer_to_list(Port))]).

server() ->
  ?LET({Hostname, PortStr}, {hostname(), port_str()}, Hostname ++ PortStr).

path() ->
  ?SUCHTHAT(Hostname,list(hostname_label()),length(Hostname) =< 255).

get_params_prop_test() ->
  ?FORALL({Server,Path}, {server(),path()},get_params("http://"++Server++"/"++string:join(Path,"/"))=:=string:join(Path,"/")). 

prop_tests()->
  proper:quickcheck(?MODULE:get_params_prop_test()).
