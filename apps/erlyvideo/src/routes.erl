%%%-------------------------------------------------------------------
%%% @author Ilya Shcherbak <ilya@erlyvideo.org>
%%% Created : 27 Oct 2011 by Ilya Shcherbak <ilya@erlyvideo.org>
%%%-------------------------------------------------------------------
-module(routes).

%% API
-export([handler/1,handle_control/2]).

-record(routes, {
	  method,
	  controller,
	  handlers=[],
	  options=[]
}).

init([]) ->
  Handlers = 
    [
     "hds_handler",
     "flv_handler",
     "adobe_handler"
    ],
  {ok, #routes{handlers=Handlers}}.

handler(URL) ->
   {ok,State}=init([]),
%% Parsing URL
  {Controller,Method,Params} = parse(URL,State),
%% Request specific parser for Controller:Method
  #routes{method=NewMethod,controller=NewController,options = NewOptions} =   case ?MODULE:handle_control({Controller,Method},State#routes{options=Params,controller=Controller}) of
    {noreply,State} ->
      {State,Params};
    {noreply,State1} ->
      State1
  end,
  {NewController,NewMethod,NewOptions}.

%%--------------------------------------------------------------------
%% Internal handler 
%% @doc
%% Handling all control handler non from generic server. 
%%
%% @spec handle_info(Info, State) -> {reply, Reply, State} |
%%                                   {noreply, State} | 
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_control({"hds",Method},#routes{options = OldOptions} = State) ->
  NewOptions=case re:run(Method,"[Ss]eg([0-9]+)-frag([0-9]+)",[{capture,all_but_first,binary}]) of
    {match,[SegNumber,FragNumber]}->
      lists:merge(OldOptions,[erlang:binary_to_list(SegNumber),erlang:binary_to_list(FragNumber)]);
    nomatch ->
       OldOptions
  end,
  {noreply,State#routes{controller=hds_handler,method=select,options=NewOptions}};

handle_control(_Handler,State) ->
  {noreply,State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

parse(URL,_State) ->
  ParamList=case re:run(URL,"http://([a-zA-Z0-9.]+):*([0-9]*)/*([-_a-zA-Z0-9.=/]*)",[{capture,all_but_first,binary}]) of
    {match,[_Host,_Port,RawParams]} ->
      RawParams;
     nomatch ->
      []
  end,
  [Controller|Tail] = re:split(ParamList,"/|%3A|&"),
  {Params,[Method]} = lists:split(length(Tail)-1,Tail),
  {erlang:binary_to_list(Controller),erlang:binary_to_list(Method),[binary_to_list(Element)||Element <- Params]}.
