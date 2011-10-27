%%%-------------------------------------------------------------------
%%% @author Ilya Shcherbak <ilya@erlyvideo.org>
%%% Created : 27 Oct 2011 by Ilya Shcherbak <ilya@erlyvideo.org>
%%%-------------------------------------------------------------------
-module(routes).

-behaviour(gen_server).

%% API
-export([start_link/0,handler/1,handle_control/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(routes, {
	  method,
	  controller,
	  handlers=[],
	  options=[]
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  Handlers = 
    [
     "hds_handler",
     "flv_handler",
     "adobe_handler"
    ],
  {ok, #routes{handlers=Handlers}}.

handler(URL) ->
  gen_server:call(?SERVER,{handler,URL}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({handler,URL}, _From, State) ->
%% Parsing URL
  {Controller,Method,Params} = parse(URL,State),
%% Request specific parser for Controller:Method
  #routes{method=NewMethod,controller=NewController,options = NewOptions} = NewState =  case ?MODULE:handle_control({Controller,Method},State#routes{options=Params,controller=Controller}) of
    {noreply,State} ->
      {State,Params};
    {noreply,State1} ->
      State1
  end,
  {reply,{ok,{NewController,NewMethod,NewOptions}},NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

  
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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
