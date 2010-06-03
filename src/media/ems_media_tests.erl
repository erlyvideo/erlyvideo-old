-module(ems_media_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(ems_media).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/ems_media.hrl").


-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

-export([test_source/0]).

-record(test, {

}).


test_source() ->
  receive
    Msg -> ok
  end.

source_timeout_test_() ->
  {setup,
    fun() -> 
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(ems_media_tests, [{source_timeout, 3}]),
      erlang:register(ems_media_test, Pid),
      
      (catch erlang:exit(whereis(ems_media_test_source), kill)),
      Source = spawn_link(?MODULE, test_source, []),
      erlang:register(ems_media_test_source, Source),
      
      ems_media:set_source(Pid, Source),
      {ok, Pid} 
    end,
    fun({ok, Pid}) -> erlang:exit(Pid, shutdown) end,
    fun(_) ->
        ?assertMatch(Pid when is_pid(Pid), whereis(ems_media_test)),
        timer:sleep(4),
        ?assertMatch(Pid when is_pid(Pid), whereis(ems_media_test)),
        ems_media_test_source ! stop,
        timer:sleep(4),
        ?assertEqual(undefined, whereis(ems_media_test))
    end
  }.
  
clients_timeout_test() ->
  ?assertEqual(a, a).
  

%%%------------------------------------------------------------------------
%%% Callback functions from ems_media
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Media::ems_media(), Options::list()) -> {ok, Media::ems_media()} |
%%                                                {stop, Reason}
%%
%% @doc Called by ems_media to initialize specific data for current media type
%% @end
%%----------------------------------------------------------------------

init(State, Options) ->
  {ok, State}.

%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {reply, Reply, State} |
%%                                        {stop, Reason, State} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({subscribe, _Client, _Options}, #ems_media{} = State) ->
  %% Subscribe returns:
  %% {reply, tick, State}  => client requires ticker (file reader)
  %% {reply, Reply, State} => client is subscribed as active receiver and receives custom reply
  %% {noreply, State}      => client is subscribed as active receiver and receives reply ``ok''
  %% {reply, {error, Reason}, State} => client receives {error, Reason}
  {noreply, State};

handle_control({unsubscribe, _Client}, #ems_media{} = State) ->
  %% Unsubscribe returns:
  %% {reply, Reply, State} => client is unsubscribed inside plugin, but not rejected from ets table
  %% {noreply, State}      => client is unsubscribed in usual way.
  %% {reply, {error, Reason}, State} => client receives {error, Reason} 
  {noreply, State};

handle_control({seek, _Client, _BeforeAfter, _DTS}, #ems_media{} = State) ->
  %% seek returns:
  %% {reply, {NewPos, NewDTS}, State} => media knows how to seek in storage
  %% {stop, Reason, State}  => stop with Reason
  %% {noreply, State}       => default action is to seek in storage.
  {noreply, State};

handle_control({source_lost, _Source}, #ems_media{} = State) ->
  %% Source lost returns:
  %% {reply, Source, State} => new source is created
  %% {stop, Reason, State}  => stop with Reason
  %% {noreply, State}       => default action. it is stop
  {stop, normal, State};

handle_control({set_source, _Source}, #ems_media{} = State) ->
  %% Set source returns:
  %% {reply, NewSource, State} => source is rewritten
  %% {noreply, State}          => just ignore setting new source
  %% {stop, Reason, State}     => stop after setting
  {noreply, State};

handle_control({set_socket, _Socket}, #ems_media{} = State) ->
  %% Set socket returns:
  %% {reply, Reply, State}  => the same as noreply
  %% {noreply, State}       => just ignore
  %% {stop, Reason, State}  => stops
  {noreply, State};

handle_control(no_clients, #ems_media{} = State) ->
  %% no_clients returns:
  %% {reply, ok, State}      => wait forever till clients returns
  %% {reply, Timeout, State} => wait for Timeout till clients returns
  %% {noreply, State}        => just ignore and live more
  %% {stop, Reason, State}   => stops. This should be default
  {stop, normal, State};

handle_control(timeout, #ems_media{} = State) ->
  {stop, normal, State};

handle_control(_Control, #ems_media{} = State) ->
  {noreply, State}.

%%----------------------------------------------------------------------
%% @spec (Frame::video_frame(), State) -> {reply, Frame, State} |
%%                                        {noreply, State}   |
%%                                        {stop, Reason, State}
%%
%% @doc Called by ems_media to parse frame.
%% @end
%%----------------------------------------------------------------------
handle_frame(Frame, State) ->
  {reply, Frame, State}.


%%----------------------------------------------------------------------
%% @spec (Message::any(), State) ->  {noreply, State}   |
%%                                   {stop, Reason, State}
%%
%% @doc Called by ems_media to parse incoming message.
%% @end
%%----------------------------------------------------------------------
handle_info(_Message, State) ->
  {noreply, State}.

  