-module(mpegts_file_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).


-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([init/1, handle_frame/2, handle_control/2, handle_info/2]).

-record(state, {
  reader
}).

%%%------------------------------------------------------------------------
%%% Callback functions from ems_media
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Options::list()) -> {ok, State}                   |
%%                            {ok, State, {Format,Storage}} |
%%                            {stop, Reason}
%%
%% @doc Called by ems_media to initialize specific data for current media type
%% @end
%%----------------------------------------------------------------------

init(Options) ->
  Host = proplists:get_value(host, Options),
  Name = proplists:get_value(name, Options),
  FileName = filename:join([file_media:file_dir(Host), binary_to_list(Name)]), 
  {ok, Reader} = ems_sup:start_mpegts_file_reader(FileName, [{consumer,self()}]),
  link(Reader),
  {ok, #state{reader = Reader}}.

%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {ok, State}       |
%%                                        {ok, State, tick} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({subscribe, _Client, _Options}, State) ->
  {ok, State};

handle_control(_Control, State) ->
  {ok, State}.

%%----------------------------------------------------------------------
%% @spec (Frame::video_frame(), State) -> {ok, Frame, State} |
%%                                        {noreply, State}   |
%%                                        {stop, Reason, State}
%%
%% @doc Called by ems_media to parse frame.
%% @end
%%----------------------------------------------------------------------
handle_frame(Frame, State) ->
  {ok, Frame, State}.


%%----------------------------------------------------------------------
%% @spec (Message::any(), State) ->  {noreply, State}   |
%%                                   {stop, Reason, State}
%%
%% @doc Called by ems_media to parse incoming message.
%% @end
%%----------------------------------------------------------------------
handle_info(_Message, State) ->
  {noreply, State}.








