-module(mpegts_file_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include("../../include/ems_media.hrl").
-include("../../include/ems.hrl").


-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

-export([can_open_file/1]).

-record(state, {
  reader
}).


can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  filename:extension(Name) == ".ts".
  

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

init(Media, Options) ->
  Host = proplists:get_value(host, Options),
  Name = proplists:get_value(name, Options),
  FileName = filename:join([file_media:file_dir(Host), binary_to_list(Name)]), 
  {ok, Reader} = ems_sup:start_mpegts_file_reader(FileName, [{consumer,self()}]),
  erlang:monitor(process, Reader),
  link(Reader),
  State = #state{reader = Reader},
  {ok, Media#ems_media{state = State}}.

%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {reply, Reply, State} |
%%                                        {stop, Reason, State} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control(timeout, State) ->
  {stop, normal, State};

handle_control(no_clients, State) ->
  %% no_clients returns:
  %% {reply, ok, State}      => wait forever till clients returns
  %% {reply, Timeout, State} => wait for Timeout till clients returns
  %% {noreply, State}        => just ignore and live more
  %% {stop, Reason, State}   => stops. This should be default
  {stop, normal, State};


handle_control(_Control, State) ->
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
handle_info({'DOWN', _Ref, process, Player, _Reason}, #state{reader = Player} = Media) ->
  ?D({"file player over"}),
  {stop, normal, Media};
  
handle_info(_Message, State) ->
  {noreply, State}.









