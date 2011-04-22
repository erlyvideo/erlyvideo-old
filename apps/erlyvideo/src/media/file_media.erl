%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        file reader
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(file_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include("ems_media.hrl").
-include_lib("kernel/include/file.hrl").
-include("../log.hrl").

-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).
-export([file_dir/1, file_format/1, default_timeout/0]).

default_timeout() ->
  ems:get_var(file_timeout, 60000).

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
  Path = proplists:get_value(url, Options),
  Host = proplists:get_value(host, Options),
  DefaultAccess = ems:get_var(file_access, Host, file),
  Access = proplists:get_value(file_access, Options, DefaultAccess),
  ClientsTimeout = proplists:get_value(clients_timeout, Options, default_timeout()),
  timer:send_after(ClientsTimeout, no_clients),
  case open_file(Access, Path, Options) of
    {error, Reason} ->
      {stop, Reason};
    {Format, Storage} ->
      State1 = State#ems_media{format = Format, storage = Storage, source_timeout = false, clients_timeout = ClientsTimeout},
      {ok, ems_media:set_media_info(State1, Format:media_info(Storage))}
  end.
  

open_file(Access, Path, Options) when is_binary(Path) ->
  open_file(Access, binary_to_list(Path), Options);

open_file(Access, Path, Options) ->
  Opts = case Access of
    file -> [binary, raw, read, {read_ahead, 100000}];
    _ -> Options
  end,
	{ok, File} = Access:open(Path, Opts),
	case file_media:file_format(Path) of
	  undefined ->
	    {error, notfound};
  	Format ->
    	InitOptions = case erlang:function_exported(Access, read_file_info, 1) of
    	  true -> 
    	    {ok, #file_info{size = Size, atime = Atime, mtime = Mtime, ctime = Ctime}} = Access:read_file_info(Path),
    	    [{size,Size},{atime,Atime},{mtime,Mtime},{ctime,Ctime}|Options];
    	  false ->
    	    Options
    	end,
    	{ok, Storage} = Format:init({Access,File}, InitOptions),
    	{Format, Storage}
  end.


%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {reply, Reply, State} |
%%                                        {stop, Reason, State} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({subscribe, _Client, _Options}, State) ->
  %% Subscribe returns:
  %% {reply, tick, State} -> client requires ticker (file reader)
  %% {reply, Reply, State} -> client is subscribed as active receiver
  %% {reply, {error, Reason}, State} -> client receives {error, Reason}
  {reply, tick, State};

handle_control({source_lost, _Source}, State) ->
  %% Source lost returns:
  %% {reply, Source, State} -> new source is created
  %% {stop, Reason, State} -> stop with Reason
  {stop, source_lost, State};

handle_control({set_source, _Source}, State) ->
  %% Set source returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {stop, refused, State};

handle_control(no_clients, State) ->
  %% no_clients returns:
  %% {reply, ok, State}      => wait forever till clients returns
  %% {reply, Timeout, State} => wait for Timeout till clients returns
  %% {noreply, State}        => just ignore and live more
  %% {stop, Reason, State}   => stops. This should be default
  {stop, normal, State};

handle_control(timeout, #ems_media{options = Options} = State) ->
  case proplists:get_value(file_access, Options, file) of
    http_file -> ?D({"Media timeout in HTTP file", proplists:get_value(url, Options)});
    _ -> ok
  end,
  {noreply, State};

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
handle_info(_Message, State) ->
  {noreply, State}.


%%-------------------------------------------------------------------------
%% @spec (Host) -> FileName::string()
%% @doc retrieves video file folder from application environment
%% @end
%%-------------------------------------------------------------------------	
file_dir(Host) ->
  ems:get_var(file_dir, Host, undefined).



file_format(Name) ->
  Readers = ems:get_var(file_formats, [mp4_reader, flv_reader, mp3_reader]),
  file_format(Name, Readers).

file_format(_Name, []) ->
  undefined;

file_format(Name, [Reader|Readers]) ->
  case Reader:can_open_file(Name) of
    true -> Reader;
    false -> file_format(Name, Readers)
  end.

