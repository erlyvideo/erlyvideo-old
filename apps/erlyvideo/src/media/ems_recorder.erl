%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Unified recorder of media
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
-module(ems_recorder).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include("../log.hrl").


%% External API
-export([start_link/2, stop/2, start_recorder/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(recorder, {
  writer,
  format
}).

start_recorder(Host, Name, Options) ->
  (catch stop(Host,Name)),
  ems_sup:start_recorder(Host, Name, Options).

start_link(Media, Options) ->
  gen_server:start_link(?MODULE, [Media, Options], []).

stop(Host, Name) ->
  case media_provider:find(Host, Name) of
    {ok, Media} -> 
    case ems_media:get(Media, recorder_pid) of
      undefined -> ok;
      Pid -> gen_server:cast(Pid, stop)
    end;
    Else -> Else
  end.



%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------



%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([Media, Options]) ->
  ems_media:play(Media, [{stream_id,1}]),
  SortBuffer = proplists:get_value(sort_buffer, Options, 10),
  URL = proplists:get_value(url, Options),
  Host = proplists:get_value(host, Options),
	FileName = ems:pathjoin(file_media:file_dir(Host), binary_to_list(URL)),
	ok = filelib:ensure_dir(FileName),
	Mode = proplists:get_value(type, Options, write),
	Format = flv_writer,
	?D({writing,Host,URL, FileName}),
  {ok, Writer} = flv_writer:init_file(FileName, [{mode,Mode},{sort_buffer,SortBuffer}]),
  ems_media:set(Media, recorder_pid, self()),
  erlang:monitor(process, Media),
	{ok, #recorder{writer = Writer, format = Format}}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(stop, #recorder{writer = Writer, format = Format} = Recorder) ->
  {ok, Writer1} = Format:write_frame(eof, Writer),
  {stop, normal, Recorder#recorder{writer = Writer1}};
  
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(#video_frame{} = Frame, #recorder{writer = Writer, format = Format} = Recorder) ->
  {ok, Writer1} = Format:write_frame(Frame, Writer),
  {noreply, Recorder#recorder{writer = Writer1}};
  
handle_info({'DOWN', _, process, _Client, _Reason}, Server) ->
  {stop, normal, Server};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
