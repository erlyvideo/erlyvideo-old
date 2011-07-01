%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        MPEG TS file reader
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlang-mpegts.
%%% 
%%% erlang-mpegts is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-mpegts is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-mpegts.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(mpegts_file_reader).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include("log.hrl").

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, read_frames/1]).

-record(file_reader, {
  reader,
  path,
  file,
  offset = 0,
  first_dts,
  timer_start,
  consumer
}).


start_link(Path, Options) ->
  {ok, Reader} = gen_server:start_link(?MODULE, [Path, Options], []),
  Reader ! timeout,
  {ok, Reader}.
  
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
init([Path, Options]) when is_binary(Path) ->
  init([binary_to_list(Path), Options]);

init([Path, Options]) ->
  ?D({Path,Options}),
  Consumer = proplists:get_value(consumer, Options),
  true = is_pid(Consumer),
  {ok, File} = file:open(Path, [read,binary,{read_ahead,131072},raw]),
  {ok, Reader} = mpegts_reader:init([[{raw_es,false}]]),
  erlang:monitor(process, Consumer),
  {ok, #file_reader{reader = Reader, consumer = Consumer, path = Path, file = File}}.


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
handle_cast(_Msg, State) ->
   {noreply, State}.

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

handle_info(timeout, #file_reader{} = State) ->
  {State1, Frames} = read_frames(State),
  {State2, Timeout} = send_frames(State1, Frames),
  case Timeout of
    eof ->
      {stop, normal, State2};
    0 ->
      handle_info(timeout, State2);
    _ ->
      {noreply, State2, Timeout}
  end;
  
handle_info({'DOWN', _Ref, process, _Consumer, _Reason}, State) ->
  {stop, normal, State};
  
handle_info(_Info, State) ->
  ?D({"Unknown message", _Info}),
  {noreply, State}.

send_frames(State, eof) ->
  {State, eof};
send_frames(State, []) -> 
  {State, 0};
send_frames(#file_reader{first_dts = undefined, consumer = Consumer} = State, [#video_frame{dts = DTS} = Frame]) ->
  Consumer ! Frame,
  {TimerStart, _} = erlang:statistics(wall_clock),
  {State#file_reader{first_dts = DTS, timer_start = TimerStart}, 0};
send_frames(#file_reader{first_dts = DTS, timer_start = TimerStart, consumer = Consumer} = State, [#video_frame{dts = LastDTS} = Frame]) ->
  Consumer ! Frame,
  DTSDelta = LastDTS - DTS,
  {Now, _} = erlang:statistics(wall_clock),
  RealDelta = Now - TimerStart,
  Timeout = if
    DTSDelta > RealDelta -> round(DTSDelta - RealDelta);
    true -> 0
  end,
  {State, Timeout};
send_frames(#file_reader{consumer = Consumer} = State, [Frame|Frames]) ->
  Consumer ! Frame,
  send_frames(State, Frames).

read_frames(#file_reader{file = File, offset = Offset, reader = Reader} = State) ->
  NewOffset = Offset + 188,
  case file:pread(File, Offset, 188) of
    {ok, Data} ->
      {ok, Reader1, Frames} = mpegts_reader:decode(Data, Reader),
      {State#file_reader{offset = NewOffset, reader = Reader1}, Frames};
    _ ->
      {State, eof}
  end.


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
