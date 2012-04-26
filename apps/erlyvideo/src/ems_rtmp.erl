%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        RTMP session for erlyvideo
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(ems_rtmp).
-author('Max Lapshin <max@maxidoors.ru>').


-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-include("log.hrl").


-export([create_client/1]).
-export([collect_stats/1]).
-export([init/1, handle_control/2, handle_info/2, handle_rtmp_call/2]).
% -export([metadata/1, metadata/2]).


init(Session) ->
  (catch ems_network_lag_monitor:watch(self())),
  {ok, Session}.

%%-------------------------------------------------------------------------
%% @spec create_client(Socket)  -> {ok, Pid}
%% @doc Very important function. rtmp_listener calls it to
%% create new process, that will accept socket.
%% @end
%%-------------------------------------------------------------------------
create_client(Socket) ->
  {ok, Pid} = rtmp_sup:start_rtmp_session(Socket, ?MODULE),
  {ok, Pid}.


%%-------------------------------------------------------------------------
%% @spec collect_stats(Host)  -> Stats::proplist
%% @doc Asynchronously asks all rtmp clients to tell about their info
%% @end
%%-------------------------------------------------------------------------
collect_stats(_Host) ->
  Pids = [Pid || {_, Pid, _, _} <- supervisor:which_children(rtmp_session_sup), Pid =/= self()],
  Info = ems:multicall(Pids, info, 1000),
  lists:sort(fun({_,Inf1}, {_,Inf2}) -> proplists:get_value(addr, Inf1) < proplists:get_value(addr, Inf2) end, Info).


handle_control({start_stream, Media, Opts}, Session) ->
  State2_ = rtmp_session:send_frame(metadata(Media, Opts), Session),
  {ok, State2_};

handle_control(terminate, Session) ->
  Host = rtmp_session:get(Session, host),
  Addr = rtmp_session:get(Session, addr),
  UserId = rtmp_session:get(Session, user_id),
  SessionId = rtmp_session:get(Session, session_id),
  Recv = rtmp_session:get(Session, bytes_recv),
  Sent = rtmp_session:get(Session, bytes_sent),
  PlayStats = rtmp_session:get(Session, play_stats),
  ems_log:access(Host, "DISCONNECT ~s ~s ~p ~p ~p ~p", [Addr, Host, UserId, SessionId, Recv, Sent]),

  Stats = [{host,Host},{recv_oct,Recv},{sent_oct,Sent},{addr,Addr},{user_id,UserId},{session_id,SessionId}|PlayStats],
  ems_event:user_disconnected(Host, self(), Stats),
  (catch rtmp_session:call_function(Host, logout, [Session])),
  ok;

handle_control({connected, UserId, SessionId}, Session) ->
  Host = rtmp_session:get(Session, host),
  ems_event:user_connected(Host, self(), [{user_id,UserId}, {session_id,SessionId}]),
  {ok, Session};

handle_control({close_stream, _StreamId, Player, Recording}, Session) ->
  ems_media:stop(Player),
  case Recording of
    true ->
      ems_media:set_source(Player, undefined),
      ok;
      % media_provider:remove(Host, Name);
    _ -> ok
  end,
  {ok, Session};

handle_control({unhandled_call, #rtmp_funcall{command = Command, args = Args} = AMF}, Session) ->
  ems_log:error(rtmp_session:get(Session, host), "Failed RTMP funcall: ~p(~p)", [Command, Args]),
  rtmp_session:fail(Session, AMF),
  {ok, Session};


handle_control({rtmp, #rtmp_message{stream_id = StreamId, type = buffer_size, body = BufferSize}}, State) ->
  Player = case rtmp_session:get_stream(StreamId, State) of
    Stream when is_tuple(Stream) -> rtmp_stream:get(Stream, pid);
    _ -> undefined
  end,
  if is_pid(Player) -> ems_media:play_setup(Player, [{client_buffer, BufferSize}]);
  true -> ok end,
  {ok, State};

handle_control(_Control, Session) ->
  {ok, Session}.


handle_info(Message, Session) ->
  Host = rtmp_session:get(Session, host),
  case ems:try_method_chain(Host, handle_info, [Message, Session]) of
    {unhandled} -> {noreply, Session};
    unhandled -> {noreply, Session};
    {stop, Reason, State1} -> {stop, Reason, State1};
    {noreply, State1} -> {noreply, State1};
    State1 when is_tuple(State1) andalso element(1, State1) == rtmp_session -> {noreply, State1}
  end.


handle_rtmp_call(Session, #rtmp_funcall{command = connect, args = [{object, PlayerInfo}|_]} = AMF) ->

  URL = proplists:get_value(tcUrl, PlayerInfo),
  {match, [_Proto, HostName, _Port, _Path]} = re:run(URL, "(.*)://([^/:]+)([^/]*)/?(.*)$", [{capture,all_but_first,binary}]),
  Host = ems:host(HostName),
  handle_rtmp_call1(rtmp_session:set(Session, host, Host), AMF);

handle_rtmp_call(Session, AMF) ->
  handle_rtmp_call1(Session, AMF).

handle_rtmp_call1(Session, #rtmp_funcall{} = AMF) ->
  Host = rtmp_session:get(Session, host),
  call_mfa(ems:get_var(rtmp_handlers, Host, [trusted_login, remove_useless_prefix, apps_streaming, apps_recording]), Session, AMF).


call_mfa([], _Session, #rtmp_funcall{}) ->
  unhandled;

call_mfa([Module|Modules], Session, #rtmp_funcall{command = Command} = AMF) ->
  case code:is_loaded(mod_name(Module)) of
    false ->
      case code:load_file(mod_name(Module)) of
        {module, _ModName} -> ok;
        _ -> erlang:error({cant_load_file, Module})
      end;
    _ -> ok
  end,
  % ?D({"Checking", Module, Command, ems:respond_to(Module, Command, 2)}),
  case ems:respond_to(Module, Command, 2) of
    true ->
      case Module:Command(Session, AMF) of
        unhandled ->
          call_mfa(Modules, Session, AMF);
        {unhandled, NewState, NewAMF} ->
          call_mfa(Modules, NewState, NewAMF);
        Reply ->
          Reply
      end;
    false ->
      call_mfa(Modules, Session, AMF)
  end.


mod_name(Mod) when is_tuple(Mod) -> element(1, Mod);
mod_name(Mod) -> Mod.




% metadata(Media) when is_pid(Media) ->
%   metadata(Media, []).
%
% %%----------------------------------------------------------------------
% %% @spec (Media::pid(), Options::proplist()) -> Metadata::video_frame()
% %%
% %% @doc Returns video_frame, prepared to send into flash
% %% @end
% %%----------------------------------------------------------------------
metadata(Media, Options) when is_pid(Media) ->
  MediaInfo = ems_media:media_info(Media),
  Info1 = add_metadata_options(MediaInfo, Options),
  metadata_frame(Info1, Options).


add_metadata_options(#media_info{} = MediaInfo, []) -> MediaInfo;
add_metadata_options(#media_info{} = MediaInfo, [{duration,Duration}|Options]) when Duration =/= undefined -> add_metadata_options(MediaInfo#media_info{duration = Duration}, Options);
add_metadata_options(#media_info{} = MediaInfo, [_|Options]) -> add_metadata_options(MediaInfo, Options).


metadata_frame(#media_info{options = Options, duration = Duration} = Media, Opts) ->
  Meta = lists:map(fun({K,V}) when is_atom(V) -> {K, atom_to_binary(V,latin1)};
                      ({K,V}) when is_tuple(V) -> {K, iolist_to_binary(io_lib:format("~p", [V]))};
                      (Else) -> Else end, Options),
  Meta1 = lists:ukeymerge(1, lists:keysort(1,Meta), video_parameters(Media)),
  DurationMeta = case Duration of
    undefined -> [];
    _ -> [{duration, Duration / 1000}]
  end,
  DTS = proplists:get_value(dts, Opts),
  #video_frame{content = metadata, body = [<<"onMetaData">>, {object, DurationMeta ++ Meta1}],
               stream_id = proplists:get_value(stream_id, Opts, 0), dts = DTS, pts = DTS}.


video_parameters(#media_info{video = [#stream_info{params = #video_params{width = Width, height = Height}}|_]}) ->
  [{height, Height}, {width, Width}];

video_parameters(#media_info{}) ->
  [].


