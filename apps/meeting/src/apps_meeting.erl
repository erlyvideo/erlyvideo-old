-module(apps_meeting).

-include_lib("rtmp/include/rtmp.hrl").
-include("meeting.hrl").

-export([play/2]).

play(Session, #rtmp_funcall{stream_id = StreamId, args = [_, <<"meeting_archive/substream/", SubStream/binary>>]} = _AMF) ->
  UserId = list_to_integer(binary_to_list(SubStream)),
  ?D({playing,substream,StreamId, UserId}),
  Player = rtmp_session:get(Session, meeting_player_pid),
  Player ! {next_frame, StreamId, UserId},
  rtmp_session:set_stream(rtmp_stream:construct([{stream_id, StreamId}]), Session);

play(Session, #rtmp_funcall{stream_id = StreamId, args = [null, <<"meeting_archive/meeting/", Meeting/binary>>]} = _AMF) ->
  ?D({play, Meeting}),
  Host = rtmp_session:get(Session, host),
  {ok, Player} = meeting_sup:start_meeting_player(Meeting, [{host,Host}]),
  meeting_player:play(Player, self()),
  Sess1 = rtmp_session:set(Session, meeting_player_pid, Player),
  rtmp_session:set_stream(rtmp_stream:construct([{stream_id, StreamId}]), Sess1);

play(_Session, _AMF) ->
  unhandled.

