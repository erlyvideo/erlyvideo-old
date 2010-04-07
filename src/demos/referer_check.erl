-module(referer_check).
-include("../../include/rtmp_session.hrl").
-export([connect/2]).

connect(#rtmp_session{host = Host, player_info = PlayerInfo} = State, _Funcall) ->
  PageUrl = proplists:get_value(pageUrl, PlayerInfo),
  {http,_,Hostname,_Port,_Path,_QueryString} = http_uri:parse(binary_to_list(PageUrl)),
  Accepted = lists:member(Hostname, ems:get_var(hostname, Host, [])),
  case Accepted of
    true -> 
      ems_log:access(Host, "CONNECT ~s ~s referer_check", [Host, PageUrl]),
      rtmp_session:accept_connection(State);
    false -> 
      ems_log:access(Host, "REJECT ~s ~s referer_check", [Host, PageUrl]),
      rtmp_session:reject_connection(State)
  end,
  State.

