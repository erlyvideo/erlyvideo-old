%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        password protected publish
%%% @reference  
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
-module(password_publish).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("../../include/ems.hrl").

-include("../../include/rtmp_session.hrl").

-export([publish/2]).


	
real_publish(#rtmp_session{host = Host, streams = Streams, socket = Socket} = State, FullName, Type, StreamId) ->

  {RawName, Args1} = http_uri2:parse_path_query(FullName),
  Name = string:join( [Part || Part <- ems:str_split(RawName, "/"), Part =/= ".."], "/"),
  Options1 = extract_publish_args(Args1),
  Options = lists:ukeymerge(1, [{type,live}], Options1),
  
  Login = ems:get_var(publish_login, Host, undefined),
  Password = ems:get_var(publish_password, Host, undefined),
  Login = proplists:get_value("login", Options),
  Password = proplists:get_value("password", Options),
  
  ems_log:access(Host, "RECORD ~p ~s ~p ~s", [Type, State#rtmp_session.addr, State#rtmp_session.user_id, Name]),
  {ok, Recorder} = media_provider:create(Host, Name, Options),
  rtmp_socket:send(Socket, #rtmp_message{type = stream_begin, stream_id = StreamId}),
  rtmp_socket:status(Socket, StreamId, ?NS_PUBLISH_START),
  State#rtmp_session{streams = ems:setelement(StreamId, Streams, Recorder)}.
  
extract_publish_args([]) -> [];
extract_publish_args({"source_timeout", "infinity"}) -> {source_timeout, infinity};
extract_publish_args({"source_timeout", "shutdown"}) -> {source_timeout, shutdown};
extract_publish_args({"source_timeout", Timeout}) -> {source_timeout, list_to_integer(Timeout)};
extract_publish_args({Key, Value}) -> {Key, Value};
extract_publish_args(List) -> [extract_publish_args(Arg) || Arg <- List].

publish(State, #rtmp_funcall{args = [null,Name, <<"record">>], stream_id = StreamId} = _AMF) -> 
  real_publish(State, Name, record, StreamId);

publish(State, #rtmp_funcall{args = [null,Name,<<"append">>], stream_id = StreamId} = _AMF) -> 
  real_publish(State, Name, append, StreamId);

publish(State, #rtmp_funcall{args = [null,Name,<<"LIVE">>], stream_id = StreamId} = _AMF) ->
  real_publish(State, Name, live, StreamId);

publish(State, #rtmp_funcall{args = [null,Name,<<"live">>], stream_id = StreamId} = _AMF) -> 
  real_publish(State, Name, live, StreamId);

publish(State, #rtmp_funcall{args = [null, false]} = AMF) ->
  apps_streaming:stop(State, AMF);

publish(State, #rtmp_funcall{args = [null, null]} = AMF) ->
  apps_streaming:stop(State, AMF);

publish(State, #rtmp_funcall{args = [null, <<"null">>]} = AMF) ->
  apps_streaming:stop(State, AMF);
  
publish(State, #rtmp_funcall{args = [null,Name], stream_id = StreamId} = _AMF) -> 
  real_publish(State, Name, live, StreamId).
