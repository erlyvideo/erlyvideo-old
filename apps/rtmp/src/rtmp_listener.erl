%%% @private
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP listener
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_listener).
-author('Max Lapshin <max@maxidoors.ru>').

%% External API
-export([start_link/4]).
-export([accept/2]).



%%--------------------------------------------------------------------
%% @spec (Port::any(), Name::atom(), Callback::atom()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, Name, Callback, Args) ->
  rtmp_gen_listener:start_link(Name, Port, ?MODULE, [Callback|Args]).

accept(CliSocket, Args) ->
  % raw_accept(CliSocket, Args).
  try raw_accept(CliSocket, Args) of
    Reply -> Reply
  catch
    % _Class:{noproc, _} -> ok;
    _Error:Reason -> error_logger:error_msg("Error in RTMP Listener: ~p~n~p~n", [Reason, erlang:get_stacktrace()])
  end.  

raw_accept(CliSocket, [Callback|Args]) ->
  {ok, RTMP} = rtmp_sup:start_rtmp_socket(accept),
  gen_tcp:controlling_process(CliSocket, RTMP),
  {ok, Pid} = erlang:apply(Callback, create_client, [RTMP|Args]),
  rtmp_socket:setopts(RTMP, [{consumer, Pid}]),
  rtmp_socket:set_socket(RTMP, CliSocket),
  ok.



