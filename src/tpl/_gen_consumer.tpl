%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Generic consumer module
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

-module(_gen_consumer).
-include("../../include/ems.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-behaviour(gen_consumer).
% gen_consumer behaviour
-export([handle_frame/2, handle_control/2, handle_info/2, init/2, terminate/1]).

-export([start_link/2]).

start_link(Options, Args) ->
  gen_consumer:start_link(?MODULE, Options, Args).


init(Options, Args) ->
  {ok, state}.


handle_frame(Frame, Stream) ->
  {noreply, Stream}.


%% Handles specific control messages from gen_consumer
handle_control({notfound, Name, Reason}, Stream) ->
  {noreply, Stream};

handle_control({start_play, Name}, Stream) ->
  {noreply, Stream#rtmp_stream{name = Name}};
  
handle_control({play_complete, Length}, Stream) ->
  {noreply, Stream};

handle_control(stop, Stream) ->
  {noreply, Stream};

handle_control({seek_failed, _Timestamp}, Stream) ->
  {noreply, Stream};

handle_control({seek_notify, Timestamp}, Stream) ->
  {noreply, Stream}.


%% Handles inbox messages
handle_info(stop, Stream) ->
  stop.
  
handle_info(Message, Stream) ->
  {noreply, Stream}.
  
  
terminate(Stream) ->
  ok.
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

