%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Generic consumer module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

