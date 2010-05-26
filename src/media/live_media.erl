%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        ems_media handler template
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2010 Max Lapshin
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
-module(live_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([init/1, handle_frame/2, handle_control/2, handle_info/2]).

-define(SOURCE_TIMEOUT, 1000).

-record(live, {
  timeout,
  ref
}).

%%%------------------------------------------------------------------------
%%% Callback functions from ems_media
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Options::list()) -> {ok, State}                   |
%%                            {ok, State, {Format,Storage}} |
%%                            {stop, Reason}
%%
%% @doc Called by ems_media to initialize specific data for current media type
%% @end
%%----------------------------------------------------------------------

init(Options) ->
  State = case proplists:get_value(wait, Options, ?SOURCE_TIMEOUT) of
    Timeout when is_number(Timeout) ->
      {ok, Ref} = timer:send_after(Timeout, source_timeout),
      #live{timeout = Timeout, ref = Ref};
    infinity ->
      #live{}
  end,
  case proplists:get_value(type, Options) of
    live -> 
      {ok, State};
    record ->
      URL = proplists:get_value(url, Options),
      Host = proplists:get_value(host, Options),
    	FileName = filename:join([file_media:file_dir(Host), binary_to_list(URL)]),
    	(catch file:delete(FileName)),
    	ok = filelib:ensure_dir(FileName),
      {ok, Writer} = flv_writer:init(FileName),
      {ok, State, {flw_writer, Writer}}
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
  {reply, ok, State};

handle_control({source_lost, _Source}, State) ->
  %% Source lost returns:
  %% {reply, Source, State} -> new source is created
  %% {stop, Reason, State} -> stop with Reason
  {reply, undefined, State};

handle_control({set_source, _Source}, #live{ref = undefined} = State) ->
  {reply, ok, State};

handle_control({set_source, _Source}, #live{ref = Ref} = State) ->
  {ok, cancel} = timer:cancel(Ref),
  {reply, ok, State#live{ref = undefined}};

handle_control(timeout, State) ->
  {noreply, State};

handle_control(_Control, State) ->
  {reply, ok, State}.

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
handle_info(source_timeout, State) ->
  {stop, timeout, State};

handle_info(_Message, State) ->
  {noreply, State}.


