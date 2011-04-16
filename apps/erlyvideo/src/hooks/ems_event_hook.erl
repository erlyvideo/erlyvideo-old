%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Central point of erlyvideo events
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
-module(ems_event_hook).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_event).
-include("../../include/erlyvideo.hrl").
-include("../log.hrl").

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-record(hook, {
  host,
  event,
  handler
}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_event
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (InitArgs) -> {ok, State}           |
%%                     {ok, State, hibernate}
%%
%% @doc Called by gen_event framework at process startup.
%% @end
%%----------------------------------------------------------------------

init([Host, Event, Handler]) ->
  {ok, #hook{host = Host, event = Event, handler = Handler}}.

%%-------------------------------------------------------------------------
%% @spec (Event, State) -> {ok, NewState}            |
%%                         {ok, NewState, hibernate} |
%%                         {swap_handler,Args1,NewState,Handler2,Args2} |
%%                         remove_handler
%% @doc Callback for events.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_event(#erlyvideo_event{event = Event, host = Host} = Evt, #hook{event = Event, host = Host, handler = Handler} = Hook) ->
  % ?D({calling, Evt, Handler}),
  case (catch Handler:handle_event(Evt)) of
    {'EXIT', Reason} -> ems_log:error(Host, "Failed event handler ~p on event ~p: ~p~n~p", [Handler, Event, Reason, erlang:get_stacktrace()]);
    _ -> ok
  end,
  {ok, Hook};

handle_event(_Event, Hook) ->
  % ?D({skipping, _Event}),
  {ok, Hook}.

%%-------------------------------------------------------------------------
%% @spec (Request, State) -> {ok, Reply, NewState}            |
%%                           {ok, Reply, NewState, hibernate} |
%%                           {swap_handler,Reply,Args1,NewState,Handler2,Args2} |
%%                           {remove_handler,Reply}
%% @doc Callback for synchronous events.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(#erlyvideo_event{event = Event, host = Host} = Evt, #hook{event = Event, host = Host, handler = Handler} = Hook) ->
  Reply = Handler:handle_event(Evt),
  {ok, Reply, Hook};

handle_call(_Event, Hook) ->
  {ok, ok, Hook}.



%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{ok, NewState}          |
%%                      {ok, NewState, hibernate} 
%%                      {swap_handler,Args1,NewState,Handler2,Args2} |
%%                      remove_handler
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(_Info, State) ->
  {ok, State}.

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
