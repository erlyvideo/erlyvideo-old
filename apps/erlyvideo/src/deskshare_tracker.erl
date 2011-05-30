%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Example of gen_server
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
-module(deskshare_tracker).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include("log.hrl").


%% External API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([start_capture/3, update_mouse/4, update_capture/4, find/2, stop/2]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


start_capture(Host, Name, Options) ->
  gen_server:call(?MODULE, {start_capture, Host, Name, Options}).


update_capture(Host, Name, Position, Data) ->
  case find(Host, Name) of
    {ok, Pid} -> deskshare:update_capture(Pid, Position, Data);
    _ -> {error, notfound}
  end.

update_mouse(Host, Name, X, Y) ->
  case find(Host, Name) of
    {ok, Pid} -> deskshare:update_mouse(Pid, X, Y);
    _ -> {error, notfound}
  end.

find(Host, Name) ->
  gen_server:call(?MODULE, {find, Host, Name}).

stop(Host, Name) ->
  case find(Host, Name) of
    {ok, Pid} -> deskshare:stop(Pid);
    _ -> {error, notfound}
  end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

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


init([]) ->
  {ok, []}.

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
handle_call({start_capture, Host, Name, Options}, _From, Captures) ->
  case proplists:get_value({Host,Name}, Captures) of
    undefined -> 
      StreamName = iolist_to_binary(io_lib:format("capture-~s", [Name])),
      {ok, Media} = media_provider:create(Host, StreamName, [{type, live}]),
      {ok, Pid} = ems_sup:start_deskshare_capture(Media, Options),
      erlang:monitor(process, Pid),
      Key = {Host,Name},
      {reply, {ok,Pid}, lists:keystore(Key, 1, Captures, {Key, Pid})};
    OldPid ->
      {reply, {ok, OldPid}, Captures}  
  end;
  


handle_call({find, Host, Name}, _From, Captures) ->
  Reply = case proplists:get_value({Host,Name}, Captures) of
    undefined -> undefined;
    Pid -> {ok, Pid}
  end,
  {reply, Reply, Captures};

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
  {stop, {unknown_cast, _Msg}, State}.

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
handle_info({'DOWN', _, process, Client, _Reason}, Captures) ->
  Captures1 = lists:keydelete(Client, 2, Captures),
  {noreply, Captures1};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

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
