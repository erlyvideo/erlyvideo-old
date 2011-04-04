%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Can parse dump commands to debug rtmp scenarios
%%% Required only for console tool ``contrib/rtmp_dump''
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%% @hidden
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
-module(rtmp_proxy).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/flv.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([run/2, create_client/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

run(ListenPort, Forward) ->
	rtmp_socket:start_server(ListenPort, rtmp_proxy1, rtmp_proxy, [Forward]).
	
	
create_client(RTMP, Forward)	->
  gen_server:start_link(?MODULE, [RTMP, Forward], []).


-record(proxy, {
  client,
  server
}).

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


init([Client, Forward]) ->
  erlang:monitor(process, Client),
  {ok, Server} = rtmp_socket:connect(Forward),
  erlang:monitor(process, Server),
  ?D({proxy_enabled, Client, Server}),
  
  {ok, #proxy{
    client = Client,
    server = Server
  }}.

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
handle_info({'DOWN', _, process, Client, _Reason}, #proxy{client = Client} = Proxy) ->
  ?D(flash_client_died),
  {stop, normal, Proxy};

handle_info({'DOWN', _, process, Server, _Reason}, #proxy{server = Server} = Proxy) ->
  ?D(flash_server_died),
  {stop, normal, Proxy};

handle_info({rtmp, RTMP, connected}, #proxy{client = RTMP} = State) ->
  ?D(flash_client_connected),
  rtmp_socket:setopts(RTMP, [{active, true}]),
  {noreply, State};

handle_info({rtmp, RTMP, timeout}, #proxy{client = RTMP} = State) ->
  ?D(flash_client_disconnected),
  {stop, normal, State};

handle_info({rtmp, RTMP, connected}, #proxy{server = RTMP} = State) ->
  ?D(flash_server_connected),
  rtmp_socket:setopts(RTMP, [{active, true}]),
  {noreply, State};

handle_info({rtmp, RTMP, timeout}, #proxy{server = RTMP} = State) ->
  ?D(flash_server_disconnected),
  {stop, normal, State};

handle_info({rtmp, Client, #rtmp_message{} = Message}, #proxy{client = Client, server = Server} = State) ->
  dump(client, Message),
  rtmp_socket:send(Server, Message),
  {noreply, State};

handle_info({rtmp, Server, #rtmp_message{} = Message}, #proxy{client = Client, server = Server} = State) ->
  dump(server, Message),
  rtmp_socket:send(Client, Message),
  {noreply, State};

handle_info(_Info, State) ->
  ?D({msg, _Info}),
  {noreply, State}.


dump(Direction, #rtmp_message{body = Bin} = Msg) ->
  ?D({Direction, Msg#rtmp_message{body = limited_bin(Bin)}}).


limited_bin(<<Bin:10/binary, _/binary>>) -> Bin;
limited_bin(Bin) -> Bin.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ?D({dying}),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
