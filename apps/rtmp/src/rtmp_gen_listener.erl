%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
%%% @doc        Generic non-blocking listener
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
-module(rtmp_gen_listener).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).


%% External API
-export([start_link/3, start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link(BindSpec, Callback, Args) ->
  gen_server:start_link(?MODULE, [BindSpec, Callback, Args], []).

start_link(Name, BindSpec, Callback, Args) ->
  gen_server:start_link({local, Name}, ?MODULE, [BindSpec, Callback, Args], []).



%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

-record(listener, {
  socket,
  ref,
  bindspec,
  addr,
  port,
  callback,
  args,
  driver
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


init([BindSpec, Callback, Args]) ->
  self() ! bind,
  {ok, #listener{bindspec = BindSpec, callback = Callback, args = Args}}.

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
handle_info(bind, #listener{bindspec = BindSpec} = Server) ->
  Opts1 = [binary, {packet, raw}, {reuseaddr, true}, 
          {keepalive, true}, {backlog, 250}, {active, false}],
  {BindAddr, Port} = case BindSpec of
    P when is_number(P) -> {undefined, P};
    {Addr, P} -> {Addr, P};
    _ when is_list(BindSpec) ->
      [AddrS, P] = string:tokens(BindSpec, ":"),
      {ok, Addr} = inet_parse:address(AddrS),
      {Addr, list_to_integer(P)}
  end,
  Opts2 = case BindAddr of
    undefined -> Opts1;
    _ -> [{ip,BindAddr}|Opts1]
  end,
  
  case gen_tcp:listen(Port, Opts2) of
    {ok, ListenSocket} ->
      {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
      {noreply, Server#listener{addr = BindAddr, port = Port, socket = ListenSocket, ref = Ref}};
    {error, eaccess} ->
      error_logger:error_msg("Error connecting to port ~p. Try to open it in firewall or run with sudo.\n", [Port]),
      {stop, eaccess, Server};
    {error, Reason} ->
      {stop, Reason, Server}
  end;
    
handle_info({inet_async, ListenSock, Ref, {ok, CliSocket}},
            #listener{socket = ListenSock, ref = Ref, callback = Callback, args = Args} = State) ->
  case set_sockopt(ListenSock, CliSocket) of
    ok ->
      case Callback:accept(CliSocket, Args) of
        ok -> ok;
        {error, Reason} ->
          error_logger:error_msg("Error while accepting socket: ~p~n~p~n", [Reason, erlang:get_stacktrace()]),
          gen_tcp:close(CliSocket)
      end,
      {ok, NewRef} = prim_inet:async_accept(ListenSock, -1),
      {noreply, State#listener{ref = NewRef}};
    {error, Reason} ->
      error_logger:error_msg("Error setting socket options: ~p.\n", [Reason]),
      {stop, Reason, State}
  end;


handle_info(_Info, State) ->
  {stop, {unknown_info, _Info}, State}.

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


set_sockopt(ListSock, CliSocket) ->
  true = inet_db:register_socket(CliSocket, inet_tcp),
  case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
      case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
      end;
    Error ->
      gen_tcp:close(CliSocket), Error
  end.
