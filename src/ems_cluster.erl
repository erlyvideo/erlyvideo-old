%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Generalized RTMP application behavior module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
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
-module(ems_cluster).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").

-behaviour(gen_server).

%% API
-export([start/0, 
         stop/0,
         is_global/0,
		 next_stream_id/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server cluster node
%%--------------------------------------------------------------------
start() ->
    gen_server_cluster:start(?MODULE, ?MODULE, [], []).

stop() -> 
    gen_server:stop({global, ?MODULE}).  

%%-------------------------------------------------------------------------
%% @spec () -> bool()
%% @doc global or local server
%% @end
%%-------------------------------------------------------------------------
is_global() ->
    LocalNode = node(),
    case catch gen_server_cluster:get_all_server_nodes(?MODULE) of
	{LocalNode, _} ->
	    true;
        _ ->
	    false
    end.

%%-------------------------------------------------------------------------
%% @spec () -> integer()
%% @doc creates an incremental, global unique, non-persistant id
%% @end
%%-------------------------------------------------------------------------
next_stream_id() ->
	gen_server:call({global,?MODULE}, {next_stream_id}). 


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%-------------------------------------------------------------------------
%% @spec (Args::any()) -> any()
%% @doc Initalization
%% @end
%%-------------------------------------------------------------------------
init([]) ->
    {ok, #ems_cluster{}}.


%%-------------------------------------------------------------------------
%% @spec (Request::any(),From::pid(),State::any()) -> any()
%% @doc Handles request from gen_server:call2,3 and gen_server:multi_call:2,3,4
%% @end
%%-------------------------------------------------------------------------
handle_call({next_stream_id}, _From, #ems_cluster{next_stream_id=Id}=State) ->
	NextId = Id+1,
    {reply, Id, State#ems_cluster{next_stream_id=NextId}}.


%%-------------------------------------------------------------------------
%% @spec (Request::any(),State::any()) -> any()
%% @doc handles request from gen_server:cast/2
%% @end
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%-------------------------------------------------------------------------
%% @spec (Info::any(),State::any()) -> any()
%% @doc Handles messages sent to server
%% @end
%%-------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%-------------------------------------------------------------------------
%% @spec (Reason::any(),State::any()) -> any()
%% @doc stops EMS cluster
%% @end
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%-------------------------------------------------------------------------
%% @spec (OldVsn::any(),State::any(),Extra::any()) -> any()
%% @doc Upgrade or Downgrade code
%% @end
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
