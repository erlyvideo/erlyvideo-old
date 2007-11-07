%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @doc        Debug Proxy server for RTMP and AMF packets
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
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_proxy).
-author('rsaccon@gmail.comm').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").

-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-export([start_link/1,start_link/2,start_link/3]).

-behavior(gen_server).
%%-------------------------------------------------------------------------
%% @spec (IP::tuple()) -> pid()
%% @doc starts Debug Proxy server equivalant to start_link(IP,1935,"/tmp")
%% @end
%%-------------------------------------------------------------------------
start_link(IP) -> start_link(IP,1935).
%%-------------------------------------------------------------------------
%% @spec (IP::tuple(),Port::integer()) -> pid()
%% @doc starts Debug Proxy server equivalant to start_link(IP,Port,"/tmp")
%% @end
%%-------------------------------------------------------------------------
start_link(IP,Port) -> start_link(IP,Port,"/tmp/").
%%-------------------------------------------------------------------------
%% @spec (IP::tuple(),Port::integer(),Dir::String) -> pid()
%% @doc starts Debug Proxy server
%% @end
%%-------------------------------------------------------------------------
start_link(IP,Port,Dir) -> 
	?D({IP,Port,Dir}),
	ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn::any(),State::any(),Extra::any()) -> any()
%% @doc Upgrade or Downgrade code
%% @end
%%-------------------------------------------------------------------------
code_change(_OldVsn,State,_Extra) -> {ok,State}.

%%-------------------------------------------------------------------------
%% @spec (Request::any(),From::pid(),State::any()) -> any()
%% @doc Handles request from gen_server:call2,3 and gen_server:multi_call:2,3,4
%% @end
%%-------------------------------------------------------------------------

handle_call(_Request,_From,State) -> {noreply,State}.
%%-------------------------------------------------------------------------
%% @spec (Request::any(),State::any()) -> any()
%% @doc handles request from gen_server:cast/2
%% @end
%%-------------------------------------------------------------------------

handle_cast(_Request,State) -> {noreply,State}.

%%-------------------------------------------------------------------------
%% @spec (Info::any(),State::any()) -> any()
%% @doc Handles messages sent to server
%% @end
%%-------------------------------------------------------------------------
handle_info({tcp,Socket, Bin},State) ->
	?D({Socket,Bin}),
	State;	
handle_info(_Info,State) -> {noreply,State}.

%%-------------------------------------------------------------------------
%% @spec (Args::any()) -> any()
%% @doc Initalization
%% @end
%%-------------------------------------------------------------------------
init(_Args) -> {ok,[]}.

%%-------------------------------------------------------------------------
%% @spec (Reason::any(),State::any()) -> any()
%% @doc stops Debug Proxy server
%% @end
%%-------------------------------------------------------------------------
terminate(_Reason,_State) -> ok.