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
-include_lib("stdlib/include/qlc.hrl").
-include("../include/ems.hrl").

-behaviour(gen_server).

%% API
-export([
    start/0, 
    stop/0,
    is_global/0,
    add_connection/2,
    connections/0,
    connection/1,
    remove_connection/1,
    is_live_stream/1,
    subscribe/2,
    unsubscribe/2,
    streams/0
    ]).

%% gen_server callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
    ]).


%%====================================================================
%% API
%%====================================================================

%%-------------------------------------------------------------------------
%% @spec (pid(), string()) -> ok | errror
%% @doc
%% adds a connection
%% @end
%%-------------------------------------------------------------------------
add_connection(ClientId, Pid) -> 
    F = fun() ->
		mnesia:write(#ems_connection{client_id=ClientId, pid=Pid})
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.
 
  
%%--------------------------------------------------------------------
%% @spec  () -> Connections::list()
%% @doc
%% returns a list of all connections
%% @end 
%%--------------------------------------------------------------------    
connections() -> 
    do(qlc:q([X || X <-mnesia:table(ems_connection)])).
 
 
%%--------------------------------------------------------------------
%% @spec (string()) -> pid() | undefined 
%% @doc
%% retrieves a connection based on clientId
%% @end 
%%--------------------------------------------------------------------    
connection(ClientId) ->
    F = fun() ->
        mnesia:read({ems_connection, ClientId})
    end,
    {atomic, Row} = mnesia:transaction(F),
    case Row of
        [] ->
            undefined;
        [{connection, ClientId, Pid}] ->
            Pid
    end.


%%--------------------------------------------------------------------
%% @spec (string()) -> ok | errror
%% @doc
%% remove a connection based on clientId
%% @end 
%%--------------------------------------------------------------------	
remove_connection(ClientId) ->
    F = fun() ->
		mnesia:delete({ems_connection, ClientId})
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.	
    
    
%%--------------------------------------------------------------------
% @spec (string()) -> true | false 
%% @doc
%% subscribe to a stream
%% @end 
%%--------------------------------------------------------------------
is_live_stream(Stream) ->
    F = fun() ->
        mnesia:read({ems_stream, Stream})
    end,
    {atomic, Row} = mnesia:transaction(F),
    case Row of
        [] ->
            false;
        [{ems_stream, Stream, wait, _}] -> %% TODO: use Record
            false;
        _ ->
            true
    end.
  
      
%%--------------------------------------------------------------------
%% @spec (string(), pid()) -> ok | errror 
%% @doc
%% subscribe to a stream
%% @end 
%%--------------------------------------------------------------------
subscribe(ClientId, Stream) ->
    F = fun() ->
        E = case mnesia:read({ems_stream, Stream}) of
            [] -> 
                #ems_stream{client_ids=[ClientId]};
            [#ems_stream{client_ids=Ids} = E1] ->
                E1#ems_stream{client_ids=[ClientId | Ids]}
        end,
        mnesia:write(E)
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.


%%--------------------------------------------------------------------
%% @spec (string(), pid()) -> ok | errror  
%% @doc
%% unsubscribe from a stream
%% @end 
%%--------------------------------------------------------------------
unsubscribe(ClientId, Stream) ->
    F = fun() ->
		case mnesia:read({ems_stream, Stream}) of
		    [] ->
			    {error, stream_not_found};
		    [#ems_stream{client_ids=Ids} = E] ->
			    mnesia:write(E#ems_stream{client_ids=lists:delete(ClientId,  Ids)})
		end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.    


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% returns a list of all streams
%% @end 
%%--------------------------------------------------------------------
streams() ->
    do(qlc:q([X || X <-mnesia:table(ems_stream)])).
        

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc 
%% Starts server cluster node
%% @end 
%%--------------------------------------------------------------------
start() ->
    Start = gen_server_cluster:start(?MODULE, ?MODULE, [], []),
    Node = node(),
    case catch gen_server_cluster:get_all_server_nodes(?MODULE) of
	    {Node, _} ->
	        up_master();
        {Master, _}->
            gen_server:call(Master, add_mnesia_slave),
            mnesia:start(),
            mnesia:change_config(extra_db_nodes, [Node])
    end,
    Start.


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
handle_call(add_mnesia_slave, From, State) ->
	mnesia:add_table_copy(schema, From, ram_copies),
    {reply, ok, State};
    
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


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
    mnesia:stop(), 
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

mnesia_tables() ->
    [{connection,
      [{ram_copies, [node()]},
       {attributes, record_info(fields, ems_connection)}]},
     {channel,
      [{ram_copies, [node()]},
       {attributes, record_info(fields, ems_stream)}]}].
       

up_master() ->
    case mnesia:create_schema([node()]) of
        {error, {_, {already_exists, _}}} -> 
            mnesia:start();
        ok -> 
            mnesia:start(),
            lists:foreach(fun ({Name, Args}) ->
        	                  case mnesia:create_table(Name, Args) of
        			              {atomic, ok} -> ok;
        			              {aborted, {already_exists, _}} -> ok
        		              end
        	              end,
        			      mnesia_tables())
    end.
    

do(QLC) ->
    F = fun() ->
		 qlc:e(QLC) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.