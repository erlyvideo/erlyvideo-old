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
    add_client/2,
    clients/0,
    client/1,
    remove_client/1,
    is_live_stream/1,
    subscribe/2,
    unsubscribe/2,
    streams/0,
    broadcast/1,
    broadcast/2,
    stop_broadcast/1
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
%% adds a client
%% @end
%%-------------------------------------------------------------------------
add_client(Id, Pid) -> 
    F = fun() ->
		mnesia:write(#ems_client{id=Id, pid=Pid})
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.
 
  
%%--------------------------------------------------------------------
%% @spec  () -> Client::list()
%% @doc
%% returns a list of all clients
%% @end 
%%--------------------------------------------------------------------    
clients() -> 
    do(qlc:q([X || X <-mnesia:table(ems_client)])).
 
 
%%--------------------------------------------------------------------
%% @spec (string()) -> pid() | undefined 
%% @doc
%% retrieves a client based on his Id
%% @end 
%%--------------------------------------------------------------------    
client(Id) ->
    F = fun() ->
        mnesia:read({ems_client, Id})
    end,
    {atomic, Row} = mnesia:transaction(F),
    case Row of
        [] ->
            undefined;
        [#ems_client{pid=Pid}] ->
            Pid
    end.


%%--------------------------------------------------------------------
%% @spec (string()) -> ok | errror
%% @doc
%% remove a client based on Id
%% @end 
%%--------------------------------------------------------------------	
remove_client(Id) ->
    F = fun() ->
		mnesia:delete({ems_client, Id})
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
is_live_stream(Id) ->
    F = fun() ->
        mnesia:read({ems_stream, Id})
    end,
    {atomic, Row} = mnesia:transaction(F),
    case Row of
        [] ->
            false;
        [#ems_stream{type=wait}] ->
            false;
        _ ->
            true
    end.
  
      
%%--------------------------------------------------------------------
%% @spec (pid(), string()) -> ok | errror 
%% @doc
%% subscribe to a stream
%% @end 
%%--------------------------------------------------------------------
subscribe(Pid, Id) ->
    F = fun() ->
        E = case mnesia:read({ems_stream, Id}) of
            [] -> 
                #ems_stream{pids=[Pid]};
            [#ems_stream{pids=Pids} = E1] ->
                E1#ems_stream{pids=[Pid | Pids]}
        end,
        mnesia:write(E)
    end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.


%%--------------------------------------------------------------------
%% @spec (pid(), string()) -> ok | errror  
%% @doc
%% unsubscribe from a stream
%% @end 
%%--------------------------------------------------------------------
unsubscribe(Pid, Id) ->
    F = fun() ->
		case mnesia:read({ems_stream, Id}) of
		    [] ->
			    {error, stream_not_found};
		    [#ems_stream{pids=Pids} = E] ->
			    mnesia:write(E#ems_stream{pids=lists:delete(Pid,  Pids)})
		end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.    


%%--------------------------------------------------------------------
%% @spec  () -> term()
%% @doc
%% returns a list of all streams
%% @end 
%%--------------------------------------------------------------------
streams() ->
    do(qlc:q([X || X <-mnesia:table(ems_stream)])).
    
   
%%--------------------------------------------------------------------
%% @spec (string()) -> ok | error 
%% @doc
%% broadcast a stream
%% @end 
%%--------------------------------------------------------------------
broadcast(Id) ->
    F = fun() ->
		case mnesia:read({ems_stream, Id}) of
		    [] ->
			    {error, stream_not_found};
		    [E] ->
			    mnesia:write(E#ems_stream{type=live})
		end
	end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.
    
                
%%--------------------------------------------------------------------
%% @spec (string(), binary()) -> ok | errror 
%% @doc
%% broadcast a stream
%% @end 
%%--------------------------------------------------------------------
broadcast(Id, Data) ->
     F = fun() -> 
         mnesia:read({ems_stream, Id})
     end,
     case mnesia:transaction(F) of    
         {atomic, [#ems_stream{pids=Pids}]} ->
             [ gen_fsm:send_event(Pid, {send, Data}) || Pid <- Pids ],
             ok;
         _ ->
             error
     end.


 %%--------------------------------------------------------------------
 %% @spec (string()) -> ok | errror 
 %% @doc
 %% stop broadcast a stream
 %% @end 
 %%--------------------------------------------------------------------
 stop_broadcast(Id) ->
     F = fun() ->
 		case mnesia:read({ems_stream, Id}) of
 		    [] ->
 			    {error, stream_not_found};
 		    [E] ->
 			    mnesia:write(E#ems_stream{type=undefined})
 		end
     end,
     case mnesia:transaction(F) of
         {atomic, ok} -> ok;
         _ -> error
     end.
         
                 	
%%--------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
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
    [{ems_client,
      [{ram_copies, [node()]},
       {attributes, record_info(fields, ems_client)}]},
     {ems_stream,
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