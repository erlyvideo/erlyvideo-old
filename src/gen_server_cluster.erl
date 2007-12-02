%%%---------------------------------------------------------------------------------------
%%% @author     Christoph Dornheim [http://www.trapexit.org/A_Framework_for_Clustering_Generic_Server_Instances]
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Framework for Clustering Generic Server Instances
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
-module(gen_server_cluster).
-author('Christoph Dornheim').
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').

-behaviour(gen_server).

-export([
    start/4,
    start_local_server/1,
    get_target_state/1,
    get_all_server_pids/1,
    get_all_server_nodes/1,
    is_running/1,
    stop/2,
    link/2,
    unlink/2
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%% the state of gen_server_cluster
-record(state, {
    name,
    globalServerPid,
    localServerPidList=[],
    targetLinkedPidList=[],
    targetModule,
    targetState
    }).


%%====================================================================
%% API
%%====================================================================
%% Starts the global or local server under the given name where 
%% the target gen_server is given by the target module.
%% If some global server is already running, the server is started as a local
%% server, otherwise as the new global server. 
start(Name, TargetModule, TargetArgs, Options) ->
    ArgsGlobalInit = {initGlobal, Name, TargetModule, TargetArgs},
    %% try starting as the global server:
    case gen_server:start({global,Name}, ?MODULE, ArgsGlobalInit, Options) of
    {ok,_GlobalServerPid}=Result ->
        io:format("~p started as global server.~n",[Name]), 
        Result;
    {error,{already_started,_GlobalServerPid}} ->
        %% global server already started, start as local server:
        start_local_server(Name)
    end.

%% Starts a local server if a global server is already running,
%% otherwise noGlobalServerRunning is returned.
start_local_server(Name) ->
    case is_running(Name) of
    false -> 
        noGlobalServerRunning;
    true ->
        ArgsLocalInit = {initLocal, Name},
        case gen_server:start({local,Name}, ?MODULE, ArgsLocalInit, []) of
        {ok, _LocalPid}=Result ->
            io:format("~p started as local server.~n",[Name]),
            Result;
        Else ->
            Else
        end
    end.

%% Returns the global server pid and the list of all local server pids:
%% {pid(), [pid()]}
get_all_server_pids(Name) ->
    gen_server:call({global,Name}, get_all_server_pids).

%% Returns the global server node and the list of all local server nodes:
%% {node(), [node()]}
get_all_server_nodes(Name) ->
    {GlobalServerPid, LocalServerPidList} = get_all_server_pids(Name),
    Fun = fun(Pid) ->
          node(Pid)
      end,
    {Fun(GlobalServerPid), lists:map(Fun, LocalServerPidList)}.


%% Returns true if there is some global server running, otherwise false.
is_running(Name) ->
    case catch get_all_server_pids(Name) of
    {Pid, PidList} when is_pid(Pid), is_list(PidList) ->
        true;
        _ ->
        false
    end.

%% Stops the global server.
stop(Name, global) ->
    gen_server:call({global, Name}, {stopByGenServerCluster, stopGlobalServer});

%% Stops both the global server and all local servers.
stop(Name, all) ->
    {_, LocalServerNodeList} = get_all_server_nodes(Name),
    Request = {stopByGenServerCluster, stopAllServer},
    case LocalServerNodeList of
    [] ->
        ok;
    _ ->
        gen_server:multi_call(LocalServerNodeList, Name, Request)
    end,
    gen_server:call({global, Name}, Request);

%% Stops the local server running on the given node.
stop(Name, Node) ->
    gen_server:multi_call([Node], Name, {stopByGenServerCluster, stopLocalServer}).

%% Returns the target state.
get_target_state(Name) ->
    gen_server:call({global,Name}, get_target_state).

%% Links the pid with the global server. This link is transferred to 
%% a new global server if the current one dies. 
link(Name, Pid) ->
    %% must be cast since this can be called by target server 
    %% which hangs if we use call instead! 
    gen_server:cast({global,Name}, {link, Pid}). 

%% Removes the link between pid and the global server that was
%% established before.
unlink(Name, Pid) ->
    gen_server:cast({global,Name}, {unlink, Pid}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

%% Called by global server at startup.
init({initGlobal, Name, TargetModule, TargetArgs}) ->
    process_flag(trap_exit, true),
    %% initialize callback server:
    TargetResult =  TargetModule:init(TargetArgs),
    case TargetResult of
    {ok, TargetState} ->
        State = #state{name=Name,
               globalServerPid=self(),
               targetModule=TargetModule,
               targetState=TargetState},
        {ok, State};
    {ok, TargetState, _Timeout} ->
        State = #state{name=Name,
               globalServerPid=self(),
               targetModule=TargetModule,
               targetState=TargetState},
        {ok, State};
    {stop, Reason} ->
        {stop, Reason};
    ignore ->
        ignore
    end;

%% Called by local server at startup.
init({initLocal, Name}) ->
    process_flag(trap_exit, true),
    State = gen_server:call({global,Name}, init_local_server_state),
    {ok,State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% Called by global server to get the target state.
handle_call(get_target_state, _From, State) ->
    Reply = State#state.targetState,
    {reply, Reply, State};

%% Called by global server to get the pids of the global and all local servers.
handle_call(get_all_server_pids, _From, State) ->
    Reply = {State#state.globalServerPid, State#state.localServerPidList},
    {reply, Reply, State};

%% Called by global server when a local server starts and wants to get the 
%% current state. The pid of this new local server is stored in the server's 
%% state and an update request is sent to all other local servers.
handle_call(init_local_server_state, {FromPid,_}, State) ->
    %% function to update state by storing new localserver pid:
    AddNewLocalServerFun = 
    fun(S) ->
        S#state{localServerPidList=[FromPid|S#state.localServerPidList]}
    end,
    NewState = update_all_server_state(State, AddNewLocalServerFun),
    %% link to this local server:
    link(FromPid),
    Reply=NewState,
    {reply, Reply, NewState};

%% Called by global or local server due to a client stop request.  
handle_call({stopByGenServerCluster, Reason}=Reply, _From, State) ->
    {stop, Reason, Reply, State};

%% Called by local server due to a target stop request.  
handle_call(stopByTarget=Reason, _From, State) ->
    {stop, Reason , State};

%% Called by global or local server and delegates the request to the target. 
handle_call(Request, From, State) ->
    delegate_to_target(State, handle_call, [Request, From, 
                        State#state.targetState]).


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% Called by local server to update its state.
%% The update function is sent by the global server and returns the new state
%% when applied to the old state. 
handle_cast({update_local_server_state, UpdateFun}, State) ->
    NewState = UpdateFun(State),
    {noreply, NewState};

%% Called by global server due to a client request for linking the pid
%% to the global server. The pid is stored in the state and an state update
%% request is sent to all local servers. 
handle_cast({link,Pid}, State) ->
    %% function to update state by storing new pid linked to target:
    AddNewTargetLinkedPidFun = 
    fun(S) ->
        %% remove Pid first to avoid duplicates:
        L = lists:delete(Pid, S#state.targetLinkedPidList),
        S#state{targetLinkedPidList=[Pid|L]}
    end,
    NewState = update_all_server_state(State, AddNewTargetLinkedPidFun),
    %% link to this local server:
    link(Pid),
    {noreply, NewState};

%% Called by global server due to a client request for deleting the link
%% between the pid and the global server. The updated state is
%% sent to all local servers.
handle_cast({unlink,Pid}, State) ->
    %% function to update state by removing link to pid:
    RemoveTargetLinkedPidFun = 
    fun(S) ->
        S#state{targetLinkedPidList
            =lists:delete(Pid, S#state.targetLinkedPidList)}
    end,
    NewState = update_all_server_state(State, RemoveTargetLinkedPidFun ),
    %% unlink to this local server:
    unlink(Pid),
    {noreply, NewState};

%% Called by global or local server and delegates the request to the target.
handle_cast(Msg, State) ->
    delegate_to_target(State, handle_cast, [Msg, State#state.targetState]).


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

%% Called by all local servers when the global server has died.
%% Each local server tries to register itself as the new global one,
%% but only one of them succeeds.
handle_info({'EXIT', Pid, Reason}, State) when Pid==State#state.globalServerPid ->
    io:format("Global server on ~p terminated: ~p.~n", [node(Pid), Reason]),
    NewGlobalServerPid = try_register_as_global_server(State#state.name, Pid),
    io:format("New global server on ~p.~n", [node(NewGlobalServerPid)]),
    %% update new global server pid and local server pid list:
    NewLocalServerPidList = lists:delete(NewGlobalServerPid,
                     State#state.localServerPidList),
    NewState = State#state{globalServerPid=NewGlobalServerPid,
               localServerPidList=NewLocalServerPidList},
    case NewGlobalServerPid==self() of
    true ->
        %% link to all local servers:
        F = fun(P) ->
            link(P)
        end,
        lists:foreach(F, NewLocalServerPidList),
        %% link to all pids the target was linked to:
        lists:foreach(F, NewState#state.targetLinkedPidList);
    false ->
        ok
    end,
    {noreply, NewState};

%% Called by global server when some local server or another process has died.
%% If a local server has died, its pid is deleted from the local server list and 
%% a corresponding state update is sent to all other local servers.
%% If some other linked process has died, this info is delegated to the target.
%% In case it was linked using this module's link function, its pid is deleted
%% from the state and a corresponding state update is sent to all other local 
%% servers.
handle_info({'EXIT', Pid, _Reason}=Info, State) ->
    case lists:member(Pid, State#state.localServerPidList) of
    true ->
        %% Pid is a local server that has terminated:
        %% function to update state by deleting the local server pid:
        RemoveLocalServerFun = 
        fun(S) ->
            S#state{localServerPidList
                =lists:delete(Pid,S#state.localServerPidList)}
        end,
        NewState = update_all_server_state(State, RemoveLocalServerFun),
        {noreply, NewState};
    false ->
        case lists:member(Pid, State#state.targetLinkedPidList) of
        true ->
            %% Pid was linked to target, so remove from link list:
            {reply,_,NewState} = handle_call({unlink,Pid}, void, State);
        false ->
            NewState = State
        end,
        %% the target created the link to pid, thus delegate:
        delegate_to_target(NewState, handle_info, 
                               [Info, NewState#state.targetState])
    end;

%% Called by global or local server and delegates the info request to the target.
handle_info(Info, State) ->
    delegate_to_target(State, handle_info, [Info, State#state.targetState]).


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------

%% Called by global or local server when it is about to terminate.
terminate(Reason, State) ->
    TargetModule = State#state.targetModule,
    TargetModule:terminate(Reason,  State#state.targetState),
    io:format("Server ~p stopped.~n", [State#state.name]).


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------

%% Currently not supported.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Called by global server to delegate a gen_server callback function
%% (handle_call, etc.) to the target module. The result returned by the
%% target is transformed to a corresponding result for the gen_server_cluster
%% where the target state is updated.
%% This update is sent to all local servers unless the target result is a
%% stop request. In that case a stop request is sent to all local servers 
%% whereas the global server will stop due to the return value.
%% The term returned by the target for the new state can be an update function 
%% or the entire new state. When applied to the old target state this update 
%% function should give the new target state. (This is provided mainly for 
%% optimization as it avoids transmitting large state terms.)
delegate_to_target(State, TargetCall, Args) ->
    TargetModule = State#state.targetModule,
    TargetResult = apply(TargetModule, TargetCall, Args),
    %% index of state in tuple:
    IndexState = case TargetResult of
             {reply, _Reply, TargetStateUpdate} ->
             3;
             {reply, _Reply, TargetStateUpdate, _Timeout}  ->
             3;
             {noreply, TargetStateUpdate} ->
             2;
             {noreply, TargetStateUpdate, _Timeout} ->
             2;
             {stop, _Reason, _Reply, TargetStateUpdate} ->
             4;
             {stop, _Reason, TargetStateUpdate} ->
             3;
             {ok, TargetStateUpdate} ->  %% for code change
             2
         end,
    %% function to update the target state:
    UpdateTargetStateFun = 
    fun(S) ->       
        %% update target state where TargetStateUpdate is 
        %% update function or new state:
        NewTargetState =
            case is_function(TargetStateUpdate,1) of
            true ->
                TargetStateUpdate(S#state.targetState);
            false ->
                TargetStateUpdate
            end,
        S#state{targetState=NewTargetState}
    end,
    NewState = update_all_server_state(State, UpdateTargetStateFun),
    %% return target reply tuple where state is substituted:
    Result = setelement(IndexState, TargetResult, NewState),

    %% if target stop, stop all local servers. 
    %% The global server will be stopped by returning the result.
    case (element(1,Result)==stop) and (State#state.localServerPidList/=[]) of
    true ->
        Fun = fun(Pid) ->
              node(Pid)
          end,
        LocalServerNodeList = lists:map(Fun, State#state.localServerPidList),
        gen_server:multi_call(LocalServerNodeList,
                      State#state.name, stopByTarget);
    false ->
        ok
    end,
    Result.

%% Called by global server to update the state of all servers.
%% The result of applying UpdateFun to State is returned, and
%% an update request to all local servers is sent. 
update_all_server_state(State, UpdateFun) ->
    NewState = UpdateFun(State),
    CastFun = fun(Pid) ->
              gen_server:cast(Pid, {update_local_server_state,UpdateFun})
          end,
    lists:foreach(CastFun, State#state.localServerPidList),
    NewState.

%% Called by each local server to try registering itself as the new global 
%% server. It waits until the old registration of OldGlobalServerPid is
%% removed and then tries to register the calling process globally under 
%% the given name. The new registered global server pid is returned which can
%% be this or one of the other local servers trying to register themselves.
try_register_as_global_server(Name, OldGlobalServerPid) ->
    case global:whereis_name(Name) of %% current global server pid in registry
    OldGlobalServerPid ->
        %% sleep up to 1s until old global server pid is deleted 
        %% from global registry:
        timer:sleep(random:uniform(1000)),
        try_register_as_global_server(Name, OldGlobalServerPid);
    undefined ->
        %% try to register this process globally:
        global:register_name(Name, self()),
        timer:sleep(100), %% wait before next registration check
        try_register_as_global_server(Name, OldGlobalServerPid);
    NewGlobalServerPid ->
        %% new global server pid (self() or other local server):
        NewGlobalServerPid
    end.
