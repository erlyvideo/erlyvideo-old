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
      
  
handle_frame(Frame, State) ->
  {ok, Frame, State}.

handle_control({set_source, _Source}, #live{ref = undefined} = State) ->
  {reply, ok, State};

handle_control({set_source, _Source}, #live{ref = Ref} = State) ->
  {ok, cancel} = timer:cancel(Ref),
  {reply, ok, State#live{ref = undefined}};

handle_control({subscribe, _Client, _Stream}, State) ->
  {ok, State}.
  
handle_info(source_timeout, State) ->
  {stop, timeout, State};

handle_info(_Message, State) ->
  {noreply, State}.
