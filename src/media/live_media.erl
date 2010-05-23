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
  case proplists:get_value(wait, Options, ?SOURCE_TIMEOUT) of
    Timeout when is_number(Timeout) ->
      {ok, Ref} = timer:send_after(Timeout, source_timeout),
      {ok, #live{timeout = Timeout, ref = Ref}};
    infinity ->
      {ok, #live{}}
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
