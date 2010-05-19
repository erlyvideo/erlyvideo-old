-module(live_media).

-export([init/2, handle_frame/2]).

init(Name, Options) ->
  {ok, state}.
  
handle_frame(Frame, State) ->
  {ok, Frame, State}.
