-module(file_media).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([init/1, handle_frame/2, handle_control/2]).
-export([file_dir/1, file_format/1]).

init(Options) ->
  io:format("Starting file media ~p~n", [Options]),
  Name = proplists:get_value(name, Options),
  Host = proplists:get_value(host, Options),
  case open_file(Name,Host) of
    {error, Reason} ->
      {error, Reason};
    Else ->
      {ok, state, Else}
  end.
  

open_file(Name, Host) when is_binary(Name) ->
  open_file(binary_to_list(Name), Host);

open_file(Name, Host) ->
  FileName = filename:join([file_media:file_dir(Host), Name]), 
	{ok, File} = file:open(FileName, [read, binary, {read_ahead, 100000}, raw]),
	case file_media:file_format(FileName) of
	  undefined ->
	    {error, notfound};
  	Format ->
    	{ok, Storage} = Format:init({file,File}),
    	{Format, Storage}
  end.



handle_frame(Frame, State) ->
  {ok, Frame, State}.

handle_control({subscribe, Consumer, StreamId}, _State) ->
  ?D({subscribe,Consumer,StreamId}),
  ems_sup:start_ticker(self(), Consumer, [{stream_id, StreamId}]).

%%-------------------------------------------------------------------------
%% @spec (Host) -> FileName::string()
%% @doc retrieves video file folder from application environment
%% @end
%%-------------------------------------------------------------------------	
file_dir(Host) ->
  ems:get_var(file_dir, Host, undefined).



file_format(Name) ->
  Readers = ems:get_var(file_formats, [mp4_reader, flv_reader]),
  file_format(Name, Readers).

file_format(_Name, []) ->
  undefined;

file_format(Name, [Reader|Readers]) ->
  case Reader:can_open_file(Name) of
    true -> Reader;
    false -> file_format(Name, Readers)
  end.

