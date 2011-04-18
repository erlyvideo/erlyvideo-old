%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Desksharing decoder
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
-module(deskshare).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([update_capture/3, update_mouse/3, stop/1]).


start_link(Consumer, Options) ->
  gen_server:start_link(?MODULE, [Consumer, Options], []).


is_valid_update(<<L:16, _C:L/binary, Rest/binary>>) -> is_valid_update(Rest);
is_valid_update(<<>>) -> true;
is_valid_update(_Block) -> false.

update_capture(Capture, Position, BlockData) when is_integer(Position) ->
  case is_valid_update(BlockData) of
    true -> gen_server:call(Capture, {update, Position, BlockData});
    false ->
      ?D({broken_update, Position, size(BlockData)}),
      {error, broken_update}
  end.

update_mouse(Capture, X, Y) ->
  gen_server:call(Capture, {update_mouse, X, Y}).
  
stop(_Room) ->
  gen_server:call(?MODULE, stop).

-record(deskshare, {
  consumer,
  block,
  screen,
  seq,
  size,
  columns,
  rows,
  blocks,
  dirty_map,
  counter,
  writer,
  header
}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

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


init([Consumer, Options]) ->
  process_flag(trap_exit, true),
  erlang:monitor(process, Consumer),
  
  {BW, BH} = Block = proplists:get_value(block, Options),
  {W, H} = Screen = proplists:get_value(screen, Options),
  
  #deskshare{size = Size} = Deskshare = calculate_dimensions(#deskshare{screen = Screen, block = Block}),
  
  Blocks = [{I,clear_field(block_size(I, Deskshare))} || I <- lists:seq(1, Size)],
  
  timer:send_interval(100, frame),
  
  Header = <<((BW div 16) - 1):4, W:12, ((BH div 16) - 1):4, H:12>>,

  % (catch file:delete("/tmp/screen.flv")),
  % {ok, Writer_} = flv_writer:start_link("/tmp/screen.flv", [{sort_buffer,0}]),
  
  {ok, Deskshare#deskshare{
    consumer = Consumer,
    seq = proplists:get_value(seq, Options),
    blocks = Blocks,
    dirty_map = clean_map(Size),
    counter = 0,
    % writer = Writer_,
    header = Header
  }}.


calculate_dimensions(#deskshare{block = {BW,BH}, screen = {W,H}} = Deskshare) ->
  Columns = W div BW + case W rem BW of
    0 -> 0;
    _ -> 1
  end,
  Rows = H div BH + case H rem BH of
    0 -> 0;
    _ -> 1
  end,
  
  Size = Columns*Rows,
  Deskshare#deskshare{columns = Columns, rows = Rows, size = Size}.


%% Right upper block
block_size(I, #deskshare{block = {BW,BH}, screen = {W,H}, size = Size}) when I == Size ->
  {(W rem BW), (H rem BH)};

block_size(I, #deskshare{block = {BW,BH}, screen = {W,_H}, columns = Columns}) when I rem Columns == 0 ->
  {(W rem BW), BH};

block_size(I, #deskshare{block = {BW,BH}, screen = {_W,H}, columns = Columns, rows = Rows}) when (I - 1) div Columns == Rows - 1 ->
  {BW, (H rem BH)};

block_size(_I, #deskshare{block = {BW,BH}}) ->
  {BW, BH}.


clear_field({W,H}) ->
  clear_field(W*H);

clear_field(Size) ->
  ClearRaw = iolist_to_binary([<<16#FFFFFF:24>> || _ <- lists:seq(1, Size)]),
  Z = zlib:open(),
  zlib:deflateInit(Z, 9),
  Clear1 = zlib:deflate(Z, ClearRaw, finish),
  zlib:close(Z),
  [<<(iolist_size(Clear1)):16>>, Clear1].
  


clean_map(Size) ->
  erlang:list_to_tuple([false || _ <- lists:seq(1,Size)]).

mark(Position, Map) ->
  setelement(Position, Map, true).
  
is_marked(Position, Map) ->
  element(Position, Map).
  

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
handle_call({update, Position, Block}, _From, #deskshare{dirty_map = Map, blocks = Blocks, size = Size} = Deskshare) ->
  if
    Position > 0 andalso Position =< Size ->
      Map1 = mark(Position, Map),
      Blocks1 = lists:keystore(Position, 1, Blocks, {Position, Block}),
      {reply, ok, Deskshare#deskshare{dirty_map = Map1, blocks = Blocks1}};
    true ->
      {reply, {error, badsize}, Deskshare}
  end;

handle_call({update_mouse, X, Y}, _From, #deskshare{consumer = Consumer} = Deskshare) ->
  Consumer ! #video_frame{
    content = metadata,
    dts = 0,
    pts = 0,
    stream_id = mixer,
    codec = screen,
    flavor = frame,
    body = [<<"onMouseMove">>, {object, [{x, X},{y,Y}]}]
  },
  {reply, ok, Deskshare};
  
handle_call(stop, _From, Deskshare) ->
  {stop, normal, Deskshare};

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
handle_info({'DOWN', _, process, _Client, _Reason}, Server) ->
  {stop, normal, Server};
  
handle_info(frame, #deskshare{dirty_map = Map, blocks = Blocks, counter = Counter, 
                              size = Size, header = Header, writer = Writer} = Deskshare) ->
  Body = generate_body(Map, is_keyframe(Counter), Size, Blocks),
  DTS = generate_dts(Counter),
  Frame = #video_frame{
    content = video,
    dts = DTS,
    pts = DTS,
    stream_id = mixer,
    codec = screen,
    flavor = is_keyframe(Counter),
    body = iolist_to_binary([Header|Body])
  },
  (catch Writer ! Frame),
  Deskshare#deskshare.consumer ! Frame,
  {noreply, Deskshare#deskshare{dirty_map = clean_map(Size), counter = Counter+1}};
  
  

handle_info(_Info, State) ->
  {noreply, State}.

is_keyframe(Counter) when Counter rem 20 == 0 -> keyframe;
is_keyframe(_) -> frame.

generate_body(Map, IsKeyframe, Size, Blocks) -> 
  lists:map(fun(I) ->
    case {is_marked(I,Map), IsKeyframe} of
      {false,frame} -> <<0,0>>;
      _ -> proplists:get_value(I, Blocks)
    end
  end, lists:seq(1,Size)).

generate_dts(Counter) -> Counter * 100.


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
  
  
  
mock_dimensions() ->
  calculate_dimensions(#deskshare{screen = {95, 97}, block = {10,10}}).
  
block_size_test_() ->
  [
  ?_assertEqual({10,10}, block_size(1, mock_dimensions())),
  ?_assertEqual({5,10}, block_size(10, mock_dimensions())),
  ?_assertEqual({5,10}, block_size(90, mock_dimensions())),
  ?_assertEqual({10,7}, block_size(91, mock_dimensions())),
  ?_assertEqual({10,7}, block_size(99, mock_dimensions())),
  ?_assertEqual({5,7}, block_size(100, mock_dimensions()))
  ].
