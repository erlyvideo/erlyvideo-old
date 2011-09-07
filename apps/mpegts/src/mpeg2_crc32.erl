%%% Authorship lost, no copyrights
-module (mpeg2_crc32).


%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

-export ([crc32/1,
          crc32/2,
          append_crc32/1,
          append_crc32/2,
          verify_crc32/1,
          verify_crc32/2]).

-on_load(start/0).
-export([start/0]).
-export([benchmark/0, benchmark_calc/2]).

%%====================================================================
%% Internal exports
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

-define (INITIAL_CRC32, 16#FFFFFFFF).


%%====================================================================
%% Records
%%====================================================================

%%====================================================================
%% External functions
%%====================================================================

start() ->
  load_nif(erlang:system_info(otp_release) >= "R13B04").
  
load_nif(true) ->
  Load = case (catch erlang:load_nif(code:lib_dir(mpegts,priv)++ "/mpeg2_crc32", 0)) of
    ok -> ok;
    {'EXIT', Reason} -> {error, Reason}
  end,
  case Load of
    ok -> ok;
    _ -> io:format("mpeg2_crc32 nif not loaded. MPEG-TS is slower~n")
  end,
  % io:format("Load mpeg2_crc32: ~p~n", [Load]),
  ok;

load_nif(false) ->
  ok.


%%
%% crc32
%%

crc32 (Buffer) ->
  crc32 (Buffer, ?INITIAL_CRC32).


%%
%% append_crc32
%%

append_crc32 (Buffer) ->
  append_crc32 (Buffer, ?INITIAL_CRC32).

%%
%% append_crc32
%%

append_crc32 (Buffer, InitCRC32) ->
  CRC32 = case crc32 (Buffer, InitCRC32) of
            0 ->
              ?INITIAL_CRC32;
            Value ->
              Value
          end,
  << Buffer/binary, CRC32: 32 >>.

%%
%% verify_crc32
%%

verify_crc32 (Buffer) ->
  verify_crc32 (Buffer, ?INITIAL_CRC32).

%%
%% verify_crc32
%%

verify_crc32 (Buffer, InitCRC32) ->
  ContentsSize = size (Buffer) - 4,

  << Contents: ContentsSize/binary, StoredCRC32: 32 >> = Buffer,

  case StoredCRC32 of
    16#00000000 ->
      Contents;
    StoredCRC32 ->
      case crc32 (Contents, InitCRC32) of
        StoredCRC32 ->
          Contents;
        _ ->
          exit (incorrect_crc)
      end
  end.

%%====================================================================
%% Internal functions
%%====================================================================

%%
%% crc32_erl
%%


crc32(Buffer, InitCRC32) when is_list(Buffer) ->
  crc32(list_to_binary(Buffer), InitCRC32);

crc32(Buffer, InitCRC32) when is_binary(Buffer) ->
  Table = { 16#00000000, 16#04c11db7, 16#09823b6e, 16#0d4326d9,
            16#130476dc, 16#17c56b6b, 16#1a864db2, 16#1e475005,
            16#2608edb8, 16#22c9f00f, 16#2f8ad6d6, 16#2b4bcb61,
            16#350c9b64, 16#31cd86d3, 16#3c8ea00a, 16#384fbdbd,
            16#4c11db70, 16#48d0c6c7, 16#4593e01e, 16#4152fda9,
            16#5f15adac, 16#5bd4b01b, 16#569796c2, 16#52568b75,
            16#6a1936c8, 16#6ed82b7f, 16#639b0da6, 16#675a1011,
            16#791d4014, 16#7ddc5da3, 16#709f7b7a, 16#745e66cd,
            16#9823b6e0, 16#9ce2ab57, 16#91a18d8e, 16#95609039,
            16#8b27c03c, 16#8fe6dd8b, 16#82a5fb52, 16#8664e6e5,
            16#be2b5b58, 16#baea46ef, 16#b7a96036, 16#b3687d81,
            16#ad2f2d84, 16#a9ee3033, 16#a4ad16ea, 16#a06c0b5d,
            16#d4326d90, 16#d0f37027, 16#ddb056fe, 16#d9714b49,
            16#c7361b4c, 16#c3f706fb, 16#ceb42022, 16#ca753d95,
            16#f23a8028, 16#f6fb9d9f, 16#fbb8bb46, 16#ff79a6f1,
            16#e13ef6f4, 16#e5ffeb43, 16#e8bccd9a, 16#ec7dd02d,
            16#34867077, 16#30476dc0, 16#3d044b19, 16#39c556ae,
            16#278206ab, 16#23431b1c, 16#2e003dc5, 16#2ac12072,
            16#128e9dcf, 16#164f8078, 16#1b0ca6a1, 16#1fcdbb16,
            16#018aeb13, 16#054bf6a4, 16#0808d07d, 16#0cc9cdca,
            16#7897ab07, 16#7c56b6b0, 16#71159069, 16#75d48dde,
            16#6b93dddb, 16#6f52c06c, 16#6211e6b5, 16#66d0fb02,
            16#5e9f46bf, 16#5a5e5b08, 16#571d7dd1, 16#53dc6066,
            16#4d9b3063, 16#495a2dd4, 16#44190b0d, 16#40d816ba,
            16#aca5c697, 16#a864db20, 16#a527fdf9, 16#a1e6e04e,
            16#bfa1b04b, 16#bb60adfc, 16#b6238b25, 16#b2e29692,
            16#8aad2b2f, 16#8e6c3698, 16#832f1041, 16#87ee0df6,
            16#99a95df3, 16#9d684044, 16#902b669d, 16#94ea7b2a,
            16#e0b41de7, 16#e4750050, 16#e9362689, 16#edf73b3e,
            16#f3b06b3b, 16#f771768c, 16#fa325055, 16#fef34de2,
            16#c6bcf05f, 16#c27dede8, 16#cf3ecb31, 16#cbffd686,
            16#d5b88683, 16#d1799b34, 16#dc3abded, 16#d8fba05a,
            16#690ce0ee, 16#6dcdfd59, 16#608edb80, 16#644fc637,
            16#7a089632, 16#7ec98b85, 16#738aad5c, 16#774bb0eb,
            16#4f040d56, 16#4bc510e1, 16#46863638, 16#42472b8f,
            16#5c007b8a, 16#58c1663d, 16#558240e4, 16#51435d53,
            16#251d3b9e, 16#21dc2629, 16#2c9f00f0, 16#285e1d47,
            16#36194d42, 16#32d850f5, 16#3f9b762c, 16#3b5a6b9b,
            16#0315d626, 16#07d4cb91, 16#0a97ed48, 16#0e56f0ff,
            16#1011a0fa, 16#14d0bd4d, 16#19939b94, 16#1d528623,
            16#f12f560e, 16#f5ee4bb9, 16#f8ad6d60, 16#fc6c70d7,
            16#e22b20d2, 16#e6ea3d65, 16#eba91bbc, 16#ef68060b,
            16#d727bbb6, 16#d3e6a601, 16#dea580d8, 16#da649d6f,
            16#c423cd6a, 16#c0e2d0dd, 16#cda1f604, 16#c960ebb3,
            16#bd3e8d7e, 16#b9ff90c9, 16#b4bcb610, 16#b07daba7,
            16#ae3afba2, 16#aafbe615, 16#a7b8c0cc, 16#a379dd7b,
            16#9b3660c6, 16#9ff77d71, 16#92b45ba8, 16#9675461f,
            16#8832161a, 16#8cf30bad, 16#81b02d74, 16#857130c3,
            16#5d8a9099, 16#594b8d2e, 16#5408abf7, 16#50c9b640,
            16#4e8ee645, 16#4a4ffbf2, 16#470cdd2b, 16#43cdc09c,
            16#7b827d21, 16#7f436096, 16#7200464f, 16#76c15bf8,
            16#68860bfd, 16#6c47164a, 16#61043093, 16#65c52d24,
            16#119b4be9, 16#155a565e, 16#18197087, 16#1cd86d30,
            16#029f3d35, 16#065e2082, 16#0b1d065b, 16#0fdc1bec,
            16#3793a651, 16#3352bbe6, 16#3e119d3f, 16#3ad08088,
            16#2497d08d, 16#2056cd3a, 16#2d15ebe3, 16#29d4f654,
            16#c5a92679, 16#c1683bce, 16#cc2b1d17, 16#c8ea00a0,
            16#d6ad50a5, 16#d26c4d12, 16#df2f6bcb, 16#dbee767c,
            16#e3a1cbc1, 16#e760d676, 16#ea23f0af, 16#eee2ed18,
            16#f0a5bd1d, 16#f464a0aa, 16#f9278673, 16#fde69bc4,
            16#89b8fd09, 16#8d79e0be, 16#803ac667, 16#84fbdbd0,
            16#9abc8bd5, 16#9e7d9662, 16#933eb0bb, 16#97ffad0c,
            16#afb010b1, 16#ab710d06, 16#a6322bdf, 16#a2f33668,
            16#bcb4666d, 16#b8757bda, 16#b5365d03, 16#b1f740b4 },
  crc32_erl_1(Buffer, Table, InitCRC32).

crc32_erl_1(<<>>, _Table, CRC32) ->
  CRC32;

crc32_erl_1(<<Value, Buffer/binary>>, Table, CRC32) ->
  crc32_erl_1 (Buffer, Table, ((CRC32 bsl 8) band 16#FFFFFFFF) bxor element (1 + ((CRC32 bsr 24) bxor Value), Table)).



%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

crc32_1_test() ->
  [?_assertEqual(4294967295, crc32(<<"">>)),
   ?_assertEqual(3794043894, crc32(<<"111111">>)),
   ?_assertEqual(0, crc32(<<255,255,255,255>>)),
   ?_assertEqual(2985771188, crc32(<<255,255,255,255,255>>))].


benchmark() ->
  Bin = iolist_to_binary(lists:map(fun(N) -> integer_to_list(N) end, lists:seq(1, 100))),
  Count = 100000,
  {Time, _} = timer:tc(?MODULE, benchmark_calc, [Bin, Count]),
  Time/Count.

benchmark_calc(_, 0) -> ok;
benchmark_calc(Bin, N) -> crc32(Bin), benchmark_calc(Bin, N-1).


