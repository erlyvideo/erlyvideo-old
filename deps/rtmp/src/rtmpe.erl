%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMPE module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmpe).
-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").

-export([s2/2, crypt/2, p/0, g/0]).

-define(DH_KEY_SIZE, 128).

-define(DH_P, <<16#ff, 16#ff, 16#ff, 16#ff, 16#ff,
          		16#ff, 16#ff, 16#ff, 16#c9, 16#0f, 16#da, 16#a2, 16#21,
          		16#68, 16#c2, 16#34, 16#c4, 16#c6, 16#62, 16#8b, 16#80,
          		16#dc, 16#1c, 16#d1, 16#29, 16#02, 16#4e, 16#08, 16#8a,
          		16#67, 16#cc, 16#74, 16#02, 16#0b, 16#be, 16#a6, 16#3b,
          		16#13, 16#9b, 16#22, 16#51, 16#4a, 16#08, 16#79, 16#8e,
          		16#34, 16#04, 16#dd, 16#ef, 16#95, 16#19, 16#b3, 16#cd,
          		16#3a, 16#43, 16#1b, 16#30, 16#2b, 16#0a, 16#6d, 16#f2,
          		16#5f, 16#14, 16#37, 16#4f, 16#e1, 16#35, 16#6d, 16#6d,
          		16#51, 16#c2, 16#45, 16#e4, 16#85, 16#b5, 16#76, 16#62,
          		16#5e, 16#7e, 16#c6, 16#f4, 16#4c, 16#42, 16#e9, 16#a6,
          		16#37, 16#ed, 16#6b, 16#0b, 16#ff, 16#5c, 16#b6, 16#f4,
          		16#06, 16#b7, 16#ed, 16#ee, 16#38, 16#6b, 16#fb, 16#5a,
          		16#89, 16#9f, 16#a5, 16#ae, 16#9f, 16#24, 16#11, 16#7c,
          		16#4b, 16#1f, 16#e6, 16#49, 16#28, 16#66, 16#51, 16#ec,
          		16#e6, 16#53, 16#81, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff,
          		16#ff, 16#ff, 16#ff>>).

-define(DH_G, <<2>>).


p() -> <<(size(?DH_P)):32, ?DH_P/binary>>.
g() -> <<(size(?DH_G)):32, ?DH_G/binary>>.

-spec dhKey(Handshake::binary(), handshake_version()) -> {First::binary(), Seed::binary(), Last::binary()}.
dhKey(<<_:1532/binary, P1, P2, P3, P4, _/binary>> = C1, version1) ->
	Offset = (P1+P2+P3+P4) rem 632 + 772,
	<<First:Offset/binary, Seed:?DH_KEY_SIZE/binary, Last/binary>> = C1,
  {First, Seed, Last};

dhKey(<<_:768/binary, P1, P2, P3, P4, _/binary>> = C1, version2) ->
	Offset = (P1+P2+P3+P4) rem 632 + 8,
	<<First:Offset/binary, Seed:?DH_KEY_SIZE/binary, Last/binary>> = C1,
  {First, Seed, Last}.


rc4_key(Key, Data) ->
  <<Out:16/binary, _/binary>> = hmac256:digest_bin(Key, Data),
  % Out = hmac256:digest_bin(Key, Data),
  crypto:rc4_set_key(Out).

crypt(Key, Data) ->
  crypto:rc4_encrypt_with_state(Key, Data).


prepare_keys(KeyIn1, KeyOut1) ->
  D1 = crypto:rand_bytes(?HS_BODY_LEN),
  {KeyIn, D2} = crypt(KeyIn1, D1),
  {KeyOut, _} = crypt(KeyOut1, D2),
  {KeyIn, KeyOut}.

crypto_keys(ServerPublic, ClientPublic, SharedSecret) ->
  KeyOut1 = rc4_key(SharedSecret, ClientPublic),
  KeyIn1 = rc4_key(SharedSecret, ServerPublic),

  prepare_keys(KeyIn1, KeyOut1).


generate_dh(ClientPublic) ->
  P = <<(size(?DH_P)):32, ?DH_P/binary>>,
  G = <<(size(?DH_G)):32, ?DH_G/binary>>,
  {<<?DH_KEY_SIZE:32, ServerPublic:?DH_KEY_SIZE/binary>>, Private} = crypto:dh_generate_key([P, G]),
  SharedSecret = crypto:dh_compute_key(<<(size(ClientPublic)):32, ClientPublic/binary>>, Private, [P, G]),
	{ServerPublic, SharedSecret}.



s2(C2, ?HS_CRYPTED) ->
  Response1 = <<0:32, 3,0,2,1, (crypto:rand_bytes(?HS_BODY_LEN - 8))/binary>>, 
  SchemeVersion = rtmp_handshake:client_scheme_version(C2),

  {_, ClientPublic, _} = dhKey(C2, SchemeVersion),
  {ServerPublic, SharedSecret} = generate_dh(ClientPublic),
  
  {ServerFirst, _, ServerLast} = dhKey(Response1, SchemeVersion),
  Response2 = <<ServerFirst/binary, ServerPublic/binary, ServerLast/binary>>,
  
  {KeyIn, KeyOut} = crypto_keys(ServerPublic, ClientPublic, SharedSecret),
  {KeyIn, KeyOut, Response2}.

  