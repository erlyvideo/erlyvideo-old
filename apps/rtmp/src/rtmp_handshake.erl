%%% @private
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @author     Thanks to all those guys, that have found how to sign handshake.
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP handshake module
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information
%%% also good reference is http://red5.googlecode.com/svn/java/server/trunk/src/org/red5/server/net/rtmp/RTMPHandshake.java
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
-module(rtmp_handshake).
-version(1.0).

-export([server/1, c1/0, c2/1, client_scheme_version/1]).


-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(DIGEST_SIZE, 32).


-define(GENUINE_FMS_KEY, <<"Genuine Adobe Flash Media Server 001",
                        16#f0,16#ee,16#c2,16#4a,16#80,16#68,16#be,16#e8,16#2e,16#00,16#d0,16#d1,
16#02,16#9e,16#7e,16#57,16#6e,16#ec,16#5d,16#2d,16#29,16#80,16#6f,16#ab,16#93,16#b8,16#e6,16#36,
16#cf,16#eb,16#31,16#ae>>).

-define(GENUINE_FP_KEY, <<"Genuine Adobe Flash Player 001",
          16#F0, 16#EE, 16#C2, 16#4A, 16#80, 16#68, 16#BE, 16#E8,
          16#2E, 16#00, 16#D0, 16#D1, 16#02, 16#9E, 16#7E, 16#57,
          16#6E, 16#EC, 16#5D, 16#2D, 16#29, 16#80, 16#6F, 16#AB,
          16#93, 16#B8, 16#E6, 16#36, 16#CF, 16#EB, 16#31, 16#AE>>).




-spec clientDigest(Handshake::binary(), handshake_version()) -> {First::binary(), Seed::binary(), Last::binary()}.

% Flash before 10.0.32.18
clientDigest(<<_:8/binary, P1, P2, P3, P4, _/binary>> = C1, version1) ->
	Offset = (P1+P2+P3+P4) rem 728 + 12,
	<<First:Offset/binary, Seed:?DIGEST_SIZE/binary, Last/binary>> = C1,
  {First, Seed, Last};


% Flash from 10.0.32.18
clientDigest(<<_:772/binary, P1, P2, P3, P4, _/binary>> = C1, version2) ->
	Offset = (P1+P2+P3+P4) rem 728 + 776,
	<<First:Offset/binary, Seed:?DIGEST_SIZE/binary, Last/binary>> = C1,
  {First, Seed, Last}.




-spec validate_scheme_version(C1::binary(), Version::handshake_version()) -> boolean().
validate_scheme_version(C1, Version) ->
  {First, ClientDigest, Last} = clientDigest(C1, Version),
  {Key, _} = erlang:split_binary(?GENUINE_FP_KEY, 30),
  ClientDigest == hmac256:digest_bin(Key, <<First/binary, Last/binary>>).


-spec client_scheme_version(C1::binary()) -> handshake_version().
client_scheme_version(C1) ->
  case validate_scheme_version(C1, version1) of
    true -> version1;
    false -> case validate_scheme_version(C1, version2) of
      true -> version2;
      false -> version1
    end
  end.

c1() ->
  [?HS_UNCRYPTED, <<0:32, 0:32>>, crypto:rand_bytes(?HS_BODY_LEN - 8)].
  
c2(<<_Time:32, _V1,_V2,_V3,_V4, _Rand/binary>> = S1) ->
  % io:format("Server: ~p, ~p.~p.~p.~p~n", [Time, V1, V2, V3, V4]),
  S1.


% server(<<?HS_UNCRYPTED, C1:?HS_BODY_LEN/binary>>) ->
%   {uncrypted, [?HS_UNCRYPTED, s1(), s2(C1)]};
% 
server(<<Encryption, C2:?HS_BODY_LEN/binary>>) ->
  
  SchemeVersion = client_scheme_version(C2),
  
  {KeyIn, KeyOut, Response2} = case Encryption of
    ?HS_CRYPTED ->
      rtmpe:s2(C2, Encryption);
    ?HS_UNCRYPTED ->
      {undefined, undefined, <<0:32, 3,0,2,1, (crypto:rand_bytes(?HS_BODY_LEN - 8))/binary>>}
  end,
        
      

  %% Now generate digest of S2

  {Digest1, _, Digest2} = clientDigest(Response2, SchemeVersion),
  {ServerFMSKey, _} = erlang:split_binary(?GENUINE_FMS_KEY, 36),
  ServerDigest = hmac256:digest_bin(ServerFMSKey, <<Digest1/binary, Digest2/binary>>),

  S2 = <<Digest1/binary, ServerDigest/binary, Digest2/binary>>,

  % ?D({serverDH, size(ServerFirst), serverDigest, size(Digest1)}),
  %% ------ S3
  
  Response4 = crypto:rand_bytes(?HS_BODY_LEN - 32),
  {_, ClientDigest, _} = clientDigest(C2, SchemeVersion),
  TempHash = hmac256:digest_bin(?GENUINE_FMS_KEY, ClientDigest),
  ClientHash = hmac256:digest_bin(TempHash, Response4),
  S3 = <<Response4/binary, ClientHash/binary>>,

  case Encryption of
    ?HS_CRYPTED ->
      {crypted, [?HS_CRYPTED, S2, S3], KeyIn, KeyOut};
    ?HS_UNCRYPTED ->
      {uncrypted, [?HS_UNCRYPTED, S2, S3]}
  end.
  
