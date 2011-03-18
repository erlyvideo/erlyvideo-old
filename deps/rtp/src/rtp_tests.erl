%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        RTP tests module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/ertp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtp.
%%%
%%% erlang-rtp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtp_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include("../include/rtp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("eunit/include/eunit.hrl").


rtp_state1() ->
  {rtp_state,11143,1300433001498,193161,undefined,
                             undefined,90.0,h264,
                             {h264_buffer,1300433001564.6667,
                              {h264,undefined,0,undefined,32,undefined,
                               undefined,undefined},
                              <<>>,undefined},
                             {stream_info,video,2,h264,
                              <<1,77,0,30,255,225,0,21,39,77,64,30,169,24,60,23,
                                252,184,3,80,96,16,107,108,43,94,247,192,64,1,0,
                                4,40,222,9,200>>,
                              undefined,undefined,
                              {video_params,480,368,0},
                              90.0,
                              [{control,"trackid=2"}]},
                             undefined,15072851865301416158}.


rtp_decode_test() ->
  Packet = <<128,97,43,135,0,3,21,177,0,201,89,153,6,5,17,3,
                              135,244,78,205,10,75,220,161,148,58,195,212,155,
                              23,31,0,128>>,
  ?assertMatch({ok, #rtp_state{}, #video_frame{flavor = frame}}, rtp_decoder:decode(Packet, rtp_state1())).