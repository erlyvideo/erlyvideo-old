%%%---------------------------------------------------------------------------------------
%%% @author     Maxim Treskin <zerthurd@gmail.com>
%%% @copyright  2010 Max Lapshin
%%% @doc        erlyvideo sip callback for pbxmate integration
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(ems_sip_pbxmate).
% -include_lib("esip/include/esip.hrl").
% -include_lib("esip/include/esip_records.hrl").
-include_lib("erlmedia/include/sdp.hrl").
-include("../log.hrl").

-export([
         init/0,
         dialog/3,
         dialog_timeout/1,
         response/1,
         origin/1
        ]).


-record(sip_cb_state, {
          sdp                  :: [binary()],
          client               :: pid(),
          client_ref           :: reference(),
          pbxmate              :: pid(),
          pbxmate_ref          :: reference(),
          pbxmate_listen_ref   :: reference(),
          dialog_timeout       :: integer(),
          response             :: tuple(), % #response{}
          origin               :: tuple()  % #origin{}
         }).

init() ->
  #sip_cb_state{
      dialog_timeout = timer:seconds(10)
     }.

dialog(Request, Origin, State) ->
  URI = esip:'#get-request'(uri, Request),
  MH = esip:'#get-request'(mheaders, Request),
  PH = esip:'#get-request'(headers, Request),
  Body = esip:'#get-request'(body, Request),
  %% Start RTP-process

  %% TrCfg = esip_config:get_config(transport),
  %% Binding = proplists:get_value(binding, TrCfg),
  %% RtpAddress = proplists:get_value(rtp_binding, TrCfg, Binding),

  %% %%{ok, ConsCodec} = ems_sound:init([{from,{pcmu,[]}}, {to,{speex,[]}}, {debug, true}]),
  %% %%ConsCodec = [{from,{pcmu,[]}}, {to,{speex,[]}}, {debug, true}],
  %% RtpOptsCl =
  %%   [
  %%    {parent, self()}
  %%    %%{audio_codec, ConsCodec}
  %%   ],
  %% {ok, RtpClPid} = ertp_sup:start_server({consumer, RtpOptsCl}),
  %% RtpClRef = erlang:monitor(process, RtpClPid),

  %% %%  {ok, ProdCodec} = ems_sound:init([{from,{pcmu,[]}}, {to,{speex,[]}}, {debug, true}]),
  %% %%ProdCodec = ConsCodec,
  %% RtpOptsPbx =
  %%   [
  %%    {parent, self()}
  %%    %%{audio_codec, ProdCodec}
  %%   ],
  %% {ok, RtpPbxPid} = ertp_sup:start_server({consumer, RtpOptsPbx}),
  %% RtpPbxRef = erlang:monitor(process, RtpPbxPid),

  %% ok = rtp_server:set_media(RtpClPid, RtpPbxPid),
  %% ok = rtp_server:set_media(RtpPbxPid, RtpClPid),

  %% {ok, MediaParams} =
  %%   {ok,[{audio,{video_frame,audio,0,0,0,pcmu,config,
  %%                {mono,bit16,8000},
  %%                undefined,
  %%                {video_config,0,0}}}
  %%       ]},

  %% MediaConfigInit = [sdp:prep_media_config(F, []) || F <- MediaParams],
  %% ?DBG("MediaConfigInit:~n~p", [MediaConfigInit]),
  %% ClientMDs = sdp:decode(Body),
  %% ?DBG("ClientMDs:~n~p", [ClientMDs]),

  %% %% FIXME
  %% TagVal = ports,
  %% Proto = udp,

  %% SDPc =
  %%   fun() ->
  %%       SessionDesc =
  %%         #session_desc{
  %%         version = "0",
  %%         originator = #sdp_o{
  %%           username = sdp:make_username(),
  %%           sessionid = sdp:make_session(),
  %%           version = "1",
  %%           netaddrtype = inet4,
  %%           address = RtpAddress},
  %%         name = "Erlyvideo",
  %%         connect = {inet4, RtpAddress},
  %%         attrs = []},

  %%       Opts =
  %%         [begin
  %%            case lists:keyfind(Type, #media_desc.type, ClientMDs) of
  %%              #media_desc{port = RtpPort}
  %%                when is_integer(RtpPort) andalso
  %%                     (RtpPort>0) ->
  %%                {ok, {ports, {ClSRTPPort, _ClSRTCPPort}}, ClListenRef} = rtp_server:listen_ports(RtpClPid, Proto, ports),
  %%                ?DBG("Listen RTP port: ~p", [ClSRTPPort]),
  %%                ok = rtp_server:add_stream(RtpClPid, ClListenRef, Stream, {TagVal, undefined}, []),
  %%                {case Type of audio -> audio_port; video -> video_port end, ClSRTPPort};
  %%              _ -> undefined
  %%            end
  %%          end || #media_desc{type = Type} = Stream <- ClientMDs],
  %%       MediaConfig = [sdp:prep_media_config(F, Opts) || F <- MediaParams],
  %%       ?DBG("MediaConfig:~n~p", [MediaConfig]),
  %%       sdp:encode(SessionDesc, MediaConfig)
  %%   end(),



  %% {PbxListenRef, SDPp} =
  %%   fun() ->
  %%       SessionDesc =
  %%         #session_desc{
  %%         version = "0",
  %%         originator = #sdp_o{
  %%           username = sdp:make_username(),
  %%           sessionid = sdp:make_session(),
  %%           version = "1",
  %%           netaddrtype = inet4,
  %%           address = RtpAddress},
  %%         name = "Erlyvideo",
  %%         connect = {inet4, RtpAddress},
  %%         attrs = []},


  %%       {ok, {ports, {PbxSRTPPort, _PbxSRTCPPort}}, PbxListenRef} = rtp_server:listen_ports(RtpPbxPid, Proto, ports),
  %%       ?DBG("Listen RTP port: ~p", [PbxSRTPPort]),
  %%       Opts = [{audio_port, PbxSRTPPort}],
  %%       MediaConfig = [sdp:prep_media_config(F, Opts) || F <- MediaParams],
  %%       ?DBG("MediaConfig:~n~p", [MediaConfig]),
  %%       {PbxListenRef, sdp:encode(SessionDesc, MediaConfig)}
  %%   end(),


  %% SipOpts =
  %%   [
  %%    {parent, self()},
  %%    {number, "SoliCallPBXTrunk"},
  %%    {address, "192.168.1.7"},
  %%    {port, 5090},
  %%    {from, "test-call"},
  %%    {sdp, SDP}
  %%   ],
  %% {ok, _Pid} = esip_transaction_sup:start_user({originating, SipOpts}),

  Contact = esip:'#new-h_contact'([{uri, URI}, {params,[{<<"transport">>, esip:'#get-origin'(proto, Origin)}]}]),
  NewPH = [{'Contact', [Contact]}],

  Response = esip:'#new-response'([
    {code, 101},
    {reason, "Dialog Establishement"},
    {mheaders, MH},
    {headers, NewPH},
    {body, undefined}
  ]),

  NewCbState =
    State#sip_cb_state{
      sdp = Body,
      %%sdp = SDPc,
      %% client = RtpClPid,
      %% client_ref = RtpClRef,
      %% pbxmate = RtpPbxPid,
      %% pbxmate_ref = RtpPbxRef,
      %% pbxmate_listen_ref = PbxListenRef,
      response = Response
     },

  {ok, DPid} = esip_dialog:start_worker(uas, self(), Request),

  DD = esip_dialog:create_ror(uas, Request, Origin),
  DialogConfig =
    [
     {user_mod, ?MODULE},
     {cb_state, NewCbState}
    ],
  ok = esip_dialog:call_worker(DD, {config, DialogConfig}),

  PbxConfig = ems:get_var(pbxmate, undefined),
  %%PbxName = list_to_binary(proplists:get_value(name, PbxConfig, "SoliCallPBXTrunk")),
  PbxAddress = proplists:get_value(address, PbxConfig, "127.0.0.1"),
  PbxPort = proplists:get_value(port, PbxConfig, esip:default_port()),


  MHeaders = esip:'#get-request'(mheaders, Request),
  From = esip:'#get-mheaders'(from, MHeaders),
  FromURI = esip:'#get-h_from'(uri, From),
  FromName = esip:'#get-h_from'(name, From),
  To = esip:'#get-mheaders'(to, MHeaders),
  ToURI = esip:'#get-h_to'(uri, To),
  ToName = esip:'#get-h_to'(name, To),

  RequestURI = esip:'#new-sip_uri'([
    {schema, sip},
    {name, <<"SoliCallPBXTrunk">>},
    {domain, {inet, {192,168,1,2}}},
    {port, 5090}
   ]),

  %% NewRequest =
  %%   Request#request{
  %%     %%uri = esip_db:contact({sip_uri,sip,<<"SoliCallPBXTrunk">>,<<"internal">>,undefined}),
  %%     uri = RequestURI,
  %%     %%body = SDPp
  %%     body = Body
  %%   },

  PbxUserOpts =
    [
     {parent, self()},
     {address, esip_parser_util:p_host(PbxAddress)},
     {port, PbxPort},
     {request_uri, RequestURI},
     {from, FromURI},
     {from_name, FromName},
     {to, ToURI},
     {to_name, ToName},
     {contact, proplists:get_value('Contact', PH)},
     %%{sdp, SDPp}
     {sdp, Body},
     {opposite_dialog, DPid}
    ],
  %%{ok, Pid} = esip_transaction_sup:start_user({originating, SipOpts}),

  %%{ok, _} = esip_dialog:start_worker(uac, undefined, NewRequest),
  %%DDOpts = esip_dialog:create_ror(uac, NewRequest, Origin, PbxUserOpts),
  %%ok = esip_dialog:call_worker(DDOpts, {originating, PbxUserOpts}),

  {ok, _TUPid} = esip_transaction_sup:start_user({originating, PbxUserOpts}),

  {ok, Response, NewCbState}.

dialog_timeout(#sip_cb_state{dialog_timeout = DTO}) ->
  DTO.

response(#sip_cb_state{response = Response}) ->
  Response.

origin(#sip_cb_state{origin = Origin}) ->
  Origin.

%% dialog_opposite(Request, Origin, State) ->
%%   ClDialog = esip_dialog:get_pid(Request),

