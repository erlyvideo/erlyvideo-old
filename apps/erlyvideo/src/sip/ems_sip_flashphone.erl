%%%---------------------------------------------------------------------------------------
%%% @author     Maxim Treskin <zerthurd@gmail.com>
%%% @copyright  2010 Max Lapshin
%%% @doc        erlyvideo sip callback
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
-module(ems_sip_flashphone).
-author('Max Lapshin <max@maxidoors.ru>').
-author('Maxim Treskin <zerthurd@gmail.com>').
-include("../log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/sdp.hrl").

-behaviour(gen_fsm).

-export([
         start_link/1
        ]).

-export([
         init/1,
         d_active/2,
         d_active/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).


-export([
         cb_init/1,
         dialog/2,
         progress/3,
         progress_timeout/1,
         create_dialog/3,
         dialog_timeout/1,
         response/1,
         origin/1
        ]).

-export([
         ack/2,
         register/3,
         unregister/1,
         unregister/2,
         reregister/2,
         accept_call/1,
         decline_call/1,
         call/3
        ]).


-record(sip_cb_state, {
          pid                  :: pid(),
          media                :: pid(),
          rtp                  :: pid(),
          rtmp                 :: pid(),
          stream_in            :: binary(),
          stream_out           :: binary(),
          sdp                  :: [binary()],
          tu                   :: pid(),
          client               :: pid(),
          client_ref           :: reference(),
          progress_timeout     :: integer(),
          dialog_timeout       :: integer(),
          opp_request          :: tuple(), % #request{}
          opp_response         :: tuple(), % #response{}
          request              :: tuple(), % #request{}
          response             :: tuple(), % #response{}
          origin               :: tuple()  % #origin{}
         }).

-record(state, {
          tu                   :: pid(),
          tu_ref               :: reference(),
          name                 :: list(),
          media                :: pid(),
          rtmp                 :: pid(),
          rtmp_ref             :: reference(),
          sdp                  :: term(),
          rtp                  :: pid(),
          rtp_ref              :: reference(),
          stream_in            :: binary(),
          stream_out           :: binary()
         }).

start_link(Args) ->
  gen_fsm:start_link(?MODULE, Args, []).

%%% gen_fsm callbacks
init(Args) ->
  ?DBG("Args: ~p", [Args]),
  ?DBG("Start dialog process for flash", []),
  TU = proplists:get_value(tu, Args),
  TURef =
    if is_pid(TU) ->
        erlang:monitor(process, TU);
       true -> undefined
    end,
  Name = binary_to_list(proplists:get_value(name, Args)),
  {ok, RTMP} = esip_registrator:set_dialog(Name, self()),
  RTMPRef =
    if is_pid(RTMP) ->
        erlang:monitor(process, RTMP);
       true -> undefined
    end,
  {ok, d_active, #state{
         tu = TU,
         tu_ref = TURef,
         rtmp = RTMP,
         rtmp_ref = RTMPRef,
         name = Name
        }}.

d_active(_Event, State) ->
  ?DBG("Unhandled event: ~p", [_Event]),
  {next_state, d_active, State}.

d_active({opposite, {ack}}, _From,
         #state{rtp = RTP,
                stream_in = _StreamIn,
                stream_out = StreamOut} = State) ->
  ?DBG("ACK from opposite", []),
  ack(RTP, StreamOut),
  {reply, ok, d_active, State};
d_active({opposite, {bye}}, _From,
         #state{} = State) ->
  ?DBG("BYE from opposite", []),
  {stop, normal, ok, State};
d_active({opposite, {ok, _Opts, _Response}}, _From,
         #state{} = State) ->
  ?DBG("OK from opposite", []),
  {reply, ok, d_active, State};
d_active({opposite, {ringing, _Response}}, _From,
         #state{} = State) ->
  ?DBG("Ringing from opposite", []),
  {reply, ok, d_active, State};

d_active({data, _LT, {ack, _URI, _UserMod}}, _From,
         #state{rtp = RTP, stream_out = StreamOut} = State) ->
  ?DBG("ACK DIALOG: ~p:~n", [self()]),
  %%send_opposite(OppDPid, {ack}),
  ack(RTP, StreamOut),
  {reply, ok, d_active, State#state{}};

d_active(Event, _From, State) ->
  ?DBG("Unhandled sync event: ~p", [Event]),
  Error = {unknown_call, Event},
  {stop, Error, {error, Error}, State}.

handle_event(Event, _StateName, State) ->
  ?DBG("Unhandled event: ~p", [Event]),
  Error = {unknown_event, Event},
  {stop, Error, State}.

handle_sync_event(Event, _From, _StateName, State) ->
  ?DBG("Unhandled sync event: ~p", [Event]),
  Error = {unknown_sync_event, Event},
  {reply, Error, {error, Error}, State}.

handle_info({config, Media, StreamIn, StreamOut, RTP}, StateName, State) ->
  ?DBG("Set media: ~p, ~p, ~p, ~p", [Media, StreamIn, StreamOut, RTP]),
  RTPRef = erlang:monitor(process, RTP),
  {next_state, StateName,
   State#state{media = Media,
               stream_in = StreamIn,
               stream_out = StreamOut,
               rtp = RTP,
               rtp_ref = RTPRef
              }};

handle_info({set_tu, TU}, StateName,
            #state{} = State) ->
  ?DBG("Set TU: ~p", [TU]),
  TURef =
    if is_pid(TU) ->
        erlang:monitor(process, TU);
       true -> undefined
    end,
  {next_state, StateName,
   State#state{
     tu = TU,
     tu_ref = TURef
    }};

handle_info({Action, Response, _Origin,
             #sip_cb_state{} = CbState}, StateName,
            #state{} = State)
  when Action =:= ok orelse
       Action =:= ringing ->
  case Action of
    ringing ->
      ?DBG("Ringing", []);
    ok ->
      ?DBG("OK", [])
  end,
  SDP = esip:'#get-response'(body, Response),
  if is_binary(SDP) andalso size(SDP) > 0 ->
      NewState = start_media(SDP, CbState, State);
     true ->
      NewState = State
  end,
  {next_state, StateName, NewState};

handle_info({declined, _Response}, _StateName,
            #state{rtp = RTP,
                   rtmp = RTMP} = State) ->
  ?DBG("Declined", []),
  if is_pid(RTP) ->
      rtp_server:stop(RTP);
     true -> ok
  end,
  if is_pid(RTMP) ->
      apps_sip:bye(RTMP);
     true -> ok
  end,
  {stop, normal, State#state{rtp = undefined}};

handle_info({bye}, _StateName,
            #state{rtmp = RTMP} = State) ->
  ?DBG("Bye", []),
  if is_pid(RTMP) ->
      apps_sip:bye(RTMP);
     true -> ok
  end,
  {stop, normal, State};

handle_info({send_create}, StateName,
            #state{tu = TU} = State) ->
  esip_transaction_user:async_event(TU, {dialog_created, self()}),
  {next_state, StateName, State};

handle_info(accept_call, StateName,
            #state{tu = TUPid} = State) ->
  ?DBG("Accept call", []),
  gen_fsm:send_event(TUPid, {opposite, {accept_call}}),
  {next_state, StateName, State};

handle_info(decline_call, StateName,
            #state{tu = TUPid} = State) ->
  ?DBG("Decline call", []),
  gen_fsm:send_event(TUPid, {opposite, {decline_call}}),
  {next_state, StateName, State};

handle_info({'DOWN', RTPRef, process, RTP, _Reason}, StateName,
            #state{rtp_ref = RTPRef} = State) ->
  ?DBG("RTP Down: ~p", [RTP]),
  {next_state, StateName,
   State#state{rtp = undefined,
               rtp_ref = undefined}};
handle_info({'DOWN', RTMPRef, process, RTMP, _Reason}, StateName,
            #state{tu = TU,
                   rtmp_ref = RTMPRef} = State) ->
  ?DBG("RTMP Down: ~p", [RTMP]),
  gen_fsm:send_event(TU, {opposite, {decline_call}}),
  {next_state, StateName,
   State#state{rtmp = undefined,
               rtmp_ref = undefined}};
handle_info({'DOWN', TURef, process, TU, _Reason}, _StateName,
            #state{tu_ref = TURef} = State) ->
  ?DBG("TU Down: ~p", [TU]),
  {stop, normal,
   State#state{tu = undefined,
               tu_ref = undefined}};
handle_info(_Info, StateName, State) ->
  ?DBG("Unhandled info: ~p", [_Info]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName,
          #state{rtp = RTP,
                 rtmp = RTMP,
                 name = Name} = _State) ->
  if is_pid(RTP) ->
      rtp_server:stop(RTP);
     true -> pass
  end,
  if is_pid(RTMP) ->
      apps_sip:bye(RTMP);
     true -> pass
  end,
  esip_registrator:set_dialog(Name, undefined),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.



cb_init(Args) ->
  esip_dialog_sup:start_mod_sup(?MODULE),
  {ok, Pid} = esip_dialog_sup:start_worker(?MODULE, Args),
  TU = proplists:get_value(tu, Args),
  {ok, #sip_cb_state{
     pid = Pid,
     tu = TU,
     progress_timeout = timer:seconds(10),
     dialog_timeout = timer:seconds(10)
    }}.


%%--------------------------------------------------------------------
%% @spec (Number::string(), Client::pid()) -> {ok, Ref}
%% @doc Registers process under specific number
%%
%% @end
%%--------------------------------------------------------------------
register(Number, Password, Client) when is_pid(Client) ->
  esip_registrator:register(Number, Password, Client, ?MODULE),
  send_reg(Number, Password).

unregister(Client) when is_pid(Client) ->
  {ok, OrigNameS, Password, _} = esip_registrator:find(Client),
  ?MODULE:unregister(OrigNameS, Password).

unregister(NameS, Password) ->
  Name = list_to_binary(NameS),
  send_reg(Name, Password, 0).

send_reg(Number, Password) ->
  send_reg(Number, Password, 3600).

send_reg(Number, Password, Expires) ->
  FlashPhoneConfig = ems:get_var(flashphone, undefined),
  case proplists:get_value(sip, FlashPhoneConfig) of
    undefined -> ok;
    SipCfg ->
      Dom = proplists:get_value(domain, SipCfg, "localhost"),
      DomainName =
        case inet_parse:address(Dom) of
          {ok, {_,_,_,_} = DomIPv4} ->
            {inet, DomIPv4};
          _ ->
            list_to_binary(Dom)
        end,
      RegAddress = proplists:get_value(proxy_addr, SipCfg, "127.0.0.1"),
      RegPort = proplists:get_value(proxy_port, SipCfg, esip:default_port()),
      %%Password = proplists:get_value(password, SipCfg, ""),
      NatRouter = proplists:get_value(nat_router, SipCfg),

      Domain = esip:'#new-sip_uri'([{domain, DomainName}, {port, RegPort}]),
      FromURI = ToURI = esip:uri(Number, DomainName),
      FromName = ToName = "Flash client " ++ Number,
      RegUserOpts =
        [
         {address, esip_parser_util:p_host(RegAddress)},
         {port, RegPort},
         {register_domain, Domain},
         {user_name, Number},
         {password, Password},
         {nat_router, NatRouter},
         {from, FromURI},
         {from_name, FromName},
         {to, ToURI},
         {to_name, ToName},
         {contact_name, Number},
         {expires, Expires}
        ],
      {ok, _TUPid} = esip_transaction_sup:start_user({registration, RegUserOpts})
  end.

reregister(Number, Password) ->
  send_reg(Number, Password).


%%--------------------------------------------------------------------
%% @spec (Number::string(), Options::proplist()) -> {ok, Ref}
%% @doc Start call to number
%%
%% @end
%%--------------------------------------------------------------------
call(Name, Options, Client) when is_list(Name) ->
  call(list_to_binary(Name), Options, Client);
call(Name, _Options, Client) when is_binary(Name) ->
  {OrigName, Password, FromName, IsTmp} =
    case esip_registrator:find(Client) of
      {ok, ONS, PW, _} -> {list_to_binary(ONS), PW, list_to_binary(ONS), false};
      _ ->
        TmpOrigName = esip_util:rand_str(8),
        FlashPhoneConfig = ems:get_var(flashphone, undefined),
        SipCfg = proplists:get_value(sip, FlashPhoneConfig, []),
        ON = proplists:get_value(peer_name, SipCfg),
        OP = proplists:get_value(peer_password, SipCfg),
        {list_to_binary(ON), list_to_binary(OP), list_to_binary(TmpOrigName), true}
    end,
  if IsTmp ->
      %% TODO: add support for users pool
      esip_registrator:register(OrigName, Password, self(), ?MODULE);
     true ->
      pass
  end,
  {ok, CbState} = ?MODULE:cb_init([{name, FromName}]),
  case originating(OrigName, Password, Name, FromName, CbState) of
    {ok, TUPid} ->
      CbState#sip_cb_state.pid ! {set_tu, TUPid},
      {ok, TUPid};
    Other -> Other
  end.


accept_call(Client) ->
  {ok, _OrigNameS, _Password, Dialog} = esip_registrator:find(Client),
  if is_pid(Dialog) ->
      Dialog ! accept_call,
      {ok, Dialog};
     true ->
      undefined
  end.

decline_call(Client) ->
  {ok, _OrigNameS, _Password, Dialog} = esip_registrator:find(Client),
  if is_pid(Dialog) ->
      Dialog ! decline_call,
      {ok, Dialog};
     true ->
      undefined
  end.

originating(OrigName, Password, Name, FromName, #sip_cb_state{pid = DPid} = CbState) ->
  ?DBG("Name: ~p, DPid: ~p", [Name, DPid]),
  StreamIn = <<FromName/binary, <<"#-in">>/binary >>,
  StreamOut = <<FromName/binary, <<"#-out">>/binary >>,

  RtpConfig = ems:get_var(rtp, undefined),
  Binding = proplists:get_value(binding, RtpConfig),


  Sess = #sdp_session{name = "Esip flash originating",
                      connect = {inet, Binding},
                      originator = #sdp_o{
                        username = sdp:make_username(),
                        sessionid = sdp:make_session(),
                        version = "1",
                        netaddrtype = inet,
                        address = Binding
                       }, attrs = []},

  ?DBG("Session:~n~p", [Sess]),

  MediaInfo =
    #media_info{flow_type = stream,
                audio = [#stream_info{content = audio, stream_id = 1, codec = pcma,
                                      params = {audio_params,1,8000}}],
                options = [{sdp_session, Sess}]},


  {ok, Media} = media_provider:create(default, StreamIn,
                                      [{media_info, MediaInfo},
                                       {type,live},
                                       {source_shutdown,shutdown}]),
  ?DBG("Media: ~p", [Media]),

  COpt = [{rate, 8000},{channels, 1}],
  TranscodeOpts = [{transcode_in, {{pcma, COpt}, {speex, COpt}}},
                   {transcode_out, {{speex, COpt}, {pcma, COpt}}}],

  {ok, RTP} = rtp:start_server([{media_info_loc, MediaInfo},
                                {consumer, Media}] ++ TranscodeOpts),
  ?DBG("Started RTP server: ~p", [RTP]),
  {ok, {PortRTP, PortRTCP}} = rtp_server:listen_ports(RTP, audio, [{transport, udp}]),

  ?DBG("RTP: ~p, ~p, ~p", [RTP, PortRTP, PortRTCP]),
  DPid ! {config, Media, StreamIn, StreamOut, RTP},
  MediaInfoReply1 = rtp_server:media_info_loc(RTP),
  ?DBG("MediaInfoReply1:~n~p", [MediaInfoReply1]),
  SDP = sdp:encode(MediaInfoReply1),

  ?DBG("SDP:~n~p", [SDP]),

  NewCbState =
    CbState#sip_cb_state{
      sdp = SDP,
      rtp = RTP,
      rtmp = self(),
      media = Media,
      stream_in = StreamIn,
      stream_out = StreamOut
     },

  FlashPhoneConfig = ems:get_var(flashphone, undefined),
  case proplists:get_value(sip, FlashPhoneConfig) of
    undefined -> ok;
    SipCfg ->
      Dom = proplists:get_value(domain, SipCfg, "localhost"),
      DomainName =
        case inet_parse:address(Dom) of
          {ok, {_,_,_,_} = DomIPv4} ->
            {inet, DomIPv4};
          _ ->
            list_to_binary(Dom)
        end,
      RegAddress = proplists:get_value(proxy_addr, SipCfg, "127.0.0.1"),
      RegPort = proplists:get_value(proxy_port, SipCfg, esip:default_port()),
      NatRouter = proplists:get_value(nat_router, SipCfg),

      ToURI = esip:'#new-sip_uri'([{name, Name}, {domain, DomainName}]),
      FromURI = esip:'#new-sip_uri'([{name, OrigName}, {domain, DomainName}]),

      SipOpts =
        [
         {address, esip_parser_util:p_host(RegAddress)},
         {port, RegPort},
         {request_uri, ToURI},
         {nat_router, NatRouter},

         {user_name, OrigName},
         {password, Password},

         {from, FromURI},
         {from_name, FromName},

         {to, ToURI},
         %%{to_name, ToName},

         {contact_name, OrigName},
         {sdp, SDP},
         {dialog, DPid},
         {user_mod, ?MODULE},
         {cb_state, NewCbState}
        ],

      {ok, TUPid} = esip_transaction_sup:start_user({originating, SipOpts}),

      %%{ok, _} = esip_dialog:start_worker(uac, undefined, NewRequest),
      %%DDOpts = esip_dialog:create_ror(uac, NewRequest, Origin, PbxUserOpts),
      %%ok = esip_dialog:call_worker(DDOpts, {originating, PbxUserOpts}),

      %%{ok, TUPid} = esip_transaction_sup:start_user({originating, PbxUserOpts}),
      {ok, TUPid}
  end.

progress(Request,
         Origin,
         #sip_cb_state{
           pid = _CbPid
          } = State) ->
  ?DBG("Flash progress", []),
  URI = esip:'#get-request'(uri, Request),
  MH = esip:'#get-request'(mheaders, Request),
  Contact = esip:'#new-h_contact'([{uri, URI}, {params,[{<<"transport">>, esip:'#get-origin'(proto, Origin)}]}]),
  NewPH = [{'Contact', [Contact]}],

  From = esip:'#get-mheaders'(from, MH),
  FromURI = esip:'#get-h_contact'(uri, From),
  CallingId = iolist_to_binary(esip_headers:c_uri(FromURI)),

  Code = 183,
  Response = esip:'#new-response'(
               [
                {code, Code},
                {reason, esip_util:code_str(Code)},
                {mheaders, MH},
                {headers, NewPH},
                {body, undefined}
               ]),

  Name = esip:'#get-sip_uri'(name, URI),
  case esip_registrator:find(Name) of
    {ok, RTMP, _Pass, _DPid} ->
      apps_sip:incoming(RTMP, CallingId);
    Other ->
      ?DBG("Error: ~p", Other)
  end,
  NewState = State#sip_cb_state{opp_request = Request,
                                response = Response},
  {ok, Response, NewState}.

dialog(Origin,
       #sip_cb_state{
         opp_request = Request,
         pid = CbPid
        } = State) ->
  URI = esip:'#get-request'(uri, Request),
  MH = esip:'#get-request'(mheaders, Request),
  Body = esip:'#get-request'(body, Request),

  Contact = esip:'#new-h_contact'([{uri, URI}, {params,[{<<"transport">>, esip:'#get-origin'(proto, Origin)}]}]),
  NewPH = [{'Contact', [Contact]}],


  Name = esip:'#get-sip_uri'(name, URI),
  case esip_registrator:find(Name) of
    {ok, RTMP, _Pass, _DPid} ->

      MediaInfoRequest = #media_info{audio = Audio, video = _Video} = sdp:decode(Body),
      ?DBG("MediaIn:~n~p", [MediaInfoRequest]),
      AudioResult = [StreamInfo#stream_info{stream_id = 1} ||
                      #stream_info{codec = speex, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],
      %% VideoResult = [StreamInfo#stream_info{stream_id = 1} ||
      %%                 #stream_info{codec = h263} = StreamInfo <- Video],
      VideoResult = [],

      RtpGlue =
        fun(MediaInfoLocal, MediaInfoRemote, TranscodeOpts) ->
            ?DBG("MediaOut:~n~p", [MediaInfoRemote]),

            StreamIn = <<Name/binary, <<"#-in">>/binary >>,
            StreamOut = << Name/binary, <<"#-out">>/binary >>,
            {ok, Media} = media_provider:create(default, StreamIn,
                                                [{type,live},
                                                 {source_shutdown,shutdown},
                                                 {media_info, MediaInfoLocal}]),

            RtpOpts = [{media_info_loc, MediaInfoLocal},
                       {media_info_rmt,MediaInfoRemote},
                       {consumer, Media}] ++ TranscodeOpts,
            {ok, RTP} = rtp:start_server(RtpOpts),
            {ok, {_PortRTP, _PortRTCP}} = rtp_server:listen_ports(RTP, audio, [{transport, udp}]),

            rtp_server:add_stream(RTP, local, MediaInfoLocal),
            rtp_server:add_stream(RTP, remote, MediaInfoRemote),

            Fun = fun() -> media_provider:play(default, StreamOut,
                                               [{type,live},
                                                {stream_id,1},
                                                {wait, infinity},
                                                {media_info, MediaInfoLocal}]) end,
            rtp_server:play(RTP, Fun),

            apps_sip:sip_call(RTMP, StreamOut, StreamIn),

            CbPid ! {config, Media, StreamIn, StreamOut, RTP},
            MediaInfoReply = rtp_server:media_info_loc(RTP),
            SDP = sdp:encode(MediaInfoReply),
            Response = esip:'#new-response'(
                         [
                          {code, 101},
                          {reason, "Dialog Establishement"},
                          {mheaders, MH},
                          {headers, NewPH},
                          {body, undefined}
                         ]),

            NewCbState =
              State#sip_cb_state{
                media = Media,
                sdp = SDP,
                response = esip:'#set-response'([{body,SDP}], Response)
               },

            CbPid ! {send_create},
            %% {ok, DPid} = esip_dialog:start_worker(uas, self(), Request),
            %% DD = esip_dialog:create_ror(uas, Request, Origin),
            %% DialogConfig =
            %%   [
            %%    {user_mod, ?MODULE},
            %%    {cb_state, NewCbState}
            %%   ],
            %% ok = esip_dialog:call_worker(DD, {config, DialogConfig}),
            %% DPid ! {set_opposite, CbPid},

            {ok, Response, NewCbState}
        end,

      case AudioResult of
        [] ->
          %% Incompatible codecs
          ?DBG("Codecs are incompatible.", []),


          RtpConfig = ems:get_var(rtp, undefined),
          Binding = proplists:get_value(binding, RtpConfig),

          Sess = #sdp_session{name = "Esip flash terminating",
                              connect = {inet, Binding},
                              originator = #sdp_o{
                                username = sdp:make_username(),
                                sessionid = sdp:make_session(),
                                version = "1",
                                netaddrtype = inet,
                                address = Binding
                               }, attrs = []},

          ?DBG("Session:~n~p", [Sess]),

          %% FIXME: use SDP and erlycode capability checking
          %% Recode

          AudioResultLocal =
            [StreamInfo#stream_info{stream_id = 1, options = lists:keydelete(port, 1, SOptions)} ||
              #stream_info{codec = pcma, params = #audio_params{sample_rate = 8000}, options = SOptions} = StreamInfo <- Audio],
          AudioResultRemote =
            [StreamInfo#stream_info{stream_id = 1} ||
              #stream_info{codec = pcma, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],

          ?DBG("AudioResultLocal:~n~p", [AudioResultLocal]),
          ?DBG("AudioResultRemote:~n~p", [AudioResultRemote]),

          MediaInfoLocal =
            MediaInfoRequest#media_info{
              audio = AudioResultLocal,
              video = VideoResult,
              options = [{sdp_session, Sess}]
             },
          ?DBG("MediaInfoLocal:~n~p", [MediaInfoLocal]),

          MediaInfoRemote =
            MediaInfoRequest#media_info{
              audio = AudioResultRemote,
              video = VideoResult
             },
          ?DBG("MediaInfoRemote:~n~p", [MediaInfoRemote]),

          COpt = [{rate, 8000},{channels, 1}],
          TranscodeOpts = [{transcode_in, {{pcma, COpt}, {speex, COpt}}},
                           {transcode_out, {{speex, COpt}, {pcma, COpt}}}],
          RtpGlue(MediaInfoLocal, MediaInfoRemote, TranscodeOpts);
        _ ->

          AudioResultLocal =
            [StreamInfo#stream_info{stream_id = 1, options = lists:keydelete(port, 1, SOptions)} ||
              #stream_info{codec = pcma, params = #audio_params{sample_rate = 8000}, options = SOptions} = StreamInfo <- AudioResult],
          AudioResultRemote =
            [StreamInfo#stream_info{stream_id = 1} ||
              #stream_info{codec = pcma, params = #audio_params{sample_rate = 8000}} = StreamInfo <- AudioResult],

          ?DBG("AudioResultLocal:~n~p", [AudioResultLocal]),
          ?DBG("AudioResultRemote:~n~p", [AudioResultRemote]),

          MediaInfoLocal =
            MediaInfoRequest#media_info{
              audio = AudioResultLocal,
              video = VideoResult
             },
          MediaInfoRemote =
            MediaInfoRequest#media_info{
              audio = AudioResultRemote,
              video = VideoResult
             },
          RtpGlue(MediaInfoLocal, MediaInfoRemote, [])
      end;
    _ ->
      {error, not_found}
  end.


create_dialog(_Response, _Origin, #sip_cb_state{pid = DPid}) ->
  ?DBG("Create dialog for terminating call: ~p", [DPid]),
  DPid ! {send_create}.

ack(_RTP, _StreamOut) ->
  ok.

progress_timeout(#sip_cb_state{progress_timeout = DTO}) ->
  DTO.

dialog_timeout(#sip_cb_state{dialog_timeout = DTO}) ->
  DTO.

response(#sip_cb_state{response = Response}) ->
  Response.

origin(#sip_cb_state{origin = Origin}) ->
  Origin.

%%
start_media(SDP,
            #sip_cb_state{
              pid = _DPid,
              rtp = RTP,
              rtmp = RTMP,
              stream_in = StreamIn,
              stream_out = StreamOut
             },
            #state{sdp = OldMediaInfo} = State) ->

  MediaInfo = #media_info{audio = Audio} = sdp:decode(SDP),
  ?DBG("MediaOut:~n~p", [MediaInfo]),
  %% TODO: Compare OldMediaInfo and MediaInfo

  if
    OldMediaInfo =:= undefined ->
      %% AudioResult = [StreamInfo#stream_info{stream_id = 1} ||
      %%                 #stream_info{codec = speex, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],
      AudioResultLocal =
        [StreamInfo#stream_info{stream_id = 1, options = lists:keydelete(port, 1, SOptions)} ||
          #stream_info{codec = pcma, params = #audio_params{sample_rate = 8000}, options = SOptions} = StreamInfo <- Audio],
      AudioResultRemote =
        [StreamInfo#stream_info{stream_id = 1} ||
          #stream_info{codec = pcma, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],

      ?DBG("AudioResultLocal:~n~p", [AudioResultLocal]),
      ?DBG("AudioResultRemote:~n~p", [AudioResultRemote]),

      MediaInfoLocal = MediaInfo#media_info{audio = AudioResultLocal},
      MediaInfoRemote = MediaInfo#media_info{audio = AudioResultRemote},

      rtp_server:add_stream(RTP, local, MediaInfoLocal),
      rtp_server:add_stream(RTP, remote, MediaInfoRemote),
      apps_sip:sip_call(RTMP, StreamOut, StreamIn),

      Fun = fun() -> media_provider:play(default, StreamOut,
                                         [{type,live},
                                          {stream_id,1},
                                          {wait, infinity},
                                          {media_info, MediaInfoLocal}]) end,
      rtp_server:play(RTP, Fun),
      MediaInfoReply = rtp_server:media_info_loc(RTP),
      ReplySDP = sdp:encode(MediaInfoReply),
      State#state{sdp = ReplySDP};
    true ->
      State
  end.
