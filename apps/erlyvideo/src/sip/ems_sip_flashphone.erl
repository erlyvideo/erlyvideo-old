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

-export([start_link/1]).

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
         init/0,
         dialog/3,
         ok/3,
         dialog_timeout/1,
         response/1,
         origin/1
        ]).

-export([
         progress/3,
         ack/2,
         register/3,
         call/2
        ]).


-record(sip_cb_state, {
          pid                  :: pid(),
          media                :: pid(),
          rtp                  :: pid(),
          rtmp                 :: pid(),
          stream_in            :: binary(),
          stream_out           :: binary(),
          sdp                  :: [binary()],
          client               :: pid(),
          client_ref           :: reference(),
          dialog_timeout       :: integer(),
          response             :: tuple(), % #response{}
          origin               :: tuple()  % #origin{}
         }).

-record(state, {
          media                :: pid(),
          rtp                  :: pid(),
          stream_in            :: binary(),
          stream_out           :: binary()
         }).

start_link(Args) ->
  gen_fsm:start_link(?MODULE, Args, []).

%%% gen_fsm callbacks
init(Args) ->
  ?DBG("Args: ~p", [Args]),
  ?DBG("Start dialog process for flash", []),
  {ok, d_active, #state{}}.

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
  {next_state, StateName,
   State#state{media = Media,
               stream_in = StreamIn,
               stream_out = StreamOut,
               rtp = RTP}};

handle_info({ok, Opts, _Response}, StateName,
            #state{} = State) ->
  ?DBG("OK: ~p", [Opts]),
  %% Duplex RTP
  {next_state, StateName, State};

handle_info({ringing, _Response}, StateName,
            #state{} = State) ->
  ?DBG("Ringing", []),
  {next_state, StateName, State};

handle_info({bye}, _StateName,
         #state{} = State) ->
  ?DBG("Bye: ~p", [self()]),
  %% STOP HERE
  {stop, normal, State};

handle_info(_Info, StateName, State) ->
  ?DBG("Unhandled info: ~p", [_Info]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.





init() ->
  esip_dialog_sup:start_mod_sup(?MODULE),
  Args = [{}],
  {ok, Pid} = esip_dialog_sup:start_worker(?MODULE, Args),
  {ok, #sip_cb_state{
     pid = Pid,
     dialog_timeout = timer:seconds(10)
    }}.



%%--------------------------------------------------------------------
%% @spec (Number::string(), Client::pid()) -> {ok, Ref}
%% @doc Registers process under specific number
%%
%% @end
%%--------------------------------------------------------------------
register(Number, Password, Client) when is_list(Number) andalso is_pid(Client) ->
  esip_registrator:register(Number, Password, Client),

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
      FromURI = ToURI = esip:'#new-sip_uri'([{name, list_to_binary(Number)}, {domain, DomainName}]),
      FromName = ToName = "Flash client " ++ Number,
      RegUserOpts =
        [
         {parent, self()},
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
         {contact_name, Number}
        ],
      {ok, _TUPid} = esip_transaction_sup:start_user({registration, RegUserOpts})
  end;

register(Number, Password, Client)
  when not (is_list(Number) andalso is_list(Password)) ->
  ?MODULE:register(a2l(Number), a2l(Password), Client).



%%--------------------------------------------------------------------
%% @spec (Number::string(), Options::proplist()) -> {ok, Ref}
%% @doc Start call to number
%%
%% @end
%%--------------------------------------------------------------------
call(Name, Options) when is_list(Name) ->
  call(list_to_binary(Name), Options);
call(Name, _Options) when is_binary(Name) ->
  {ok, CbState} = ?MODULE:init(),
  originating(Name, CbState).

originating(Name, #sip_cb_state{pid = DPid} = CbState) ->
  ?DBG("Name: ~p, DPid: ~p", [Name, DPid]),
  {ok, OrigNameS, Password} = esip_registrator:find(self()),
  OrigName = list_to_binary(OrigNameS),
  ?DBG("OrigName: ~p", [OrigName]),

  StreamIn = <<OrigName/binary, <<"#-in">>/binary >>,
  StreamOut = << OrigName/binary, <<"#-out">>/binary >>,
  {ok, Media} = media_provider:create(default, StreamIn,
                                      [{type,live},{source_shutdown,shutdown}]),
  %%apps_sip:sip_call(RTMP, StreamOut, StreamIn),
  ?DBG("Media: ~p", [Media]),
  RtpConfig = ems:get_var(rtp, undefined),
  Binding = proplists:get_value(binding, RtpConfig),


  Sess = #sdp_session{name = "Esip flash call",
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
                audio = [#stream_info{content = audio, stream_id = 1, codec = speex,
                                      params = {audio_params,1,8000}}],
                options = [{sdp_session, Sess}]},



  %% {ok, RTP} = rtp:start_server([{media_info_in, MediaInfo},
  %%                               {media_info_out, MediaInfo},
  %%                               {consumer, Media}]),

  {ok, RTP} = rtp:start_server([{media_info_loc, MediaInfo},
                                {consumer, Media}]),
  {ok, {PortRTP, PortRTCP}} = rtp_server:listen_ports(RTP, 1, [{transport, udp}]),

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
      FromName = "Flash client " ++ OrigName,

      SipOpts =
        [
         {parent, self()},
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

hostpath(URL) ->
  %%{match, [Path, HostPort]} = re:run(URL, "sip:([^@]+)@(.*)", [{capture, [1,2], binary}]),
  SU = esip:p_uri(URL),
  Name = esip:'#get-sip_uri'(name, SU),
  Domain = esip:'#get-sip_uri'(domain, SU),
  Port = esip:'#get-sip_uri'(port, SU),
  ?DBG("SIP URI: ~p", [SU]),
  {ems:host(iolist_to_binary([Domain, ":", integer_to_list(Port)])), Name}.


progress(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"PROGRESS", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),
  case Module:Function(Host, esip, proplists:get_value('Authorization', Headers)) of
    undefined ->
      ?D({Module, Function}),
      {error, authentication};
    _Session ->
      ?D({Module, Function, _Session}),
      Instream = <<Path/binary, <<"-in">>/binary >>,
      Outstream = << Path/binary, <<"-out">>/binary >>,
      case esip_registrator:get(Path) of
        {ok, RTMP, _Pass} ->
          {ok, Media} = media_provider:create(default, Instream, [{type,live},{source_shutdown,shutdown}]),
          apps_sip:sip_call(RTMP, Outstream, Instream),
          {ok, Media};
        _ ->
          {error, not_found}
      end
  end.

dialog(Request,
       Origin,
       #sip_cb_state{
         pid = CbPid
        } = State) ->
  URI = esip:'#get-request'(uri, Request),
  MH = esip:'#get-request'(mheaders, Request),
  Body = esip:'#get-request'(body, Request),

  Contact = esip:'#new-h_contact'([{uri, URI}, {params,[{<<"transport">>, esip:'#get-origin'(proto, Origin)}]}]),
  NewPH = [{'Contact', [Contact]}],


  Name = esip:'#get-sip_uri'(name, URI),
  case esip_registrator:find(Name) of
    {ok, RTMP, _Pass} ->

      MediaInfoRequest = #media_info{audio = Audio} = sdp:decode(Body),
      ?DBG("MediaIn:~n~p", [MediaInfoRequest]),
      AudioResult = [StreamInfo#stream_info{stream_id = 1} ||
                      #stream_info{codec = speex, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],


      RtpGlue =
        fun(MediaInfoReply, TranscodeOpts) ->
            ?DBG("MediaOut:~n~p", [MediaInfoReply]),

            StreamIn = <<Name/binary, <<"#-in">>/binary >>,
            StreamOut = << Name/binary, <<"#-out">>/binary >>,
            {ok, Media} = media_provider:create(default, StreamIn,
                                                [{type,live},{source_shutdown,shutdown}]),

            RtpOpts = [{media_info_loc, MediaInfoReply},
                       {media_info_rmt,MediaInfoReply},
                       {consumer, Media}] ++ TranscodeOpts,
            {ok, RTP} = rtp:start_server(RtpOpts),
            rtp_server:add_stream(RTP, remote, MediaInfoReply),
            {ok, {_PortRTP, _PortRTCP}} = rtp_server:listen_ports(RTP, 1, [{transport, udp}]),

            apps_sip:sip_call(RTMP, StreamOut, StreamIn),

            CbPid ! {config, Media, StreamIn, StreamOut, RTP},
            MediaInfoReply1 = rtp_server:media_info_loc(RTP),
            SDP = sdp:encode(MediaInfoReply1),
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

            {ok, DPid} = esip_dialog:start_worker(uas, self(), Request),

            DD = esip_dialog:create_ror(uas, Request, Origin),
            DialogConfig =
              [
               {user_mod, ?MODULE},
               {cb_state, NewCbState}
              ],
            ok = esip_dialog:call_worker(DD, {config, DialogConfig}),
            DPid ! {set_opposite, CbPid},

            {ok, Response, NewCbState}
        end,

      case AudioResult of
        [] ->
          %% Incompatible codecs
          ?DBG("Codecs are incompatible.", []),

          %% FIXME: use SDP and erlycode capability checking
          %% Recode
          RecAudioResult = [StreamInfo#stream_info{stream_id = 1} ||
                             #stream_info{codec = pcma, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],

          MediaInfoReply =
            MediaInfoRequest#media_info{
              audio = RecAudioResult,
              video = []
             },
          ?DBG("MediaInfoReply:~n~p", [MediaInfoReply]),

          COpt = [{rate, 8000},{channels, 1}],
          TranscodeOpts = [{transcode_in, {{pcma, COpt}, {speex, COpt}}},
                           {transcode_out, {{speex, COpt}, {pcma, COpt}}}],
          RtpGlue(MediaInfoReply, TranscodeOpts);
        _ ->
          MediaInfoReply =
            MediaInfoRequest#media_info{
              audio = AudioResult,
              video = []
             },
          RtpGlue(MediaInfoReply, [])
      end;
    _ ->
      {error, not_found}
  end.


ok(Response, _Origin,
   #sip_cb_state{
               pid = _DPid,
               rtp = RTP,
               rtmp = RTMP,
               stream_in = StreamIn,
               stream_out = StreamOut
              } = CbState) ->

  MediaInfo = #media_info{audio = Audio} = sdp:decode(esip:'#get-response'(body, Response)),
  ?DBG("MediaOut:~n~p", [MediaInfo]),
  AudioResult = [StreamInfo#stream_info{stream_id = 1} ||
                  #stream_info{codec = speex, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],
  ?DBG("AudioResult:~n~p", [AudioResult]),

  rtp_server:add_stream(RTP, remote, MediaInfo),

  apps_sip:sip_call(RTMP, StreamOut, StreamIn),

  Fun = fun() -> media_provider:play(default, StreamOut, [{type,live}, {stream_id,1}]) end,
  rtp_server:play(RTP, Fun),

  {ok, CbState}.

ack(RTP, StreamOut) ->
  Fun = fun() -> media_provider:play(default, StreamOut, [{type,live}, {stream_id,1}]) end,
  rtp_server:play(RTP, Fun).

dialog_timeout(#sip_cb_state{dialog_timeout = DTO}) ->
  DTO.

response(#sip_cb_state{response = Response}) ->
  Response.

origin(#sip_cb_state{origin = Origin}) ->
  Origin.

%% TODO: move to some utils module

a2l(A) when is_atom(A) -> atom_to_list(A);
a2l(A) when is_integer(A) -> integer_to_list(A);
a2l(A) when is_list(A) -> A;
a2l(A) when is_binary(A) -> binary_to_list(A);
a2l(A) -> A.
