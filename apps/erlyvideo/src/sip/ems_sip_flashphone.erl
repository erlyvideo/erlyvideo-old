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
% -include_lib("esip/include/esip_records.hrl").
% -include_lib("esip/include/esip.hrl").
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
         dialog_timeout/1,
         response/1,
         origin/1
        ]).

-export([
         progress/3,
         ack/2,
         register/2,
         call/2
        ]).


-record(sip_cb_state, {
          pid                  :: pid(),
          media                :: pid(),
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
  #sip_cb_state{
                 pid = Pid,
                 dialog_timeout = timer:seconds(10)
               }.



%%--------------------------------------------------------------------
%% @spec (Number::string(), Client::pid()) -> {ok, Ref}
%% @doc Registers process under specific number
%%
%% @end
%%--------------------------------------------------------------------
register(Number, Client) when is_list(Number) andalso is_pid(Client) ->
  esip_registrator:register(Number, Client),

  FlashPhoneConfig = ems:get_var(flashphone, undefined),
  case proplists:get_value(sip, FlashPhoneConfig) of
    undefined -> ok;
    SipCfg ->
      DomainName = list_to_binary(proplists:get_value(domain, SipCfg, "localhost")),
      RegAddress = proplists:get_value(proxy_addr, SipCfg, "127.0.0.1"),
      RegPort = proplists:get_value(proxy_port, SipCfg, esip:default_port()),
      Password = proplists:get_value(password, SipCfg, ""),
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

register(Number, Client) when is_binary(Number) ->
  ?MODULE:register(binary_to_list(Number), Client).



%%--------------------------------------------------------------------
%% @spec (Number::string(), Options::proplist()) -> {ok, Ref}
%% @doc Start call to number
%%
%% @end
%%--------------------------------------------------------------------
call(Number, _Options) when is_list(Number) ->
  esip_registrator:get(Number);

call(Number, Options) when is_binary(Number) ->
  call(binary_to_list(Number), Options).


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
        {ok, RTMP} ->
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
    {ok, RTMP} ->

      MediaInfoRequest = #media_info{audio = Audio} = sdp:decode(Body),
      ?DBG("MediaIn:~n~p", [MediaInfoRequest]),
      AudioResult = [StreamInfo#stream_info{stream_id = 1} ||
                      #stream_info{codec = speex, params = #audio_params{sample_rate = 8000}} = StreamInfo <- Audio],

      case AudioResult of
        [] ->
          %% Incompatible codecs
          ?DBG("Codecs are incompatible.", []),
          {error, not_acceptable};
        _ ->
          Response = esip:'#new-response'([
            {code, 101},
            {reason, "Dialog Establishement"},
            {mheaders, MH},
            {headers, NewPH},
            {body, undefined}
          ]),
           

          MediaInfoReply =
            MediaInfoRequest#media_info{
              audio = AudioResult,
              video = []
             },
          ?DBG("MediaOut:~n~p", [MediaInfoReply]),

          StreamIn = <<Name/binary, <<"#-in">>/binary >>,
          StreamOut = << Name/binary, <<"#-out">>/binary >>,
          {ok, Media} = media_provider:create(default, StreamIn,
                                              [{type,live},{source_shutdown,shutdown}]),
          apps_sip:sip_call(RTMP, StreamOut, StreamIn),

          {ok, RTP} = rtp:start_server([{media_info_in, MediaInfoRequest},
                                        {media_info_out,MediaInfoReply},
                                        {consumer, Media}]),
          CbPid ! {config, Media, StreamIn, StreamOut, RTP},
          MediaInfoReply1 = rtp_server:media_info_out(RTP),
          SDP = sdp:encode(MediaInfoReply1),
          Response = esip:'#new-response'([
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

          %% Здесь стартовать RTP-сервер, слушающий UDP и делающий перекодировку кодеков.

          %% FromURI = Request#request.mheaders#mheaders.from#h_from.uri,
          %% FromName = Request#request.mheaders#mheaders.from#h_from.name,
          %% ToURI = Request#request.mheaders#mheaders.to#h_to.uri,
          %% ToName = Request#request.mheaders#mheaders.to#h_to.name,

          {ok, Response, NewCbState}
      end;
    _ ->
      {error, not_found}
  end.


ack(RTP, StreamOut) ->
  Fun = fun() -> media_provider:play(default, StreamOut, [{type,live}, {stream_id,1}]) end,
  rtp_server:play(RTP, Fun).

dialog_timeout(#sip_cb_state{dialog_timeout = DTO}) ->
  DTO.

response(#sip_cb_state{response = Response}) ->
  Response.

origin(#sip_cb_state{origin = Origin}) ->
  Origin.
