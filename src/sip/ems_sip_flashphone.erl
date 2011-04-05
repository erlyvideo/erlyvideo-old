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
-include_lib("esip/include/esip_records.hrl").
-include_lib("esip/include/esip.hrl").
-include_lib("erlmedia/include/sdp.hrl").

-export([
         init/0,
         dialog/3,
         dialog_timeout/1,
         response/1,
         origin/1
        ]).

-export([
         progress/3,
         ack/3,
         register/2,
         call/2
        ]).


-record(sip_cb_state, {
          sdp                  :: [binary()],
          client               :: pid(),
          client_ref           :: reference(),
          dialog_timeout       :: integer(),
          response             :: #response{},
          origin               :: #origin{}
         }).

init() ->
  #sip_cb_state{
      dialog_timeout = timer:seconds(10)
     }.



%%--------------------------------------------------------------------
%% @spec (Number::string(), Client::pid()) -> {ok, Ref}
%% @doc Registers process under specific number
%%
%% @end
%%--------------------------------------------------------------------
register(Number, Client) when is_list(Number) andalso is_pid(Client) ->
  esip_registrator:register(Number, Client);

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
  #sip_uri{name = Name, domain = Domain, port = Port} = SU = esip:p_uri(URL),
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

ack(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"PLAY", Host, Path, Headers}),
  ems_log:access(Host, "SIP PLAY ~s ~s", [Host, Path]),
  Outstream = << Path/binary, <<"-out">>/binary >>,
  {ok, Media} = media_provider:play(default, Outstream, [{stream_id,1}]),
  {ok, Media}.

dialog(#request{uri = URI,
                mheaders = MH,
                headers = PH,
                body = Body} = Request,
       Origin, State) ->

  NewPH = [{'Contact',
            [#h_contact{uri = URI,
                        params = [{<<"transport">>, Origin#origin.proto}]}]}],

  Response =
    #response{
    code = 101,
    reason = "Dialog Establishement",
    mheaders = MH,
    headers = NewPH,
    body = undefined
   },

  NewCbState =
    State#sip_cb_state{
      sdp = Body,
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

  %% Здесь стартовать RTP-сервер, слушающий UDP и делающий перекодировку кодеков.

  FromURI = Request#request.mheaders#mheaders.from#h_from.uri,
  FromName = Request#request.mheaders#mheaders.from#h_from.name,
  ToURI = Request#request.mheaders#mheaders.to#h_to.uri,
  ToName = Request#request.mheaders#mheaders.to#h_to.name,

  {ok, Response, NewCbState}.

dialog_timeout(#sip_cb_state{dialog_timeout = DTO}) ->
  DTO.

response(#sip_cb_state{response = Response}) ->
  Response.

origin(#sip_cb_state{origin = Origin}) ->
  Origin.
