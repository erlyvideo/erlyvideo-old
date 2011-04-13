% ==========================================================================================================
% MISULTIN - Socket
%
% >-|-|-(°>
% 
% Copyright (C) 2009, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
% All rights reserved.
%
% Code portions from Sean Hinde have been originally taken under BSD license from Trapexit at the address:
% <http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features>
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(misultin_socket).
-vsn('0.3').

% API
-export([start_link/2]).

% internale
-export([accept/3, request/2, headers/3, headers/4, body/2]).

% macros
-define(MAX_HEADERS_COUNT, 100).

% records
-record(c, {
	sock,
	port,
	loop,
	recv_timeout
}).

% includes
-include("misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
start_link(ClientSocket, Loop) ->
	proc_lib:start_link(?MODULE, accept, [self(), ClientSocket, Loop]).

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the socket.
accept(Parent, Sock, Loop) ->
  RecvTimeout = 6000,
  proc_lib:init_ack(Parent, {ok, self()}),
  receive
    socket ->
      inet:setopts(Sock, [{active, once}, {keepalive, true}, {send_timeout, RecvTimeout}, {send_timeout_close, true}]),
      ?DEBUG(debug, "activated controlling process", [])
  after 60000 ->
    exit({error, controlling_failed})
  end,
  % build connection record
  {ok, {Addr, Port}} = inet:peername(Sock),
  ListenPort = inet:port(Sock),
  C = #c{sock = Sock, port = ListenPort, loop = Loop, recv_timeout = RecvTimeout},
  % jump to state 'request'
  ?DEBUG(debug, "jump to state request", []),
  ?MODULE:request(C, #req{peer_addr = Addr, peer_port = Port}).
  
	

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% REQUEST: wait for a HTTP Request line. Transition to state headers if one is received. 
request(#c{sock = Sock, recv_timeout = RecvTimeout} = C, Req) ->
	inet:setopts(Sock, [{active, once}]),
	receive
		{http, Sock, {http_request, Method, Path, Version}} ->
			?DEBUG(debug, "received full headers of a new HTTP packet", []),
			?MODULE:headers(C, Req#req{vsn = Version, method = Method, uri = Path, connection = close}, []);
		{http, Sock, {http_error, Error}} ->
		  exit({http_error, Error})
	after RecvTimeout ->
		?DEBUG(debug, "request timeout, sending error", []),
		send(Sock, ?REQUEST_TIMEOUT_408)
	end.

% HEADERS: collect HTTP headers. After the end of header marker transition to body state.
headers(C, Req, H) ->
	headers(C, Req, H, 0).
headers(#c{sock = Sock, recv_timeout = RecvTimeout} = C, Req, H, HeaderCount) when HeaderCount =< ?MAX_HEADERS_COUNT ->
	inet:setopts(Sock, [{active, once}]),
	receive
		{http, Sock, {http_header, _, 'Content-Length', _, Val}} ->
			?MODULE:headers(C, Req#req{content_length = list_to_integer(Val)}, [{'Content-Length', list_to_integer(Val)}|H], HeaderCount + 1);
		{http, Sock, {http_header, _, 'Host', _, Val}} ->
		  Host = list_to_binary(hd(string:tokens(Val, ":"))),
			?MODULE:headers(C, Req#req{host = Host}, [{'Host', Host}|H], HeaderCount + 1);
		{http, Sock, {http_header, _, Header, _, Val}} ->
			?MODULE:headers(C, Req, [{Header, Val}|H], HeaderCount + 1);
		{http, Sock, {http_error, Error}} ->
		  erlang:exit({http_error, Error});
		{http, Sock, http_eoh} ->
			case ?MODULE:body(C, Req#req{headers = lists:reverse(H), socket = Sock}) of
			  #c{} = C1 ->
			    inet:setopts(Sock, [{active,once},{packet,http}]),
			    ?MODULE:request(C1, Req);
			  _ ->
			    exit(normal)
			end
	after RecvTimeout ->
		?DEBUG(debug, "headers timeout, sending error", []),
		send(Sock, ?REQUEST_TIMEOUT_408)
	end;
headers(_C, _Req, _H, _HeaderCount) ->
  erlang:exit(too_much_headers).

% BODY: collect the body of the HTTP request if there is one, and lookup and call the implementation callback.
% Depending on whether the request is persistent transition back to state request to await the next request or exit.
body(#c{sock = Sock, recv_timeout = RecvTimeout} = C, #req{content_length = ContentLength} = Req) ->
	case Req#req.method of
	  'HEAD' ->
			handle_get(C, Req);
		'GET' ->
			handle_get(C, Req);
		'POST' ->
			case ContentLength of 
				_ when is_number(ContentLength) andalso ContentLength < 10*1024*1024 ->
					inet:setopts(Sock, [{packet, raw}, {active, false}]),
					case gen_tcp:recv(Sock, ContentLength, RecvTimeout) of
						{ok, Bin} ->
							handle_post(C, Req#req{body = Bin});
						{error, timeout} ->
							?DEBUG(debug, "request timeout, sending error", []),
							send(Sock, ?REQUEST_TIMEOUT_408);	
						_Other ->
							?DEBUG(debug, "tcp normal error treating post: ~p", [_Other]),
							exit(normal)
					end;
  			_ when is_number(ContentLength) ->
					send(Sock, ?REQUEST_TOO_LARGE_413),
					exit(normal);
				_ ->
					% TODO: provide a fallback when content length is not or wrongly specified
					?DEBUG(debug, "specified content length is not a valid integer number: ~p", [Req#req.content_length]),
					send(Sock, ?CONTENT_LENGTH_REQUIRED_411),
					exit(normal)
			end;
		'PUT' ->
		  inet:setopts(Sock, [{packet, raw}, {active, false}]),
			handle_get(C, Req);
		_Other ->
			?DEBUG(debug, "method not implemented: ~p", [_Other]),
			send(Sock, ?NOT_IMPLEMENTED_501),
			exit(normal)
	end.

% handle a get request
handle_get(C, #req{} = Req) ->
	case Req#req.uri of
		{abs_path, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {abs_path, F}}),
			C;
		{absoluteURI, http, _Host, _, Path} ->
			{F, Args} = split_at_q_mark(Path, []),
			call_mfa(C, Req#req{args = Args, uri = {absoluteURI, F}}),
			C;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			send(C#c.sock, ?NOT_IMPLEMENTED_501),
			close;
		{scheme, _Scheme, _RequestString} ->
			send(C#c.sock, ?NOT_IMPLEMENTED_501),
			close;
		_  ->
			send(C#c.sock, ?FORBIDDEN_403),
			close
	end.

% handle a post request
handle_post(C, #req{} = Req) ->
	case Req#req.uri of
		{abs_path, _Path} ->
			call_mfa(C, Req),
			C;
		{absoluteURI, http, _Host, _, _Path} ->
			call_mfa(C, Req),
			C;
		{absoluteURI, _Other_method, _Host, _, _Path} ->
			send(C#c.sock, ?NOT_IMPLEMENTED_501),
			close;
		{scheme, _Scheme, _RequestString} ->
			send(C#c.sock, ?NOT_IMPLEMENTED_501),
			close;
		_  ->
			send(C#c.sock, ?FORBIDDEN_403),
			close
	end.

% Description: Main dispatcher
call_mfa(#c{sock = Sock, loop = Loop} = _C, Request) ->
	% spawn listening process for Request messages [only used to support stream requests]
	% create request
	inet:setopts(Sock, [{active, once}]),
	Req = misultin_req:new(Request),
	% call loop
	try Loop(Req) of
	  {HttpCode, Headers0, Body} ->
			% received normal response
			?DEBUG(debug, "sending response", []),
			% flatten body [optimization since needed for content length]
			BodyBinary = convert_to_binary(Body),
			% provide response
			Headers = add_content_length(Headers0, BodyBinary),
			Req:stream(head, HttpCode, Headers),
			Req:stream(BodyBinary);
		{raw, Body} ->
			send(Sock, Body);
		_ ->
			% loop exited normally, kill listening socket
			ok
	catch	
		_Class:_Error ->
			?DEBUG(error, "worker crash: ~p:~p:~p", [_Class, _Error, erlang:get_stacktrace()]),
      error_logger:error_msg("FAIL ~p ~p~n~p:~p:~p", [Req:get(method), Req:get(uri), _Class, _Error, erlang:get_stacktrace()]),
      send(Sock, [?INTERNAL_SERVER_ERROR_500, io_lib:format("~p:~p:~p", [_Class, _Error, erlang:get_stacktrace()])])
	end.

% Description: Ensure Body is binary.
convert_to_binary(Body) when is_list(Body) ->
	list_to_binary(lists:flatten(Body));
convert_to_binary(Body) when is_binary(Body) ->
	Body;
convert_to_binary(Body) when is_atom(Body) ->
	list_to_binary(atom_to_list(Body)).

% Description: Add content length
add_content_length(Headers, Body) ->
	case proplists:get_value('Content-Length', Headers) of
		undefined ->
			[{'Content-Length', size(Body)}|Headers];
		false ->
			Headers
	end.

% Description: Encode headers

% Split the path at the ?
split_at_q_mark([$?|T], Acc) ->
	{lists:reverse(Acc), T};
split_at_q_mark([H|T], Acc) ->
	split_at_q_mark(T, [H|Acc]);
split_at_q_mark([], Acc) ->
	{lists:reverse(Acc), []}.

% TCP send
send(Sock, Data) ->
	case gen_tcp:send(Sock, Data) of
		ok ->
			ok;
		{error, _Reason} ->
			?DEBUG(debug, "worker crash: ~p", [_Reason]),
			exit(normal)
	end.

	
% ============================ /\ INTERNAL FUNCTIONS =======================================================
