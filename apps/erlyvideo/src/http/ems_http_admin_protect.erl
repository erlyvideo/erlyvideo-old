%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        HTTP password protection for admin page
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(ems_http_admin_protect, [Login, Password]).
-author('Max Lapshin <max@maxidoors.ru>').


-export([http/4]).


http(_Host, _Method, _Path, Req) ->
  Headers = Req:get(headers),
  case http_auth(Headers) of
    true -> unhandled;
    false -> Req:respond(401,[{'WWW-Authenticate', "Basic realm=Erlyvideo admin panel"}],"Please login in system")
  end.

http_auth(Headers) ->
  proplists:get_value('Authorization',Headers) == "Basic "++base64:encode_to_string(Login ++ ":" ++ Password).
