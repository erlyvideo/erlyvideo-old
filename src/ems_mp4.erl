%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @author     Takuma Mori <mori@sgra.co.jp> [http://www.sgra.co.jp/en/], SGRA Corporation
%%% @copyright  2008 Takuma Mori, 2009 Max Lapshin
%%% @doc        MP4 decoding module, rewritten from RubyIzumi
%%% @reference  See <a href="http://github.com/maxlapshin/erlyvideo" target="_top">http://github.com/maxlapshin/erlyvideo</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2008 Takuma Mori, 2009 Max Lapshin
%%%    This program is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU Affero General Public License as
%%%    published by the Free Software Foundation, either version 3 of the
%%%    License, or any later version.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------

-module(ems_mp4).
-author('max@maxidoors.ru').
-include("../include/ems.hrl").

-export([read_header/1]).
% -export([read_header/1,read_frame/1,read_frame/2,to_tag/2,header/1, parse_meta/1, encodeTag/2]).

read_header(IoDev) -> 
  case find_moov(IoDev) of
    {moov, Atom} -> Atom;
    Else -> Else
  end.
    
find_moov(IoDev) ->
  case next_atom(IoDev) of
    {moov, Atom} -> {moov, Atom};
    {error, Reason} -> {error, Reason};
    {_, _} -> find_moov(IoDev);
    Else -> Else
  end.
  

next_atom(IoDev) ->
  case file:read(IoDev, 8) of
    {ok, Data} ->
      <<AtomLength:32/big-integer, AtomName:4/binary>> = list_to_binary(Data),
      ?D({"Atom: ", AtomName, AtomLength}),
      case file:read(IoDev, AtomLength - 8) of
        {ok, Atom} ->
          {binary_to_atom(AtomName, utf8), Atom};
        eof ->
          {error, unexpected_eof};
        {error, Reason} ->
          {error, Reason}         
      end;
    eof -> 
      {error, unexpected_eof};
    {error, Reason} -> 
      {error, Reason}           
  end.
    
    
    
    
    
    