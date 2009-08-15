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
    {moov, Atom} -> parse_moov(Atom, #mp4_parser{});
    Else -> Else
  end.
    
find_moov(IoDev) ->
  ?D("Finding moov"),
  case next_atom(IoDev) of
    {moov, Atom} -> {moov, Atom};
    {error, Reason} -> {error, Reason};
    {_, _} -> find_moov(IoDev);
    Else -> Else
  end.
  
parse_moov(Atom, Mp4Parser) when size(Atom) == 0 ->
  Mp4Parser;
  
parse_moov(Atom, Mp4Parser) when size(Atom) < 4 ->
  {error, "Invalid moov atom"};
  
parse_moov(<<AllAtomLength:32/big-integer, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) when (size(AtomRest) >= AllAtomLength - 8) ->
  AtomLength = AllAtomLength - 8,
  <<Atom:AtomLength/binary, Rest/binary>> = AtomRest,
  AtomName = binary_to_atom(BinaryAtomName, utf8),
  ?D({"Valid atom in moov", AtomName}),
  NewMp4Parser = parse_atom(AtomName, Atom, Mp4Parser),
  parse_moov(Rest, NewMp4Parser);
  
parse_moov(<<AllAtomLength:32/big-integer, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) ->
  ?D({"Invalid moov", AllAtomLength, binary_to_atom(BinaryAtomName, utf8)}),
  Mp4Parser.

parse_atom(trak, Atom, #mp4_parser{track_number = TrackNumber, tracks = Tracks} = Mp4Parser) ->
  parse_moov(Atom, Mp4Parser#mp4_parser{track_number = (TrackNumber+1), tracks = [{TrackNumber, []} | Tracks]});

parse_atom(edts, Atom, Mp4Parser) ->
  parse_moov(Atom, Mp4Parser);
parse_atom(mdia, Atom, Mp4Parser) ->
  parse_moov(Atom, Mp4Parser);
parse_atom(minf, Atom, Mp4Parser) ->
  parse_moov(Atom, Mp4Parser);
parse_atom(stbl, Atom, Mp4Parser) ->
  parse_moov(Atom, Mp4Parser);


parse_atom(stsd, <<EntryCount:32/big-integer>>, Mp4Parser) ->
  Mp4Parser;
parse_atom(stsz, _, Mp4Parser) ->
  Mp4Parser;
parse_atom(stsc, _, Mp4Parser) ->
  Mp4Parser;
parse_atom(stco, _, Mp4Parser) ->
  Mp4Parser;
parse_atom(stts, _, Mp4Parser) ->
  Mp4Parser;
  
parse_atom(mdhd,<<0:8/integer, _Flags:24/integer, _Ctime:32/big-integer, 
                  _Mtime:32/big-integer, TimeScale:32/big-integer, Duration:32/big-integer,
                  _Language:2/binary, _Quality:16/big-integer>>, 
                #mp4_parser{track_number = TrackNumber, tracks = Tracks} = Mp4Parser) ->
  ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Parser;
  
parse_atom(mdhd, <<1:8/integer, _Flags:24/integer, _Ctime:64/big-integer, _Mtime:64/big-integer, TimeScale:32/big-integer, Duration:64/big-integer, _Language:2/binary, _Quality:16/big-integer>>, Mp4Parser) ->
  Mp4Parser;
  
parse_atom(stss, _, Mp4Parser) ->
  Mp4Parser;
parse_atom(_, _, Mp4Parser) ->
  Mp4Parser.

extract_language(<<L1:5/integer, L2:5/integer, L3:5/integer, _:1/integer>>) ->
  [L1+16#60, L2+16#60, L3+16#60].

next_atom(IoDev) ->
  case file:read(IoDev, 8) of
    {ok, Data} ->
      <<AtomLength:32/big-integer, AtomName:4/binary>> = list_to_binary(Data),
      ?D({"Atom: ", AtomName, AtomLength}),
      case file:read(IoDev, AtomLength - 8) of
        {ok, Atom} ->
          {binary_to_atom(AtomName, utf8), list_to_binary(Atom)};
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
    
    
    
    
    
    