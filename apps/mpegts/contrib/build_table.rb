#!/usr/bin/env ruby
require 'iconv'

f = open("src/iso8859_5.erl", "w+")

f << <<-EOF
-module(iso8859_5).
-export([decode/1]).

decode(Bin) when is_binary(Bin) ->
  decode(Bin, <<>>).

EOF


0.upto(255) do |ch|
  list = Iconv.iconv("utf-8", "iso8859-5", [ch].pack("C*")).first.unpack("C*")
  f << <<-EOF
decode(<<#{ch}, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, #{list.join(",")}>>);

EOF
end

f << <<-EOF
decode(<<>>, Acc) -> Acc.

EOF