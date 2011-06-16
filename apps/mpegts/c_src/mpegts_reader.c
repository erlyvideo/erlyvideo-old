/*  @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
 *  @copyright  2010 Max Lapshin
 *  @doc        MPEG TS NIF module for finding PES
 *  Links:
 *   http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
 *   http://en.wikipedia.org/wiki/MPEG-TS
 *   http://en.wikipedia.org/wiki/Packetized_Elementary_Stream
 *   http://en.wikipedia.org/wiki/Program_Specific_Information
 *  @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
 *  @end
 * 
 *  This file is part of erlang-mpegts.
 *  
 *  erlang-mpegts is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 * 
 *  erlang-mpegts is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with erlang-mpegts.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * ---------------------------------------------------------------------------------------
 */
#include <stdio.h>
#include "erl_nif.h"

enum {START, END};

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv,
          ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}


static int
find_nal(ErlNifBinary data, int i, int border) {
  if (i + 3 >= data.size) {
    return -1;
  }
  for(; i + 2 < data.size; i++) {
    if (i + 3 < data.size && data.data[i] == 0 && data.data[i+1] == 0 && data.data[i+2] == 0 && data.data[i+3] == 1) {
      return border == START ? i + 4 : i;
    }
    if (data.data[i] == 0 && data.data[i+1] == 0 && data.data[i+2] == 1) {
      return border == START ? i + 3 : i;
    }
  }
  return -1;
}

static ERL_NIF_TERM
extract_nal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM bin, nal, rest;
  int start, end;
  ErlNifBinary data;
  
  if (argc < 1) {
    return enif_make_badarg(env);
  }
  
  bin = argv[0];
  if (!enif_inspect_binary(env, bin, &data)) {
    return enif_make_badarg(env);
  }
  
  start = find_nal(data, 0, START);
  
  if(start == -1) {
    return enif_make_atom(env, "undefined");
  }
  
  end = find_nal(data, start, END);
  if (end == -1) {
    return enif_make_atom(env, "undefined");
  }
  
  if (start < 0 || start > data.size - 1 || end < 0 || end < start || end > data.size) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "Invalid start1/start2: %d/%d (%lu)", start, end, data.size);
    return enif_make_tuple2(env, 
      enif_make_atom(env, "error"),
      enif_make_string(env, buf, ERL_NIF_LATIN1)
    );
  }
  
  nal = enif_make_sub_binary(env, bin, start, end - start);
  
  rest = enif_make_sub_binary(env, bin, end, data.size - end);
  
  return enif_make_tuple3(env, enif_make_atom(env, "ok"), nal, rest);
}

static ErlNifFunc mpegts_reader_funcs[] =
{
    {"extract_nal1", 1, extract_nal}
};

ERL_NIF_INIT(mpegts_reader, mpegts_reader_funcs, NULL, reload, upgrade, unload)

