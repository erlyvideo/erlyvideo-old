#include <stdio.h>
#include "erl_nif.h"


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
find_nal(ErlNifBinary data, int i) {
  if (i + 4 >= data.size) {
    return -1;
  }
  for(; i + 4 < data.size; i++) {
    if (data.data[i] == 0 && data.data[i+1] == 0 && data.data[i+2] == 0 && data.data[i+3] == 1) {
      return i;
    }
  }
  return -1;
}

static ERL_NIF_TERM
extract_nal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM bin, nal, rest;
  int start, next;
  ErlNifBinary data;
  
  if (argc < 1) {
    return enif_make_badarg(env);
  }
  
  bin = argv[0];
  if (!enif_inspect_binary(env, bin, &data)) {
    return enif_make_badarg(env);
  }
  
  start = find_nal(data, 0);
  
  if(start == -1) {
    return enif_make_atom(env, "undefined");
  }
  
  start += 4;
  
  next = find_nal(data, start);
  if (next == -1) {
    next = data.size;
  }
  
  if (start < 0 || start > data.size - 1 || next < 0 || next < start || next > data.size) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "Invalid start1/start2: %d/%d (%d)", start, next, data.size);
    return enif_make_tuple2(env, 
      enif_make_atom(env, "error"),
      enif_make_string(env, buf, ERL_NIF_LATIN1)
    );
  }
  
  nal = enif_make_sub_binary(env, bin, start, next - start);
  rest = enif_make_sub_binary(env, bin, next, data.size - next);
  
  return enif_make_tuple3(env, enif_make_atom(env, "ok"), nal, rest);
}

static ErlNifFunc mpegts_reader_funcs[] =
{
    {"extract_nal", 1, extract_nal}
};

ERL_NIF_INIT(mpegts_reader, mpegts_reader_funcs, NULL, reload, upgrade, unload)

