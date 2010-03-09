#include "erl_nif.h"


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

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


static ERL_NIF_TERM
find_nal_start_code(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM bin = argv[0];
  ERL_NIF_TERM offset = argv[1];
  int i;
  unsigned int current_offset;
  ErlNifBinary data;
  
  if (!enif_inspect_binary(env, bin, &data)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, offset, &current_offset)) {
    return enif_make_badarg(env);
  }
  for(i = 0; i < data.size; i++) {
    if (data.data[i] == 0 && data.data[i+1] == 0 && data.data[i+2] == 0 && data.data[i+3] == 1) {
      enif_make_ulong(env, current_offset + i);
    }
  }
  return enif_make_atom(env, "false");
}

static ErlNifFunc mpegts_reader_funcs[] =
{
    {"find_nal_start_code", 2, find_nal_start_code}
};

ERL_NIF_INIT(mpegts_reader, mpegts_reader_funcs, load, reload, upgrade, unload)

