// Authorship lost, no copyrights
#include <stdint.h>
#include "erl_nif.h"
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>


static ERL_NIF_TERM
stsz_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary stsz;
  uint32_t i;
  uint32_t size, *sizes;
  
  if (!enif_inspect_binary(env, argv[0], &stsz)) {
    return enif_make_badarg(env);
  }
  
  sizes = (uint32_t *)stsz.data;
  
  for(i = 0; i < stsz.size / 4; i++) {
    size += ntohl(sizes[i]);
  }
  
  return enif_make_ulong(env, size);
}


static ERL_NIF_TERM
prepare_chunks(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  
}


static ErlNifFunc mux_funcs[] =
{
  {"prepare_chunks", 3, prepare_chunks},
  {"stsz_size", 1, stsz_size}
};

ERL_NIF_INIT(mmap, mux_funcs, 0, 0, 0, 0)
