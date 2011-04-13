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

typedef struct {
  int fd;
  ssize_t size;
  void *ptr;
  int debug;
} Mmap;

ErlNifResourceType *MmapResource;

#define PATH_SIZE 1024

static void 
mmap_destructor(ErlNifEnv* env, void *obj)
{
  Mmap *desc = obj;
  if(desc->debug) fprintf(stderr, "MMap free: %p\r\n", obj);
  munmap(desc->ptr, desc->size);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  MmapResource = enif_open_resource_type(env, NULL, "mmap_resource", mmap_destructor, ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER, NULL);
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
mmap_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Mmap *desc;
  ERL_NIF_TERM f;
  ErlNifBinary bin_path;
  char path[PATH_SIZE];
  int fd;
  struct stat st;
  int debug = 0;
  ERL_NIF_TERM opt, opts;
  
  
  if (!enif_inspect_binary(env, argv[0], &bin_path)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Should pass binary filename", ERL_NIF_LATIN1));
  }
  if (!enif_is_list(env, argv[1])) {
    return enif_make_badarg(env);
  }
  
  bzero(path, PATH_SIZE);
  strncpy(path, (const char *)bin_path.data, bin_path.size >= PATH_SIZE ? PATH_SIZE - 1 : bin_path.size);
  
  opts = argv[1];
  while(enif_get_list_cell(env, opts, &opt, &opts)) {
    if(!enif_compare(opt, enif_make_atom(env, "debug"))) {
      debug = 1;
    }
  }
  
  
  fd = open(path, O_RDONLY);
  if(fd == -1) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, strerror(errno), ERL_NIF_LATIN1));
  }
  if(fstat(fd, &st)) {
    close(fd);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, strerror(errno), ERL_NIF_LATIN1));
  }
  
  if (debug) fprintf(stderr, "Opened file %s %ld\r\n", path, (ssize_t)st.st_size);
  
  desc = (Mmap *)enif_alloc_resource(MmapResource, sizeof(Mmap));
  if(!desc) {
    close(fd);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Couldn't allocate mmap resource", ERL_NIF_LATIN1));
  }
  desc->fd = fd;
  desc->size = st.st_size;
  if (debug) fprintf(stderr, "Mmaping file: %p\r\n", desc);
  desc->ptr = mmap(NULL, desc->size, PROT_READ, MAP_FILE | MAP_PRIVATE, desc->fd, 0);
  close(fd);
  if(desc->ptr == MAP_FAILED) {
    enif_release_resource(desc);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Couldn't mmap", ERL_NIF_LATIN1));
  }
  if (debug) fprintf(stderr, "Mmaped file to %p\r\n", desc->ptr);
  f = enif_make_resource_binary(env, (void *)desc, desc->ptr, desc->size);
  enif_release_resource(desc);
  desc->debug = debug;
  
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), f);
}

static ERL_NIF_TERM
mmap_pread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM sub;
  unsigned long offset, size;
  
  if(!enif_inspect_binary(env, argv[0], &bin)) {
    return enif_make_badarg(env);
  }
  if(!enif_get_ulong(env, argv[1], &offset)) {
    return enif_make_badarg(env);
  }
  if(!enif_get_ulong(env, argv[2], &size)) {
    return enif_make_badarg(env);
  }
  
  if((sub = enif_make_sub_binary(env, argv[0], offset, size))) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), sub);
  }
  return enif_make_ulong(env, 42);
}

static ErlNifFunc mmap_funcs[] =
{
  {"mmap_open", 2, mmap_open},
  {"mmap_pread", 3, mmap_pread}
};

ERL_NIF_INIT(mmap, mmap_funcs, load, reload, upgrade, unload)
