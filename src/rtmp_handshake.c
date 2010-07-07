#include <stdint.h>
#include "erl_nif.h"

#include <openssl/bn.h>
#include <openssl/dh.h>
#include <openssl/rc4.h>

// 2^1024 - 2^960 - 1 + 2^64 * { [2^894 pi] + 129093 }
#define P1024 \
	"FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1" \
	"29024E088A67CC74020BBEA63B139B22514A08798E3404DD" \
	"EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245" \
	"E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED" \
	"EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381" \
	"FFFFFFFFFFFFFFFF"

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
generate_dh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary client_public, shared, server_public;
  DH *dh;
  uint32_t shared_size;
  BIGNUM *peer_public;
  
  if (!enif_inspect_binary(env, argv[0], &client_public)) {
    return enif_make_badarg(env);
  }
  
  dh = DH_new();
  dh->p = BN_new();
  dh->g = BN_new();
  dh->length = 1024;
  
  BN_hex2bn(&dh->p, P1024);
  BN_set_word(dh->g, 2);
  
  DH_generate_key(dh);
  shared_size = DH_size(dh);
  
  enif_alloc_binary(env, shared_size, &shared);
  peer_public = BN_bin2bn(client_public.data, client_public.size, NULL);
  
  DH_compute_key(shared.data, peer_public, dh);

  enif_alloc_binary(env, BN_num_bytes(dh->pub_key), &server_public);
  BN_bn2bin(dh->pub_key, server_public.data);
   
  return enif_make_tuple2(env, enif_make_binary(env, &server_public), enif_make_binary(env, &shared));
}

static ERL_NIF_TERM
crypto_keys(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary client_public, shared, server_public;
}

static ErlNifFunc rtmp_handshake_funcs[] =
{
    {"generate_dh", 1, generate_dh},
    {"crypto_keys", 3, crypto_keys}
};

ERL_NIF_INIT(rtmp_handshake, rtmp_handshake_funcs, load, reload, upgrade, unload)

