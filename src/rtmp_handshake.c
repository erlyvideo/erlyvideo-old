#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

#include <openssl/bn.h>
#include <openssl/dh.h>
#include <openssl/rc4.h>
#include <openssl/sha.h>
#include <openssl/hmac.h>

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
  
  enif_alloc_binary(env, DH_size(dh), &shared);
  peer_public = BN_bin2bn(client_public.data, client_public.size, NULL);
  
  DH_compute_key(shared.data, peer_public, dh);

  enif_alloc_binary(env, BN_num_bytes(dh->pub_key), &server_public);
  BN_bn2bin(dh->pub_key, server_public.data);
   
  return enif_make_tuple2(env, enif_make_binary(env, &server_public), enif_make_binary(env, &shared));
}

static ERL_NIF_TERM
crypto_keys(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary client_public, shared, server_public, key_in, key_out;
  uint8_t digest[SHA256_DIGEST_LENGTH];
  unsigned int digestLen = 0;
  
  int i;

  if(!enif_inspect_binary(env, argv[0], &server_public)) return enif_make_badarg(env);
  if(!enif_inspect_binary(env, argv[1], &client_public)) return enif_make_badarg(env);
  if(!enif_inspect_binary(env, argv[2], &shared)) return enif_make_badarg(env);
  
  if(!enif_alloc_binary(env, sizeof(RC4_KEY), &key_in)) return enif_make_badarg(env);
  if(!enif_alloc_binary(env, sizeof(RC4_KEY), &key_out)) return enif_make_badarg(env);
  
  
  HMAC_CTX ctx;
  HMAC_CTX_init(&ctx);
  HMAC_Init_ex(&ctx, shared.data, shared.size, EVP_sha256(), 0);
  HMAC_Update(&ctx, client_public.data, client_public.size);
  HMAC_Final(&ctx, digest, &digestLen);
  HMAC_CTX_cleanup(&ctx);
  
  printf("RC4KeyOut:");
  for(i = 0; i < 16; i++) printf(" %02x", digest[i]);
  printf("\n");

  RC4_set_key((RC4_KEY *)key_out.data, 16, digest);

  HMAC_CTX_init(&ctx);
  HMAC_Init_ex(&ctx, shared.data, shared.size, EVP_sha256(), 0);
  HMAC_Update(&ctx, server_public.data, server_public.size);
  HMAC_Final(&ctx, digest, &digestLen);
  HMAC_CTX_cleanup(&ctx);

  printf("RC4KeyIn:");
  for(i = 0; i < 16; i++) printf(" %x", digest[i]);
  RC4_set_key((RC4_KEY *)key_in.data, 16, digest);
  printf("\n");

  uint8_t data[1536];
  RC4((RC4_KEY *)key_in.data, 1536, data, data);
  RC4((RC4_KEY *)key_out.data, 1536, data, data);
  
  return enif_make_tuple2(env, enif_make_binary(env, &key_in), enif_make_binary(env, &key_out));
}

static ERL_NIF_TERM
crypt_rc4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key_in, key_out, data_in, data_out;
  
  if(!enif_inspect_binary(env, argv[0], &key_in)) {
    return enif_make_badarg(env);
  }
  if(!enif_inspect_iolist_as_binary(env, argv[1], &data_in)) {
    return enif_make_badarg(env);
  }
  
  if(!data_in.size) {
    return enif_make_tuple2(env, argv[0], argv[1]);
  }
  
  if(!enif_alloc_binary(env, key_in.size, &key_out)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Couldn't duplicate key", ERL_NIF_LATIN1));
  }

  if(!enif_alloc_binary(env, data_in.size, &data_out)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Couldn't duplicate data", ERL_NIF_LATIN1));
  }

  memcpy(key_out.data, key_in.data, key_in.size);
  memcpy(data_out.data, data_in.data, data_in.size);
  
  // RC4((RC4_KEY *)key_out.data, data_out.size, data_in.data, data_out.data);
  RC4((RC4_KEY *)key_out.data, data_out.size, data_out.data, data_out.data);

  return enif_make_tuple2(env, enif_make_binary(env, &key_out), enif_make_binary(env, &data_out));
}

static ErlNifFunc rtmp_handshake_funcs[] =
{
    {"generate_dh", 1, generate_dh},
    {"crypto_keys", 3, crypto_keys},
    {"crypt", 2, crypt_rc4}
};

ERL_NIF_INIT(rtmp_handshake, rtmp_handshake_funcs, load, reload, upgrade, unload)

