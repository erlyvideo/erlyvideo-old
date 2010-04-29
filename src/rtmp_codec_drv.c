#include <erl_driver.h>
#include <string.h>
#include <stdint.h>

#define DEFAULT_CHUNK_SIZE 128
#define MAX_TIMESTAMP 0x00ffffff

/* commands and responses */
#define ENCODE 1
#define DECODE 2
#define OK 3
#define MORE 4
#define CONTINUE 5
#define SET_CHUNK_SIZE_IN 6
#define SET_CHUNK_SIZE_OUT 7
#define GET_CHUNK_SIZE_IN 8
#define GET_CHUNK_SIZE_OUT 9
#define CHUNK_ABORT 10
#define ALLOC_CSID 11

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

struct hash_table {
  size_t level;
  struct chunk **chunks;
};

typedef struct {
  uint32_t chunk_size_in;
  uint32_t chunk_size_out;
  uint16_t next_free_csid;
  struct hash_table *chunk_buf_in;
  struct hash_table *chunk_buf_out;
} state_data;

/* Refer to Adobe's RTMP specification for details
 * about RTMP stream packet structure
 */
struct rtmp {
  uint32_t chunk_type;       /* Chunk Type                  */
  uint32_t chunk_stream_id;  /* Chunk Stream ID             */
  uint32_t timestamp;        /* TimeStamp or TimeStampDelta */
  uint32_t msg_type_id;      /* RTMP Message Type ID        */
  uint32_t msg_stream_id;    /* RTMP Message Stream ID      */
};

struct chunk {
  uint32_t type;
  uint32_t stream;
  uint32_t len;
  uint32_t timestamp;
  uint32_t timestamp_delta;
  uint32_t data_size;
  char *data;
};

static void free_chunk(struct chunk *chunk)
{
  if (chunk != NULL) {
    if (chunk->data != NULL) driver_free(chunk->data);
    driver_free(chunk);
  }
}

static struct hash_table *init_hash_table()
{
  struct hash_table *ht = driver_alloc(sizeof(struct hash_table));
  ht->level = 4; /* Minimum table size is 2^4 = 16 elements */
  size_t size = 1 << ht->level;
  size_t i;
  ht->chunks = driver_alloc(sizeof(struct chunk *) * size);
  for (i = 0; i < size; i++) {
    ht->chunks[i] = NULL;
  };
  return ht;
}

static void destroy_hash_table(struct hash_table *ht)
{
  if (ht != NULL) {
    size_t i;
    size_t size = 1 << ht->level;
    for (i = 0; i < size; i++) {
      if (ht->chunks[i] != NULL) free_chunk(ht->chunks[i]);
    };
    if (ht->chunks != NULL) driver_free(ht->chunks);
    driver_free(ht);
  }
}

static struct chunk *hash_table_lookup(struct hash_table *ht, uint16_t csid)
{
  size_t size = 1 << ht->level;
  if (csid < size) {
    return ht->chunks[csid];
  };
  return NULL;
}

static void hash_table_delete(struct hash_table *ht, uint16_t csid)
{
  size_t size = 1 << ht->level;
  if (csid < size) {
    free_chunk(ht->chunks[csid]);
    ht->chunks[csid] = NULL;
  }
}

static void hash_table_insert(struct hash_table *ht,
			      uint16_t csid, struct chunk *chunk)
{
  size_t prev_size = 1 << ht->level;
  if (csid >= prev_size) {
    size_t i = 0;
    while (1 << ht->level <= csid) ht->level++;
    size_t new_size = 1 << ht->level;
    ht->chunks = driver_realloc(ht->chunks, sizeof(struct chunk *) * new_size);
    for (i = prev_size; i < new_size; i++) ht->chunks[i] = NULL;
  };
  if (ht->chunks[csid] != NULL) free_chunk(ht->chunks[csid]);
  ht->chunks[csid] = chunk;
}

inline uint32_t read_uint16(char *ptr)
{
  uint8_t a = ptr[0];
  uint8_t b = ptr[1];
  return (a << 8) | b;
}

inline uint32_t read_uint24(char *ptr)
{
  uint8_t a = ptr[0];
  uint8_t b = ptr[1];
  uint8_t c = ptr[2];
  return (a << 16) | (b << 8) | c;
}

inline uint32_t read_uint32(char *ptr)
{
  uint8_t a = *ptr;
  uint8_t b = *(ptr + 1);
  uint8_t c = *(ptr + 2);
  uint8_t d = *(ptr + 3);
  return (a << 24) | (b << 16) | (c << 8) | d;
}

inline uint32_t read_uint32_little(char *ptr)
{
  uint8_t a = *ptr;
  uint8_t b = *(ptr + 1);
  uint8_t c = *(ptr + 2);
  uint8_t d = *(ptr + 3);
  return a | (b << 8) | (c << 16) | (d << 24);
}

inline void write_uint16(char *ptr, uint32_t x)
{
  ptr[0] = (x >> 8) & 0xff;
  ptr[1] = x & 0xff;
}

inline void write_uint24(char *ptr, uint32_t x)
{
  ptr[0] = (x >> 16) & 0xff;
  ptr[1] = (x >> 8) & 0xff;
  ptr[2] = x & 0xff;
}

inline void write_uint32(char *ptr, uint32_t x)
{
  ptr[0] = (x >> 24) & 0xff;
  ptr[1] = (x >> 16) & 0xff;
  ptr[2] = (x >> 8) & 0xff;
  ptr[3] = x & 0xff;
}

inline void write_uint32_little(char *ptr, uint32_t x)
{
  ptr[0] = x & 0xff;
  ptr[1] = (x >> 8) & 0xff;
  ptr[2] = (x >> 16) & 0xff;
  ptr[3] = (x >> 24) & 0xff;
}

inline void encode_rtmp_basic_header(char *rbuf, int *rlen,
				     uint32_t chunk_type,
				     uint32_t chunk_stream_id)
{
  if (chunk_stream_id < 64) {
    rbuf[(*rlen)++] = (chunk_type << 6) | chunk_stream_id;
    return;
  };
  if (chunk_stream_id < 320) {
    rbuf[(*rlen)++] = chunk_type << 6;
    rbuf[(*rlen)++] = chunk_stream_id - 64;
    return;
  };
  if (chunk_stream_id >= 320) {
    rbuf[(*rlen)++] = (chunk_type << 6) | 1;
    write_uint16(rbuf + *rlen, chunk_stream_id - 64);
    *rlen += 2;
    return;
  }
}

inline void encode_rtmp_ts(char *rbuf, int *rlen,
			   uint32_t chunk_type, uint32_t timestamp)
{
  if (chunk_type < 3) {
    write_uint24(rbuf + *rlen, MIN(timestamp, MAX_TIMESTAMP));
    *rlen += 3;
  }
}

inline void encode_rtmp_len(char *rbuf, int *rlen,
			    uint32_t chunk_type, uint32_t chunk_len)
{
  if (chunk_type < 2) {
    write_uint24(rbuf + *rlen, chunk_len);
    *rlen += 3;
  }
}

inline void encode_rtmp_type_id(char *rbuf, int *rlen,
				uint32_t chunk_type,
				uint32_t msg_type_id)
{
  if (chunk_type < 2) {
    rbuf[(*rlen)++] = msg_type_id & 0xff;
  }
}

inline void encode_rtmp_stream_id(char *rbuf, int *rlen,
				  uint32_t chunk_type,
				  uint32_t msg_stream_id)
{
  if (chunk_type == 0) {
    write_uint32_little(rbuf + *rlen, msg_stream_id);
    *rlen += 4;
  }
}

inline void encode_rtmp_ext_ts(char *rbuf, int *rlen,
			       uint32_t chunk_type,
			       uint32_t timestamp)
{
  if (chunk_type < 3) {
    if (timestamp > MAX_TIMESTAMP) {
      write_uint32(rbuf + *rlen, timestamp);
      *rlen += 4;
      return;
    }
    if (timestamp == MAX_TIMESTAMP) {
      write_uint32(rbuf + *rlen, 0);
      *rlen += 4;
      return;
    }
  }
}

static void encode_rtmp(const struct rtmp *rtmp,
			char *data, int data_len, int chunk_len,
			char *rbuf, int *rlen)
{
  encode_rtmp_basic_header(rbuf, rlen, rtmp->chunk_type, rtmp->chunk_stream_id);
  encode_rtmp_ts(rbuf, rlen, rtmp->chunk_type, rtmp->timestamp);
  encode_rtmp_len(rbuf, rlen, rtmp->chunk_type, chunk_len);
  encode_rtmp_type_id(rbuf, rlen, rtmp->chunk_type, rtmp->msg_type_id);
  encode_rtmp_stream_id(rbuf, rlen, rtmp->chunk_type, rtmp->msg_stream_id);
  encode_rtmp_ext_ts(rbuf, rlen, rtmp->chunk_type, rtmp->timestamp);
  memcpy(rbuf + *rlen, data, data_len);
  *rlen += data_len;
}

static char *do_encode(state_data *state, struct rtmp *rtmp,
		       char *data, int data_len, int *rlen)
{
  int chunk_size = MIN(data_len, state->chunk_size_out);
  int chunk_num = data_len / state->chunk_size_out;
  char *rbuf = driver_alloc((chunk_num + 1) * (chunk_size + 20));
  int pos = chunk_size;
  encode_rtmp(rtmp, data, chunk_size, data_len, rbuf, rlen);
  rtmp->chunk_type = 3;
  while (pos < data_len) {
    encode_rtmp(rtmp, data + pos, MIN(chunk_size, data_len - pos),
		0, rbuf, rlen);
    pos += chunk_size;
  };
  return rbuf;
}

static char *encode(state_data *state, char *buf, int buflen, int *rlen)
{
  struct rtmp rtmp;
  *rlen = 0;
  /* We're putting the data from Erlang as:
     <<MsgTypeID:32, MsgStreamID:32, TimeStamp:32, CSID:32, Data/binary>>
     So here we read it
  */
  char *data = buf + 16;
  uint32_t data_len = buflen - 16;
  uint32_t type = read_uint32(buf);
  uint32_t stream = read_uint32(buf + 4);
  uint32_t timestamp = read_uint32(buf + 8);
  uint32_t chunk_stream = read_uint32(buf + 12);
  struct chunk *prev_chunk;
  struct chunk *new_chunk = driver_alloc(sizeof(struct chunk));
  new_chunk->stream = stream;
  new_chunk->type = type;
  new_chunk->len = data_len;
  new_chunk->timestamp = timestamp;
  new_chunk->timestamp_delta = 0;
  new_chunk->data = NULL;
  rtmp.chunk_type = 0;
  rtmp.chunk_stream_id = chunk_stream;
  rtmp.timestamp = timestamp;
  rtmp.msg_type_id = type;
  rtmp.msg_stream_id = stream;
  prev_chunk = hash_table_lookup(state->chunk_buf_out, chunk_stream);
  while (prev_chunk) {
    /* Here goes a stupid machinery of chunks assembling.
       Adobe, burn in hell! */
    int timestamp_delta = timestamp - prev_chunk->timestamp;
    if (prev_chunk->stream != stream || timestamp_delta < 0) break;
    new_chunk->timestamp_delta = timestamp_delta;
    if (type != prev_chunk->type ||
	data_len != prev_chunk->len ||
	data_len > state->chunk_size_out) {
      rtmp.chunk_type = 1;
      rtmp.timestamp = timestamp_delta;
      break;
    };
    if (timestamp_delta == prev_chunk->timestamp_delta) {
      rtmp.chunk_type = 3;
      break;
    };
    rtmp.chunk_type = 2;
    rtmp.timestamp = timestamp_delta;
    break;
  };
  state->next_free_csid = MAX(state->next_free_csid, chunk_stream + 1);
  hash_table_insert(state->chunk_buf_out, chunk_stream, new_chunk);
  return do_encode(state, &rtmp, data, data_len, rlen);
}

inline char *more(int *rlen)
{
  /* Returns <<?MORE>> back to Erlang */
  *rlen = 1;
  char *res = driver_alloc(1);
  res[0] = MORE;
  return res;
}

static char *decode_rtmp_data(state_data *state, struct rtmp *rtmp,
			      char *buf, int buflen,
			      uint32_t msg_len, int *rlen)
{
  uint32_t chunk_size = state->chunk_size_in;
  uint32_t len = MIN(msg_len, chunk_size);
  struct chunk *prev_chunk;
  struct chunk *new_chunk;
  struct chunk chunk;
  chunk.type = rtmp->msg_type_id;
  chunk.stream = rtmp->msg_stream_id;
  chunk.len = msg_len;
  chunk.timestamp = rtmp->timestamp;
  chunk.timestamp_delta = 0;
  chunk.data_size = 0;
  chunk.data = NULL;
  if (rtmp->chunk_type != 0) {
    prev_chunk = hash_table_lookup(state->chunk_buf_in,
				   rtmp->chunk_stream_id);
    if (prev_chunk != NULL) {
      chunk.stream = prev_chunk->stream;
      chunk.data_size = prev_chunk->data_size;
      chunk.data = prev_chunk->data;
      chunk.timestamp = rtmp->timestamp + prev_chunk->timestamp;
      chunk.timestamp_delta = rtmp->timestamp;
      switch (rtmp->chunk_type) {
      case 1:
	break;
      case 2:
	len = MIN(prev_chunk->len, chunk_size);
	chunk.type = prev_chunk->type;
	chunk.len = prev_chunk->len;
	break;
      case 3:
	chunk.type = prev_chunk->type;
	chunk.len = prev_chunk->len;
	chunk.timestamp_delta = prev_chunk->timestamp_delta;
	if (prev_chunk->data_size == 0) {
	  len = MIN(prev_chunk->len, chunk_size);
	  chunk.timestamp = prev_chunk->timestamp + prev_chunk->timestamp_delta;
	} else {
	  len = MIN(prev_chunk->len - prev_chunk->data_size, chunk_size);
	  chunk.timestamp = prev_chunk->timestamp;
	};
	break;
      };
    } else {
      return NULL;
    };
  };
  if (buflen >= len) {
    uint32_t data_size = len + chunk.data_size;
    char *data = driver_alloc(data_size);
    char *result;
    memcpy(data, chunk.data, chunk.data_size);
    memcpy(data + chunk.data_size, buf, len);
    new_chunk = driver_alloc(sizeof(struct chunk));
    new_chunk->type = chunk.type;
    new_chunk->stream = chunk.stream;
    new_chunk->len = chunk.len;
    new_chunk->timestamp = chunk.timestamp;
    new_chunk->timestamp_delta = chunk.timestamp_delta;
    /* We return complete result as:
       <<?OK, Type:32, StreamID:32, TimeStamp:32,
         CSID:32, DataSize:32, Data:DataSize/binary, Tail/binary>>
       And incomplete result as:
       <<?MORE, Tail/binary>>
    */
    if (data_size == chunk.len) {
      new_chunk->data_size = 0;
      new_chunk->data = NULL;
      *rlen = 20 + data_size + buflen - len + 1;
      result = driver_alloc(*rlen);
      result[0] = OK;
      write_uint32(result + 1, chunk.type);
      write_uint32(result + 5, chunk.stream);
      write_uint32(result + 9, chunk.timestamp);
      write_uint32(result + 13, rtmp->chunk_stream_id);
      write_uint32(result + 17, data_size);
      memcpy(result + 21, data, data_size);
      memcpy(result + 21 + data_size, buf + len, buflen - len);
      driver_free(data);
    } else {
      new_chunk->data_size = data_size;
      new_chunk->data = data;
      *rlen = buflen - len + 1;
      result = driver_alloc(*rlen);
      result[0] = CONTINUE;
      memcpy(result + 1, buf + len, buflen - len);
    };
    hash_table_insert(state->chunk_buf_in, rtmp->chunk_stream_id, new_chunk);
    return result;
  } else {
    return more(rlen);
  }
}

static char *decode_rtmp_ext_ts(state_data *state, struct rtmp *rtmp,
				char *buf, int buflen, int data_len, int *rlen)
{
  if (rtmp->timestamp == MAX_TIMESTAMP) {
    if (buflen >= 4) {
      uint32_t ext_timestamp = read_uint32(buf);
      if (ext_timestamp) rtmp->timestamp = ext_timestamp;
      return decode_rtmp_data(state, rtmp, buf+4, buflen-4, data_len, rlen);
    } else {
      return more(rlen);
    }
  };
  return decode_rtmp_data(state, rtmp, buf, buflen, data_len, rlen);
}

static char *decode_rtmp_header(state_data *state, struct rtmp *rtmp,
				char *buf, int len, int *rlen)
{
  if (rtmp->chunk_type == 0 && len >= 11) {
    rtmp->timestamp = read_uint24(buf);
    uint32_t data_len = read_uint24(buf+3);
    rtmp->msg_type_id = buf[6];
    rtmp->msg_stream_id = read_uint32_little(buf+7);
    return decode_rtmp_ext_ts(state, rtmp, buf + 11, len - 11, data_len, rlen);
  };
  if (rtmp->chunk_type == 1 && len >= 7) {
    rtmp->timestamp = read_uint24(buf);
    uint32_t data_len = read_uint24(buf+3);
    rtmp->msg_type_id = buf[6];
    return decode_rtmp_ext_ts(state, rtmp, buf + 7, len - 7, data_len, rlen);
  };
  if (rtmp->chunk_type == 2 && len >= 3) {
    rtmp->timestamp = read_uint24(buf);
    return decode_rtmp_ext_ts(state, rtmp, buf + 3, len - 3, 0, rlen);
  };
  if (rtmp->chunk_type == 3) {
    return decode_rtmp_data(state, rtmp, buf, len, 0, rlen);
  };
  return more(rlen);
}

static char *decode(state_data *state, char *buf, int buflen, int *rlen)
{
  *rlen = 0;
  if (buflen > 0) {
    struct rtmp rtmp;
    uint32_t chunk_type = (buf[0] >> 6) & 0x3;
    uint32_t chunk_stream = buf[0] & 0x3f;
    rtmp.chunk_type = chunk_type;
    rtmp.chunk_stream_id = chunk_stream;
    rtmp.timestamp = 0;
    rtmp.msg_type_id = 0;
    rtmp.msg_stream_id = 0;
    if (chunk_stream == 0 && buflen > 1) {
      rtmp.chunk_stream_id = buf[1] + 64;
      return decode_rtmp_header(state, &rtmp, buf + 2, buflen - 2, rlen);
    };
    if (chunk_stream == 1 && buflen > 2) {
      rtmp.chunk_stream_id = read_uint16(buf+1) + 64;
      return decode_rtmp_header(state, &rtmp, buf + 3, buflen - 3, rlen);
    };
    if (chunk_stream > 1) {
      return decode_rtmp_header(state, &rtmp, buf + 1, buflen - 1, rlen);
    };
    return more(rlen);
  };
  return more(rlen);
}

static ErlDrvData driver_start(ErlDrvPort port, char *buf)
{
  state_data *state = (state_data *) driver_alloc(sizeof(state_data));
  state->chunk_size_in = DEFAULT_CHUNK_SIZE;
  state->chunk_size_out = DEFAULT_CHUNK_SIZE;
  state->next_free_csid = 3;
  state->chunk_buf_in = init_hash_table();
  state->chunk_buf_out = init_hash_table();
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) state;
}

static void driver_stop(ErlDrvData handle)
{
  state_data *state = (state_data *) handle;
  destroy_hash_table(state->chunk_buf_in);
  destroy_hash_table(state->chunk_buf_out);
  driver_free(state);
}

static int driver_control(ErlDrvData handle,
			  unsigned int command,
			  char *buf, int len,
			  char **rbuf, int rlen)
{
  ErlDrvBinary *b;
  char *data = NULL;
  rlen = 0;
  state_data *state = (state_data *) handle;
  switch (command) {
  case ENCODE:
    data = encode(state, buf, len, &rlen);
    break;
  case DECODE:
    data = decode(state, buf, len, &rlen);
    break;
  case SET_CHUNK_SIZE_IN:
    state->chunk_size_in = read_uint32(buf);
    break;
  case SET_CHUNK_SIZE_OUT:
    state->chunk_size_out = read_uint32(buf);
    break;
  case GET_CHUNK_SIZE_IN:
    rlen = 4;
    b = driver_alloc_binary(rlen);
    write_uint32(b->orig_bytes, state->chunk_size_in);
    *rbuf = (char *)b;
    break;
  case GET_CHUNK_SIZE_OUT:
    rlen = 4;
    b = driver_alloc_binary(rlen);
    write_uint32(b->orig_bytes, state->chunk_size_out);
    *rbuf = (char *)b;
    break;
  case ALLOC_CSID:
    rlen = 4;
    b = driver_alloc_binary(rlen);
    write_uint32(b->orig_bytes, state->next_free_csid++);
    *rbuf = (char *)b;
    break;
  case CHUNK_ABORT:
    hash_table_delete(state->chunk_buf_in, read_uint32(buf));
    break;
  };
  if (data != NULL) {
    b = driver_alloc_binary(rlen);
    memcpy(b->orig_bytes, data, rlen);
    driver_free(data);
    *rbuf = (char *) b;
  };
  return rlen;
}

ErlDrvEntry driver_entry = {
  NULL,			/* F_PTR init, N/A */
  driver_start,		/* L_PTR start, called when port is opened */
  driver_stop,		/* F_PTR stop, called when port is closed */
  NULL,			/* F_PTR output, called when erlang has sent */
  NULL,			/* F_PTR ready_input, called when input descriptor ready */
  NULL,			/* F_PTR ready_output, called when output descriptor ready */
  "rtmp_codec_drv",	/* char *driver_name, the argument to open_port */
  NULL,			/* F_PTR finish, called when unloaded */
  NULL,			/* handle */
  driver_control,	/* F_PTR control, port_command callback */
  NULL,			/* F_PTR timeout, reserved */
  NULL			/* F_PTR outputv, reserved */
};

DRIVER_INIT(rtmp_codec_drv) /* must match name in driver_entry */
{
  return &driver_entry;
}
