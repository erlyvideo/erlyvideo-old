#include <erl_driver.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/uio.h>
#include <http_parser.h>

#ifndef IOV_MAX
#define IOV_MAX 1000
#endif

typedef enum {LISTENER_MODE, CLIENT_MODE} SocketMode;

static ErlDrvTermData atom_http;
static ErlDrvTermData method_atoms[HTTP_PATCH+1];

enum {
    CMD_LISTEN = 1,
    CMD_ACTIVE_ONCE = 2
    };

#pragma pack(1)
typedef struct {
  uint16_t port;
  uint16_t backlog;
  uint8_t reuseaddr;
  uint8_t keepalive;
  uint16_t timeout;
} Config;
#pragma options align=reset

typedef struct {
  ErlDrvBinary *field;
  ErlDrvBinary *value;
} Header;

#define HTTP_MAX_HEADERS 100

typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner_pid;
  int socket;
  unsigned long timeout;
  SocketMode mode;
  http_parser_settings settings;
  http_parser *parser;
  ErlDrvBinary* buffer;
  Header headers[HTTP_MAX_HEADERS];
  int headers_count;
  ErlDrvBinary *url;
  
  Config config; // Only for listener mode
} HTTP;

static int microtcp_init(void) {
  atom_http = driver_mk_atom("http");
  int i;
  for(i = 0; i <= HTTP_PATCH; i++) {
    method_atoms[i] = driver_mk_atom((char *)http_method_str(i));
  }
  return 0;
}


static ErlDrvData microtcp_drv_start(ErlDrvPort port, char *buff)
{
    HTTP* d = (HTTP *)driver_alloc(sizeof(HTTP));
    bzero(d, sizeof(HTTP));
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->owner_pid = driver_caller(port);
    return (ErlDrvData)d;
}


static void microtcp_drv_stop(ErlDrvData handle)
{
  HTTP* d = (HTTP *)handle;
  if(d->mode == LISTENER_MODE) {
    fprintf(stderr, "Listener port is closing: %d\r\n", ntohs(d->config.port));
  }
  driver_select(d->port, (ErlDrvEvent)(d->socket), (int)DO_READ|DO_WRITE, 0);
  close(d->socket);
  driver_free((char*)handle);
}


static void tcp_exit(HTTP *d)
{
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ|DO_WRITE, 0);
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("tcp_closed"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_TUPLE, 2
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  driver_free_binary(d->buffer);
  driver_exit(d->port, 0);
}

static void microtcp_drv_outputv(ErlDrvData handle, ErlIOVec *ev)
{
  HTTP* d = (HTTP *)handle;
  driver_enqv(d->port, ev, 0);
  //fprintf(stderr, "Queue %d bytes, %d\r\n", ev->size,  driver_sizeq(d->port));
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 1);
}



static void microtcp_drv_output(ErlDrvData handle, ErlDrvEvent event)
{
  HTTP* d = (HTTP*) handle;
  SysIOVec* vec;
  int vlen = 0;
  size_t written;
  driver_set_timer(d->port, d->timeout);
  vec = driver_peekq(d->port, &vlen);
  if(!vec || !vlen) {
    driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 0);
    return;
  }
  written = writev(d->socket, (const struct iovec *)vec, vlen > IOV_MAX ? IOV_MAX : vlen);
  if(vlen > IOV_MAX) {
    fprintf(stderr, "Buffer overloaded: %d, %d\r\n", vlen, (int)(driver_sizeq(d->port) - written));
  }
  if(written == -1) {
    if((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN)) {
        fprintf(stderr, "Error in writev: %s, %d bytes left\r\n", strerror(errno), (int)driver_sizeq(d->port));
      tcp_exit(d);
      return;
    }
  } else {
    ErlDrvSizeT rest = driver_deq(d->port, written);
    // fprintf(stderr, "Network write: %d (%d)\r\n", (int)written, (int)rest);
    
  }
}

static void activate_read(HTTP *d) {
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 1);
}

static void deactivate_read(HTTP *d) {
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 0);
}

static ErlDrvSSizeT microtcp_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {
  HTTP* d = (HTTP*) handle;
  
  switch(command) {
    case CMD_LISTEN: {
      int flags;
      struct sockaddr_in si;
      
      d->socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
      if(len != sizeof(Config)) {
        driver_failure_atom(d->port, "invalid_config");
        return 0;
      }
      memcpy(&d->config, buf, sizeof(Config));
      
      bzero(&si, sizeof(si));
      si.sin_family = AF_INET;
      si.sin_port = d->config.port; // It comes in network byte order
      si.sin_addr.s_addr = htonl(INADDR_ANY);
      if(bind(d->socket, (struct sockaddr *)&si, sizeof(si)) == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
      }
      flags = fcntl(d->socket, F_GETFL);
      if(flags == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
      }
      if(fcntl(d->socket, F_SETFL, flags | O_NONBLOCK) == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
      }
      
      int on = 1;
      if(d->config.reuseaddr || 1) {
        fprintf(stderr, "Reusing listen addr\r\n");
        setsockopt(d->socket, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)); 
      }
      if(d->config.keepalive) {
        setsockopt(d->socket, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof(on));
      }
      
      activate_read(d);
      d->mode = LISTENER_MODE;
      listen(d->socket, d->config.backlog);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_ACTIVE_ONCE: {
      activate_read(d);
      driver_set_timer(d->port, d->timeout);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    break;
    default:
    return 0;
  }
  return 0;
}

static int on_message_begin(http_parser *p) {
  // fprintf(stderr, "S> INCOME REQUEST\r\n");
  HTTP *d = (HTTP *)p->data;
  d->url = NULL;
  d->headers_count = 0;
  return 0;
}

static int on_url(http_parser *p, const char *url, size_t len) {
  // TODO: here should be special accelerated cache replier
  HTTP *d = (HTTP *)p->data;
  d->url = driver_alloc_binary(len);
  activate_read(d);
  memcpy(d->url->orig_bytes, url, len);
  return 0;
}

static int on_header_field(http_parser *p, const char *field, size_t len) {
  HTTP *d = (HTTP *)p->data;
  activate_read(d);
  if(d->headers_count >= HTTP_MAX_HEADERS) {
    p->http_errno = HPE_HEADER_OVERFLOW;
    return 1;
  }
  
  ErlDrvBinary *bin = d->headers[d->headers_count].field = driver_alloc_binary(len);
  memcpy(bin->orig_bytes, field, len);
  
  return 0;
}

static int on_header_value(http_parser *p, const char *field, size_t len) {
  HTTP *d = (HTTP *)p->data;
  activate_read(d);

  ErlDrvBinary *bin = d->headers[d->headers_count].value = driver_alloc_binary(len);
  memcpy(bin->orig_bytes, field, len);
  
  d->headers_count++;
  
  return 0;
}

static int on_headers_complete(http_parser *p) {
  HTTP *d = (HTTP *)p->data;
  
  int count = 2 + 2 + 2 + 4 + 6 + d->headers_count*(4*2 + 2) + 3 + 2;
  ErlDrvTermData reply[count];
  
  int i = 0;
  
  // fprintf(stderr, "S> %s %.*s HTTP/%d.%d\r\n", http_method_str(p->method), (int)d->url->orig_size, d->url->orig_bytes, p->http_major, p->http_minor);
  
  
  reply[i++] = ERL_DRV_ATOM;
  reply[i++] = atom_http;
  reply[i++] = ERL_DRV_PORT;
  reply[i++] = driver_mk_port(d->port);
  reply[i++] = ERL_DRV_ATOM;
  reply[i++] = method_atoms[p->method];
  reply[i++] = ERL_DRV_BINARY;
  reply[i++] = (ErlDrvTermData)d->url;
  reply[i++] = (ErlDrvTermData)d->url->orig_size;
  reply[i++] = 0;
  
  reply[i++] = ERL_DRV_UINT;
  reply[i++] = p->http_major;
  reply[i++] = ERL_DRV_UINT;
  reply[i++] = p->http_minor;
  reply[i++] = ERL_DRV_TUPLE;
  reply[i++] = 2;
  
  int j;
  
  for(j = 0; j < d->headers_count; j++) {
    reply[i++] = ERL_DRV_BINARY;
    reply[i++] = (ErlDrvTermData)d->headers[j].field;
    reply[i++] = (ErlDrvTermData)d->headers[j].field->orig_size;
    reply[i++] = 0;

    reply[i++] = ERL_DRV_BINARY;
    reply[i++] = (ErlDrvTermData)d->headers[j].value;
    reply[i++] = (ErlDrvTermData)d->headers[j].value->orig_size;
    reply[i++] = 0;
    
    reply[i++] = ERL_DRV_TUPLE;
    reply[i++] = 2;

    // fprintf(stderr, "S> %.*s: %.*s\r\n", (int)d->headers[j].field->orig_size, d->headers[j].field->orig_bytes, (int)d->headers[j].value->orig_size, d->headers[j].value->orig_bytes);
  }
  
  reply[i++] = ERL_DRV_NIL;
  reply[i++] = ERL_DRV_LIST;
  reply[i++] = j+1;
  
  reply[i++] = ERL_DRV_TUPLE;
  reply[i++] = 6;
  
  driver_output_term(d->port, reply, i);
  // reply[10] = ERL_DRV_TUPLE;
  // reply[11] = 4;
  // int o = driver_output_term(d->port, reply, 12);
  deactivate_read(d);
  
  // fprintf(stderr, "S> -- (%d, %d) \r\n", i, o);
  if(p->method == HTTP_POST || p->method == HTTP_PUT) {
    return 5;
  } else {
    return 1;
  }
}

static int on_body(http_parser *p, const char *body, size_t len) {
  fprintf(stderr, "S> body chunk %d\r\n", (int)len);
  return 0;
}

static int on_message_complete(http_parser *_) {
  // fprintf(stderr, "S> REQUEST COMPLETE\r\n");
  return 0;
}

static void accept_tcp(HTTP *d)
{
  socklen_t sock_len;
  struct sockaddr_in client_addr;
  int fd = accept(d->socket, (struct sockaddr *)&client_addr, &sock_len);
  if(fd == -1) {
    return;
  }
  HTTP* c = driver_alloc(sizeof(HTTP));

  c->owner_pid = d->owner_pid;
  c->socket = fd;
  c->mode = CLIENT_MODE;

  ErlDrvPort client = driver_create_port(d->port, d->owner_pid, "microtcp_drv", (ErlDrvData)c);
  c->port = client;
  c->timeout = d->config.timeout;
  c->parser = malloc(sizeof(http_parser));
  c->parser->data = c;
  c->settings.on_message_begin = on_message_begin;
  c->settings.on_url = on_url;
  c->settings.on_header_field = on_header_field;
  c->settings.on_header_value = on_header_value;
  c->settings.on_headers_complete = on_headers_complete;
  c->settings.on_body = on_body;
  c->settings.on_message_complete = on_message_complete;
  http_parser_init(c->parser, HTTP_REQUEST);
  c->buffer = driver_alloc_binary(10240);
  driver_set_timer(c->port, c->timeout);
  set_port_control_flags(c->port, PORT_CONTROL_FLAG_BINARY);
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("tcp_connection"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_PORT, driver_mk_port(client),
    ERL_DRV_TUPLE, 3
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
}


static void microtcp_drv_input(ErlDrvData handle, ErlDrvEvent event)
{
  HTTP* d = (HTTP*) handle;
  
  if(d->mode == LISTENER_MODE) {
    accept_tcp(d);
    return;
  }
  
  if(d->mode == CLIENT_MODE) {
    
    ssize_t n = recv(d->socket, d->buffer->orig_bytes, d->buffer->orig_size, 0);
    // ErlDrvTermData reply[] = {
    //   ERL_DRV_ATOM, driver_mk_atom("tcp"),
    //   ERL_DRV_PORT, driver_mk_port(d->port),
    //   ERL_DRV_BINARY, (ErlDrvTermData)d->buffer, (ErlDrvTermData)n, 0,
    //   ERL_DRV_TUPLE, 3
    // };
    // driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
    
    if(n == 0 || (n < 0 && errno == ECONNRESET)) {
      tcp_exit(d);
      return;
    }
    
    if(n < 0) {
      if((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN)) {
        fprintf(stderr, "Error in recv: %s\r\n", strerror(errno));
        tcp_exit(d);
      }  
      return;
    }
    
    ssize_t nparsed = http_parser_execute(d->parser, &d->settings, d->buffer->orig_bytes, n);
    
    if(d->parser->upgrade) {
      fprintf(stderr, "Websockets not supported\n");
      tcp_exit(d);
      return;
    }
    
    if(nparsed != n) {
      if(d->parser->http_errno == HPE_CB_headers_complete) {
        fprintf(stderr, "Restarting http headers: %s %d %d/%d\r\n", http_method_str(d->parser->method), d->parser->state, (int)nparsed, (int)n);
        nparsed = http_parser_execute(d->parser, &d->settings, d->buffer->orig_bytes + nparsed, n - nparsed);
      } else {
        fprintf(stderr, "Handle HTTP error: %s(%s)\n", http_errno_name(d->parser->http_errno), http_errno_description(d->parser->http_errno));
        tcp_exit(d);
        return;
      }
    }
    
    // fprintf(stderr, "Parsed all: %d, %d\r\n", (int)nparsed, d->parser->state);
    
    // ErlDrvTermData reply[] = {
    //   ERL_DRV_ATOM, driver_mk_atom("tcp"),
    //   ERL_DRV_PORT, driver_mk_port(d->port),
    //   ERL_DRV_BINARY, (ErlDrvTermData)c->, (ErlDrvTermData)n, 0,
    //   ERL_DRV_TUPLE, 3
    // };
    // driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
    return;
  }
}


static void microtcp_inet_timeout(ErlDrvData handle)
{
  HTTP* d = (HTTP *)handle;
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("tcp_error"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_PORT, driver_mk_atom("timeout"),
    ERL_DRV_TUPLE, 3
  };
}

ErlDrvEntry microtcp_driver_entry = {
    microtcp_init,			/* F_PTR init, N/A */
    microtcp_drv_start,		/* L_PTR start, called when port is opened */
    microtcp_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,	                /* F_PTR output, called when erlang has sent */
    microtcp_drv_input,		/* F_PTR ready_input, called when input descriptor ready */
    microtcp_drv_output,	/* F_PTR ready_output, called when output descriptor ready */
    "microtcp_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    microtcp_drv_command,			/* F_PTR control, port_command callback */
    microtcp_inet_timeout,			/* F_PTR timeout, reserved */
    microtcp_drv_outputv,	/* F_PTR outputv, reserved */
    NULL,                      /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MINOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(microtcp_drv) /* must match name in driver_entry */
{
    return &microtcp_driver_entry;
}
