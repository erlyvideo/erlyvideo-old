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

#ifndef IOV_MAX
#define IOV_MAX 1000
#endif

typedef enum {LISTENER_MODE, CLIENT_MODE} SocketMode;

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
  uint32_t upper_limit;
  uint32_t lower_limit;
} Config;
#pragma options align=reset


typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner_pid;
  int socket;
  uint32_t upper_limit;
  uint32_t lower_limit;
  unsigned long timeout;
  int paused_output;
  SocketMode mode;
  Config config; // Only for listener mode
} Emstcp;




static ErlDrvData microtcp_drv_start(ErlDrvPort port, char *buff)
{
    Emstcp* d = (Emstcp *)driver_alloc(sizeof(Emstcp));
    bzero(d, sizeof(Emstcp));
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->owner_pid = driver_caller(port);
    return (ErlDrvData)d;
}


static void microtcp_drv_stop(ErlDrvData handle)
{
  Emstcp* d = (Emstcp *)handle;
  if(d->mode == LISTENER_MODE) {
    fprintf(stderr, "Listener port is closing: %d\r\n", ntohs(d->config.port));
  } else {
    fprintf(stderr, "Client socket is closing\r\n");
  }
  driver_select(d->port, (ErlDrvEvent)(d->socket), DO_READ|DO_WRITE, 0);
  close(d->socket);
  driver_free((char*)handle);
}


static void tcp_exit(Emstcp *d)
{
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ|DO_WRITE, 0);
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("tcp_closed"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_TUPLE, 2
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  driver_exit(d->port, 0);
}

static void microtcp_drv_outputv(ErlDrvData handle, ErlIOVec *ev)
{
    Emstcp* d = (Emstcp *)handle;
    if(d->paused_output) {
      return;
    } else if(ev->size + driver_sizeq(d->port) > d->upper_limit) {
      d->paused_output = 1;
      fprintf(stderr, "Pausing client\r\n");
      ErlDrvTermData reply[] = {
        ERL_DRV_ATOM, driver_mk_atom("tcp_paused"),
        ERL_DRV_PORT, driver_mk_port(d->port),
        ERL_DRV_TUPLE, 2
      };
      driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
      
    } else {
      driver_enqv(d->port, ev, 0);
      //fprintf(stderr, "Queue %d bytes, %d\r\n", ev->size,  driver_sizeq(d->port));
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 1);
    }
}



static void microtcp_drv_output(ErlDrvData handle, ErlDrvEvent event)
{
  Emstcp* d = (Emstcp*) handle;
  SysIOVec* vec;
  int vlen = 0;
  size_t written;
  driver_set_timer(d->port, d->timeout);
  vec = driver_peekq(d->port, &vlen);
  if(!vec || !vlen) {
    driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 0);
    return;
  }
  fprintf(stderr, "Flushing buffer %d\r\n", driver_sizeq(d->port));
  written = writev(d->socket, (const struct iovec *)vec, vlen > IOV_MAX ? IOV_MAX : vlen);
  if(vlen > IOV_MAX) {
    fprintf(stderr, "Buffer overloaded: %d, %d\r\n", vlen, driver_sizeq(d->port) - written);
  } else {
    fprintf(stderr, "Network write: %d (%d)\r\n", written, driver_sizeq(d->port) - written);
  }
  if(written == -1) {
    if((errno != EWOULDBLOCK) && (errno != EINTR)) {
        fprintf(stderr, "Error in writev: %s %p %d/%d\r\n", strerror(errno), vec, vlen, driver_sizeq(d->port));
      tcp_exit(d);
      return;
    }
  } else {
    driver_deq(d->port, written);
    if(d->paused_output && driver_sizeq(d->port) <= d->lower_limit) {
      ErlDrvTermData reply[] = {
        ERL_DRV_ATOM, driver_mk_atom("tcp_resumed"),
        ERL_DRV_PORT, driver_mk_port(d->port),
        ERL_DRV_TUPLE, 2
      };
      driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
      fprintf(stderr, "Resuming client\r\n");
      d->paused_output = 0;
    }
  }
}

static int microtcp_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen) {
  Emstcp* d = (Emstcp*) handle;
  
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
      if(d->config.reuseaddr) {
        fprintf(stderr, "Reusing listen addr\r\n");
        setsockopt(d->socket, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)); 
      }
      if(d->config.keepalive) {
        setsockopt(d->socket, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof(on));
      }
      
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 1);
      d->mode = LISTENER_MODE;
      listen(d->socket, d->config.backlog);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_ACTIVE_ONCE: {
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 1);
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

static void accept_tcp(Emstcp *d)
{
  socklen_t sock_len;
  struct sockaddr_in client_addr;
  int fd = accept(d->socket, (struct sockaddr *)&client_addr, &sock_len);
  if(fd == -1) {
    return;
  }
  Emstcp* c = driver_alloc(sizeof(Emstcp));

  c->owner_pid = d->owner_pid;
  c->socket = fd;
  c->mode = CLIENT_MODE;

  ErlDrvPort client = driver_create_port(d->port, d->owner_pid, "microtcp_drv", (ErlDrvData)c);
  c->port = client;
  c->upper_limit = d->config.upper_limit;
  c->lower_limit = d->config.lower_limit;
  c->paused_output = 0;
  c->timeout = d->config.timeout;
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
  Emstcp* d = (Emstcp*) handle;
  
  if(d->mode == LISTENER_MODE) {
    accept_tcp(d);
    return;
  }
  
  if(d->mode == CLIENT_MODE) {
    driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 0);
    ErlDrvBinary* bin = driver_alloc_binary(10240);
    size_t n = recv(d->socket, bin->orig_bytes, bin->orig_size, 0);
    if(n <= 0) {
      if((errno != EWOULDBLOCK) && (errno != EINTR)) {
        fprintf(stderr, "Error in recv: %s\r\n", strerror(errno));
        tcp_exit(d);
      }  
      return;
    }
    
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("tcp"),
      ERL_DRV_PORT, driver_mk_port(d->port),
      ERL_DRV_BINARY, (ErlDrvTermData)bin, (ErlDrvTermData)n, 0,
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
    return;
  }
}


static void microtcp_inet_timeout(ErlDrvData handle)
{
  Emstcp* d = (Emstcp *)handle;
  fprintf(stderr, "Timeout in socket\r\n");
  tcp_exit(d);
}

ErlDrvEntry microtcp_driver_entry = {
    NULL,			/* F_PTR init, N/A */
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
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(microtcp_drv) /* must match name in driver_entry */
{
    return &microtcp_driver_entry;
}
