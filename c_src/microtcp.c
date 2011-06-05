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


const int MPEGTS_SIZE = 188;
const int BUFFER_SIZE = 12000;
const int LIMIT_SIZE = 1300;
#define PID_COUNT 8192

typedef enum {LISTENER_MODE, CLIENT_MODE} SocketMode;

enum {
    CMD_LISTEN = 1,
    CMD_ACTIVE_ONCE = 2
    };

typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner_pid;
  int socket;
  uint32_t backlog;
  uint32_t upper_limit;
  uint32_t lower_limit;
  int paused_output;
  SocketMode mode;
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
  driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ|DO_WRITE, 0);
  close(d->socket);
  driver_free((char*)handle);
}


static void microtcp_drv_outputv(ErlDrvData handle, ErlIOVec *ev)
{
    Emstcp* d = (Emstcp *)handle;
    if(d->paused_output) {
      fprintf(stderr, "Blocking output because port is paused\r\n");
    } else if(ev->size + driver_sizeq(d->port) > d->upper_limit) {
      fprintf(stderr, "Blocking output %d bytes because it will overflow limit %d/%d\r\n", ev->size, driver_sizeq(d->port), d->upper_limit);
      d->paused_output = 1;
    } else {
      fprintf(stderr, "Output %d bytes (%d in queue)\r\n", ev->size, driver_sizeq(d->port));
      driver_enqv(d->port, ev, 0);
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 1);
    }
}



static void microtcp_drv_output(ErlDrvData handle, ErlDrvEvent event)
{
  Emstcp* d = (Emstcp*) handle;
  SysIOVec* vec;
  int vlen;
  size_t written;
  vec = driver_peekq(d->port, &vlen);
  if(!vec) {
    driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 0);
    return;
  }
  written = writev(d->socket, vec, vlen);
  if(written == -1) {
    if((errno != EWOULDBLOCK) && (errno != EINTR)) {
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_WRITE, 0);
      ErlDrvTermData reply[] = {
        ERL_DRV_ATOM, driver_mk_atom("tcp_closed"),
        ERL_DRV_PORT, driver_mk_port(d->port),
        ERL_DRV_TUPLE, 2
      };
      driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
      driver_exit(d->port, 0);
      return;
    }
    fprintf(stderr, "Error(%d): %s\r\n", errno, strerror(errno));
  } else {
    driver_deq(d->port, written);
    if(driver_sizeq(d->port) <= d->lower_limit) {
      d->paused_output = 0;
    }
    // fprintf(stderr, "Flush %d to net, %d left\r\n", written, driver_sizeq(d->port));
  }
}

static int microtcp_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen) {
  Emstcp* d = (Emstcp*) handle;
  
  switch(command) {
    case CMD_LISTEN: {
      int sock;
      int port;
      int flags;
      struct sockaddr_in si;
      port = atoi(buf);
      // fprintf(stderr, "Connecting to port %d\r\n", port);
      sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
      
      bzero(&si, sizeof(si));
      si.sin_family = AF_INET;
      si.sin_port = htons(port);
      si.sin_addr.s_addr = htonl(INADDR_ANY);
      if(bind(sock, (struct sockaddr *)&si, sizeof(si)) == -1) {
        driver_failure_posix(d->port, errno);
        return 0;
        // memcpy(*rbuf, "error", 5);
        // return 5;
      }
      d->socket = sock;
      flags = fcntl(d->socket, F_GETFL);
      assert(flags >= 0);
      assert(!fcntl(d->socket, F_SETFL, flags | O_NONBLOCK));
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 1);
      d->mode = LISTENER_MODE;
      d->backlog = 30;
      listen(d->socket, d->backlog);
      memcpy(*rbuf, "ok", 2);
      return 2;
    }
    
    case CMD_ACTIVE_ONCE: {
      // fprintf(stderr, "Activated once: %p\r\n", d->port);
      driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 1);
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
  // fprintf(stderr, "Incomming connection\r\n");
  socklen_t sock_len;
  struct sockaddr_in client_addr;
  int fd = accept(d->socket, (struct sockaddr *)&client_addr, &sock_len);
  Emstcp* c = driver_alloc(sizeof(Emstcp));

  c->owner_pid = d->owner_pid;
  c->socket = fd;
  c->mode = CLIENT_MODE;

  ErlDrvPort client = driver_create_port(d->port, d->owner_pid, "microtcp_drv", (ErlDrvData)c);
  c->port = client;
  c->upper_limit = 10000000;
  c->lower_limit = 1000;
  c->paused_output = 0;
  set_port_control_flags(client, PORT_CONTROL_FLAG_BINARY);
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
    // fprintf(stderr, "Input in client: %p\r\n", d->port);
    driver_select(d->port, (ErlDrvEvent)d->socket, DO_READ, 0);
    ErlDrvBinary* bin = driver_alloc_binary(10240);
    size_t n = recv(d->socket, bin->orig_bytes, bin->orig_size, 0);
    if(n <= 0) {
      ErlDrvTermData reply[] = {
        ERL_DRV_ATOM, driver_mk_atom("tcp_closed"),
        ERL_DRV_PORT, driver_mk_port(d->port),
        ERL_DRV_TUPLE, 2
      };
      driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
      driver_exit(d->port, 0);
      return;
    }
    
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("tcp"),
      ERL_DRV_PORT, driver_mk_port(d->port),
      ERL_DRV_BINARY, bin, n, 0,
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
    return;
  }
  // 
  // if(errno != EAGAIN) {
  //   driver_failure_posix(d->port, errno);    
  // }
  
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
    NULL,			/* F_PTR timeout, reserved */
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
