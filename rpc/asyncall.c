
/*
 * - Clients write args into a cht entry.
 * - Then client appends a message to a log indicating: hostid, prog/vers/proc, arg key
 * - rpcd polls the log, when it sees a new entry it sends the message.
 * - When rpcd receives a reply, it writes the results back into the same entry. 
 * - clients need only poll the entry for a seqno bump, when it increments they know they have a result. 
 */

struct asyncall_msg {
  uint64_t hostid;
  uint32_t progid;
  uint32_t versid;
  uint32_t procid;
  char bufkey[CHT_KEY_SIZE];
};

static struct cht_s asyncall_cht;
static struct log_s asyncall_log;

static void asyncall_open( void ) {
  static int initialized = 0;
  int sts;
  
  if( !initialized ) {
    sts = log_open( mmf_default_path( "asyncall.log", NULL ), NULL, &asyncall_log );
    sts = cht_open( mmf_default_path( "asyncall.cht", NULL ), NULL, &asyncall_cht );
    initialized = 1;
  }
}


/* 
 * - generate cht key (random key)
 * - write argbuf into cht entry (readonly flag set)
 * - set handle to the cht entry seq 
 * - append msg to log 
 */
int asyncall_send( struct asyncall_msg *msg, char *argbuf, int argsize );

/* 
 * - lookup cht entry for given key. if entry missing error
 * - compare entry seq against handle. if seq = handle + 1 then we have a result. if seq > handle + 1 error. if seq < handle error. 
 * - read cht entry into resbuf. delete entry 
 * - success
 */
int asyncall_recv( char *key, char *resbuf, int ressize );


int asyncall_send( struct asyncall_msg *msg, char *argbuf, int argsize ) {
  struct cht_entry centry;
  memset( &centry, 0, sizeof(centry) );
  sec_rand( centry.key, sizeof(centry.key) );
  memcpy( msg->key, centry->key, CHT_KEY_SIZE );
  centry.flags = CHT_READONLY;  
  cht_write( &asyncall_cht, &centry, argbuf, argsize );

  log_write_buf( &asyncall_log, LOG_BINARY, msg, sizeof(*msg), NULL );
  return 0;
}

int asyncall_recv( char *key, char *resbuf, int ressize ) {
  struct cht_entry centry;
  sts = cht_read( &asyncall_cht, key, resbuf, ressize, &centry );  
  if( sts ) return -1;
  if( centry.seq > 1 ) {
    cht_delete( &asyncall_cht, key );
    return 0;
  }
  
  return -1;
}

/* --------- server side ------------ */

static void asyncall_donecb( struct xdr_s *res, void *cxt ) {
}

static void asyncall_iter_cb( struct rpc_iterator *iter ) {
  /* read entry from log */
  static uint64_t logid;

  int sts, ne;
  struct log_entry lentry;
  struct asyncall_msg msg;
  struct log_iov iov[1];

  while( 1 ) {
    memset( &lentry, 0, sizeof(lentry) );
    iov[0].buf = (char *)&msg;
    iov[0].len = sizeof(msg);
    lentry.iov = iov;
    lentry.niov = 1;
    sts = log_read( &asyncall_log, logid, &lentry, 1, &ne );
    if( sts || !ne ) break;

    logid = lentry.id;  
    if( lentry.msglen != sizeof(msg) ) continue;

    /* got a new entry, read its argbuf and send */
    sts = cht_read( &asyncall_cht, msg.key, argbuf, sizeof(argbuf), NULL );
    /* TODO */

    /* await a reply */
    hrauth_call_udp_async( &hcall, &args, NULL );
  }
}

static struct rpc_iterator asyncall_iter =
{
    NULL,
    0,
    1000,
    asyncall_iter_cb,
    NULL
};

int asyncall_register( void ) {
  rpc_iterator_register( &asyncall_iter );
}
