
#include <fju/raft.h>
#include <fju/log.h>
#include <inttypes.h>

/*
 * Example raft application.
 * - Create a cluster with appid=REX_APPID
 * - call rex_register() during initialization to register the application
 * - Issue a command by calling rpclt raft.command clid=xxx command=...base64...
 * - The rex_command callback should then be called on both sides of the cluster.
 */

#define REX_APPID 10000

static void rex_command( struct raft_app *app, struct raft_cluster *cl, uint64_t cmdseq, char *buf, int len ) {
  log_writef( NULL, LOG_LVL_INFO, "rex_command clid=%"PRIx64" seq=%"PRIu64" len=%u", cl->clid, cmdseq, len );
}

static void rex_snapsave( struct raft_app *app, struct raft_cluster *cl, uint64_t term, uint64_t seq ) {
  log_writef( NULL, LOG_LVL_INFO, "rex_snapshot clid=%"PRIx64" term=%"PRIu64" seq=%"PRIu64"", cl->clid, term, seq );

  raft_snapshot_save( cl->clid, term, seq, 0, "hello from rex", 15 );
  raft_snapshot_save( cl->clid, term, seq, 15, NULL, 0 );
}

static void rex_snapload( struct raft_app *app, struct raft_cluster *cl, char *buf, int len ) {
  log_writef( NULL, LOG_LVL_INFO, "rex_snapshot_load clid=%"PRIx64" len=%u", cl->clid, len );
}

static struct raft_app rex_app =
  {
   NULL,
   REX_APPID,
   rex_command,
   rex_snapsave,
   rex_snapload
  };

void rex_register( void ) {
  raft_app_register( &rex_app );
}

