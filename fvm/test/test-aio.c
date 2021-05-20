
#ifdef WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/log.h>
#include <fju/fvm.h>
#include <fju/mmf.h>

static struct mmf_s gmmf;
static struct fvm_module mod;
static char gbuf[4096*2];

static int testaio_init( struct xdr_s *args, struct xdr_s *res ) {
  int sts;
  sts = mmf_open2( mmf_default_path( "aio.dat", NULL ), &gmmf, MMF_OPEN_ASYNC );
  mmf_write( &gmmf, "", 1, 255 );
  return 0;
}

static void read_donecb( struct mmf_s *mmf, char *buf, int nbytes, void *prv ) {
  log_writef( NULL, LOG_LVL_INFO, "testaio_read_donecb len=%d", nbytes );
}

static int testaio_read( struct xdr_s *args, struct xdr_s *res ) {
  int sts;
  char *gbufp;
  
  gbufp = gbuf;
  if( (uint64_t)gbufp % 4096 ) gbufp += 4096 - ((uint64_t)gbufp % 4096);
  sts = rpcd_aio_read( &gmmf, gbufp, 4096, 0, read_donecb, NULL );
  log_writef( NULL, LOG_LVL_INFO, "testaio_read sts=%d", sts );

  res->offset = 0;
  res->count = 0;
  return 0;
}

static void write_donecb( struct mmf_s *mmf, char *buf, int nbytes, void *prv ) {
  log_writef( NULL, LOG_LVL_INFO, "testaio_write_donecb len=%d now=%"PRIu64"", nbytes, rpc_now() );
}

static int testaio_write( struct xdr_s *args, struct xdr_s *res ) {
  int sts;
  int len;
  char *gbufp;

  gbufp = gbuf;
  if( (uint64_t)gbufp % 4096 ) gbufp += 4096 - ((uint64_t)gbufp % 4096);

  len = 4096;
  sts = xdr_decode_opaque( args, (uint8_t *)gbufp, &len );
  if( sts ) return sts;
  if( len < 4096 ) memset( gbufp + len, 0, 4096 - len );

  sts = rpcd_aio_write( &gmmf, gbufp, 4096, 0, write_donecb, NULL );
  log_writef( NULL, LOG_LVL_INFO, "testaio_write sts=%d len=%u now=%"PRIu64"", sts, len, rpc_now() );

  res->offset = 0;
  res->count = 0;
  return 0;
}


void testaio_register( void ) {
  strcpy( mod.name, "TestAIO" );
  FVM_MODULE_ADDPROC(mod,"Init",0,testaio_init);
  FVM_MODULE_ADDPROC(mod,"Read",0,testaio_read);
  FVM_MODULE_ADDPROC(mod,"Write",0x400000000000010LL,testaio_write);
  
  mod.timestamp = time( NULL );
  mod.flags = FVM_MODULE_STATIC|FVM_MODULE_NATIVE;
  fvm_module_register( &mod );
  
}
