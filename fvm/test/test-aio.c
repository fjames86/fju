
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <fju/rpc.h>
#include <fju/rpcd.h>
#include <fju/log.h>
#include <fju/fvm.h>
#include <fju/mmf.h>

static struct mmf_s gmmf;
static struct fvm_module mod;
static char gbuf[256];

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
  
  sts = rpcd_aio_read( &gmmf, gbuf, sizeof(gbuf), 0, read_donecb, NULL );
  log_writef( NULL, LOG_LVL_INFO, "testaio_read sts=%d", sts );

  res->offset = 0;
  res->count = 0;
  return 0;
}

static void write_donecb( struct mmf_s *mmf, char *buf, int nbytes, void *prv ) {
  log_writef( NULL, LOG_LVL_INFO, "testaio_write_donecb len=%d", nbytes );
}

static int testaio_write( struct xdr_s *args, struct xdr_s *res ) {
  int sts;
  int len;

  len = sizeof(gbuf);
  sts = xdr_decode_opaque( args, (uint8_t *)gbuf, &len );
  if( sts ) return sts;
  
  sts = rpcd_aio_write( &gmmf, gbuf, len, 0, write_donecb, NULL );
  log_writef( NULL, LOG_LVL_INFO, "testaio_write sts=%d len=%u", sts, len );

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
