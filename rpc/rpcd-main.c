
#include "rpcd.h"
#include "shauth.h"
#include <hrauth.h>

#define SHAUTH_SECRET "123abcd123"

static void init_cb( void ) {
  /* register rpc programs */
  rpcbind_register();

  /* register providers */
  {
    uint8_t key[32];
    int i;
    char *p, *terminator;
    char tmp[4];
    
    memset( key, 0, sizeof(key) );
    p = SHAUTH_SECRET;
    for( i = 0; i < 32; i++ ) {
      memset( tmp, 0, 4 );
      if( *p ) {
	tmp[0] = *p;
	p++;
      }
      if( *p ) {
	tmp[1] = *p;
	p++;
      }
      
      key[i] = (uint8_t)strtoul( tmp, &terminator, 16 );
      if( *terminator ) break;
      
      if( !*p ) break;
    }
    shauth_register( key );
  }

  hrauth_register();

}

int main( int argc, char **argv ) {

  /* run daemon */
  rpcd_main( argc, argv, init_cb );
  
  return 0;
}

