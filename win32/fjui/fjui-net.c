
#include "fjui.h"

void fjui_net_service( void ) {
	/* poll networking */
	rpc_service( 0 );
}
