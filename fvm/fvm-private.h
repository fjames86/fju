
#ifndef FVM_PRIVATE_H
#define FVM_PRIVATE_H

int fvm_write_mem( struct fvm_state *fvm, char *buf, int len, int offset );
void fvm_push_value( struct fvm_state *fvm, uint16_t val );

#endif

