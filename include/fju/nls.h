/*
 * MIT License
 * 
 * Copyright (c) 2019 Frank James
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
*/
 
/*
 * This file was generated by cfgen.lisp

(CFGEN:GEN ("nls") NIL (("share" (("name" :STRING 64) ("hshare" :UINT64 NIL)))))

 *
 */

#ifndef NLS_H
#define NLS_H

#include <stdint.h>
#include <fju/log.h>

#define NLS_MAX_COOKIE 16

/* local log shared over RPC */
struct nls_share {
    uint64_t hshare;
    char path[256];
};

/* remote shared log - spooled reads */
struct nls_remote {
  uint64_t hostid;        /* remote host id */
  uint64_t hshare;        /* shared log descriptor */
  uint64_t seq;           /* last known log seqno and message id */
  uint64_t lastid;
  uint64_t timestamp;     /* when to next send a notreg call */
  uint64_t last_contact;  /* when last heard from server */
  uint32_t notify_period;
};

/* notification context - who to send a message to if local share gets updated */
struct nls_notify {
  uint64_t tag;
  uint64_t hostid;
  uint64_t hshare;
  uint64_t seq;
  uint64_t lastid;
  uint64_t timestamp;        /* when to next check local seqno */
  uint8_t cookie[NLS_MAX_COOKIE];
  uint32_t period;
};

/* database properties */
struct nls_prop {
    uint32_t version;
#define NLS_VERSION 1
    uint64_t seq;
    uint32_t share_max;
    uint32_t share_count;
    uint32_t remote_max;
    uint32_t remote_count;
    uint32_t notify_max;
    uint32_t notify_count;
    uint32_t rpc_timeout;    /* milliseconds to wait before rpc timeout */
    uint32_t notreg_period;   /* seconds between notreg calls */
    uint32_t notify_period;   /* seconds to wait before polling seq to send notifications */
};

int nls_open( void );
int nls_close( void );
int nls_prop( struct nls_prop *prop );
int nls_reset( void );
int nls_set_rpc_timeout( uint32_t timeout );
int nls_set_notreg_period( uint32_t timeout );
int nls_set_notify_period( uint32_t timeout );

int nls_share_list( struct nls_share *list, int n );
int nls_share_by_hshare( uint64_t hshare, struct nls_share *share );
int nls_share_add( struct nls_share *entry );
int nls_share_rem( uint64_t hshare );
int nls_share_set( struct nls_share *entry );
int nls_share_open( struct nls_share *share, struct log_s *log );

int nls_remote_list( struct nls_remote *list, int n );
int nls_remote_by_hshare( uint64_t hshare, struct nls_remote *remote );
int nls_remote_add( struct nls_remote *entry );
int nls_remote_rem( uint64_t hshare );
int nls_remote_set( struct nls_remote *entry );
int nls_remote_open( struct nls_remote *remote, struct log_s *log );

int nls_notify_list( struct nls_notify *list, int n );
int nls_notify_by_tag( uint64_t tag, struct nls_notify *notify );
int nls_notify_by_hshare( uint64_t hostid, uint64_t hshare, struct nls_notify *notify );
int nls_notify_add( struct nls_notify *entry );
int nls_notify_rem( uint64_t tag );
int nls_notify_set( struct nls_notify *entry );


#define NLS_RPC_PROG 0x27E1FAEE
#define NLS_RPC_VERS 1
void nls_register( void );

#define NLS_EVENT_REMOTEAPPEND 0   /* remote log had some messages appened parm=&remote */

#endif

