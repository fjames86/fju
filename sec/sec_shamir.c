
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include <fju/sec.h>

#define SHAMIR_PRIME      283
#define SHAMIR_MAX_K      32

/* Add two polynomials */
static uint8_t p_add( uint8_t a, uint8_t b ) {
  return a ^ b;
}

/* Multiply a polynomial by x */
static uint8_t mul_x( uint8_t a ) {
  if( (a >> 7) & 0x1 ) {
    return (a << 1) ^ SHAMIR_PRIME;
  } else {
    return (a << 1);
  }
}

static uint8_t mul_x_power( uint8_t a, uint8_t x_power ) {
  uint8_t res = a;
  while( x_power > 0 ) {
    res = mul_x( res );
    x_power--;
  }
  return res;
}

/* Multiply two polynomials in GF(2^8) */
static uint8_t p_mul( uint8_t a, uint8_t b ) {
  uint8_t res = 0;
  int degree;
  
  for( degree = 7; degree >= 0; degree-- ) {
    if( (b >> degree) & 0x1 ) {
      res = p_add( res, mul_x_power(a, degree) );
    }
  }
  return res;
}

static uint8_t p_inv( uint8_t a ) {
  static uint8_t mul_inv_tab[256][256];
  static int initialized = 0;
  
  /* Build the table so that table[a][1] = inv(a) */
  if( !initialized ) {
    int row, col;
    for( row = 0; row < 256; row++ ) {
      for( col = 0; col < 256; col++ ) {
        mul_inv_tab[row][p_mul(row, col)] = col;
      }
    }

    initialized = 1;
  }

  return mul_inv_tab[a][1];
}

/* Divide two polynomials in GF(2^8) */
static uint8_t p_div( uint8_t a, uint8_t b ) {
  return p_mul( a, p_inv( b ) );
}

static uint8_t rand_byte( void ) {
  return sec_rand_uint32() % 0xff;
}

/* poly must be an array of length degree+1 */
static uint8_t *make_random_poly( uint8_t *poly, int degree, uint8_t secret ) {
  int i;
  for( i = 1; i < degree + 1; i++ ) {
    poly[i] = rand_byte();
  }
  poly[0] = secret;
  return poly;
}

static uint8_t poly_eval( uint8_t *poly, int degree, uint8_t x ) {
  uint8_t res = 0;
  uint8_t coeff, term;
  int times;
  
  for (; degree >= 0; degree--) {
    coeff = poly[degree];
    term = 0x01;
    for (times = degree; times > 0; times--) {
      term = p_mul( term, x );
    }
    res = p_add( res, p_mul(coeff, term) );
  }
  return res;
}

/* Interpolate a (k-1) degree polynomial and evaluate it at x = 0 */
static uint8_t poly_interpolate( uint8_t *xs, uint8_t *ys, int k ) {
  uint8_t res = 0;
  uint8_t prod;
  int j, m;
  
  for( j = 0; j < k; j++ ) {
    prod = 0x01;
    for( m = 0; m < k; m++ ) {
      if( m != j ) {
        prod = p_mul( prod, p_div( xs[m], p_add( xs[m], xs[j] ) ) );
      }
    }
    res = p_add( res, p_mul( ys[j], prod ) );
  }
  return res;
}

/* generate shares */
int sec_shamir_split( uint8_t *secret, int secretlen, struct sec_shamir_share *share, int nshares, int k ) {
  int i, secret_idx;
  uint8_t poly[SHAMIR_MAX_K];

  if( k > SHAMIR_MAX_K ) return -1;
  
  for( i = 0; i < nshares; i++ ) {
    /* get or generate xval */
    share[i].sharebuf[0] = share[i].flags & SEC_SHAMIR_XVAL ? share[i].xval : rand_byte();
    share[i].xval = share[i].sharebuf[0];
  }

  for( secret_idx = 0; secret_idx < secretlen; secret_idx++ ) {
    make_random_poly( poly, k - 1, secret[secret_idx] );

    for( i = 0; i < nshares; i++ ) {
      share[i].sharebuf[secret_idx + 1] = poly_eval( poly, k - 1, share[i].sharebuf[0] );
    }
  }

  return 0;
}

/* merge shares to reform secret */
int sec_shamir_join( uint8_t *secret, int secretlen, struct sec_shamir_share *share, int nshares ) {
  int secret_idx, i, k;
  uint8_t xs[SHAMIR_MAX_K], ys[SHAMIR_MAX_K];

  if( nshares > SHAMIR_MAX_K ) return -1;
  k = nshares;

  for( secret_idx = 1; secret_idx <= secretlen; secret_idx++ ) {
    for( i = 0; i < k; i++ ) {
      xs[i] = share[i].sharebuf[0];
      ys[i] = share[i].sharebuf[secret_idx];

      secret[secret_idx-1] = poly_interpolate( xs, ys, k );
    }
  }

  return 0;
}

