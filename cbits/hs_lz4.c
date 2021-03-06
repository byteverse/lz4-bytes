#include "Rts.h"
#include "lz4.h"
#include "lz4hc.h"

HsInt hs_compress_fast
  ( const char* src
  , HsInt soff
  , char* dst
  , HsInt doff
  , HsInt slen
  , HsInt dstCapacity
  , HsInt acceleration
  ) {
  HsInt r;
  r = (HsInt)(LZ4_compress_fast(src + soff,dst + doff,slen,dstCapacity,(int)acceleration));
  return r;
}

HsInt hs_compress_HC
  ( const char* src
  , HsInt soff
  , char* dst
  , HsInt doff
  , HsInt slen
  , HsInt dstCapacity
  , HsInt level
  ) {
  HsInt r;
  r = (HsInt)(LZ4_compress_HC(src + soff,dst + doff,slen,dstCapacity,(int)level));
  return r;
}

HsInt hs_decompress_safe
  ( const char* src
  , HsInt soff
  , char* dst
  , HsInt doff
  , HsInt slen
  , HsInt dstCapacity
  ) {
    HsInt r;
    r = (HsInt)LZ4_decompress_safe(src + soff, dst + doff, slen, dstCapacity);
    return r;
  }
