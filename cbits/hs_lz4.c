#include "Rts.h"
#include "lz4.h"

HsInt hs_compress_default
  ( const char* src
  , HsInt soff
  , char* dst
  , HsInt doff
  , HsInt slen
  , HsInt dstCapacity
  ) {
  HsInt r;
  r = (HsInt)(LZ4_compress_default(src + soff,dst + doff,slen,dstCapacity));
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
