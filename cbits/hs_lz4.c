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

// Started on streaming compression and then realized it has some
// restrictions that make it pretty hard to wrap in Haskell. Read
// the docs for LZ4_compress_fast_continue in lz4.h before trying
// to do this. Basically, if any attempt to do this is ever made to
// do this, the only way forward is to use UnliftedArray ByteArray
// and do the whole thing in one big uninterruptible FFI call since
// nothing is allowed to move. But even then, decompression speed
// will be negatively impacted since you might end up with a bunch
// of small LZ4 chunks. And once you've got more than one LZ4 chunk,
// you have to start thinking about framing too.
//
// HsInt hs_compress_fast_chunks_start
//   ( LZ4_stream_t* ctx
//   , const char* src
//   , HsInt soff
//   , char* dst
//   , HsInt doff
//   , HsInt slen
//   , HsInt dstCapacity
//   , HsInt acceleration
//   ) {
//   int result;
//   result = LZ4_compress_fast_extState(ctx, src+soff, dest+doff, slen, dstCapacity, acceleration);
//   return result;
// }
// 
// HsInt hs_compress_fast_chunks_continue
//   ( LZ4_stream_t* ctx
//   , const char* src
//   , HsInt soff
//   , char* dst
//   , HsInt doff
//   , HsInt slen
//   , HsInt dstCapacity
//   , HsInt acceleration
//   ) {
//   int result;
//   result = LZ4_compress_fast_continue(ctx, src+soff, dest+doff, slen, dstCapacity, acceleration);
//   return result;
// }

