#include "Rts.h"
#include "lz4.h"
#include "lz4frame.h"
#include "lz4hc.h"
#include <string.h>

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

// This implementation was copied adapted from the Python lz4.frame subpackage.
// This returns a zero on success. Here are the main changes made to the Python
// code:
// * This function is only called in contexts where the size of the decompressed
//   data is communicated some other way, so we never need to grow the destination.
// * full_frame is removed because it is always true
// * This uses the unsafe FFI, so we can set stableDst to true unconditionally
// * We get rid of return_bytearray and return_bytes_read because we
//   only return an indicator of whether or not the exact number of
//   expected bytes were decompressed.
HsInt hs_decompress_frame(LZ4F_dctx * context, const char* const source, HsInt const source_off, HsInt const source_size, char* const destination, HsInt const decompressed_length)
{
  size_t source_remain = source_size;
  size_t source_read;
  const char * source_cursor;
  const char * source_end;
  char * destination_cursor;
  size_t destination_written;
  size_t result = 0;
  LZ4F_frameInfo_t frame_info;
  LZ4F_decompressOptions_t options = {0};
  int end_of_frame = 0;
  size_t destination_write = decompressed_length;

  options.stableDst = 1;
  options.skipChecksums = 1;

  source_cursor = source + source_off;
  source_end = source + source_off + source_size;
  source_remain = source_size;
  source_read = source_size;

  result = LZ4F_getFrameInfo (context, &frame_info, source_cursor, &source_read);
  if (LZ4F_isError (result)) { return result; }

  /* Advance the source_cursor pointer past the header - the call to
     getFrameInfo above replaces the passed source_read value with the
     number of bytes read. Also reduce source_remain accordingly. */
  source_cursor += source_read;
  source_remain -= source_read;

  /* If the uncompressed content size is available, we'll use that to size
     the destination buffer. Otherwise, guess at twice the remaining source
     source as a starting point, and adjust if needed. */
  if (frame_info.contentSize > 0)
  {
    if (frame_info.contentSize != decompressed_length) {
      return 20000;
    }
  }

  source_read = source_remain;

  destination_cursor = destination;
  destination_written = 0;

  while (1)
    {
      /* Decompress from the source string and write to the destination
         until there's no more source string to read, or until we've reached the
         frame end.

         On calling LZ4F_decompress, source_read is set to the remaining length
         of source available to read. On return, source_read is set to the
         actual number of bytes read from source, which may be less than
         available. NB: LZ4F_decompress does not explicitly fail on empty input.

         On calling LZ4F_decompress, destination_write is the number of bytes in
         destination available for writing. On exit, destination_write is set to
         the actual number of bytes written to destination. */
      result = LZ4F_decompress (context, destination_cursor, &destination_write, source_cursor, &source_read, &options);

      if (LZ4F_isError (result)) { return result; }

      destination_written += destination_write;
      source_cursor += source_read;
      source_read = source_end - source_cursor;

      if (result == 0) {
        end_of_frame = 1;
        break;
      }
      else if (source_cursor == source_end) { break; }
      else if (destination_written == decompressed_length) { break; }
      destination_cursor = destination + destination_written;
      destination_write = decompressed_length - destination_written;
    }

  if (result > 0) { return 20001; }
  if (end_of_frame != 1) { return 20002; }
  if (LZ4F_isError (result)) { return 20003; }

  return 0;
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
