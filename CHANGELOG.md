# Revision history for lz4-bytes

## 0.2.0.0 -- 2025-07-07

* Make it possible to compress much larger buffers as a frame
* Correctly handle frames with multiple blocks during decompression

## 0.1.2.0 -- 2025-03-26

* Add `Lz4.Frame.decompressU`.

## 0.1.1.1 -- 2024-01-31

* Update package metadata.

## 0.1.1.0 -- 2023-07-27

* Add `Lz4.Frame` module for producing LZ4 frames. This is being added
  because the Apache Arrow format uses LZ4 frames as one of its compression
  methods.

## 0.1.0.2 -- 2020-03-09

* Add `lz4hc.h` to `extra-source-files`.

## 0.1.0.1 -- 2020-03-09

* Add `lz4.h` to `extra-source-files`.

## 0.1.0.0 -- 2020-01-31

* First version. Released on an unsuspecting world.
