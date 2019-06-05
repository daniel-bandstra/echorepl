#include "sample.h"

void sample_add(sample_t *dst, nframes_t offset, sample_t sample) {
  dst[offset] += sample;
}


sample_t sample_read(sample_t *src, nframes_t offset) {
  return src[offset];
}

void sample_write(sample_t *dst, nframes_t offset, sample_t sample) {
  dst[offset] = sample;
}
