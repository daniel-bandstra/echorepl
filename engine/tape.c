#include "tape.h"
#include <stdlib.h>
#include <sys/mman.h>

struct tape *create_tape(nframes_t tape_len) {
  struct tape *output = calloc(1, sizeof(struct tape));
  mlock(output, sizeof(struct tape));
  
  output->tape_len = tape_len;
  output->samples = calloc(tape_len, sizeof(sample_t));
  mlock(output->samples, tape_len * sizeof(sample_t));

  return output;
}

void delete_tape(struct tape *tape) {
  munlock(tape->samples, tape->tape_len * sizeof(sample_t));
  free(tape->samples);
  munlock(tape, sizeof(struct tape));
  free(tape);
}

void pos_play (sample_t *dst, struct tape *src, int64_t pos, sample_t gain) {
  pos += src->start_pos;

  if (gain != 0.0) {
    if (pos < src->fadein_start_pos) {
    } else if (pos < src->fadein_end_pos) {
      *dst += (src->samples[pos] *
		      ((pos - src->fadein_start_pos) /
		       src->splice) *
		      gain);
    } else if (pos < src->fadeout_start_pos) {
      *dst += (src->samples[pos] *
		      gain);
    } else if (pos < src->fadeout_end_pos) {
      *dst += (src->samples[pos] *
		      ((src->fadeout_end_pos - pos) /
		       src->splice) *
		      gain);
    }
  }
}
