#ifndef TAPE_H
#define TAPE_H

#include <jack/types.h>

typedef jack_nframes_t nframes_t;
typedef jack_default_audio_sample_t sample_t;

struct tape {
  // tape medium
  nframes_t tape_len;
  sample_t *samples;

  // loop splice position
  nframes_t start_pos;
  nframes_t fadein_start_pos;
  nframes_t fadein_end_pos;

  nframes_t fadeout_start_pos;
  nframes_t fadeout_end_pos;
  sample_t splice;
};

struct tape *create_tape(nframes_t);

void delete_tape(struct tape *);

void pos_play (sample_t *, struct tape *, int64_t, sample_t);

#endif
