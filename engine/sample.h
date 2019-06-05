#ifndef SAMPLE_H
#define SAMPLE_H

#include <jack/types.h>

typedef jack_default_audio_sample_t sample_t;
typedef jack_nframes_t nframes_t;


void sample_add(sample_t *, nframes_t, sample_t);

sample_t sample_read(sample_t *, nframes_t);

void sample_write(sample_t *, nframes_t, sample_t);

#endif
