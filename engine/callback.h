#ifndef CALLBACK_H
#define CALLBACK_H

#include <jack/jack.h>
#include <jack/ringbuffer.h>
#include <pthread.h>
#include <stdint.h>

typedef jack_default_audio_sample_t sample_t;
typedef jack_nframes_t nframes_t;

typedef struct callback_info_ {
  int monitor; // whether Jack should do software monitoring

  jack_client_t *client;

  jack_port_t *in_port;
  jack_port_t *out_port;
  
  nframes_t buffer_size;
  
  jack_ringbuffer_t *in_buf;
  jack_ringbuffer_t *frames_buf;
  
  sample_t *out_buf;
  
  pthread_mutex_t lock;
  pthread_cond_t cond;
} callback_info;

callback_info *new_callback_info(size_t);
void delete_callback_info(callback_info *);

int echorepl_callback(nframes_t, void *);

// functions for lisp to read/write

nframes_t get_sample_frame(callback_info *); // this is blocking, call it first
sample_t get_sample(callback_info *);
sample_t *get_out_pointer(callback_info *, nframes_t);
#endif
