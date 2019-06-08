#include "callback.h"

#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

callback_info *new_callback_info(size_t buffer_size) {
  callback_info *result = calloc(1, sizeof(callback_info));
  mlock(result, sizeof(callback_info));
  
  result->buffer_size = buffer_size;
  
  result->in_buf = jack_ringbuffer_create(buffer_size * sizeof(sample_t));
  jack_ringbuffer_mlock(result->in_buf);
  
  result->frames_buf = jack_ringbuffer_create(buffer_size * sizeof(nframes_t));
  jack_ringbuffer_mlock(result->frames_buf);
  
  result->out_buf = calloc(buffer_size, sizeof(sample_t));
  mlock(result->out_buf, buffer_size * sizeof(sample_t));
  
  pthread_mutex_init(&result->lock, NULL);
  pthread_cond_init(&result->cond, NULL);
  return result;
}


void delete_callback_info(callback_info *container) {
  pthread_mutex_destroy(&container->lock);
  pthread_cond_destroy(&container->cond);
  
  jack_ringbuffer_free(container->in_buf);
  jack_ringbuffer_free(container->frames_buf);

  munlock(container->out_buf, container->buffer_size * sizeof(sample_t));
  free(container->out_buf);
  
  munlock(container, sizeof(callback_info));
  free(container);
}

int echorepl_callback(nframes_t count, void *info_ptr) {
  sample_t *in, *out;
  callback_info *info = (callback_info *)info_ptr;
  nframes_t time = jack_last_frame_time(info->client);

  in = jack_port_get_buffer(info->in_port, count);
  out = jack_port_get_buffer(info->out_port, count);

  if (info->monitor) {
    memcpy(out, in, count * sizeof(sample_t));
  } else {
    memset(out, 0, count * sizeof(sample_t));
  }

  jack_ringbuffer_write(info->in_buf,
			(char *)in,
			count * sizeof(sample_t));

  for (nframes_t i = 0; i < count; i++) {
    jack_ringbuffer_write(info->frames_buf, (char *)&time, sizeof(nframes_t));
    info->out_buf[time % info->buffer_size] = 0.0;
    out[i] += info->out_buf[(time + 1) % info->buffer_size];
    time++;
  }

  pthread_cond_broadcast(&info->cond);

  return 0;
}

// functions for lisp to read/write

nframes_t get_sample_frame(callback_info *info) { // this is blocking, call it first
  nframes_t time;

  pthread_mutex_lock(&info->lock);

  while(jack_ringbuffer_read(info->frames_buf,
			     (char *)&time,
			     sizeof(nframes_t)) == 0) {
    pthread_cond_wait(&info->cond, &info->lock);
  }

  pthread_mutex_unlock(&info->lock);
  
  return time;
}

sample_t get_sample(callback_info *info) {
  sample_t sample;

  jack_ringbuffer_read(info->in_buf, (char *)&sample, sizeof(sample_t));

  return sample;
}
