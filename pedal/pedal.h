#ifndef PEDAL_H
#define PEDAL_H

#include <jack/jack.h>

struct pedal_event {
  char event;
  jack_time_t time;
};

int open_pedal(char *);
void close_pedal(int);

int pedal_color(int, int, int, int);

void read_pedal(int, struct pedal_event *);

#endif
