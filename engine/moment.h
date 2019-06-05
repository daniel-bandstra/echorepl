#ifndef MOMENT_H
#define MOMENT_H

// moment type and functions

typedef struct moment_ {
  long long int frame;
  float fraction;
} moment;

moment moment_plus (moment, moment);

moment moment_minus (moment, moment);

int moment_less_than (moment, moment);

#endif
