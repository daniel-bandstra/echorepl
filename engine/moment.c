#include "moment.h"

#include <math.h>

// moment functions

moment moment_plus (moment a, moment b) {
  moment result;
  double frame_add;
  float fraction_add = a.fraction + b.fraction;
  result.fraction = modf(fraction_add, &frame_add);
  result.frame = a.frame + b.frame + frame_add;
  return result;
}

moment moment_minus (moment a, moment b) {
  moment result;
  double frame_add;
  float fraction_add = a.fraction - b.fraction;
  result.fraction = modf(fraction_add, &frame_add);
  result.frame = (a.frame - b.frame) + frame_add;
  
  if (result.fraction < 0.0) {
    result.fraction += 1;
    result.frame -= 1;
  }
  
  return result;
}

int moment_less_than (moment a, moment b) {
  return (a.frame < b.frame) ||
    ((a.frame == b.frame) && (a.fraction < b.fraction));
}
