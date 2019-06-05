#include "pedal.h"
#include "pedal/pedal_flags.h"

#include <fcntl.h>
#include <termios.h>
#include <unistd.h>

int open_pedal(char *portname) {
  // open the terminal
  int fd = open(portname, O_RDWR | O_NOCTTY);
  if (fd < 0) {
    return fd;
  }
 
  // get the options already set
  struct termios options;
  tcgetattr(fd, &options);
 
  // set options for Arduino
 
  // speed
  cfsetispeed(&options, B9600);
  cfsetospeed(&options, B9600);
  options.c_cflag &= ~PARENB; // no parity bit
  options.c_cflag &= ~CSTOPB; // no stop bit
  options.c_cflag &= ~CSIZE;  // 8 bit bytes
  options.c_cflag |= CS8;
  options.c_cflag &= ~CRTSCTS; // no hardware flow control
  options.c_cflag |= CREAD | CLOCAL; // enable receiver, local line
  options.c_iflag &= ~(IXON | IXOFF | IXANY); // enable software flow control
  options.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG); // raw input
  options.c_oflag &= ~OPOST; // no output processing
 
  options.c_cc[VMIN] = 0;   // don't block if there's no data
  options.c_cc[VTIME] = 10; // but wait 1 second to see
 
  // commit new options
  tcsetattr(fd, TCSANOW, &options);
 
  tcflush(fd, TCIFLUSH); // flush the buffer
  
  return fd;
}

void close_pedal(int fd) {
  pedal_color(fd, 0, 0, 0);
  close(fd);
}

int pedal_color(int fd, int red, int green, int blue) {
  char buf[2];
  int error = 6;
  buf[0] = led_red;
  buf[1] = red;
  error -= write(fd, buf, 2);
  buf[0] = led_green;
  buf[1] = green;
  error -= write(fd, buf, 2);
  buf[0] = led_blue;
  buf[1] = blue;
  error -= write(fd, buf, 2);
  tcflush(fd, TCIFLUSH);
  return error;
}

void read_pedal(int fd, struct pedal_event *event) {
  char c;

  event->event = read(fd, &c, 1) ? c : nothing;
  event->time = jack_get_time();
}
