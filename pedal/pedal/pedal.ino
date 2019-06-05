#include "pedal_flags.h"

#define UP HIGH
#define DOWN LOW

// Button

const int a_button_pin = 2;
const int b_button_pin = 3;

const unsigned long debounce_delay = 10;
const unsigned long long_time = 1000; // timing a long tap
const unsigned long double_time = 500; // timing a double tap

int a_button_state = UP;
int a_reading;
int a_last_reading = UP;
unsigned long a_debounce_time = 0;
unsigned long a_down_time;

int b_button_state = UP;
int b_reading;
int b_last_reading = UP;
unsigned long b_debounce_time = 0;
unsigned long b_down_time;

// LED

const int red_pin = 11;
const int green_pin = 10;
const int blue_pin = 9;

void red(int value) {analogWrite(red_pin, 255 - value);}
void green(int value) {analogWrite(green_pin, 255 - value);}
void blue(int value) {analogWrite(blue_pin, 255 - value);}

void setup() {
  pinMode(a_button_pin, INPUT_PULLUP);
  pinMode(b_button_pin, INPUT_PULLUP);

  pinMode(red_pin, OUTPUT);
  pinMode(green_pin, OUTPUT);
  pinMode(blue_pin, OUTPUT);

  red(0);
  green(0);
  blue(0);
  
  //start the serial interface
  Serial.begin(9600);
}

void loop() {
  unsigned long now = millis();
  
  // Button A
  a_reading = digitalRead(a_button_pin);

  if (a_reading != a_last_reading) {
    a_debounce_time = now;
    a_last_reading = a_reading;
  } else if (((now - a_debounce_time) > debounce_delay) && (a_reading != a_button_state)) {
    a_button_state = a_reading;
  
    if (a_button_state == DOWN) {
      Serial.write(a_down);
      if ((now - a_down_time) < double_time) {
	Serial.write(a_double_tap);
      }
      a_down_time = now;
    } else {
      Serial.write(a_up);
    }
  }
  if ((a_button_state == DOWN) && ((now - a_down_time) > long_time)) {
    Serial.write(a_long_tap);
    a_down_time = now;
  }
  
  // Button B
  b_reading = digitalRead(b_button_pin);

  if (b_reading != b_last_reading) {
    b_debounce_time = now;
    b_last_reading = b_reading;
  } else if (((now - b_debounce_time) > debounce_delay) && (b_reading != b_button_state)) {
    b_button_state = b_reading;
  
    if (b_button_state == DOWN) {
      Serial.write(b_down);
      if ((now - b_down_time) < double_time) {
	Serial.write(b_double_tap);
      }
      b_down_time = now;
    } else {
      Serial.write(b_up);
    }
  }
  if ((b_button_state == DOWN) && ((now - b_down_time) > long_time)) {
    Serial.write(b_long_tap);
    b_down_time = now;
  }

  // Serial Read
  if (Serial.available() > 0) {
    char c = Serial.read();
    switch (c) {
    case led_red:
      while (Serial.available() < 1) {}
      red(Serial.read());
      break;
    case led_green:
      while (Serial.available() < 1) {}
      green(Serial.read());
      break;
    case led_blue:
      while (Serial.available() < 1) {}
      blue(Serial.read());
      break;
    default:
      break;
    }
  }
}
