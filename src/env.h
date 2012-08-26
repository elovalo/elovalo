/* It is assumed that X and Y form one layer on the LED cube. The
 * actual LED count may be smaller, of course.
 * 
 * SHIFT_REGISTER_BYTES tells how many bytes there are in shift
 * register of Z layer switcher.
 * 
 * If you are changing LED count or GS_DEPTH, make sure you have
 * compatible implementation of set_led() in effects/lib/utils.c */

#define LEDS_X 8
#define LEDS_Y 8
#define LEDS_Z 8
#define GS_DEPTH 12
#define BYTES_PER_LAYER 96
#define SHIFT_REGISTER_BYTES 1
