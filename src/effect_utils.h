/* If you are changing LED count make sure you are not using set_led
     which is optimized to work only 12-bit depths and when y and z
     dimensions have length of 8. */

#include "env.h"

/* Defining set_led() as a macro which chooses the most efficient
 * implementation available */
#if LEDS_Y == 8 && LEDS_Z == 8 && GS_DEPTH == 12
#define set_led(x,y,z,i) set_led_8_8_12(x,y,z,i)
#else
#error "There is no set_led() implementation for this geometry"
#endif

/* Maximum intensity returned from the 2D plotting function */
#define MAX_INTENSITY ((LEDS_Z-1)*(1 << GS_DEPTH)-1)

/* Generates wrapper function for two-dimensional plots to make the
 * implementations much simpler */
#define TWOD(wrap)                                 \
  uint16_t wrap##_kernel(uint8_t x, uint8_t y);	   \
  void wrap(void){effect_2d_plot(&wrap##_kernel);} \
  uint16_t wrap##_kernel(uint8_t x, uint8_t y)

/* 2D plotting function. Takes frame number, x coordinate, y
 * coordinate, and returns intensity value from 0 to
 * max_intensity. May have multiple definitions. */
typedef uint16_t(*plot_2d_t)(uint8_t,uint8_t);

/* Arbitary plotting function. The implementation is responsible to
 * set pixels by itself. Buffer flipping is done outside this
 * function. */
typedef void(*draw_t)(void);

/* Effect initializator type. The initializator is run after an effect
 * is started. */
typedef void(*init_t)(void);

void set_led_8_8_12(uint8_t x, uint8_t y, uint8_t z, uint16_t i);

void effect_2d_plot(plot_2d_t f); 

void clear_buffer(void);

uint16_t ticks;
