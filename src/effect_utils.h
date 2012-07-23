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

#define MAX_INTENSITY ((LEDS_Z-1)*(1 << GS_DEPTH)-1)

/* 2D plotting function. Takes frame number, x coordinate, y
 * coordinate, and returns intensity value from 0 to
 * max_intensity. May have multiple definitions. */
typedef uint16_t(*plot_func_t)(uint8_t,uint8_t);

void set_led_8_8_12(uint8_t x, uint8_t y, uint8_t z, uint16_t i);

void effect_2d_plot(plot_func_t f); 
void effect_layers_tester(void);

void clear_buffer(void);

uint16_t ticks;
