#ifndef EFFECT_UTILS_H
#define EFFECT_UTILS_H

#include "../../env.h"

/* Defining set_led() as a macro which chooses the most efficient
 * implementation available */
#if LEDS_X == 8 && LEDS_Y == 8 && GS_DEPTH == 12 && BYTES_PER_LAYER == 96
#define set_led(x,y,z,i) set_led_8_8_12(x,y,z,i)
#define get_led(x,y,z) get_led_8_8_12(x,y,z)
#else
#error "There is no set_led() implementation for this geometry"
#endif

/* Maximum intensity returned from the 2D plotting function */
#define MAX_2D_PLOT_INTENSITY ((LEDS_Z-1)*(1 << GS_DEPTH)-1)

/* Generates wrapper function for two dimensional plots to make the
 * implementations much simpler */
#define XZ(wrap)						\
  static uint16_t wrap##_kernel(uint8_t x, uint8_t y);		\
  static void wrap(void){effect_2d_plot(&wrap##_kernel);}	\
  static uint16_t wrap##_kernel(uint8_t x, uint8_t y)

/* Generates wrapper function for three dimensional plots to make
 * the implementations much simpler */
#define XYZ(wrap)						\
  static void wrap##_kernel(uint8_t x, uint8_t y, uint8_t z);		\
  static void wrap(void){iterate_3d(&wrap##_kernel);}	\
  static void wrap##_kernel(uint8_t x, uint8_t y, uint8_t z)

/* 2D plotting function. Takes frame number, x coordinate, y
 * coordinate, and returns intensity value from 0 to
 * max_intensity. May have multiple definitions. */
typedef uint16_t(*plot_2d_t)(uint8_t,uint8_t);

/* Arbitary drawing function. The implementation is responsible to set
 * pixels by itself. Buffer flipping is done outside this function. */
typedef void(*draw_t)(void);

/* Effect initializator type. The initializator is run before any
 * drawing is done on that effect. */
typedef void(*init_t)(void);

/* This structure holds information about the effects and how to draw
 * them. */
typedef struct {
	const char *name;     // Name for effect. Used in file dumps.
	init_t init;          // Initializatior, may be NULL.
	draw_t draw;          // Drawing function, run once per buffer swap.
	uint8_t flip_buffers; // Flip buffers during execution.
} effect_t;

#define NO_FLIP 0
#define FLIP 1

typedef struct {
	uint16_t debug_value; // Settable via serial port only.
} sensors_t;

// XXX: might want to replace flipBuffers with a set of bitfields
// if more flags are needed

void set_row(uint8_t x, uint8_t z, uint8_t y1, uint8_t y2, uint16_t intensity);

void set_led_8_8_12(uint8_t x, uint8_t y, uint8_t z, uint16_t i);

uint16_t get_led_8_8_12(uint8_t x, uint8_t y, uint8_t z);

uint16_t get_led_wrap(int8_t x, int8_t y, int8_t z);

void effect_2d_plot(plot_2d_t f);

typedef void(*iterate_3d_t)(uint8_t,uint8_t,uint8_t);
void iterate_3d(iterate_3d_t f);

void clear_buffer(void);

uint8_t randint(uint8_t min, uint8_t max);
uint8_t clamp(uint8_t a, uint8_t min, uint8_t max);

extern uint16_t ticks;
extern sensors_t sensors;

#define MAX_INTENSITY ((1<<GS_DEPTH)-1)

#endif // EFFECT_UTILS_H
