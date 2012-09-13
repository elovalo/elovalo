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
#define XY(wrap)						\
  static void wrap##_kernel(uint8_t x, uint8_t y);		\
  static void wrap(void){clear_buffer();iterate_xy(&wrap##_kernel);}	\
  static void wrap##_kernel(uint8_t x, uint8_t y)

/* Generates wrapper function for three dimensional plots to make
 * the implementations much simpler */
#define XYZ(wrap)						\
  static void wrap##_kernel(uint8_t x, uint8_t y, uint8_t z);		\
  static void wrap(void){clear_buffer();iterate_xyz(&wrap##_kernel);}	\
  static void wrap##_kernel(uint8_t x, uint8_t y, uint8_t z)

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
	uint16_t debug_value; // Settable via serial port only. TODO: to be removed
	uint8_t distance1;
	uint8_t distance2;
	uint8_t ambient_light;
	uint8_t sound_pressure_level;
} sensors_t;

// XXX: might want to replace flipBuffers with a set of bitfields
// if more flags are needed

void set_row(uint8_t x, uint8_t z, uint8_t y1, uint8_t y2, uint16_t intensity);

void set_z(uint8_t x, uint8_t y, uint16_t intensity);

void set_led_8_8_12(uint8_t x, uint8_t y, uint8_t z, uint16_t i);

uint16_t get_led_8_8_12(uint8_t x, uint8_t y, uint8_t z);

uint16_t get_led_wrap(int8_t x, int8_t y, int8_t z);

typedef void(*iterate_xy_t)(uint8_t,uint8_t);
void iterate_xy(iterate_xy_t f);

typedef void(*iterate_xyz_t)(uint8_t,uint8_t,uint8_t);
void iterate_xyz(iterate_xyz_t f);

void clear_buffer(void);

uint8_t randint(uint8_t min, uint8_t max);
uint8_t clamp(uint8_t a, uint8_t min, uint8_t max);

extern uint16_t ticks;
extern sensors_t sensors;

#define MAX_INTENSITY ((1<<GS_DEPTH)-1)

// math utils from
// ftp://ftp.isc.org/pub/usenet/comp.sources.unix/volume26/line3d
#define MAX(a,b) (((a)>(b))?(a):(b))
#define ABS(a) (((a)<0) ? -(a) : (a))

/* take sign of a, either -1, 0, or 1 */
#define ZSGN(a) (((a)<0) ? -1 : (a)>0 ? 1 : 0)

#endif // EFFECT_UTILS_H
