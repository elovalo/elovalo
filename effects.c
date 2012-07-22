/**
 * Led cube effects
 *
 * Not integrated to the main code, yet.
 */

// Disable assertions on embedded environment
#ifdef AVR
#define NDEBUG
#endif

#include <assert.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include "main.h"
#include "effects.h"

/* If you are changing LED count make sure you are not using set_led
   which is optimized to work only 12-bit depths and when y and z
   dimensinons have length of 8. */

// Number of LEDs and grayscale intensity depth of LED driver
#define LEDS_X 8
#define LEDS_Y 8
#define LEDS_Z 8
#define GS_DEPTH 12

// TODO add timer which increments ticks_volatile!

/* ticks is set to ticks_volatile every time when frame calculation is
 * started. This keeps ticks stable and removes tearing. */
uint16_t ticks = 0; 

/* ticks_volatile is incremented roughly every 1 millisecond and
 * overflows every 64th second. */
volatile uint16_t ticks_volatile = 0;

/* Some constants based on defines set above. +7 >> 3 trick in
   buffer_len is ceiling function, which makes the buffer size to
   round towards next full byte. */
const uint16_t max_intensity = (LEDS_Z-1)*(1 << GS_DEPTH)-1;
const uint16_t buffer_len = (LEDS_X * LEDS_Y * LEDS_Z * GS_DEPTH + 7) >> 3;

/**
 * Sets led intensity. i is the intensity of the LED in range 0..4095.
 */
void set_led(uint8_t x, uint8_t y, uint8_t z, uint16_t i)
{
	/* Assert (on testing environment) that we supply correct
	 * data. */
	assert(x < LEDS_X);
	assert(y < LEDS_Y);
	assert(z < LEDS_Z);
	assert(i < (1 << GS_DEPTH));
	
	/* This function is hard-wired to certain cube geometry to
	 * make it efficient on AVR platform. If these conditions are
	 * not satisfied, you may not use this function or you should
	 * use slower implementation. Note that there is no assertion
	 * on LEDS_X. It may be arbitary. Checking conditions: */
	assert(LEDS_Y == 8);
	assert(LEDS_Z == 8);
	assert(GS_DEPTH == 12);

	/* Backbuffer is bit packed: 2 voxels per 3 bytes when
	 * GS_DEPTH is 12. This calculates bit position efficiently by
	 * using bit shifts. With AVR's 8-bit registers this is
	 * optimized to do first operations with uint8_t's and do the
	 * last shift with uint16_t because it's the only one which
	 * overflows from 8 bit register. */
	const uint16_t bit_pos = 12 * (z | y << 3 | (uint16_t)x << 6);

	/* Byte position is done simply by truncating the last 8 bits
	 * of the data. Variable raw is filled with the data. */
	const uint16_t byte_pos = bit_pos >> 3;
	assert(byte_pos < buffer_len);
	uint16_t raw = (BackBuffer[byte_pos] << 8) | BackBuffer[byte_pos+1];

	/* If 12-bit value starts from the beginning of the data
	 * (bit_pos is dividable by 8) then we put the data starting
	 * from MSB, otherwise we start from MSB - 4 bits. */
	if (bit_pos & 0x7) raw = (raw & 0xf000) | i;
	else raw = (raw & 0x000f) | (i << 4);

	/* Store data back to buffer */
	BackBuffer[byte_pos] = raw >> 8;
	BackBuffer[byte_pos+1] = raw;
}

/**
 * This function plots a 2-dimensional plot of a given function
 */
void effect_2d_plot(plot_func_t f)
{
	const uint16_t gs_mask = (1<<GS_DEPTH) - 1;
	clear_buffer();

	for (uint8_t x=0; x < LEDS_X; x++) {
		for (uint8_t y=0; y < LEDS_Y; y++) {
			// Get the intensity.
			uint16_t i = (*f)(x,y);

			// Check we receive correct intensity
			assert(i <= max_intensity);

			// Do linear interpolation (two voxels per x-y pair)
			uint8_t lower_z = i >> GS_DEPTH;
			uint16_t upper_i = i & gs_mask;
			uint16_t lower_i = gs_mask - upper_i;
			set_led(x,y,lower_z,lower_i);
			set_led(x,y,lower_z+1,upper_i);
		}
	}
}

/**
 * Plot 2-dimensional sine waves which are moving. The parameters are
 * not tuned, this is just taken from my head. */
uint16_t plot_sine(uint8_t x, uint8_t y) {
	const float sine_scaler = (float)max_intensity/4;

	return sine_scaler * (2 + sin((float)x/2+(float)ticks/150) + sin((float)y/2+(float)ticks/300));
}

/**
 * Constant plot. Helps to spot errors in drawing code. Replace this
 * with something more useful in the future.
 */
uint16_t plot_constant(uint8_t x, uint8_t y) {
	return 10000;
}

/**
 * An example how to plot sine with this thingy.
 */
void effect_2d_plot_sine(void) {
	effect_2d_plot(&plot_sine);
}

/**
 * Simple test function which draws voxel layers.
 */
void effect_layers_tester(void)
{
	clear_buffer();
	uint8_t z = (ticks/20 % LEDS_Z);

	for (uint8_t x=0; x<LEDS_X; x++) {
		for (uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, (1<<GS_DEPTH) - 1);
		}
	}
}		

/**
 * Sets all voxels in back buffer as black
 */
void clear_buffer(void) {
	memset(BackBuffer,0,buffer_len);
}
