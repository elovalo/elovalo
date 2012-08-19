/**
 * Led cube effect utilities
 *
 * Not integrated to the main code, yet.
 */

// Disable assertions on embedded environment
#ifdef AVR
#define NDEBUG
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "env.h"
#include "cube.h"
#include "effect_utils.h"

/* If you are changing LED count make sure you are not using set_led
   which is optimized to work only 12-bit depths and when y and z
   dimensions have length of 8. */

/* ticks is set to ticks_volatile every time when frame calculation is
 * started. This keeps ticks stable and removes tearing. */
uint16_t ticks;

/**
 * Sets led intensity. i is the intensity of the LED in range
 * 0..4095. This implementation is AVR optimized and handles only
 * cases where LEDS_X and LEDS_Y are 8, GS_DEPTH is 12, and layer has
 * no padding. Do not call directly, use set_led() instead.
 */
void set_led_8_8_12(uint8_t x, uint8_t y, uint8_t z, uint16_t i)
{
	/* Assert (on testing environment) that we supply correct
	 * data. */
	assert(x < LEDS_X);
	assert(y < LEDS_Y);
	assert(z < LEDS_Z);
	assert(i < (1 << GS_DEPTH));

	/* Cube buffers are bit packed: 2 voxels per 3 bytes when
	 * GS_DEPTH is 12. This calculates bit position efficiently by
	 * using bit shifts. With AVR's 8-bit registers this is
	 * optimized to do first operations with uint8_t's and do the
	 * last shift with uint16_t because it's the only one which
	 * overflows from 8 bit register. */
	const uint16_t bit_pos = 12 * (x | y << 3 | (uint16_t)z << 6);

	/* Byte position is done simply by truncating the last 8 bits
	 * of the data. Variable raw is filled with the data. */
	const uint16_t byte_pos = bit_pos >> 3;
	assert(byte_pos < GS_BUF_BYTES);
	uint16_t raw = (gs_buf_back[byte_pos] << 8) | gs_buf_back[byte_pos+1];

	/* If 12-bit value starts from the beginning of the data
	 * (bit_pos is dividable by 8) then we put the data starting
	 * from MSB, otherwise we start from MSB - 4 bits. */
	if (bit_pos & 0x7) raw = (raw & 0xf000) | i;
	else raw = (raw & 0x000f) | (i << 4);

	/* Store data back to buffer */
	gs_buf_back[byte_pos] = raw >> 8;
	gs_buf_back[byte_pos+1] = raw;
}

/**
 * Gets led intensity from front buffer. Returns intensity of a LED in
 * range 0..4095.  This implementation is AVR optimized and handles
 * only cases where LEDS_X and LEDS_Y are 8, GS_DEPTH is 12, and layer
 * has no padding. Do not call directly, use get_led() instead.
 */
uint16_t get_led_8_8_12(uint8_t x, uint8_t y, uint8_t z)
{
	/* Assert (on testing environment) that we supply correct
	 * data. */
	assert(x < LEDS_X);
	assert(y < LEDS_Y);
	assert(z < LEDS_Z);

	/* Cube buffers are bit packed: 2 voxels per 3 bytes when
	 * GS_DEPTH is 12. This calculates bit position efficiently by
	 * using bit shifts. With AVR's 8-bit registers this is
	 * optimized to do first operations with uint8_t's and do the
	 * last shift with uint16_t because it's the only one which
	 * overflows from 8 bit register. */
	const uint16_t bit_pos = 12 * (x | y << 3 | (uint16_t)z << 6);

	/* Byte position is done simply by truncating the last 8 bits
	 * of the data. Variable raw is filled with the data. */
	const uint16_t byte_pos = bit_pos >> 3;
	assert(byte_pos < GS_BUF_BYTES);
	uint16_t raw = (gs_buf_front[byte_pos] << 8) | gs_buf_front[byte_pos+1];

	/* If 12-bit value starts from the beginning of the data
	 * (bit_pos is dividable by 8) then we get data starting from
	 * MSB, otherwise we get from MSB - 4 bits. */
	return (bit_pos & 0x7) ? raw & 0x0fff : raw >> 4;
}

/**
 * This function plots a 2-dimensional plot of a given function
 */
void effect_2d_plot(plot_2d_t f)
{
	const uint16_t gs_mask = (1<<GS_DEPTH) - 1;
	clear_buffer();

	for (uint8_t x=0; x < LEDS_X; x++) {
		for (uint8_t y=0; y < LEDS_Y; y++) {
			// Get the intensity.
			uint16_t i = (*f)(x,y);

			// Check we receive correct intensity
			assert(i <= MAX_2D_PLOT_INTENSITY);

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
 * Sets all voxels in back buffer as black
 */
void clear_buffer(void)
{
	memset(gs_buf_back,0,GS_BUF_BYTES);
}

uint8_t clamp(uint8_t x, uint8_t a, uint8_t b)
{
	return x < a ? a : (x > b ? b : x);
}

uint8_t randint(uint8_t min, uint8_t max)
{
    // not optimal always, math needs checking
	return (rand() % max) + min;
}
