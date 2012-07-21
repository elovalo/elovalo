/**
 * Led cube effects
 *
 * Not integrated to the main code, yet.
 */

#include <assert.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include "main.h"
#include "effects.h"

// TODO add timer which increments ticks_volatile!

/* ticks is set to ticks_volatile every time when frame calculation is
 * started. This keeps ticks stable and removes tearing. */
uint16_t ticks = 0; 

/* ticks_volatile is incremented roughly every 1 millisecond and
 * overflows every 64th second. */
volatile uint16_t ticks_volatile = 0;

/* Maximum intensity value is (number_of_rows-1)*(2^intensity_depth)-1
 * = 7*(2^12)-1 */
const int max_intensity = 28671;

/**
 * Sets led intensity. i is the intensity of the LED in range 0..4095.
 */
void set_led(uint8_t x, uint8_t y, uint8_t z, uint16_t i)
{
	/* Assert (on testing environment) that we supply correct
	 * data. */
	assert (x < 8);
	assert (y < 8);
	assert (z < 8);
	assert (i < 4096);

	/* Backbuffer has 12 bit voxels and is packed (2 voxels per 3
	 * bytes). This also assumes that y << 3 never overflows the
	 * 8-bit integer because we asserted that it is not larger
	 * than 7 and 7 << 3 is 56. */
	uint16_t bit_pos = 12 * (((uint16_t)x<<6) + (y<<3) + z);

	/* Byte position is done simply by truncating the last 8 bits
	 * of the data. */
	uint16_t byte_pos = bit_pos >> 3;
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
	clear_buffer();

	for (uint8_t x=0; x<8; x++) {
		for (uint8_t y=0; y<8; y++) {
			// Get the intensity. This has 28676 values.
			uint16_t i = (*f)(x,y);

			// Check we receive correct intensity
			assert(i <= max_intensity);

			// Do linear interpolation (two voxels per x-y pair)
			uint8_t lower_z = i >> 12;
			uint16_t upper_i = i & 0x0fff;
			uint16_t lower_i = 0x0fff - upper_i;
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
	uint8_t z = (ticks/20 % 8);

	for (uint8_t x=0;x<8;x++) {
		for (uint8_t y=0;y<8;y++) {
			set_led(x,y,z,4095);
		}
	}
}		

/**
 * Sets all voxels in back buffer as black
 */
void clear_buffer(void) {
	memset(BackBuffer,0,768);
}
