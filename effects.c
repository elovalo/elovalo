/**
 * Led cube effects
 *
 * Not integrated to the main code, yet.
 */

#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include "main.h"
#include "effects.h"

// TODO add timer which incredents ticks!
uint16_t ticks = 0; /* This variable is incremented roughly every 1
		       millisecond and overflows every 64th second. */
volatile uint16_t ticks_volatile = 0;

// BackBuffer contains the buffer to be drawn (should be lowerCase!!!)

/* Maximum intensity value is (number_of_rows-1)*(2^intensity_depth)-1
 * = 7*(2^12)-1 */
const int max_intensity = 28671;

/**
 * Sets led intensity. i is the intensity of the LED in range 0..4095.
 */
void set_led(int x, int y, int z, int i)
{
	// TODO x, y and z should be uint8s

	// TODO do these checks as assertions

	// Drop pixel which are out of scope or too bright
	if (x < 0 || x >= 8 || y < 0 || y >= 8 || z < 0 || z >= 8 || i < 0 || i >= 4096) {
		// Go mad
		printf("Invalid pixel coordinate (%d,%d,%d) @ %d\n",x,y,z,i);
	}

	/* Backbuffer has 12 bit voxels and is packed (2 voxels per 3
	 * bytes) */
	int bit_pos = 12 * ((x<<6) + (y<<3) + z);

	int byte_pos = bit_pos >> 3;
	uint16_t raw = (BackBuffer[byte_pos] << 8) | BackBuffer[byte_pos+1];

	// Update table
	if (bit_pos & 0x7) raw = (raw & 0xf000) | i;
	else raw = (raw & 0x000f) | (i << 4);

	// Store data back to buffer
	BackBuffer[byte_pos] = raw >> 8;
	BackBuffer[byte_pos+1] = raw;
}

/**
 * This function plots a 2-dimensional plot of a given function
 */
void effect_2d_plot(plot_func_t f)
{
	clear_buffer();

	for (int x=0; x<8; x++) {
		for (int y=0; y<8; y++) {
			// Get the intensity. This has 28676 values.
			int i = (*f)(x,y);

			// TODO as assertions, currently printf shit
			if (i < 0 || i > max_intensity) {
				printf("Invalid intensity %d at (%d,%d)\n",i,x,y);
			}

			// Do linear interpolation (two voxels per x-y pair)
			int lower_z = i >> 12;
			int upper_i = i & 0x0fff;
			int lower_i = 0x0fff - upper_i;
			set_led(x,y,lower_z,lower_i);
			set_led(x,y,lower_z+1,upper_i);
		}
	}
}

/**
 * Plot 2-dimensional sine waves which are moving. The parameters are
 * not tuned, this is just taken from my head. */
int plot_sine(int x, int y) {
	const int sine_scaler = max_intensity/4;

	return sine_scaler * (2 + sin((float)x/2+(float)ticks/150) + sin((float)y/2+(float)ticks/300));
}

int plot_constant(int x, int y) {
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

	for (int x=0;x<8;x++) {
		for (int y=0;y<8;y++) {
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
