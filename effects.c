/**
 * Led cube effects
 *
 * Not integrated to the main code, yet.
 *
 * NB. Do not expect this to compile!
 */

#include "main.h"


// TODO some variables which are constants now...
int frame_counter = 0; /* This variable should be some kind of time
			  counter, eg. instruction counter */

// BackBuffer contains the buffer to be drawn (should be lowerCase!!!)

/* 2D plotting function. Takes frame number, x coordinate, y
 * coordinate, and returns intensity value from 0 to
 * max_intensity. May have multiple definitions. */
typedef int(*plot_func_t)(int,int,int);

/* Maximum intensity value is (number_of_rows-1)*(2^intensity_depth)-1
 * = 7*(2^12)-1 */
const int max_intensity = 28671;

/**
 * Sets led intensity. i is the intensity of the LED in range 0..4095.
 */
void set_led(int x, int y, int z, int i)
{
	/* Backbuffer has 12 bit voxels and is packed (2 voxels per 3
	 * bytes) */
	int voxelIx = 12*12*12*x + 12*12*y + 12*z;

	uint16 *raw = BackBuffer + (voxelIx / 8);

	// Protect from overflows. Comment out if you need to save CPU cycles.
	if (i < 0) i = 0;
	else if (i > 4095) i = 4095;

	// Update tablee
	if (voxelIx % 8) *raw = *raw & 0xf000 | i;
	else *raw = *raw & 0x000f | i << 4;
}

/**
 * This function plots a 2-dimensional plot of a given function
 */
void effect_2d_plot(plot_func_t f)
{
	for (int x=0; x<8; x++) {
		for (int y=0; y<8; y++) {
			// Get the intensity. This has 28676 values.
			int i = (*f)(frame_counter,x,y);
			// Do linear interpolation (two voxels per x-y pair)
			int lower_z = i / 4096;
			int lower_i = i % 4096;
			int upper_i = 4095-i;
			set_led(x,y,lower_z,lower_i);
			set_led(x,y,lower_z+1,upper_i);
		}
	}
}

/**
 * Plot 2-dimensional sine waves which are moving. The parameters are
 * not tuned, this is just taken from my head. */
int plot_sine(int t, int x, int y) {
	const int sine_scaler = max_intensity/2-1;
	return sine_scaler * (1 + sin(x+t) + sin(y+t/2));
}

/**
 * An example how to plot sine with this thingy.
 */
void effect_2d_plot_sine(void) {
	effect_2d_plot(&plot_sine);
}
