/**
 * Led cube effects
 *
 * Not integrated to the main code, yet.
 */

#include <stdint.h>
#include <math.h>
#include "effect_utils.h"

/**
 * Plot 2-dimensional sine waves which are moving. The parameters are
 * not tuned, this is just taken from my head. */
uint16_t plot_sine(uint8_t x, uint8_t y)
{
	const float sine_scaler = (float)MAX_INTENSITY/4;

	return sine_scaler * (2 + sin((float)x/2+(float)ticks/150) + sin((float)y/2+(float)ticks/300));
}

/**
 * Constant plot. Helps to spot errors in drawing code. Replace this
 * with something more useful in the future.
 */
uint16_t plot_constant(uint8_t x, uint8_t y)
{
	return 10000;
}
