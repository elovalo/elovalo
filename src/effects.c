/**
 * Led cube effects
 *
 * Not integrated to the main code, yet.
 */

#include <stdint.h>
#include <math.h>
#include "env.h"
#include "effect_utils.h"

/**
 * Plot 2-dimensional sine waves which are moving. The parameters are
 * not tuned, this is just taken from my head. */
TWOD(effect_sine)
{
	const float sine_scaler = (float)MAX_INTENSITY/4;

	return sine_scaler * (2 + sin((float)x/2+(float)ticks/150) + sin((float)y/2+(float)ticks/300));
}

/**
 * Constant plot. Helps to spot errors in drawing code. Replace this
 * with something more useful in the future.
 */
TWOD(effect_constant)
{
	return 10000;
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
