#include "common.h"

/*
 * 3000, FLIP
 */
void layers_effect(void)
{
	clear_buffer();
	uint8_t z = ((ticks >> 7) % LEDS_Z);

	for(uint8_t x=0; x<LEDS_X; x++) {
		for(uint8_t y=0; y<LEDS_Y; y++) {
			set_led(x, y, z, MAX_INTENSITY);
		}
	}
}
