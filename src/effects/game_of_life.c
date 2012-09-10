# pragma FLIP

#include "common.h"

static uint8_t get_amount_of_neighbours(uint8_t x, uint8_t y, uint8_t z);
static void set_leds(uint8_t x, uint8_t y, uint8_t z);

void init(void)
{
	// TODO: might want to use some other seed. using heart for testing
	heart_shape();
}

void effect(void) {
	iterate_xyz(set_leds);
}

void set_leds(uint8_t x, uint8_t y, uint8_t z)
{
	uint8_t neighbours = get_amount_of_neighbours((int8_t)x, (int8_t)y, (int8_t)z);

	if(neighbours >= 9 && neighbours <= 14) set_led(x, y, z, MAX_INTENSITY);
	else set_led(x, y, z, 0);
}

static uint8_t get_amount_of_neighbours(uint8_t x, uint8_t y, uint8_t z) {
	uint8_t ret = 0;

	for(int8_t cx = -1; cx <= 1; cx++)
		for(int8_t cy = -1; cy <= 1; cy++)
			for(int8_t cz = -1; cz <= 1; cz++)
				ret += get_led_wrap(x + cx, y + cy, z + cz) > 0;

	return ret;
}
