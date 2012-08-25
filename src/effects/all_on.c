#include "common.h"

static void set_intensity(uint8_t x, uint8_t y, uint8_t z);

/*
 * 1000, FLIP
 */
void all_on_init(void)
{
	iterate_3d(set_intensity);
}
static void set_intensity(uint8_t x, uint8_t y, uint8_t z) {
	set_led(x, y, z, MAX_INTENSITY);
}
