#include "common.h"

void effect(void) {
	uint8_t a = (ticks >> 5) & 7;
	uint8_t b = 7 - ((ticks >> 5) & 7);

	clear_buffer();
	cube_shape(a, a, a, b, b, b, MAX_INTENSITY);
}
