# pragma FLIP

#include "common.h"

void effect(void)
{
	clear_buffer();

	for (uint8_t x=0; x<8; x++) {
		for (uint8_t y=0; y<8; y++) {
			srand(5*x+y*3);
			set_led(x,y,((ticks >> 3)+rand()) & 7, MAX_INTENSITY >> 3);
		}
	}

	const uint8_t water = (ticks >> 5) & 7;

	for (uint8_t z=0; z<water; z++) {
		for (uint8_t x=0; x<8; x++) {
			for (uint8_t y=0; y<8; y++) {
				set_led(x,y,7-z, MAX_INTENSITY);
			}
		}
	}
}
