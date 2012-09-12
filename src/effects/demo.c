# pragma FLIP

#include "common.h"

void effect(void)
{
	// TODO use real kernels instead of loops!
	
	clear_buffer();

	for (uint8_t x=0; x<8; x++) {
		for (uint8_t y=0; y<8; y++) {
			srand(x*LEDS_Y+y);
			set_led(x,y,((ticks >> 3)+rand()) & 7, MAX_INTENSITY >> 3);
		}
	}

	const uint8_t water = ((ticks >> 7) % 7) + 1;
	const uint8_t surface = ticks & 127;

	for (uint8_t z=0; z<water; z++) {
		for (uint8_t x=0; x<8; x++) {
			for (uint8_t y=0; y<8; y++) {
				set_led(x,y,7-z, MAX_INTENSITY);
			}
		}
	}

	for (uint8_t x=0; x<8; x++) {
		for (uint8_t y=0; y<8; y++) {
			set_led(x,y,8-water, surface << 5);
		}
	}
}
