/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2012 Elovalo project group 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
# pragma FLIP

#include "common.h"

void effect(void)
{
	// TODO use real kernels instead of loops!
	
	clear_buffer();

	for (uint8_t x=0; x<8; x++) {
		for (uint8_t y=0; y<8; y++) {
			srand(3*(x*LEDS_Y+y));
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
