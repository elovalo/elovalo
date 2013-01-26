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
	if ((uint16_t)((float)ticks / 13.392857142857142) % 4) {
		clear_buffer();
		cube_shape(0, 0, 0, 7, 7, 7, MAX_INTENSITY);
	} else {
		for (uint8_t x=0; x<LEDS_X; x++) {
			for (uint8_t y=0; y<LEDS_Y; y++) {
				for (uint8_t z=0; z<LEDS_Z; z++) {
					set_led(x,y,z,MAX_INTENSITY);
				}
			}
		}
	}
}
