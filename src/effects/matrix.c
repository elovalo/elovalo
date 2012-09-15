/*
 *  Copyright 2012 Elovalo project group 
 *  
 *  This file is part of Elovalo.
 *  
 *  Elovalo is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Elovalo is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Elovalo.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "common.h"

typedef struct {
	uint8_t x;
	uint8_t y;
	uint8_t z;
} xyz_t;

const uint8_t xy_pairs_len = 10;
xyz_t xy_pairs[10];

void init(void)
{
	for(uint8_t i = 0; i < xy_pairs_len; i++) {
		xy_pairs[i] = (xyz_t){
			.x = randint(0, LEDS_X),
			.y = randint(0, LEDS_Y),
			.z = randint(0, LEDS_Z)
		};
	}

	clear_buffer();
}
void effect(void)
{
	clear_buffer();

	for(uint8_t i = 0; i < xy_pairs_len; i++) {
		xyz_t xyz = xy_pairs[i];

		set_led(xyz.x, xyz.y, xyz.z, MAX_INTENSITY);

		uint8_t z = xyz.z;

		z++;

		if(z >= LEDS_Z) z = 0;

		xy_pairs[i].z = z;
	}
}
