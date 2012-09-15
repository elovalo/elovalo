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

#pragma FLIP

#include "common.h"

const uint8_t matrix_xyz_len = 10;
xyz_t matrix_xyz[10];

void init(void)
{
	for(uint8_t i = 0; i < matrix_xyz_len; i++) {
		matrix_xyz[i] = (xyz_t){
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

	for(uint8_t i = 0; i < matrix_xyz_len; i++) {
		xyz_t xyz = matrix_xyz[i];

		for(uint8_t j = 0; j < 3; j++) {
			if(xyz.z + j < LEDS_Z) {
				set_led(xyz.x, xyz.y, xyz.z + j, MAX_INTENSITY);
			}
		}

		uint8_t z = xyz.z;

		z++;

		if(z >= LEDS_Z) z = 0;

		matrix_xyz[i].z = z;
	}
}
