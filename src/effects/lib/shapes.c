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

#include <math.h>
#include <stdint.h>
#include "utils.h"

static void heart_layer(uint8_t x, uint16_t intensity);

void heart_shape(void)
{
	heart_layer(1, (float)MAX_INTENSITY / 100);
	heart_layer(2, (float)MAX_INTENSITY / 25);
	heart_layer(3, MAX_INTENSITY);
	heart_layer(4, MAX_INTENSITY);
	heart_layer(5, (float)MAX_INTENSITY / 25);
	heart_layer(6, (float)MAX_INTENSITY / 100);
}

static void heart_layer(uint8_t x, uint16_t intensity) {
	set_row(x, 0, 1, 2, intensity);
	set_row(x, 0, 5, 6, intensity);
	set_row(x, 1, 0, 7, intensity);
	set_row(x, 2, 0, 7, intensity);
	set_row(x, 3, 0, 7, intensity);
	set_row(x, 4, 1, 6, intensity);
	set_row(x, 5, 2, 5, intensity);
	set_row(x, 6, 3, 4, intensity);
}

void sphere_shape(float xi, float yi, float zi, float rsq_min, float rsq_max, float fac)
{
	for(float x = xi; x < LEDS_X + xi; x++) {
		for(float y = yi; y < LEDS_Y + yi; y++) {
			for(float z = zi; z < LEDS_Z + zi; z++) {
				float sq = x * x + y * y + z * z;

				if(rsq_min * fac < sq && sq < rsq_max * fac) {
					set_led(x - zi, y - yi, z - zi, MAX_INTENSITY);
				}
			}
		}
	}
}
