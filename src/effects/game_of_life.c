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
# pragma MAX_FPS 10

#include "common.h"

static uint8_t get_amount_of_neighbours(uint8_t x, uint8_t y, uint8_t z);
static void set_leds(uint8_t x, uint8_t y, uint8_t z);

struct {
	uint8_t is_alive;
} vars;

void init(void)
{
	// TODO: might want to use some other seed. using heart for testing
	heart_shape();
}

void effect(void) {
	vars.is_alive = 0;
	iterate_xyz(set_leds);

	if(!vars.is_alive) {
		heart_shape();
	}
}

void set_leds(uint8_t x, uint8_t y, uint8_t z)
{
	uint8_t neighbours = get_amount_of_neighbours((int8_t)x, (int8_t)y, (int8_t)z);

	if((neighbours >= 6 && neighbours <= 15) || randint(0, 10) > 8) {
		set_led(x, y, z, MAX_INTENSITY);
		vars.is_alive = 1;
	}
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
